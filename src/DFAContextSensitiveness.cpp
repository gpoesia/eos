#include "DFAContextSensitiveness.h"
#include "ContextDFA.h"
#include "ContextSensitiveOptimizationDescription.h"

#include <string>
#include <set>

#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "DFAContextSensitiveness"

using namespace llvm;

namespace eos {

namespace {

BasicBlock *CreateCaseForState(Function *SmFeed, Argument &SmInput,
                               GlobalVariable *SmState, BasicBlock *Default,
                               const Context &C,
                               const ContextDFA::ContextInfo &CI,
                               const ContextDFA &DFA) {
  BasicBlock *StateBB =
      BasicBlock::Create(SmFeed->getContext(),
                         ("__sm_feed_case_" + std::to_string(CI.ID)), SmFeed);
  SwitchInst *StateSwitch =
      SwitchInst::Create(&SmInput, Default, CI.Transitions.size(), StateBB);
  LLVMContext &LLVMCtx = SmFeed->getContext();
  IntegerType *Int64Ty = Type::getInt64Ty(LLVMCtx);

  for (auto TransitionIt = DFA.transitionsBegin(C), End = DFA.transitionsEnd(C);
       TransitionIt != End; ++TransitionIt) {
    auto IDTInfoPair = *TransitionIt;

    Function *Target = IDTInfoPair.second->Target;
    // Returns are handled on call sites.
    if (!Target) {
      continue;
    }

    BasicBlock *TransitionBB = BasicBlock::Create(
        LLVMCtx, ("__sm_feed_case_" + std::to_string(CI.ID) + "_tr_" +
                  std::to_string(IDTInfoPair.first)),
        SmFeed);
    // TransitionBB will contain two instructions:
    // 1- a StoreInst to store the new state in SmState.
    if (!IDTInfoPair.second) {
      errs() << "Error: null TransitionInfo: State " << CI.ID << " ("
             << C.getFunction()->getName() << "), Transition "
             << IDTInfoPair.first << "\n";
      assert(false);
    }

    new StoreInst(ConstantInt::get(Int64Ty, IDTInfoPair.second->DestinationID),
                  SmState, TransitionBB);
    // 2- a ReturnInst that returns the correct function to be called in the
    // current context, according to the optimization.
    ReturnInst::Create(LLVMCtx,
                       Target ? dyn_cast<Value>(CastInst::CreatePointerCast(
                                    Target, Int64Ty, "fptr_cast", TransitionBB))
                              : dyn_cast<Value>(ConstantInt::get(Int64Ty, 0)),
                       TransitionBB);

    StateSwitch->addCase(ConstantInt::get(Int64Ty, IDTInfoPair.first),
                         TransitionBB);
  }

  return StateBB;
}

Function *GenerateSmFeed(const ContextDFA &DFA,
                         const ContextSensitiveOptimizationDescription &Desc,
                         Module &M,
                         GlobalVariable *SmState) {
  IntegerType *Int64Ty = Type::getInt64Ty(M.getContext());
  // SmFeed's FunctionType:
  FunctionType *T = FunctionType::get(
      Int64Ty, // returns a pointer to the correct function.
      Int64Ty, // receives a single parameter: the transition ID
      false);  // is not varArg.
  Function *SmFeed = Function::Create(
      T, GlobalValue::LinkageTypes::ExternalLinkage, "__sm_feed", &M);

  // SmFeed takes a single parameter: the transition ID, input to the SM.
  Argument &SmInput = *SmFeed->arg_begin();

  // SmFeed will have the following IR structure:
  // (1) The entry basic block has a single instruction: a SwitchInst
  //     that operates on SmState
  // (2) Each branch of the SwitchInst in (1) is another basic block with
  //     a single SwitchInst, which switches on SmInput.
  // (3) Each branch of the switches in (2) is a basic block with the following
  //     structure:
  //     i   - StoreInst that saves the new state in SmState
  //     ii  - ReturnInst that returns the address of the function to be called.
  // Because of dependencies, we need to create SmFeed backwards: (3), then (2)
  // and then (1).

  // (1) Creates entry basic block, its SwitchInst, and the default basic block.
  BasicBlock *Entry =
      BasicBlock::Create(M.getContext(), "__sm_feed_entry", SmFeed);
  BasicBlock *Default =
      BasicBlock::Create(M.getContext(), "__sm_feed_default", SmFeed);
  Constant *NullPtr = ConstantInt::get(Int64Ty, 0);
  ReturnInst::Create(M.getContext(), NullPtr, Default);
  LoadInst *SmStateLoad = new LoadInst(SmState, "__sm_state_load", Entry);
  SwitchInst *EntrySwitch =
      SwitchInst::Create(SmStateLoad, Default, DFA.getNumberOfStates(), Entry);
  // (2) Create case for each context in the DFA.
  for (auto &C : DFA) {
    BasicBlock *CaseBB = CreateCaseForState(SmFeed, SmInput, SmState, Default,
                                            C.first, C.second, DFA);
    EntrySwitch->addCase(ConstantInt::get(Int64Ty, C.second.ID), CaseBB);
  }

  // Adds return at the end.
  BasicBlock *EndBB =
      BasicBlock::Create(M.getContext(), "__sm_feed_ret", SmFeed);
  ReturnInst::Create(M.getContext(), NullPtr, EndBB);

  return SmFeed;
}

void ModifyFunctionCalls(const ContextDFA &DFA, llvm::Function *F,
                         DFAConstructionContext &DFAContext,
                         llvm::Function *SmFeed, IntegerType *Int64Ty,
                         GlobalVariable *SmState,
                         bool IsMain = false) {
  LLVM_DEBUG(errs() << "Modifying " << F->getName() << "\n");
  for (BasicBlock &BB : *F) {
    for (auto InstIt = BB.begin(), End = BB.end(); InstIt != End; ++InstIt) {
      if (auto Call = dyn_cast<CallInst>(&*InstIt)) {
        unsigned CallID =
            DFA.getCallID(IsMain ? Context::MAIN_FUNCTION
                                 : DFAContext.getCanonicalFunction(F),
                          DFAContext.getCanonicalCall(Call));
        LLVM_DEBUG(errs() << "Call " << Call << " ("
                     << DFAContext.getCanonicalCall(Call)
                     << ") has CallID = " << CallID << "\n");
        if (CallID != ContextDFA::UNINITIALIZED_ID) {
          LLVM_DEBUG(errs() << "Before: " << BB << "\n");

          // This call is a transition for some states in the context DFA.
          // It will be replaced by four instructions:
          // 1- Save the previous state in a local variable
          LoadInst *StateBefore = new LoadInst(SmState, "load_sm_state", Call);

          // 2- call SmFeed(CallID): get a pointer to the function,
          CallInst *SmFeedCall1 =
              CallInst::Create(SmFeed, ConstantInt::get(Int64Ty, CallID),
                               "__sm_feed(CallID)", Call);

          // 3- call pointer received in (1)
          CastInst *PtrCast = CastInst::CreateBitOrPointerCast(
              SmFeedCall1, Call->getCalledFunction()->getType(),
              "__sm_feed-return-cast", Call);
          Call->setCalledFunction(Call->getCalledFunction()->getFunctionType(), PtrCast);

          // 4- Restore previous state.
          auto Next = std::next(InstIt);
          if (Next == BB.end()) {
            new StoreInst(StateBefore, SmState, &BB);
          } else {
            new StoreInst(StateBefore, SmState, &*Next);
          }

          LLVM_DEBUG(errs() << "After: " << BB << "\n");
        }
      }
    }
  }
}

void ModifyFunctionCalls(
    const ContextDFA &DFA,
    llvm::Module &M,
    DFAConstructionContext &DFAContext,
    llvm::Function *SmFeed,
    GlobalVariable *SmState,
    const ContextSensitiveOptimizationDescription &Desc) {
  IntegerType *Int64Ty = Type::getInt64Ty(M.getContext());

  for (auto FunctionIt = DFAContext.contextSensitiveFunctionsBegin(),
            End = DFAContext.contextSensitiveFunctionsEnd();
       FunctionIt != End; ++FunctionIt) {
    ModifyFunctionCalls(DFA, FunctionIt->second, DFAContext, SmFeed, Int64Ty, SmState);
  }

  std::set<const llvm::Function *> OptimizedFunctions;
  for (auto ContextIt = Desc.begin(), End = Desc.end();
       ContextIt != End; ++ContextIt) {
    if (OptimizedFunctions.insert(ContextIt->second).second) {
      ModifyFunctionCalls(DFA, ContextIt->second, DFAContext, SmFeed, Int64Ty, SmState);
    }
  }

  ModifyFunctionCalls(DFA, M.getFunction("main"), DFAContext, SmFeed, Int64Ty, SmState,
                      true);
}


void InsertInlineDFATransitions(const ContextDFA &DFA, llvm::Function *F,
                                DFAConstructionContext &DFAContext,
                                llvm::LLVMContext &LLVMCtx,
                                IntegerType *Int64Ty,
                                GlobalVariable *SmState,
                                bool IsMain = false) {
  LLVM_DEBUG(errs() << "Modifying " << F->getName() << "\n");

  std::set<unsigned> ModifiedCalls;

  for (BasicBlock &BB : *F) {
    for (auto InstIt = BB.begin(), End = BB.end(); InstIt != End; ++InstIt) {
      if (auto Call = dyn_cast<CallInst>(&*InstIt)) {
        auto CanonicalCall = DFAContext.getCanonicalCall(Call);
        unsigned CallID =
            DFA.getCallID(IsMain ? Context::MAIN_FUNCTION
                                 : DFAContext.getCanonicalFunction(F),
                          CanonicalCall);

        if (CallID != ContextDFA::UNINITIALIZED_ID && ModifiedCalls.count(CallID) == 0) {
          ModifiedCalls.insert(CallID);

          // This call is a transition for some states in the context DFA.
          // It will be replaced by four instructions:
          // (1) Save the previous state in a local variable
          LoadInst *StateBefore = new LoadInst(SmState, "load_sm_state", Call);

          // Get all possible states.
          std::vector<const ContextDFA::ContextInfo *> PossibleStates;

          for (auto StatesIt = DFA.possibleSourceStatesBegin(CanonicalCall),
                 StatesEnd = DFA.possibleSourceStatesEnd(CanonicalCall);
               StatesIt != StatesEnd; ++StatesIt) {
            PossibleStates.push_back(&*StatesIt);
          }

          // (2) Switch on the possible states at this point of the program.
          // (inserted below).

          // (3) Capture return value in a phi node.
          PHINode *CallTarget = PHINode::Create(PointerType::getUnqual(Call->getFunctionType()),
                                                PossibleStates.size() + 1,
                                                "", Call);
          BasicBlock *CallSiteBB = BB.splitBasicBlock(CallTarget, "call_site");

          CallTarget->addIncoming(Call->getCalledFunction(), &BB);

          // Remove automatically inserted unconditional branch.
          BB.getInstList().pop_back();

          // Switch (2) (inserted *before* ReturnValue).
          SwitchInst *StateSwitch =
            SwitchInst::Create(StateBefore, CallSiteBB, PossibleStates.size(), &BB);

          for (const ContextDFA::ContextInfo *CI : PossibleStates) {
            BasicBlock *StateBB = BasicBlock::Create(LLVMCtx, "", F);
            auto TransitionIt = CI->Transitions.find(CallID);
            assert(TransitionIt != CI->Transitions.end() &&
                   "Impossible state for CallInst");
            unsigned TargetState = TransitionIt->second.DestinationID;
            llvm::Function *TargetFunction = TransitionIt->second.Target;

            // 2a- Store new state.
            new StoreInst(ConstantInt::get(Int64Ty, TargetState),
                          SmState,
                          StateBB);
            // 2b - Get target address.
            CallTarget->addIncoming(TargetFunction, StateBB);

            // 2c - Jump to call site.
            BranchInst::Create(CallSiteBB, StateBB);

            StateSwitch->addCase(ConstantInt::get(Int64Ty, CI->ID),
                                 StateBB);

            StateBB->moveBefore(CallSiteBB);
          }

          // 4- Restore previous state. (skip two instructions: phi and call).
          auto Next = std::next(std::next(CallSiteBB->begin()));
          if (Next == CallSiteBB->end()) {
            new StoreInst(StateBefore, SmState, CallSiteBB);
          } else {
            new StoreInst(StateBefore, SmState, &*Next);
          }

          Call->setCalledFunction(Call->getFunctionType(), CallTarget);

          // Because the Basic Block was split, we cannot (and do not need to)
          // keep iterating over its instructions. Not only the iterator was
          // invalidated, but also the next instructions of interest were now
          // moved to a different basic block.
          break;
        }
      }
    }
  }
}

void InsertInlineDFATransitions(
    const ContextDFA &DFA,
    llvm::Module &M,
    DFAConstructionContext &DFAContext,
    GlobalVariable *SmState,
    const ContextSensitiveOptimizationDescription &Desc) {
  IntegerType *Int64Ty = Type::getInt64Ty(M.getContext());
  LLVMContext &LLVMCtx = M.getContext();

  for (auto FunctionIt = DFAContext.contextSensitiveFunctionsBegin(),
            End = DFAContext.contextSensitiveFunctionsEnd();
       FunctionIt != End; ++FunctionIt) {
    InsertInlineDFATransitions(DFA, FunctionIt->second, DFAContext, LLVMCtx,
                               Int64Ty, SmState);
  }

  std::set<const llvm::Function *> OptimizedFunctions;
  for (auto ContextIt = Desc.begin(), End = Desc.end();
       ContextIt != End; ++ContextIt) {
    if (OptimizedFunctions.insert(ContextIt->second).second) {
      InsertInlineDFATransitions(DFA, ContextIt->second, DFAContext, LLVMCtx,
                                 Int64Ty, SmState);
    }
  }

  InsertInlineDFATransitions(DFA, M.getFunction("main"), DFAContext, LLVMCtx,
                             Int64Ty, SmState, true);
}

} // namespace

size_t ApplyContextSensitiveOptimization(const ContextSensitiveOptimizationDescription &Desc,
                                       llvm::Module &M) {
  std::map<llvm::CallInst *, llvm::CallInst *> CanonicalCalls;
  return ApplyContextSensitiveOptimization(Desc, M);
}

size_t ApplyContextSensitiveOptimization(
  const ContextSensitiveOptimizationDescription &Desc,
  llvm::Module &M,
  const std::map<llvm::CallInst *, llvm::CallInst *> &CanonicalCalls) {
  DFAConstructionContext DFAContext(M);
  DFAContext.mergeCanonicalCallsMap(CanonicalCalls);

  const ContextDFA &DFA = ContextDFA::build(Desc, DFAContext);
   // The state of the State Machine is maintained in a global variable.
  // The llvm::Module takes ownership.
  IntegerType *Int64Ty = Type::getInt64Ty(M.getContext());
  GlobalVariable *SmState =
      new GlobalVariable(M,
                         Int64Ty,
                         false,   // isConstant
                         GlobalValue::LinkageTypes::CommonLinkage,
                         ConstantInt::get(Int64Ty, 0), // initializer
                         "__sm_state");

  bool Inline = true;

  if (Inline) {
    InsertInlineDFATransitions(DFA, M, DFAContext, SmState, Desc);
  } else {
    llvm::Function *SmFeed = GenerateSmFeed(DFA, Desc, M, SmState);
    ModifyFunctionCalls(DFA, M, DFAContext, SmFeed, SmState, Desc);
  }

  return DFA.getNumberOfStates();
}

} // namespace eos
