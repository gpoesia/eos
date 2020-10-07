#include "ContextDFA.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#define DEBUG_TYPE "ContextDFA"

namespace eos {

std::map<const llvm::Function *, llvm::Function *>::const_iterator
DFAConstructionContext::contextSensitiveFunctionsBegin() const {
  return ContextSensitiveVersion.begin();
}

std::map<const llvm::Function *, llvm::Function *>::const_iterator
DFAConstructionContext::contextSensitiveFunctionsEnd() const {
  return ContextSensitiveVersion.end();
}

void DFAConstructionContext::mergeCanonicalCallsMap(
    const std::map<llvm::CallInst *, llvm::CallInst *> &ExtraCanonicalCalls) {
  for (auto It = ExtraCanonicalCalls.begin(), End = ExtraCanonicalCalls.end();
       It != End; ++It) {
    CanonicalCall[It->first] = It->second;
  }
}


const llvm::CallInst *DFAConstructionContext::getCanonicalCall(const llvm::CallInst *Call) {
  auto CanonicalCallIt = CanonicalCall.find(Call);
  if (CanonicalCallIt != CanonicalCall.end()) {
    return CanonicalCallIt->second;
  }
  return Call;
}

llvm::Function *
DFAConstructionContext::getCanonicalFunction(llvm::Function *F) {
  auto CanonicalFunctionIt = CanonicalFunction.find(F);
  if (CanonicalFunctionIt != CanonicalFunction.end()) {
    return CanonicalFunctionIt->second;
  }
  return F;
}

llvm::Function *
DFAConstructionContext::getContextSensitiveVersion(llvm::Function *F) {
  auto CSVIt = ContextSensitiveVersion.find(F);
  if (CSVIt == ContextSensitiveVersion.end()) {
    llvm::ValueToValueMapTy VMap;
    llvm::Function *Clone = llvm::CloneFunction(F, VMap, nullptr);
    Clone->setName("__ctx_s_" + F->getName());
    mapClonedCalls(Clone, F);
    CanonicalFunction[Clone] = F;
    return ContextSensitiveVersion[F] = Clone;
  } else {
    return CSVIt->second;
  }
}

void DFAConstructionContext::mapClonedCalls(llvm::Function *Clone,
                                            llvm::Function *Original) {
  for (auto BBCloneIt = Clone->begin(), BBOrigIt = Original->begin(),
            End = Clone->end();
       BBCloneIt != End; ++BBOrigIt, ++BBCloneIt) {
    for (auto CloneInstIt = BBCloneIt->begin(), OrigInstIt = BBOrigIt->begin(),
              End = BBCloneIt->end();
         CloneInstIt != End; ++CloneInstIt, ++OrigInstIt) {
      if (llvm::CallInst *Call =
              llvm::dyn_cast<llvm::CallInst>(&*CloneInstIt)) {
        llvm::CallInst *OriginalCall;
        assert((OriginalCall = llvm::dyn_cast<llvm::CallInst>(&*OrigInstIt)) &&
               "Clone is not an exact copy of the original function!");
        CanonicalCall[Call] = OriginalCall;
        LLVM_DEBUG(llvm::errs() << Clone->getName() << ": CanonicalCall[" << Call
                                << "] = " << OriginalCall << "("
                                << Original->getName() << ")\n");
      }
    }
  }
}

const unsigned ContextDFA::UNINITIALIZED_ID = -1U;

const ContextDFA::TransitionInfo ContextDFA::UNINITIALIZED_TRANSITION{
    UNINITIALIZED_ID, nullptr};

void ContextDFA::addTransition(const Context &Src, const llvm::CallInst *Call,
                               llvm::Function *Target,
                               bool ContextSensitiveTarget,
                               DFAConstructionContext &C) {
  ContextInfo &SrcInfo = getContextInfo(Src);

  Context Destination{Src};
  Destination.addCall(Call);
  ContextInfo &DestInfo = getContextInfo(Destination);

  unsigned CallID = getOrCreateCallID(Src.getFunction(), Call);

  llvm::Function *TargetImplementation =
      ContextSensitiveTarget ? C.getContextSensitiveVersion(Target) : Target;

  // Call transition.
  SrcInfo.Transitions.insert(std::make_pair(
      CallID, TransitionInfo{DestInfo.ID, TargetImplementation}));
  // Ret transition.
  DestInfo.Transitions.insert(std::make_pair(
      getReturnTransitionID(Target), TransitionInfo{SrcInfo.ID, nullptr}));

  // When optimizing a call, remember to add a proper return to the original
  // target as well. Otherwise, `Destination` may have no associated return
  // transition ID.
  if (Target != Call->getCalledFunction()) {
    getReturnTransitionID(Call->getCalledFunction());
  }

  PossibleSourceStates[C.getCanonicalCall(Call)].insert(SrcInfo.ID);
}

unsigned ContextDFA::getOrCreateCallID(const llvm::Function *F,
                                       const llvm::CallInst *Call) {
  LLVM_DEBUG(llvm::errs() << "getCallID " << F << " " << Call << "\n");
  auto It = CallID.find(F);
  if (It == CallID.end()) {
    IDToCall[F][1] = Call;
    return CallID[F][Call] = 1;
  } else {
    bool HasReturn = IDToCall[F].count(0);
    unsigned ID = It->second.insert(std::make_pair(Call, It->second.size() - HasReturn + 1))
        .first->second;
    IDToCall[F].insert(std::make_pair(ID, Call));
    return ID;
  }
}

unsigned ContextDFA::getCallID(const llvm::Function *F,
                               const llvm::CallInst *Call) const {
  LLVM_DEBUG(llvm::errs() << "getCallID " << F << " " << Call << "\n");

  auto It = CallID.find(F);
  if (It == CallID.end()) {
    return UNINITIALIZED_ID;
  } else {
    auto CallIt = It->second.find(Call);
    if (CallIt == It->second.end()) {
      return UNINITIALIZED_ID;
    }
    return CallIt->second;
  }
}

const ContextDFA::TransitionInfo &
ContextDFA::getTransitionInfo(const Context &C,
                              const llvm::CallInst *Call) const {
  auto CInfoIt = CI.find(C);
  if (CInfoIt == CI.end()) {
    return ContextDFA::UNINITIALIZED_TRANSITION;
  }

  unsigned CallID = getCallID(C.getFunction(), Call);

  if (CallID == UNINITIALIZED_ID) {
    return UNINITIALIZED_TRANSITION;
  }

  auto TransitionsIt = CInfoIt->second.Transitions.find(CallID);

  if (TransitionsIt == CInfoIt->second.Transitions.end()) {
    return UNINITIALIZED_TRANSITION;
  }

  return TransitionsIt->second;
}

void ContextDFA::createContextInsensitiveTransitions() {
  for (auto &ContextCInfoPair : CI) {
    const auto &StateContext = ContextCInfoPair.first;
    auto ContextID = ContextCInfoPair.second.ID;

    LLVM_DEBUG(llvm::errs() << "createContextInsensitiveTransitions ID: " << ContextID << "\n");

    bool HasReturnOnlyState = CorrespondingReturnOnlyState.count(ContextID);

    if (!ContextCInfoPair.second.isReturnOnly) {
      // For each missing transition, add a corresponding context-insensitive
      // call (this transition does not belong to any optimization path).
      for (auto TransitionIt = transitionsBegin(StateContext),
             End = transitionsEnd(StateContext);
           TransitionIt != End; ++TransitionIt) {
        auto IDTInfoPair = *TransitionIt;
        if (!IDTInfoPair.second) {
          // Ensure the context has a corresponding return-only state.
          if (!HasReturnOnlyState) {
            Context CorrespondingROContext{ContextCInfoPair.first};
            CorrespondingROContext.addCall(nullptr);
            unsigned NextID = getNextContextID();
            CorrespondingReturnOnlyState[ContextID] = NextID;
            CI.insert(std::make_pair(CorrespondingROContext,
                                     ContextInfo{
                                       NextID,
                                         {{getReturnTransitionID(nullptr),
                                               TransitionInfo{ContextID, nullptr}}},
                                         true,
                                           }));
            HasReturnOnlyState = true;
            LLVM_DEBUG(llvm::errs() << "Created " << NextID << "\n");
          }

          const llvm::CallInst *Call = getCall(StateContext.getFunction(),
                                               IDTInfoPair.first);
          ContextCInfoPair.second.Transitions[IDTInfoPair.first] =
            TransitionInfo {CorrespondingReturnOnlyState[ContextID],
                            Call->getCalledFunction()};
        }
      }
    }
  }
}


const llvm::CallInst *ContextDFA::getCall(const llvm::Function *F, unsigned ID) const {
  auto It = IDToCall.find(F);
  if (It == IDToCall.end()) {
    return nullptr;
  }

  auto CallIt = It->second.find(ID);
  if (CallIt == It->second.end()) {
    return nullptr;
  }

  return CallIt->second;
}

ContextDFA ContextDFA::build(const ContextSensitiveOptimizationDescription &D,
                             DFAConstructionContext &DFAContext) {
  ContextDFA DFA;
  for (const auto Opt : D) {
    const auto &OptContext = Opt.first;
    Context C;
    decltype(OptContext.begin()) CallIt, NextCallIt;

    // Iterate over calls but stop one call before the last (the last is
    // optimized and the others are not).
    for (CallIt = OptContext.begin(); std::next(CallIt) != OptContext.end();
         ++CallIt) {
      DFA.addTransition(C, *CallIt, (*CallIt)->getCalledFunction(), true,
                        DFAContext);
      C.addCall(*CallIt);
    }

    DFA.addTransition(C, *CallIt, Opt.second, true, DFAContext);
    DFAContext.CanonicalFunction[Opt.second] = OptContext.getFunction();
  }

  DFA.createContextInsensitiveTransitions();

  return DFA;
}

ContextDFA::TransitionIterator
ContextDFA::transitionsBegin(const Context &C) const {
  auto CIIt = CI.find(C);

  assert(CIIt != CI.end() && "Context not in the DFA");
  auto CallIDIt = CallID.find(C.getFunction());

  LLVM_DEBUG(if (C.getFunction() && C.getFunction() != Context::MAIN_FUNCTION)
            llvm::errs() << "F: " << C.getFunction()->getName() << "\n");

  assert(CallIDIt != CallID.end() &&
         "Function does not have a list of transitions.");
  return TransitionIterator(CallIDIt->second.begin(), &CIIt->second);
}

ContextDFA::TransitionIterator
ContextDFA::transitionsEnd(const Context &C) const {
  return TransitionIterator(CallID.find(C.getFunction())->second.end(),
                            &CI.find(C)->second);
}

ContextDFA::ContextIterator
ContextDFA::possibleSourceStatesBegin(const llvm::CallInst *Call) const {
  auto It = PossibleSourceStates.find(Call);
  assert(It != PossibleSourceStates.end() &&
         "CallInst not present into the DFA.");
  return ContextIterator(*this, It->second.begin());
}

ContextDFA::ContextIterator
ContextDFA::possibleSourceStatesEnd(const llvm::CallInst *Call) const {
  auto It = PossibleSourceStates.find(Call);
  assert(It != PossibleSourceStates.end() &&
         "CallInst not present into the DFA.");
  return ContextIterator(*this, It->second.end());
}

} // namespace eos
