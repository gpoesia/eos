#include "CloneConstantArgs.h"
#include <cstdlib>
#include <string>
#include <iostream>
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/Support/CommandLine.h"

#include "../ContextSensitiveOptimizationDescription.h"
#include "../DFAContextSensitiveness.h"
#include "../CloneWarsContextSensitiveness.h"

using namespace llvm;
using namespace eos;
using namespace std;

CloneConstantArgs::CloneConstantArgs() : ModulePass(ID) {
  FunctionsCount    = 0;
  FunctionsCloned   = 0;
  ClonesCount       = 0;
  CallsCount        = 0;
  PromissorCalls    = 0;
  CallsReplaced     = 0;
  NumInstKilled        = 0;
}

static cl::opt<bool>RunWPCP("wpcp", cl::Hidden,
                 cl::desc("[gpoesia] Run link-time whole-program constant propagation."));

static cl::opt<bool>CloneWars("clone-wars", cl::Hidden,
                 cl::desc("Apply optimization through exhaustive cloning, as opposed "
                          "to using the DFA approach"));

Module::iterator LastFn;

namespace {

int countOptTreeNodes(ContextSensitiveOptimizationDescription &Opts) {
  set<Context> Contexts;

  for (const auto &Opt : Opts) {
    const auto &Ctx = Opt.first;

    Context C;

    for (auto CallIt = Ctx.begin(), CallEnd = Ctx.end();
         CallIt != CallEnd; ++CallIt) {
      C.addCall(*CallIt);
      Contexts.insert(C);
    }
  }

  return Contexts.size();  // main() is not cloned. Hence, no + 1 here.
}

void moduleSize(llvm::Module &M,
                long long &NumberOfInstructions,
                long long &NumberOfFunctions) {
  NumberOfInstructions = NumberOfFunctions = 0;

  for (auto &F : M) {
    NumberOfFunctions += !F.isDeclaration();
    for (auto &BB : F) {
      NumberOfInstructions += BB.size();
    }
  }
}

long long INF = 10000000000000000LL - 1;

long long countContexts(llvm::Function *F,
                        std::map<Function *, long long> &Count) {
  auto It = Count.find(F);
  if (It != Count.end())
    return It->second;

  long long &C = Count[F];
  C = 0;

  for (auto &BB : *F) {
    for (auto &Inst : BB) {
      if (CallInst *CI = dyn_cast_or_null<CallInst>(&Inst)) {
        if (llvm::Function *CalledFunc = CI->getCalledFunction()) {
          auto C2 = countContexts(CalledFunc, Count);
          if (C2 == INF) {
            C = INF;
            break;
          }
          C += 1 + C2;
          if (C > INF) {
            C = INF;
            break;
          }
        }
      }
    }
  }

  return C;
}

}

bool CloneConstantArgs::runOnModule(Module &M) {
/*  if (!RunWPCP) {
    std::cerr << "Not running WPCP\n";
    return false;
  } else {
  } */
  std::cerr << "Running WPCP\n";

  // Get some statistics before modifying the module.
  long long NumberOfFunctions, NumberOfInstructions;
  moduleSize(M, NumberOfInstructions, NumberOfFunctions);

  Function *Main = M.getFunction("main");
  if (!Main) {
    std::cerr << "No main function (" << NumberOfFunctions << " functions in this module).\n";
    return false;
  }

  map<Function *, long long> ContextsCount;
  long long NumberOfContexts = Main ? (1 + countContexts(Main, ContextsCount)) : 0;

  ContextSensitiveOptimizationDescription Opt;
  propagateConstants(Main, Context(), nullptr, Opt);
  errs() << "Optimized contexts: " << Opt.size() << "\n";

  size_t DFAStates = 0;

  if (true || CloneWars) {
    std::cerr << "Using clone wars.\n";
    ApplyContextSensitiveOptimizationWithClones(Opt, M, CanonicalCall);
  } else {
    std::cerr << "Using DFA.\n";
    DFAStates = ApplyContextSensitiveOptimization(Opt, M, CanonicalCall);
  }

  ofstream out("/home/gpoesia/benchmark.stats", ios::app);
  out << NumberOfFunctions
      << " " << NumberOfInstructions
      << " " << NumberOfContexts
      << " " << Opt.size()
      << " " << countOptTreeNodes(Opt)
      << " " << DFAStates
      << "\n";

  return Opt.size() > 0;
}

namespace {
string randomString() {
  string S;
  for (unsigned i = 0; i < 10; i++) {
    S += 'a' + (rand() % 26);
  }
  return S;
}

bool hasConstantArguments(llvm::CallInst *CI) {
  CallSite CS(CI);

  for (Value *ArgValue : CS.args()) {
    if (isa<Constant>(ArgValue) && !isa<UndefValue>(ArgValue)) {
      return true;
    }
  }
  return false;
}

int AllocatedClones = 0;

}

bool CloneConstantArgs::propagateConstants(Function *F, const Context &C, CallInst *FCall,
                                           ContextSensitiveOptimizationDescription &Opt) {
  // Prune the optimization to avoid overly large binaries.
  if (AllocatedClones >= 15000) {
    return false;
  }

  LLVM_DEBUG(errs() << "Visiting " << F->getName() << "\n");

  // If we're not in the main function, operate on a clone. Otherwise, use the original.
  Function *OptF = FCall ? cloneFunctionWithConstArgs((++AllocatedClones, F), FCall, "clone") : F;
  bool CouldOptimizeF = propagateConstants(*OptF);

  LLVM_DEBUG(errs() << "\rAllocated clones: " << AllocatedClones);

  if (FCall && !CouldOptimizeF) {
    CouldOptimizeF = hasConstantArguments(FCall);
  }
  LLVM_DEBUG(errs() << "Could optimize? " << CouldOptimizeF << "\n");

  bool OptimizedChildren = false;

  for (auto &BB : *OptF) {
    for (auto &Inst : BB) {
      if (CallInst *Call = dyn_cast_or_null<CallInst>(&Inst)) {
        Function *Callee = Call->getCalledFunction();

        // Ignore indirect calls and external functions.
        if (!Callee || Callee->isDeclaration())
          continue;

        CallInst *Canonical = getCanonicalCall(Call);

        if (Canonical->getCalledFunction() == Callee &&
            (hasConstantArguments(Call) || C.getCallSequence().size() < 5)) {
          Context C2 = C;

          if (!(Canonical->getCalledFunction() == Callee)) {
            errs() << "Original Function: " << *F << "\n"
                   << "\nClone: " << *OptF << "\n";
          }

          assert(Canonical->getCalledFunction() == Callee &&
                 "Canonical call and clone should call the same function!");
          C2.addCall(Canonical);
          OptimizedChildren = propagateConstants(Callee, C2, Call, Opt) || OptimizedChildren;
          LLVM_DEBUG(errs() << "Back to " << F->getName() << "\n");
        }
      }
    }
  }

  if (!OptimizedChildren && CouldOptimizeF) {
    // If F is main, it was already optimized in-place.
    // Otherwise, insert the clone into the module.
    if (FCall) {
      Opt.addOptimization(C, OptF);
      LLVM_DEBUG(errs() << "Optimizing " << F->getName() << "\n");
    }
  } else if (FCall) {
    OptF->eraseFromParent();
  }

  return OptimizedChildren || CouldOptimizeF;
}

/*iterates over function instructions, finding calls with constant arguments*/
bool CloneConstantArgs::findConstantArgs(Module &M, CallGraph &CG) {
  bool Changed = false;

  for (auto F = ++LastFn, end = M.end(); F != end; ++F) {
    auto &Caller = *F;
    LastFn = F;

    //external funtions don't concern us
    if (Caller.isDeclaration()) { continue; }

    FunctionsCount++;

    //propagate constants within the calling function
    Changed |= propagateConstants(Caller);

    for (auto BB = F->begin(), BBE = F->end(); BB != BBE; ++BB) {
      for  (auto I = BB->begin(), IE = BB->end(); I != IE; ++I) {
        //we only care about call instructions
        if (!isa<CallInst>(I)) { continue; }

        auto Call = cast<CallInst>(&*I);
        Function *Callee = dyn_cast_or_null<Function>(Call->getCalledValue()->
                                                      stripPointerCasts());
        //callee function must be valid and also not external
        if (!Callee || Callee->isDeclaration()) { continue; }

        CallsCount++;

        //don't clone recursive functions or we'll end up in an infinite loop
        if (isRecursive(Callee, CG)) { continue; };

        //declare the function's call site
        CallSite CS(Call);

        auto formalArgIter = Callee->arg_begin();
        auto actualArgIter = CS.arg_begin();
        int size = Callee->arg_size();

        //iterate over arguments storing formal->actual constant pairs
        for (int i = 0; i < size; ++i, ++actualArgIter, ++formalArgIter) {
          Value *actualArg = *actualArgIter;
          Argument *formalArg = &*formalArgIter;

          //if argument is constant (and not null), it's a candidate
          if ((isa<Constant>(actualArg)) && !(isa<UndefValue>(actualArg))) {
            /*errs() << "Function " << Caller.getName();
            errs() << " calling function " << Callee->getName();
            errs() << " with args:\n\t";
            formalArg->dump(); errs() << "\t"; actualArg->dump();*/
            arguments[Call].push_back(std::make_pair(formalArg, actualArg));
          }
        }
      }
    }
  }

  return Changed;
}

/*Determines which functions can potentially be cloned. Those are functions
that: (1) Do not have external linkage and (2) Are actually called at some
point in the code :)*/
void CloneConstantArgs::collectFn2Clone() {
  for(auto it = arguments.begin(); it != arguments.end(); ++it) {
    CallInst* Call = it->first;

    Function* F = Call->getCalledFunction();
    if (!F->hasAvailableExternallyLinkage()) {
      if (!fn2Clone.count(F)) PromissorCalls += F->getNumUses();
      fn2Clone[F].push_back(Call);
    }
  }
}

/*Clone functions and replace occurrences of calls to it with its clone*/
bool CloneConstantArgs::cloneFunctions() {
  bool modified = false;

  for(auto it = fn2Clone.begin(); it != fn2Clone.end(); ++it) {
    FunctionsCloned++;
    Function *F = it->first;
    //std::map<std::vector<std::pair<Argument*, Value*>>, Function*> clonedFns;

    for(unsigned long i = 0; i < it->second.size(); i++) {
      CallInst* Call = it->second.at(i);
      std::vector<std::pair<Argument*, Value*>> userArgs = arguments[Call];

      //if (!clonedFns.count(userArgs)) {
        // Clone function if a proper clone doesn't already exist
        std::stringstream suffix;
        suffix << ".const" << i;
        Function* NF = cloneFunctionWithConstArgs(F, Call, suffix.str());
        replaceCallingInst(Call, NF);
        //clonedFns[userArgs] = NF;
        ClonesCount++;
        errs() << "Generated clone: " << F->getName() << "(";
        for (unsigned i = 0; i < userArgs.size(); i++) {
          errs() << *(userArgs[i].second) << ", ";
        }
        errs() << ")\n";
      //}
      /*else {
        // Otherwise use existing clone
        Function* NF = clonedFns.at(userArgs);
        replaceCallingInst(Call, NF);
      }*/
      arguments.erase(Call);
      CallsReplaced++;
      modified = true;
    }
    fn2Clone.erase(F);
  }
  return modified;
}

/*replaces a function call with its clone 'fn'*/
void CloneConstantArgs::replaceCallingInst(CallInst* Call, Function* fn) {
  Call->setCalledFunction(fn);
}

/*Create a clone for the given function 'Fn', in the calling context from
'caller'*/
Function* CloneConstantArgs::cloneFunctionWithConstArgs(Function *Fn,
                                                        CallInst* Call,
                                                        std::string suffix) {
  // Start by computing a new prototype for the function, which is the
  // same as the old function
  Function *NF = Function::Create(Fn->getFunctionType(), Fn->getLinkage());
  NF->copyAttributesFrom(Fn);

  // After the parameters have been copied, we should copy the parameter
  // names, to ease function inspection afterwards.
  Function::arg_iterator NFArg = NF->arg_begin();
  for (auto Arg = Fn->arg_begin(), ArgEnd = Fn->arg_end();
       Arg != ArgEnd; ++Arg, ++NFArg) {
    NFArg->setName(Arg->getName());
  }

  // To avoid name collision, we add a suffix.
  NF->setName(Fn->getName() + suffix + randomString());

  // fill clone content
  ValueToValueMapTy VMap;
  SmallVector<ReturnInst*, 8> Returns;
  Function::arg_iterator NI = NF->arg_begin();
  for (auto I = Fn->arg_begin(); NI != NF->arg_end(); ++I, ++NI) {
    VMap[&*I] = &*NI;
  }
  CloneFunctionInto(NF, Fn, VMap, false, Returns);

  // Set canonical CallInst for calls in clone before the clone is changed.
  for (auto BBIt1 = Fn->begin(), BBIt2 = NF->begin(), BBEnd = Fn->end();
       BBIt1 != BBEnd; ++BBIt1, ++BBIt2) {
    auto InstIt2 = BBIt2->begin(), Inst2End = BBIt2->end();
    for (auto InstIt1 = BBIt1->begin(), InstEnd = BBIt1->end();
         InstIt1 != InstEnd; ++InstIt1) {
      if (CallInst *OriginalCallInst = dyn_cast_or_null<CallInst>(&*InstIt1)) {
        CallInst *ClonedCallInst;
        while (InstIt2 != Inst2End && !(ClonedCallInst = dyn_cast_or_null<CallInst>(&*InstIt2))) {
          ++InstIt2;
        }
        assert(InstIt2 != Inst2End && "Clone has less calls than original function.");
        assert(isa<CallInst>(ClonedCallInst) &&
               "Clone should have the same call instructions as original function.");
        CanonicalCall[ClonedCallInst] = getCanonicalCall(OriginalCallInst);
        LLVM_DEBUG(errs() << "Canonical[" << ClonedCallInst << "] = "
                     << CanonicalCall[ClonedCallInst] << "\n");
        ++InstIt2;
      }
    }
  }

  if (Call) {
    CallSite CS(Call);

    auto formalArgIter = NF->arg_begin();
    auto actualArgIter = CS.arg_begin();
    int size = NF->arg_size();

    std::map<Argument*, Value*> argsMap;

    // Iterate over arguments and replace uses of constant arguments with
    // constants themselves.
    for (int i = 0; i < size; ++i, ++actualArgIter, ++formalArgIter) {
      Value *actualArg = *actualArgIter;
      Argument *formalArg = &*formalArgIter;

      //if argument is constant (and not null), it's a candidate
      if ((isa<Constant>(actualArg)) && !(isa<UndefValue>(actualArg))) {
        LLVM_DEBUG(errs() << formalArg->getName() << " is constant and has "
                     << formalArg->getNumUses() << " uses.\n");
        formalArg->replaceAllUsesWith(actualArg);
      }
    }
  }

  Fn->getParent()->getFunctionList().push_back(NF);
  return NF;
}

/*Performs intra-procedural constant propagation within a function to expose
more calls with constant arguments*/
bool CloneConstantArgs::propagateConstants(Function &F) {
  std::set<Instruction*> WorkList;
  for (Instruction &I: instructions(&F))
    WorkList.insert(&I);

  bool Changed = false;
  const DataLayout &DL = F.getParent()->getDataLayout();
  TargetLibraryInfo *TLI =
      &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);

  while (!WorkList.empty()) {
    Instruction *I = *WorkList.begin();
    WorkList.erase(WorkList.begin());

    if (!I->use_empty())
      if (Constant *C = ConstantFoldInstruction(I, DL, TLI)) {
        for (User *U : I->users())
          WorkList.insert(cast<Instruction>(U));

        I->replaceAllUsesWith(C);

        WorkList.erase(I);
        if (isInstructionTriviallyDead(I, TLI)) {
          I->eraseFromParent();
          ++NumInstKilled;
        }

        Changed = true;
      }
  }
  return Changed;
}

/*Determines whether Function @F is recursive, either directly or indirectly*/
bool CloneConstantArgs::isRecursive(Function *F, CallGraph &CG) {
  CallGraphNode *Node = CG.getOrInsertFunction(F);
  if (!Node)
    return false;
  for (scc_iterator<CallGraphNode*> SCCI = scc_begin(Node); !SCCI.isAtEnd(); ++SCCI) {
    const std::vector<CallGraphNode*> &nextSCC = *SCCI;
    if (nextSCC.size() > 1 || SCCI.hasLoop()) {
      return true;
    }
  }
  return false;
}

void CloneConstantArgs::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetLibraryInfoWrapperPass>();
  AU.addRequired<CallGraphWrapperPass>();
}

void CloneConstantArgs::print(raw_ostream& O, const Module* M) const {
  O << "# functions; \
        # cloned functions; \
        # clones; \
        # calls; \
        # promissing calls; \
        # replaced calls; \
        # instructions killed;\n";
  O << FunctionsCount << ";"
    << FunctionsCloned << ";"
    << ClonesCount << ";"
    << CallsCount << ";"
    << PromissorCalls << ";"
    << CallsReplaced << ";"
    << NumInstKilled << "\n";
}

// Register the pass to the LLVM framework
char CloneConstantArgs::ID = 0;
static RegisterPass<CloneConstantArgs> X("clone-constant-args",
                                         "Clone functions with constant args.",
                                          false,
                                          false);

// RegisterStandardPasses R(PassManagerBuilder::EP_LTOOptimizerLast,
//                         addAnalyzeBenchmarkPass);

void addWPConstProp(const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {
  std::cerr << "CloneConstantArgs registered!\n" << std::endl;
  PM.add(new llvm::CloneConstantArgs);
}

int globallyRegisterWPCP() {
  PassManagerBuilder::addGlobalExtension(
      PassManagerBuilder::EP_FullLinkTimeOptimizationLast,
      addWPConstProp);
  return 0;
}

// static int RegisterWPCP = globallyRegisterWPCP();
