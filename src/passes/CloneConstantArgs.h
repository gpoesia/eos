#include <sstream>
#include <ios>
#include <fstream>
#include <string>
#include <iostream>
#include <set>

#include "llvm/Pass.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/ConstantFolding.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/CallSite.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Local.h"

#ifndef DEBUG_TYPE
#define DEBUG_TYPE "CloneConstantArgs"
#endif

namespace eos {
class Context;
class ContextSensitiveOptimizationDescription;
}

namespace llvm {

  STATISTIC(FunctionsCount,  "Number of functions");
  STATISTIC(FunctionsCloned, "Number of cloned functions");
  STATISTIC(ClonesCount,     "Number of functions that are clones");
  STATISTIC(CallsCount,      "Number of calls");
  STATISTIC(PromissorCalls,  "Number of promissor calls");
  STATISTIC(CallsReplaced,   "Number of replaced calls");
  STATISTIC(NumInstKilled,   "Number of instructions killed (constprop)");

  class CloneConstantArgs : public ModulePass {

    std::map<CallInst *, std::vector<std::pair<Argument *, Value *>>> arguments;
    std::map<Function *, std::vector <CallInst *>> fn2Clone;
    std::map<CallInst *, CallInst *> CanonicalCall;

    CallInst *getCanonicalCall(CallInst *Call) const {
      auto It = CanonicalCall.find(Call);
      if (It != CanonicalCall.end()) {
        return It->second;
      }
      return Call;
    }

    bool propagateConstants(Function &F);
    bool propagateConstants(Function *F, const eos::Context &C, CallInst *Call,
                            eos::ContextSensitiveOptimizationDescription &Opt);
    bool isRecursive(Function *F, CallGraph &CG);
    bool findConstantArgs(Module &M, CallGraph &CG);
    bool cloneFunctions();
    void collectFn2Clone();
    Function* cloneFunctionWithConstArgs(Function *Fn, CallInst* Call,
                                         std::string suffix);
    void replaceCallingInst(CallInst *Call, Function* fn);

   public:
    static char ID;

    CloneConstantArgs();
    virtual bool runOnModule(Module &M) override;
    virtual void getAnalysisUsage(AnalysisUsage &AU) const override;
    virtual void print(raw_ostream& O, const Module* M) const override;
  };
}
