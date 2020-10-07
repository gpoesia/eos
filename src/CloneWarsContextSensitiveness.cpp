#include "CloneWarsContextSensitiveness.h"
#include "ContextDFA.h"
#include "ContextSensitiveOptimizationDescription.h"

#include <string>
#include <vector>
#include <map>

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#define DEBUG_TYPE "CloneWarsContextSensitiveness"

using namespace llvm;

namespace eos {

namespace {
std::string randomString() {
  std::string S;
  for (unsigned i = 0; i < 10; i++) {
    S += 'a' + (rand() % 26);
  }
  return S;
}

CallInst *getCanonicalCall(
        CallInst *Call,
        std::map<llvm::CallInst *, llvm::CallInst *> &CanonicalCalls) {
  auto It = CanonicalCalls.find(Call);
  if (It != CanonicalCalls.end()) {
    return It->second;
  }
  return Call;
}

// Clones function OF to use as a context-tracker, returns pointer to newly
// created clone
Function *cloneFunction(Function *OF, const CallInst *Call,
                        std::string suffix,
                        std::map<CallInst *, CallInst *> &CanonicalCalls) {
  Function *NF = Function::Create(OF->getFunctionType(), OF->getLinkage());
  NF->copyAttributesFrom(OF);

  // Name clone with proper suffix.
  NF->setName(OF->getName() + suffix);

  // Keep parameter names consistent.
  auto NFait = NF->arg_begin();
  for (auto OFait = OF->arg_begin(); OFait != OF->arg_end();
       ++OFait, ++NFait) {
    NFait->setName(OFait->getName());
  }

  // Copy over function content
  ValueToValueMapTy ValueMap;
  SmallVector<ReturnInst*, 8> ReturnVs;
  auto OFait = OF->arg_begin();
  for (auto NFait = NF->arg_begin(); NFait != NF->arg_end();
       ++NFait, ++OFait) {
    ValueMap[&*OFait] = &*NFait;
  }

  CloneFunctionInto(NF, OF, ValueMap, false, ReturnVs);

  // Update canonical calls map.
  for (auto BBIt1 = OF->begin(), BBIt2 = NF->begin(), End = OF->end();
       BBIt1 != End; ++BBIt1, ++BBIt2) {
    for (auto InstIt1 = BBIt1->begin(), InstIt2 = BBIt2->begin(),
           InstEnd = BBIt1->end();
         InstIt1 != InstEnd; ++InstIt1, ++InstIt2) {
      if (CallInst *OriginalCall = dyn_cast<CallInst>(&*InstIt1)) {
        CallInst *ClonedCall = dyn_cast<CallInst>(&*InstIt2);
        assert(ClonedCall && "Clone should have exactly the same instructions!");
        CanonicalCalls[ClonedCall] = getCanonicalCall(OriginalCall,
                                                      CanonicalCalls);
      }
    }
  }

  // Add clone to Module's function list
  OF->getParent()->getFunctionList().push_back(NF);

  return NF;
}

}  // anonymous namespace



void ApplyContextSensitiveOptimizationWithClones(
    const ContextSensitiveOptimizationDescription &Desc,
    llvm::Module &M,
    const std::map<llvm::CallInst *, llvm::CallInst *> &InitialCanonicalCalls) {

  Function *MainF = M.getFunction("main");

  std::map<CallInst*, CallInst*> CanonicalCalls = InitialCanonicalCalls;
  std::map<Context, Function*> ClonedContexts;

  // Iterate over optimized contexts.
  for (auto It = Desc.begin(), End = Desc.end(); It != End; ++It) {
    Context OptimizedContext = It->first;
    Function *OptF = It->second;

    //context is initially empty, and caller function is main
    Context CurrentContext;
    Context NextContext;
    Function *CurrF = MainF;

    for (auto CallIt = OptimizedContext.begin(), End = OptimizedContext.end();
         CallIt != OptimizedContext.end(); ++CallIt) {
      const CallInst* Call = *CallIt;

      NextContext.addCall(Call);
      Function *Target;

      // For optimized contexts the target function is the optimized
      // function itself.
      if (Desc.isContextOptimized(NextContext)) {
        Target = Desc.getOptimizationFor(NextContext);
      } else {  // Otherwise, target is a clone (newly created or pre-existing).
        if (ClonedContexts.find(NextContext) != ClonedContexts.end()) {
          Target = ClonedContexts[NextContext];
        } else {
          std::string Suffix = "_clonew" + randomString();
          Target = cloneFunction(Call->getCalledFunction(), Call, Suffix,
                                 CanonicalCalls);
          ClonedContexts[NextContext] = Target;
        }
      }

      // Find the original CallInst and change it to call the new target.
      for (auto BB = CurrF->begin(); BB != CurrF->end(); ++BB) {
        for (auto I = BB->begin(); I != BB->end(); ++I) {
          if (CallInst *CI = dyn_cast<CallInst>(I)) {
            if (getCanonicalCall(CI, CanonicalCalls) == Call) {
              CI->setCalledFunction(Target);
            }
          }
        }
      }

      CurrF = Target;
    }
  }
}

} //namespace eos
