#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include <map>
#include <set>

using namespace std;

namespace llvm {

class AnalyzeBenchmark : public ModulePass {
private:
  map<llvm::Function *, long long> Count;

public:
  AnalyzeBenchmark() : ModulePass(ID) {}

  bool runOnModule(llvm::Module &M) {
    auto Main = M.getFunction("main");
    long long Count = Main ? (1 + count(Main)) : 0;
    ofstream out("/home/gabriel/result", "a");
    out << M.getModuleIdentifier() << " " << Count << "\n";
    return false;
  }

  static char ID;

private:
  long long count(llvm::Function *F) {
    auto It = Count.find(F);
    if (It != Count.end())
      return It->second;

    long long C = 0;

    for (auto &BB : *F) {
      for (auto &Inst : BB) {
        if (CallInst *CI = dyn_cast_or_null<CallInst>(&Inst)) {
          if (llvm::Function *CalledFunc = F->getCalledFunction()) {
            C += 1 + count(CalledFunc);
          }
        }
      }
    }

    return Count[F] = C;
  }

};

char AnalyzeBenchmark::ID = 0;

namespace {

RegisterPass<AnalyzeBenchmark> X("dfacs-analyze", "DFA-CS Analysis Pass",
                                 false, false);

void addAnalyzeBenchmarkPass(const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {
  PM.add(new AnalyzeBenchmark);
  Builder.populateLTOPassManager(PM);
}


RegisterStandardPasses R(PassManagerBuilder::EP_OptimizerLast | PassManagerBuilder::EP_EnabledOnOptLevel0,
                         addAnalyzeBenchmarkPass);

}  // namespace
}  // namespace llvm
