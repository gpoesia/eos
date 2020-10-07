#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include <map>
#include <set>
#include <fstream>

using namespace std;
using namespace llvm;

namespace {
long long INF = 1000000000000000000LL;
}

namespace llvm {

class AnalyzeBenchmark : public ModulePass {
private:
  map<llvm::Function *, long long> Count;

public:
  bool runOnModule(llvm::Module &M) {
    auto Main = M.getFunction("main");

    if (!Main)
      return false;

    unsigned NumberOfFunctions = M.getFunctionList().size();
    unsigned NumberOfInstructions = countInstructions(M);

    StringRef File = Main->getSubprogram() ? Main->getSubprogram()->getDirectory() : StringRef("");

    long long Count = Main ? (1 + count(Main)) : 0;
    ofstream out("/home/gabriel/result", ios::app);
    out << File.data() << " "
        << NumberOfFunctions << " "
        << NumberOfInstructions << " "
        << Count << "\n";
    return false;
  }

  static char ID;

  AnalyzeBenchmark() : ModulePass(ID) {}

private:

  long long countInstructions(llvm::Module &M) {
    long long IC = 0;

    for (auto &F : M) {
      for (auto &BB : F) {
        IC += BB.size();
      }
    }

    return IC;
  }

  long long count(llvm::Function *F) {

    auto It = Count.find(F);
    if (It != Count.end())
      return It->second;

    long long &C = Count[F];
    C = 0;

    for (auto &BB : *F) {
      for (auto &Inst : BB) {
        if (CallInst *CI = dyn_cast_or_null<CallInst>(&Inst)) {
          if (llvm::Function *CalledFunc = CI->getCalledFunction()) {
            auto C2 = count(CalledFunc);
            if (C2 == INF) {
              C = INF;
              break;
            }
            C += 1 + count(CalledFunc);
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
};

char AnalyzeBenchmark::ID = 0;
void initializeAnalyzeBenchmarkPass(PassRegistry &Registry);

}  // namespace llvm

INITIALIZE_PASS(AnalyzeBenchmark, "dfacs-analyze", "DFA-CS Analyze Benchmark", false, false)

void addAnalyzeBenchmarkPass(const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {
  PM.add(new llvm::AnalyzeBenchmark);
}

int globallyRegister() {
  PassManagerBuilder::addGlobalExtension(
      PassManagerBuilder::EP_FullLinkTimeOptimizationLast,
      addAnalyzeBenchmarkPass);
  return 0;
}

// RegisterStandardPasses R(PassManagerBuilder::EP_LTOOptimizerLast,
//                         addAnalyzeBenchmarkPass);

int RegisterPleasePlease2 = globallyRegister();
