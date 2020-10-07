// Pass used to test  context-aware code generation methods.
// It reads optimizations to be applied from the print-opts command-line option.
// These optimizations specify that when the program follows a certain call path,
// the last call can be replaced by a call to "print(X)", where X is an specified
// string.
// The format in which optimizations are specified is the following:
// (i_0,i_1,...,i_n=X_i;)*
// Meaning that optimizations should be terminated by a semicolon, and each
// optimization is formatted as a list of integers separated by commas, followed
// by a =, followed by a string that should be print in the calling context
// specified by the indices. The indices are indices of call instructions that
// appear in the program. The first index refers to a Call Instruction in the
// main function; the second index to a call instruction in the called function,
// and so on. So 0,0,0=hello; is an optimization that means: the function called
// by the first function called by main can be replaced in that context by
// printf("hello").

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <map>
#include <vector>
#include <string>
#include <sstream>

#include "ContextSensitiveOptimizationDescription.h"
#include "DFAContextSensitiveness.h"

using namespace llvm;
using namespace std;
using namespace eos;

namespace {

cl::opt<string> PrintOpts("print-opts", cl::desc("Context-sensitive print optimizations to perform"),
                          cl::value_desc("print-opts"));

class OptimizePrints : public ModulePass {
public:

  static char ID;
  OptimizePrints() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    auto Desc = parsePrintOpts(M);
    ApplyContextSensitiveOptimization(Desc, M);
    return true;
  }

private:
  map<Function *, vector<CallInst *>> buildCallsMap(Module &M) const {
    map<Function *, vector<CallInst *>> CM;

    for (Function &F : M) {
      for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
          if (CallInst *CI = dyn_cast_or_null<CallInst>(&I)) {
            CM[&F].push_back(CI);
          }
        }
      }
    }

    return CM;
  }

  ContextSensitiveOptimizationDescription parsePrintOpts(Module &M) {
    ContextSensitiveOptimizationDescription Opt;
    istringstream IS(PrintOpts);

    auto Map = buildCallsMap(M);

    int CID = -1;
    Context Current;
    Function *F = M.getFunction("main");

    while (IS >> CID) {
      Current.addCall(Map[F][CID]);
      F = Map[F][CID]->getCalledFunction();

      char Next = IS.get();

      if (Next == '=') {
        string Output;

        while ((Next = IS.get()) != ';' && IS) {
          Output += Next;
        }

        Opt.addOptimization(Current, buildOptimizedPrintFunction(F, M, Output));

        F = M.getFunction("main");
        Current = Context();
      }
    }

    return Opt;
  }

  static Function *buildOptimizedPrintFunction(Function *Original,
                                               Module &M,
                                               const string &S) {
    Function *PrintS = Function::Create(Original->getFunctionType(),
                                        GlobalValue::LinkageTypes::InternalLinkage,
                                        "print_opt",
                                        &M);
    BasicBlock *BB = BasicBlock::Create(M.getContext(), "", PrintS);

    Constant *StrConstant = ConstantDataArray::getString(M.getContext(), S);
    GlobalVariable *StrGlobalConst = new GlobalVariable(M, StrConstant->getType(), true,
                                                        GlobalValue::InternalLinkage,
                                                        StrConstant, "strglobalconst");
    Constant* Zero = Constant::getNullValue(IntegerType::getInt32Ty(M.getContext()));
    SmallVector<Value*, 2> Indices {Zero, Zero};

    auto GEP = ConstantExpr::getInBoundsGetElementPtr(StrConstant->getType(),
                                                      StrGlobalConst,
                                                      Indices);
    CallInst::Create(M.getFunction("printf"),
                     GEP,
                     "optfunc",
                     BB);
    ReturnInst::Create(M.getContext(), BB);
    return PrintS;
  }
};

char OptimizePrints::ID = 0;

static RegisterPass<OptimizePrints> X("optimize-prints", "Optimize Prints",
                                      false, false);

}  // namespace
