// Representation for contexts and context-sensitive optimization.
//
// Contexts are basically a sequence of call instructions that can
// be followed starting from main. A context-sensitive optimization
// relates contexts with optimized versions of functions that can
// only be called from that context.

#ifndef CONTEXT_SENSITIVE_OPTIMIZATION_DESCRIPTION
#define CONTEXT_SENSITIVE_OPTIMIZATION_DESCRIPTION

#include "llvm/IR/Instructions.h"

#include <algorithm>
#include <iterator>
#include <map>
#include <vector>

namespace llvm {
class Function;
} // namespace llvm

namespace eos {

class Context {
public:
  Context() = default;

  typedef std::vector<const llvm::CallInst *> CallSequence;

  Context(const CallSequence &CallSeq) : CallSeq(CallSeq) {}
  const CallSequence &getCallSequence() const { return CallSeq; }

  bool operator<(const Context &ctx) const {
    return std::lexicographical_compare(CallSeq.begin(), CallSeq.end(),
                                        ctx.CallSeq.begin(), ctx.CallSeq.end());
  }

  CallSequence::const_iterator begin() const { return CallSeq.begin(); }

  CallSequence::const_iterator end() const { return CallSeq.end(); }

  void addCall(const llvm::CallInst *Call) { CallSeq.push_back(Call); }

  llvm::Function *getFunction() const {
    if (CallSeq.size() && CallSeq.back()) {
      return CallSeq.back()->getCalledFunction();
    } else if (CallSeq.size()) {
      return nullptr;
    } else {
      return MAIN_FUNCTION;
    }
  }

  static llvm::Function *const MAIN_FUNCTION;

private:
  CallSequence CallSeq;
};

class ContextSensitiveOptimizationDescription {
  std::map<Context, llvm::Function *> Optimizations;

public:
  void addOptimization(const Context &C, llvm::Function *F) {
    Optimizations[C] = F;
  }

  decltype(Optimizations)::const_iterator begin() const {
    return Optimizations.begin();
  }

  decltype(Optimizations)::const_iterator end() const {
    return Optimizations.end();
  }

  bool isContextOptimized(const Context &C) const {
    return Optimizations.count(C);
  }

  llvm::Function *getOptimizationFor(const Context &C) const {
    auto It = Optimizations.find(C);
    if (It != Optimizations.end()) {
      return It->second;
    }
    return nullptr;
  }

  size_t size() const {
    return Optimizations.size();
  }
};

} // namespace eos

#endif // CONTEXT_SENSITIVE_OPTIMIZATION_DESCRIPTION
