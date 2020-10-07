 // Implements a given context-sensitive optimization in a module using
 // ehxaustive procedure cloning to keep track of contexts during execution.
 //
 // This should be used by a ModulePass that wishes to apply an actual
 // optimization. It is optimization-agnostic, and it won't carry out any
 // optimizations on its own: instead, it expects a description of the
 // optimization to be provided, and it modifies the module's code so
 // that the code is optimized accordingly.

#ifndef CLONEWARS_CONTEXT_SENSITIVENESS_H_
#define CLONEWARS_CONTEXT_SENSITIVENESS_H_
//
#include <map>

namespace llvm {
class Module;
class CallInst;
class Function;
} // namespace llvm

namespace eos {

class Context;
class ContextSensitiveOptimizationDescription;

void ApplyContextSensitiveOptimizationWithClones(
  const ContextSensitiveOptimizationDescription &Desc, llvm::Module &M,
  const std::map<llvm::CallInst *, llvm::CallInst *> &CanonicalCalls);
}

#endif
