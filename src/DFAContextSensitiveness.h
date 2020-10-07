// Implements a given context-sensitive optimization in a module using an
// implicit state machine to track the context throughout the program.
//
// This should be used by a ModulePass that wishes to apply an actual
// optimization. It is optimization-agnostic, and it won't carry out any
// optimizations on its own: instead, it expects a description of the
// optimization to be provided, and it modifies the module's code so
// that the code is optimized accordingly.

#ifndef DFA_CONTEXT_SENSITIVENESS_H_
#define DFA_CONTEXT_SENSITIVENESS_H_

#include <map>

namespace llvm {
class Module;
class CallInst;
} // namespace llvm

namespace eos {

class Context;
class ContextSensitiveOptimizationDescription;

size_t ApplyContextSensitiveOptimization(
  const ContextSensitiveOptimizationDescription &Desc, llvm::Module &M,
  const std::map<llvm::CallInst *, llvm::CallInst *> &CanonicalCalls);

size_t ApplyContextSensitiveOptimization(const ContextSensitiveOptimizationDescription &Desc,
                                       llvm::Module &M);

} // namespace eos

#endif // DFA_CONTEXT_SENSITIVENESS_H_
