// Represents a DFA that tracks calling context throughout a program.
// Contexts and transitions are mapped to integer IDs when added to the DFA.
//
// Note that this is just a representation of the DFA, and does not simulate its
// behavior when given actual input. The goal is that generated code will do
// that implicitly, not this data structure created during compile-time.

#ifndef CONTEXT_DFA_H_
#define CONTEXT_DFA_H_

#include "ContextSensitiveOptimizationDescription.h"
#include <iterator>
#include <map>
#include <set>
#include <memory>

namespace llvm {
class CallInst;
class Function;
class Module;
} // namespace llvm

namespace eos {

class DFAConstructionContext {
public:
  DFAConstructionContext(llvm::Module &M) : M(M) {}

  void mergeCanonicalCallsMap(
    const std::map<llvm::CallInst *, llvm::CallInst *> &CanonicalCall);

  llvm::Function *getContextSensitiveVersion(llvm::Function *F);

  const llvm::CallInst *getCanonicalCall(const llvm::CallInst *Call);

  llvm::Function *getCanonicalFunction(llvm::Function *F);

  std::map<const llvm::Function *, llvm::Function *>::const_iterator
  contextSensitiveFunctionsBegin() const;

  std::map<const llvm::Function *, llvm::Function *>::const_iterator
  contextSensitiveFunctionsEnd() const;

private:
  llvm::Module &M;
  std::map<const llvm::Function *, llvm::Function *> ContextSensitiveVersion;
  std::map<const llvm::Function *, llvm::Function *> CanonicalFunction;
  std::map<const llvm::CallInst *, llvm::CallInst *> CanonicalCall;
  void mapClonedCalls(llvm::Function *Clone, llvm::Function *F);

  friend class ContextDFA;
};

class ContextDFA {
public:
  static const unsigned UNINITIALIZED_ID;

  struct TransitionInfo {
    unsigned DestinationID;
    llvm::Function *Target;
  };

  struct ContextInfo {
    unsigned ID;

    // Map from call ID to information regarding the call target
    // (target state ID and LLVM function).
    std::map<unsigned, TransitionInfo> Transitions;

    // Return-only contexts are contexts that have only one transition,
    // returning to some other context. They are used to represent
    // context-unaware functions: when a context-aware function calls a
    // context-unaware function, the state machine transitions to a state
    // that will return to the previous state when control reaches the
    // context-sensitive function again.
    bool isReturnOnly;
  };

  class TransitionIterator
      : public std::iterator<std::input_iterator_tag,
                             std::pair<unsigned, const TransitionInfo *>> {
  public:
    TransitionIterator() = delete;
    TransitionIterator(const TransitionIterator &) = default;

    bool operator==(const TransitionIterator &It) const {
      return CI == It.CI && TransitionIDIt == It.TransitionIDIt;
    }

    bool operator!=(const TransitionIterator &It) const {
      return !(*this == It);
    }

    std::pair<unsigned, const TransitionInfo *> operator*() const {
      auto CIt = CI->Transitions.find(TransitionIDIt->second);

      if (CIt != CI->Transitions.end()) {
        return std::make_pair(TransitionIDIt->second, &CIt->second);
      } else {
        return std::make_pair(TransitionIDIt->second, nullptr);
      }
    }

    TransitionIterator &operator++() {
      ++TransitionIDIt;
      return *this;
    }

    TransitionIterator operator++(int) {
      auto Ret = *this;
      ++TransitionIDIt;
      return Ret;
    }

  private:
    std::map<const llvm::CallInst *, unsigned>::const_iterator TransitionIDIt;
    const ContextInfo *CI;

    TransitionIterator(decltype(TransitionIDIt) TIDIt, decltype(CI) CIPtr)
        : TransitionIDIt(TIDIt), CI(CIPtr) {}

    friend class ContextDFA;
  };

  class ContextIterator {
    //      : public std::iterator<std::input_iterator_tag, const ContextInfo &> {

  public:
    ContextIterator() = delete;
    ContextIterator(const ContextIterator &) = default;

    bool operator==(const ContextIterator &CIt) const {
      return It == CIt.It;
    }

    bool operator!=(const ContextIterator &It) const {
      return !(*this == It);
    }

    const ContextInfo &operator*() const {
      return *DFA.IDToContextInfo.find(*It)->second;
    }

    ContextIterator &operator++() {
      ++It;
      return *this;
    }

    ContextIterator operator++(int) {
      auto Ret = *this;
      ++It;
      return Ret;
    }

  private:
    const ContextDFA &DFA;
    std::set<unsigned>::iterator It;

    ContextIterator(const ContextDFA &DFA, std::set<unsigned>::iterator It)
      : DFA(DFA), It(It) {}

    friend class ContextDFA;
  };


private:
  std::map<Context, ContextInfo> CI;
  std::map<unsigned, const ContextInfo *> IDToContextInfo;
  std::map<unsigned, unsigned> CorrespondingReturnOnlyState;
  std::map<const llvm::Function *, std::map<const llvm::CallInst *, unsigned>>
      CallID;
  std::map<const llvm::Function *, std::map<unsigned, const llvm::CallInst *>>
      IDToCall;
  std::map<const llvm::CallInst *, std::set<unsigned>> PossibleSourceStates;

  static const TransitionInfo UNINITIALIZED_TRANSITION;

  unsigned getNextContextID() const { return static_cast<unsigned>(CI.size()); }

  ContextInfo &getContextInfo(const Context &C) {
    auto Result = CI.insert(
        std::make_pair(C, ContextInfo{getNextContextID(), {}, false}));
    if (Result.second) {
      IDToContextInfo[Result.first->second.ID] = &Result.first->second;
    }
    return Result.first->second;
  }

  unsigned getOrCreateCallID(const llvm::Function *F,
                             const llvm::CallInst *Call);

  const llvm::CallInst *getCall(const llvm::Function *F, unsigned ID) const;

  unsigned getReturnTransitionID(const llvm::Function *F) {
    return CallID[F][nullptr] = 0;
  }

  void addTransition(const Context &Src, const llvm::CallInst *Call,
                     llvm::Function *Target, bool ContextSensitiveTarget,
                     DFAConstructionContext &C);

  void createContextInsensitiveTransitions();

public:
  const TransitionInfo &getTransitionInfo(const Context &C,
                                          const llvm::CallInst *Call) const;

  const llvm::Function *getFunctionForContext(const Context &C) const;

  unsigned getCallID(const llvm::Function *F, const llvm::CallInst *Call) const;

  bool isInDFA(const llvm::Function *F) const { return CallID.count(F); }

  static ContextDFA build(const ContextSensitiveOptimizationDescription &D,
                          DFAConstructionContext &C);

  size_t getNumberOfStates() const { return CI.size(); }

  std::map<Context, ContextInfo>::const_iterator begin() const {
    return CI.begin();
  }

  std::map<Context, ContextInfo>::const_iterator end() const {
    return CI.end();
  }

  ContextIterator possibleSourceStatesBegin(const llvm::CallInst *Call) const;
  ContextIterator possibleSourceStatesEnd(const llvm::CallInst *Call) const;

  TransitionIterator transitionsBegin(const Context &C) const;
  TransitionIterator transitionsEnd(const Context &C) const;
};

} // namespace eos

#endif // CONTEXT_DFA_H_
