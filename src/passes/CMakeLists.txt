#add_llvm_library(LLVMEos
#   AnalyzeBenchmark.cpp
#   CloneConstantArgs.cpp

#  /home/gabriel/eos/llvm/dfa-context-sensitiveness/DFAContextSensitiveness.cpp
#  /home/gabriel/eos/llvm/dfa-context-sensitiveness/ContextDFA.cpp
#  /home/gabriel/eos/llvm/dfa-context-sensitiveness/ContextSensitiveOptimizationDescription.cpp

#   ADDITIONAL_HEADER_DIRS
#   ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms
#   ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms/Eos
#   )

# set(CMAKE_CXX_FLAGS "-ggdb -std=c++11")

add_llvm_component_library(LLVMEos
  AnalyzeBenchmark.cpp
  CloneConstantArgs.cpp

  ../DFAContextSensitiveness.cpp
  ../CloneWarsContextSensitiveness.cpp
  ../ContextDFA.cpp
  ../ContextSensitiveOptimizationDescription.cpp

  DEPENDS
  intrinsics_gen

  #  PLUGIN_TOOL
  #  opt
  )

# add_dependencies(LLVMEos intrinsics_gen)
