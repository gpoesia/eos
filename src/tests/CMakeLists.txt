include_directories("..")

set(CMAKE_CXX_FLAGS "-ggdb -std=c++14")

add_llvm_library(OptimizePrintTree MODULE
  OptimizePrintTree.cpp
  ../DFAContextSensitiveness.cpp
  ../ContextDFA.cpp
  ../ContextSensitiveOptimizationDescription.cpp

  PLUGIN_TOOL
  opt
  )
