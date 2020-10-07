#include "ContextSensitiveOptimizationDescription.h"

namespace eos {

llvm::Function *const Context::MAIN_FUNCTION =
    reinterpret_cast<llvm::Function *>(0x1);

} // namespace eos
