#include <R_ext/Error.h>

#if defined(_WIN32)
#  if defined(__GNUC__) || defined(__clang__)
__attribute__((noreturn))
#  endif
#else
#  if defined(__GNUC__) || defined(__clang__)
__attribute__((noreturn, visibility("hidden")))
#  endif
#endif
void abort(void) {
    Rf_error("Rust called abort(), but converted to an R error.");
}
