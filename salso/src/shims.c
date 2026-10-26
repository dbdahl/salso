// Last-resort conversion of runtime termination calls to R errors.
// This skips Rust cleanup and cannot safely recover from worker-thread failures.
// Ordinary panics should unwind to the #[roxido] boundary instead.

#if defined(__GNUC__) || defined(__clang__)
__attribute__((noreturn))
#endif
extern void Rf_error(const char *, ...);

#if defined(_WIN32)
#  define SHIM_ATTRS
#  if defined(__GNUC__) || defined(__clang__)
#    undef SHIM_ATTRS
#    define SHIM_ATTRS __attribute__((noreturn))
#  endif
#else
#  define SHIM_ATTRS
#  if defined(__GNUC__) || defined(__clang__)
#    undef SHIM_ATTRS
#    define SHIM_ATTRS __attribute__((noreturn, visibility("hidden")))
#  endif
#endif

#if defined(__GNUC__) || defined(__clang__)
__attribute__((noreturn))
#endif
static void exit_shim(const char *fn, int status) {
    Rf_error("Intercepted %s(%d) from Rust and converted to an R error.", fn, status);
}

SHIM_ATTRS void abort(void)       { exit_shim("abort", 0); }
SHIM_ATTRS void _exit(int status) { exit_shim("_exit", status); }
SHIM_ATTRS void exit(int status)  { exit_shim("exit", status); }
SHIM_ATTRS void _Exit(int status) { exit_shim("_Exit", status); }
