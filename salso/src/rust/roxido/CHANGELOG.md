# Changelog

## 0.7.1

Breaking changes:

- `i32`, `usize` and `u8` arguments, and `RScalar`'s `i32()`, `usize()` and `u8()`, reject doubles
  that are not whole numbers, as vctrs and rlang do. `i32` used to round, so `2.7` became `3`.
  Whole doubles, such as `1` and `1e3`, still work.
- Reading `NA_character_` is an error rather than the string `"NA"`; `RVector<char>::is_na()`
  tests for it, and a `&str` argument rejects it. `RScalar::str()` returns a `Result`.
- Strings are read as UTF-8 by R's `translateCharUTF8`, so latin1 strings now work and strings
  with "bytes" encoding are an error. ASCII and UTF-8 strings are read as before, at no extra cost.
- An in-place argument raises an R error if it is `NULL` or the same object as another argument
  that is borrowed, as in `f(x, x)` in R.

New:

- An argument declared as `Option<T>`, such as `Option<&RMatrix>`, is `None` when R passes `NULL`
  and is otherwise converted as `T`.
- `set_attribute_with_pc` builds an attribute from R objects without `unsafe`.
- List `set` and `set_attribute` accept iterators as well as scalars and slices.
- Vectors, matrices and arrays have unsafe `new_uninit()`, which skips the zeroing of `new()` for
  a result that is about to be completely written, for example by worker threads.
- Errors for `&mut` and in-place arguments name the argument.
- `rprint!`, `rprintln!`, `reprint!` and `reprintln!` accept everything `format!` does.
- Compile errors for values that cannot be converted to R objects suggest a fix.

Build:

- The package's Rust code is compiled once per installation rather than twice, and Cargo decides
  what to rebuild. Changes it tracks outside `src/rust`, such as a path dependency, `RUSTFLAGS`
  or a new toolchain, are no longer missed.
- `build.rs` finds the `#[roxido]` functions by reading the crate's source files, following its
  modules and `#[cfg]` attributes. A `#[roxido]` function produced by another macro is not
  registered.
- After `roxido update`, add the build dependencies that `build.rs` needs to `src/rust/Cargo.toml`:

  ```toml
  [build-dependencies]
  syn = { version = "3", features = ["full"] }
  quote = "1.0"
  ```
- The template's release profile no longer includes debug information, which makes the compiled
  library much smaller (9.0 MB versus 3.6 MB for one package). Panics still report their file and
  line. Remove `debug = 1` from `[profile.release]` in `src/rust/Cargo.toml` by hand, since
  `roxido update` does not change that file.

Fixes:

- `&RVector<()>` and other `<()>` arguments accept any storage mode.
- A latin1 string passed as a `&str` argument no longer panics.
- `stop()` on `None` raises an R error with a message.
- The errors of `as_scalar`, `as_array` and `as_symbol` describe the expected type.
- On Windows with a GNU toolchain, the build adds to `RUSTFLAGS` rather than replacing it.

## 0.7.0

R objects and `&Pc` stay on R's main thread. Mutable arguments use copy-on-write.

Changes that compile but behave differently (the compiler will not point these out):

- A `&mut` argument is copied whenever R may still reference it, which is almost always.
  Changes reach R only if the function returns the object.
- A `&mut` argument raises an R error if it receives an object R cannot copy: `NULL`, an
  environment, a symbol, an external pointer or a primitive function. Declare an argument
  that may be `NULL` as `&RObject` and test `is_null()`. `clone` and list `get_mut` also
  reject these objects.
- `#[unsafe(in_place)]` skips the copy, so every R reference to the object sees the change. It is
  unsound if R can pass the same object in another argument, as in `f(x, x)` in R.
- List `get` and `get_mut` reject ALTREP lists, whose elements may be temporary. Copy them to
  ordinary lists first, for example with `lapply(x, identity)` in R.
- `R::null()`, `R::random_bytes()`, `RSymbol` functions and console calls panic off R's main
  thread or before initialization. Borrowed slices still work in scoped threads.
- `RFunction` calls quote arguments that R would evaluate, such as symbols, calls and bytecode.
- `new()` initializes numeric and logical data to zero.

Updating code:

1. List the `&mut` arguments of `#[roxido]` functions, for example with
   `grep -n '&mut' src/rust/src/*.rs`, and decide for each one:
   - only read: change it to `&`;
   - may be `NULL`: change it to `&RObject` and test `is_null()`;
   - modified and returned: keep `&mut`;
   - modified in place for R to see: mark it `#[unsafe(in_place)]`, only if R can never pass
     the same object in another argument.
2. If a library needs `&mut [T]` for data it only reads, prefer a read-only API. Copying with
   `.to_vec()` costs the same as a `&mut` argument.
3. Fix the compile errors:
   - `ToR<_>` is not implemented for `&mut RScalar<f64>` (or another R object) in
     `list.set(i, x)`: pass the Rust value instead of `x.to_r(pc)`, and pass vectors and
     arrays as slices. Build an element from several R objects with `list.set_with_pc(i, |pc| ...)`;
     unsafe `set_object` stores an existing R object.
   - Attribute setters take Rust values, such as `x.set_class(&["name"])` and
     `m.set_dimnames(&[Some(&rows), Some(&["a", "b"])])`. The `*_object` variants accept
     existing R objects and are unsafe. `get_dimnames` returns `&RList`.
   - Helper arguments of type `&mut Pc` become `&Pc`. Replace `Pc::default()` with
     `pc.nested(|pc| ...)`; only the innermost counter may protect objects.
   - `get_attribute` takes `pc` and protects its result.
   - `set_loop_with_pc` takes no index and returns `()`.
   - `RFunction::call0` through `call5`, `Pc::protect`, and external-pointer `decode_*`,
     `reencode`, `encode_full` and `set_address` require `unsafe`. `decode_val` needs
     `&mut self`; stored Rust values must be `'static`.
   - `&mut RExternalPtr` and `&mut RSymbol` arguments require `#[unsafe(in_place)]`.
   - Raw `SEXP` results need `unsafe { RObject::from_sexp(x, pc) }`. Generated entry points
     are unsafe, and `RObjectVariant` is sealed.
   - `__private_set_custom_panic_hook` is renamed `__private_init`; `roxido update` handles this.
4. Code that used `from_value(0, ...)` because `new()` was uninitialized can use `new()`.
5. Compare the old and new builds: run the examples and tests of both and diff the output.

Fixes:

- `RVector<char>::from_value` and `set_loop_with_pc` no longer overflow R's protection stack.
- Logical scalars are new objects, not R's shared `TRUE` and `FALSE`.
- Empty vectors give valid empty slices.

Remaining limits:

- R errors raised inside R's API still skip Rust destructors and thread joins; see "R errors"
  and "Threads" in the crate documentation.
- Strings borrowed from some third-party ALTREP character vectors can dangle. Copy them to
  ordinary vectors first, for example with `c(x)` in R.
