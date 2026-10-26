//! The paper
//! [*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
//! complements
//! [*Writing R Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html)
//! (the official guide for writing R extensions) for those interested in developing
//! [R](https://www.r-project.org/) packages using
//! [Rust](https://www.rust-lang.org/). It highlights idiosyncrasies of
//! [R](https://www.r-project.org/) and [Rust](https://www.rust-lang.org/) that must
//! be addressed by any integration and describes how to develop
//! [Rust](https://www.rust-lang.org/)-based packages which comply with the [CRAN
//! Repository Policy](https://cran.r-project.org/web/packages/policies.html).  The
//! [paper]( https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
//! introduces the cargo framework, a
//! transparent [Rust](https://www.rust-lang.org/)-based API which wraps
//! commonly-used parts of [R](https://www.r-project.org/)'s API with minimal
//! overhead and allows a programmer to easily add additional wrappers.
//!
//! This crate provides the Rust API for the cargo framework.  Of particular note
//! is [R] and [RObject].
//!
//! # Example
//!
//! This example function takes two R vectors and computes the convolution
//! of them. The body of a function with the roxido attribute may return any value
//! with a `to_r` method, and its parameters may be references to R objects or Rust
//! values converted from them.
//!
//! ```
//! use roxido::*;
//!
//! #[roxido]
//! fn convolve2(a: &RVector, b: &RVector) {
//!     let vec = RVector::from_value(0.0, a.len() + b.len() - 1, pc);
//!     let ab = vec.slice_mut();
//!     for (i, ai) in a.to_f64(pc).slice().iter().enumerate() {
//!         for (j, bj) in b.to_f64(pc).slice().iter().enumerate() {
//!             ab[i + j] += ai * bj;
//!         }
//!     }
//!     vec
//! }
//! ```
//!
//! # R errors
//!
//! A panic or [stop!] in a `#[roxido]` function unwinds Rust normally and then raises an R error.
//! An R error raised inside R's API instead long-jumps over Rust frames, which Rust does not
//! formally support, and skips their destructors. Roxido avoids common sources, such as NUL bytes
//! in strings, but not all: allocation failure, overflowing R's protection stack (see
//! [Pc::nested]), an invalid attribute such as a mismatched `dim`, a coercion warning under
//! `options(warn = 2)`, or an ALTREP vector that fails to expand can still raise one.
//! As a last resort, the package's shims turn `abort` and `exit` into R errors; this skips Rust
//! cleanup and is not safe on worker threads. Avoid them and panicking destructors; use [stop!].
//!
//! # Threads
//!
//! R objects and references to [Pc] cannot be sent to other threads. Borrowed data slices can be
//! used in scoped threads. Calls such as `rprintln!()` and [RSymbol::new] check [R::is_main_thread]
//! and panic on worker threads or before package initialization. To report that panic as an R
//! error, propagate it back to the `#[roxido]` caller on R's main thread: join each thread and
//! pass a panic's payload to [std::panic::resume_unwind]. Do not rely on [std::thread::scope]
//! to join a thread that might panic, since the scope then replaces the panic's message with
//! "a scoped thread panicked".
//!
//! Allocate R objects and obtain slices before spawning workers. Until all workers have joined,
//! the main thread must not call anything that can raise an R error, such as allocating R
//! objects: the error would jump past the joins while workers still borrow R data. The `rprint!`
//! family, [R::flush_console], and [R::check_user_interrupt] are safe because they run under
//! `R_ToplevelExec`.
//!
//! ```
//! fn sum(values: &[f64]) -> f64 {
//!     let (left, right) = values.split_at(values.len() / 2);
//!     std::thread::scope(|scope| {
//!         let worker = scope.spawn(|| left.iter().sum::<f64>());
//!         let total = right.iter().sum::<f64>();
//!         total + worker.join().unwrap_or_else(|e| std::panic::resume_unwind(e))
//!     })
//! }
//! # assert_eq!(sum(&[1.0; 3000]), 3000.0);
//! ```
//!
//! # Mutation
//!
//! A `&mut` argument to a `#[roxido]` function, and an element from [RList::get_mut], is copied
//! first if anything else in R may reference it, as R does; for arguments, this is the usual case.
//! Return the object to make changes visible in R. Declaring `&mut RVector<f64>` rather than
//! `&mut [f64]` lets you return it without a second copy.
//!
//! ```
//! use roxido::*;
//!
//! fn numerical_kernel(values: &mut [f64]) {
//!     values.iter_mut().for_each(|x| *x *= 2.0);
//! }
//!
//! #[roxido]
//! fn transform(x: &mut RVector<f64>) {
//!     numerical_kernel(x.slice_mut());
//!     x
//! }
//! ```
//!
//! Marking an argument `#[unsafe(in_place)]`, as in `#[unsafe(in_place)] x: &mut [f64]`, skips the
//! copy, so every R reference to the object sees the change, including literals in compiled R
//! code. R cannot copy external pointers, symbols, or environments, so `&mut` access to them
//! requires `#[unsafe(in_place)]`; [RList::get_mut] and [RObjectVariant::clone] reject them.
//! An in-place argument raises an R error if it is `NULL` or the same object as another argument
//! that is borrowed. This does not detect an object that is reachable from another argument, such
//! as a list element or an attribute, so the aliasing rule below still applies.
//!
//! ## Aliasing
//!
//! R lets many references reach one object; Rust does not. While Rust holds a mutable reference
//! to an R object, nothing else may access it, and while Rust holds a shared reference, including
//! a slice or `&str` from it, nothing else may modify it. This applies to everything reachable
//! from the object, such as list elements and attributes. Safe methods uphold this rule; for
//! example, [RList::set] and [RObjectVariant::set_class] store a new object made from a Rust value,
//! and [RList::set_with_pc] stores a new object that its closure builds.
//! The unsafe ones let R reach an object Rust may still be using: in-place arguments, setters that
//! store an existing object, such as [RList::set_object], and [RFunction] calls, whose callbacks
//! may keep their arguments or return existing objects.

// Helpful resources:
//   https://cran.r-project.org/doc/manuals/r-release/R-ints.html
//   https://svn.r-project.org/R/trunk/src/include/Rinternals.h
//   https://github.com/hadley/r-internals
//   https://www.tidyverse.org/blog/2019/05/resource-cleanup-in-c-and-the-r-api
//   https://github.com/wch/r-source

/// A procedural macro to facilitate calling a Rust function from R.
pub use roxido_macro::roxido;

pub use rbindings;
pub use rbindings::SEXP;

use rbindings::*;
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{CStr, CString, NulError, c_char, c_void};
use std::marker::PhantomData;

trait SEXPMethods {
    /// # Safety
    /// Expert use only.
    unsafe fn transmute<T: RObjectVariant, A>(self, _anchor: &A) -> &T;

    /// # Safety
    /// Expert use only.
    #[expect(clippy::mut_from_ref)]
    unsafe fn transmute_mut<T: RObjectVariant, A>(self, _anchor: &A) -> &mut T;

    /// # Safety
    /// Expert use only.
    unsafe fn transmute_static<T: RObjectVariant>(self) -> &'static T;
}

impl SEXPMethods for SEXP {
    unsafe fn transmute<T: RObjectVariant, A>(self, _anchor: &A) -> &T {
        unsafe { &*self.cast::<T>() }
    }

    unsafe fn transmute_mut<T: RObjectVariant, A>(self, _anchor: &A) -> &mut T {
        unsafe { &mut *self.cast::<T>() }
    }

    unsafe fn transmute_static<T: RObjectVariant>(self) -> &'static T {
        unsafe { &*self.cast::<T>() }
    }
}

mod private {
    pub trait Sealed {}
}

/// Methods shared by Roxido's R object types.
pub trait RObjectVariant: private::Sealed + Sized {
    /// Return the underlying SEXP, a pointer to an R object.
    fn sexp(&self) -> SEXP {
        self as *const Self as SEXP
    }

    /// Recharacterize as a reference to an [`RObject`].
    fn as_robject(&self) -> &RObject {
        unsafe { self.transmute() }
    }

    /// Recharacterize as a mutable reference to an [`RObject`].
    fn as_robject_mut(&mut self) -> &mut RObject {
        unsafe { self.transmute_mut() }
    }

    /// # Safety
    /// The object must have the R type represented by `T`.
    unsafe fn transmute<T: RObjectVariant>(&self) -> &T
    where
        Self: Sized,
    {
        unsafe { self.sexp().transmute(self) }
    }

    /// # Safety
    /// The object must have the R type represented by `T`.
    unsafe fn transmute_mut<T: RObjectVariant>(&mut self) -> &mut T
    where
        Self: Sized,
    {
        unsafe { self.sexp().transmute_mut(self) }
    }

    /// Duplicate an R object.
    ///
    /// Panics if R returns the original object, as it does for external pointers and symbols.
    ///
    #[expect(clippy::mut_from_ref)]
    fn clone<'a>(&self, pc: &'a Pc) -> &'a mut Self {
        unsafe { pc.protect_and_transmute(duplicate_for_mutation(self.sexp(), false).stop()) }
    }

    /// Get the class of an R object.
    fn get_class(&self) -> &RVector<char> {
        unsafe { Rf_getAttrib(self.sexp(), R_ClassSymbol).transmute(self) }
    }

    /// Set the class of an R object.
    fn set_class(&mut self, names: &[&str]) {
        unsafe {
            let pc = Pc::__private_new();
            self.set_class_object(names.to_r(&pc));
        }
    }

    /// Set the class of an R object to an existing character vector.
    ///
    /// # Safety
    /// `names` is stored without copying; see the [aliasing rule](crate#aliasing).
    unsafe fn set_class_object(&mut self, names: &RVector<char>) {
        unsafe {
            Rf_classgets(self.sexp(), names.sexp());
        }
    }

    /// Get an attribute, protecting values R may create on demand, such as row names.
    fn get_attribute<'a>(&'a self, which: &RSymbol, pc: &'a Pc) -> &'a RObject {
        unsafe {
            pc.protect(Rf_getAttrib(self.sexp(), which.sexp()))
                .transmute(self)
        }
    }

    /// Convert a Rust value, such as a scalar, slice, or iterator, to a new R object and set it as
    /// an attribute.
    fn set_attribute<T: RObjectVariant, M>(&mut self, which: &RSymbol, value: impl IntoR<T, M>) {
        self.set_attribute_with_pc(which, |pc| value.into_r(pc));
    }

    /// Set as an attribute the object that `x` builds with a temporary protection counter.
    ///
    /// `x` can return only an object it creates from the counter, or a `'static` object such as
    /// `NULL` or a symbol, so nothing else references it.
    fn set_attribute_with_pc<T: RObjectVariant, F: FnOnce(&Pc) -> &T>(
        &mut self,
        which: &RSymbol,
        x: F,
    ) {
        unsafe {
            let pc = Pc::__private_new();
            Rf_setAttrib(self.sexp(), which.sexp(), x(&pc).sexp());
        }
    }

    /// Set an existing R object as an attribute.
    ///
    /// # Safety
    /// `value` is stored without copying; see the [aliasing rule](crate#aliasing).
    unsafe fn set_attribute_object(&mut self, which: &RSymbol, value: &impl RObjectVariant) {
        unsafe {
            Rf_setAttrib(self.sexp(), which.sexp(), value.sexp());
        }
    }
}

/// Methods for R objects that can be sliced.
pub trait RSliceable<T> {
    /// Return a slice of the R object's data.
    fn slice(&self) -> &[T];

    /// Return a mutable slice of the R object's data.
    fn slice_mut(&mut self) -> &mut [T];
}

/// Methods for R objects that have a length attribute.
pub trait RHasLength: RObjectVariant {
    /// Returns the length of the R object.
    fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.sexp()) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    /// Checks to see if the R object is empty.
    fn is_empty(&self) -> bool {
        unsafe { Rf_xlength(self.sexp()) == 0 }
    }

    /// Checks to see if the R object is a scalar (i.e., has length equal to one).
    fn is_scalar(&self) -> bool {
        unsafe { Rf_xlength(self.sexp()) == 1 }
    }
}

/// Get and set methods for R objects with names.
pub trait RHasNames: RHasLength {
    /// Get names of the R object.
    fn get_names(&self) -> &RVector<char> {
        unsafe { Rf_getAttrib(self.sexp(), R_NamesSymbol).transmute(self) }
    }

    /// Set names of the R object.
    fn set_names(&mut self, names: &[&str]) -> Result<(), &'static str> {
        unsafe {
            let pc = Pc::__private_new();
            self.set_names_object(names.to_r(&pc))
        }
    }

    /// Set names of the R object to an existing character vector.
    ///
    /// # Safety
    /// `names` is stored without copying; see the [aliasing rule](crate#aliasing).
    unsafe fn set_names_object(&mut self, names: &RVector<char>) -> Result<(), &'static str> {
        if unsafe { Rf_length(names.sexp()) != Rf_length(self.sexp()) } {
            return Err("Length of names is not correct.");
        }
        unsafe {
            Rf_namesgets(self.sexp(), names.sexp());
        }
        Ok(())
    }
}

/// Method for creating a scalar in R (i.e., an R vector of length one) from a value.
pub trait RScalarConstructor<T> {
    #[expect(clippy::mut_from_ref)]
    fn from_value(value: T, pc: &Pc) -> &mut Self;
}

/// Methods for creating an R object from an iterator, which must yield exactly its reported length.
pub trait RFromIterator<T> {
    #[expect(clippy::mut_from_ref)]
    fn from_iter1<S>(iter: S, pc: &Pc) -> &mut Self
    where
        S: IntoIterator<Item = T> + ExactSizeIterator;

    #[expect(clippy::mut_from_ref)]
    fn from_iter2<'a, 'b, S>(iter: S, pc: &'a Pc) -> &'a mut Self
    where
        S: IntoIterator<Item = &'b T> + ExactSizeIterator,
        T: 'b;
}

/// Get and set methods for scalars.
pub trait RGetSet0<T> {
    /// Get the value of a scalar.
    fn get(&self) -> T;

    /// Set the value of a scalar.
    fn set(&mut self, value: T);
}

/// Get and set methods for one-dimensional data.
pub trait RGetSet1<T> {
    /// Get the value at a certain index in the R object.
    fn get(&self, index: usize) -> Result<T, &'static str>;

    /// Set the value at a certain index in the R object.
    fn set(&mut self, index: usize, value: T) -> Result<(), &'static str>;
}

/// Get and set methods for two-dimensional data.
pub trait RGetSet2<T> {
    /// Get the value at a certain row and column in the R object.
    fn get(&self, row: usize, col: usize) -> Result<T, &'static str>;

    /// Set the value at a certain row and column in the R object.
    fn set(&mut self, row: usize, col: usize, value: T) -> Result<(), &'static str>;
}

/// Get and set methods for N-dimensional data.
pub trait RGetSetN<T> {
    /// Get the value at a certain index in the R object.
    fn get(&self, index: &[usize]) -> Result<T, &'static str>;

    /// Set the value at a certain index in the R object.
    fn set(&mut self, index: &[usize], value: T) -> Result<(), &'static str>;
}

/// Methods for creating a new vector in R.
pub trait RVectorConstructors<T> {
    /// Allocate a vector initialized to zero, `false`, or empty strings.
    #[expect(clippy::mut_from_ref)]
    fn new(length: usize, pc: &Pc) -> &mut Self;

    /// Allocate a vector without initializing its data, which saves time when every element is
    /// about to be written, for example by worker threads. Strings are still initialized to empty.
    ///
    /// # Safety
    /// Write every element, for example through `slice_mut`, before reading any of them and before
    /// R can see the vector, such as by returning it.
    #[expect(clippy::mut_from_ref)]
    unsafe fn new_uninit(length: usize, pc: &Pc) -> &mut Self;

    #[expect(clippy::mut_from_ref)]
    fn from_value(value: T, length: usize, pc: &Pc) -> &mut Self;

    #[expect(clippy::mut_from_ref)]
    fn from_array<const N: usize>(slice: [T; N], pc: &Pc) -> &mut Self;

    #[expect(clippy::mut_from_ref)]
    fn from_slice<'a>(slice: &[T], pc: &'a Pc) -> &'a mut Self;
}

/// Methods for creating a new matrix in R.
pub trait RMatrixConstructors<T> {
    /// Allocate a matrix initialized to zero.
    #[expect(clippy::mut_from_ref)]
    fn new(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self;

    /// Allocate a matrix without initializing its data, which saves time when every element is
    /// about to be written, for example by worker threads.
    ///
    /// # Safety
    /// Write every element, for example through `slice_mut`, before reading any of them and before
    /// R can see the matrix, such as by returning it.
    #[expect(clippy::mut_from_ref)]
    unsafe fn new_uninit(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self;

    #[expect(clippy::mut_from_ref)]
    fn from_value(value: T, nrow: usize, ncol: usize, pc: &Pc) -> &mut Self;
}

/// Methods for creating a new array in R.
pub trait RArrayConstructors<T> {
    /// Allocate an array initialized to zero.
    #[expect(clippy::mut_from_ref)]
    fn new<'a>(dim: &[usize], pc: &'a Pc) -> &'a mut Self;

    /// Allocate an array without initializing its data, which saves time when every element is
    /// about to be written, for example by worker threads.
    ///
    /// # Safety
    /// Write every element, for example through `slice_mut`, before reading any of them and before
    /// R can see the array, such as by returning it.
    #[expect(clippy::mut_from_ref)]
    unsafe fn new_uninit<'a>(dim: &[usize], pc: &'a Pc) -> &'a mut Self;

    #[expect(clippy::mut_from_ref)]
    fn from_value<'a>(value: T, dim: &[usize], pc: &'a Pc) -> &'a mut Self;
}

static TOO_LONG: &str = "Could not fit usize into i32.";

// Convert a double to an integer type, requiring a whole number within the type's range.
fn whole_number<T: TryFrom<i128>>(y: f64) -> Result<T, &'static str> {
    if R::is_na_f64(y) {
        Err("Equals R's NA for doubles.")
    } else if y.is_nan() {
        Err("Equals R's NaN.")
    } else if y.is_finite() && y.fract() != 0.0 {
        Err("Not a whole number.")
    } else {
        // A whole double converts to i128 exactly, or saturates if it is too large for any type.
        T::try_from(y as i128).map_err(|_| {
            if y >= 0.0 {
                "Greater than the maximum value."
            } else if T::try_from(-1).is_err() {
                "Negative value not expected."
            } else {
                "Less than the minimum value."
            }
        })
    }
}

// Describe an object that R cannot copy, or return `None` if R can copy it.
// `sexp` must be a live R object on R's main thread.
unsafe fn uncopyable_description(sexp: SEXP) -> Option<&'static str> {
    match unsafe { TYPEOF(sexp) } as u32 {
        NILSXP => Some("NULL"),
        SYMSXP => Some("a symbol"),
        ENVSXP => Some("an environment"),
        SPECIALSXP | BUILTINSXP => Some("a primitive function"),
        EXTPTRSXP => Some("an external pointer"),
        BCODESXP => Some("byte code"),
        WEAKREFSXP => Some("a weak reference"),
        CHARSXP => Some("an element of a character vector"),
        PROMSXP => Some("a promise"),
        _ => None,
    }
}

// `sexp` must be a live R object on R's main thread.
unsafe fn check_mutable(sexp: SEXP) -> Result<(), &'static str> {
    match unsafe { uncopyable_description(sexp) } {
        Some(_) => {
            Err("R cannot copy this object for mutable access; use explicit unsafe borrowing.")
        }
        None => Ok(()),
    }
}

// `sexp` must be rooted on R's main thread; protect the result before allocating again.
unsafe fn duplicate_for_mutation(sexp: SEXP, shallow: bool) -> Result<SEXP, &'static str> {
    let copy = unsafe {
        if shallow {
            Rf_shallow_duplicate(sexp)
        } else {
            Rf_duplicate(sexp)
        }
    };
    if copy == sexp {
        Err("R did not copy this object for mutable access; use explicit unsafe borrowing.")
    } else {
        Ok(copy)
    }
}

// `data` must be writable for `len` elements, with no outstanding borrows.
unsafe fn fill_uninit<T: Copy>(data: *mut T, len: usize, value: T) {
    for i in 0..len {
        unsafe { data.add(i).write(value) };
    }
}

// As for `fill_uninit`; the iterator's length is checked, not trusted.
unsafe fn write_iter<T: Copy>(data: *mut T, len: usize, mut iter: impl Iterator<Item = T>) {
    for i in 0..len {
        let value = iter
            .next()
            .stop_str("Iterator length does not match its reported length.");
        unsafe { data.add(i).write(value) };
    }
    if iter.next().is_some() {
        crate::stop!("Iterator length does not match its reported length.");
    }
}

fn charsxp_from_str(x: &str) -> SEXP {
    if x.contains('\0') {
        crate::stop!("R strings cannot contain NUL bytes.");
    }
    unsafe {
        Rf_mkCharLenCE(
            x.as_ptr() as *const c_char,
            x.len().try_into().stop_str(TOO_LONG),
            cetype_t_CE_UTF8,
        )
    }
}

// Read an element of a character vector as UTF-8, as R recommends for C code.
fn charsxp_as_str<T>(sexp: SEXP, _anchor: &T) -> Result<&str, &'static str> {
    unsafe {
        if sexp == R_NaString {
            return Err("Equals R's NA for strings.");
        }
        // R raises an error when asked to translate a string with "bytes" encoding.
        if Rf_getCharCE(sexp) == cetype_t_CE_BYTES {
            return Err("Has \"bytes\" encoding, which cannot be converted to UTF-8.");
        }
        // ASCII and UTF-8 strings are returned as they are. Others, such as latin1, are translated
        // into memory that R releases when the `.Call` returns.
        let c_str = CStr::from_ptr(Rf_translateCharUTF8(sexp));
        c_str.to_str().map_err(|_| "Not valid UTF-8.")
    }
}

fn dimnames_to_r<'a>(dimnames: &[Option<&[&str]>], pc: &'a Pc) -> &'a RList {
    let list = RList::new(dimnames.len(), pc);
    for (i, names) in dimnames.iter().enumerate() {
        if let Some(names) = names {
            let _ = list.set(i, *names);
        }
    }
    list
}

thread_local! {
    static IS_MAIN_THREAD: Cell<bool> = const { Cell::new(false) };
    static PC_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// Panic unless on R's main thread. Needed only where no R object or `Pc` is at hand.
#[inline]
fn check_main_thread() {
    if !R::is_main_thread() {
        not_main_thread()
    }
}

#[cold]
#[inline(never)]
fn not_main_thread() -> ! {
    crate::stop!("R's API was used outside R's main thread or before roxido was initialized.")
}

#[cold]
#[inline(never)]
fn not_innermost() -> ! {
    crate::stop!("Only the innermost protection counter may protect objects.")
}

/// Run `f` under `R_ToplevelExec`, returning `false` if it raised an R error.
fn toplevel_exec(f: unsafe extern "C" fn(*mut c_void), data: *mut c_void) -> bool {
    let depth = PC_DEPTH.with(Cell::get);
    let ok = unsafe { R_ToplevelExec(Some(f), data) } != 0;
    // R code run by `f`, such as an event-loop callback, may have jumped over nested counters.
    PC_DEPTH.with(|d| d.set(depth));
    ok
}

// `main_thread_only` is neither `Send` nor `Sync`, which keeps R objects on R's main thread.
macro_rules! robject_variant {
    ($name:ident) => {
        #[repr(C)]
        pub struct $name {
            sexprec: SEXPREC,
            main_thread_only: PhantomData<*const ()>,
        }
        impl private::Sealed for $name {}
        impl RObjectVariant for $name {}
    };
}

robject_variant!(RObject);
robject_variant!(RSymbol);
robject_variant!(RList);
robject_variant!(RDataFrame);
robject_variant!(RFunction);
robject_variant!(RExternalPtr);
robject_variant!(RError);

macro_rules! robject_variant_with_type {
    ($name:ident) => {
        #[repr(C)]
        pub struct $name<RMode = ()> {
            sexprec: SEXPREC,
            rtype: PhantomData<RMode>,
            main_thread_only: PhantomData<*const ()>,
        }
        impl<T> private::Sealed for $name<T> {}
        impl<T> RObjectVariant for $name<T> {}
        impl<T> RHasLength for $name<T> {}
    };
}

robject_variant_with_type!(RScalar);
robject_variant_with_type!(RVector);
robject_variant_with_type!(RMatrix);
robject_variant_with_type!(RArray);

impl RHasLength for RList {}
impl RHasLength for RDataFrame {}

impl<T> RHasNames for RScalar<T> {}
impl<T> RHasNames for RVector<T> {}
impl RHasNames for RList {}
impl RHasNames for RDataFrame {}

/// Protects R objects until the current scope ends.
/// Only the innermost counter may add roots because R unprotects in stack order.
pub struct Pc {
    counter: Cell<i32>,
    depth: usize,
}

impl Drop for Pc {
    fn drop(&mut self) {
        let count = self.counter.get();
        if count > 0 {
            unsafe { Rf_unprotect(count) };
        }
        PC_DEPTH.with(|depth| depth.set(self.depth - 1));
    }
}

impl Pc {
    /// Create the protection counter supplied as `pc` by `#[roxido]`.
    ///
    /// # Safety
    /// Drop counters in reverse creation order, and expose only `&Pc` to safe code.
    #[doc(hidden)]
    pub unsafe fn __private_new() -> Self {
        check_main_thread();
        let depth = PC_DEPTH.with(|depth| {
            depth.set(depth.get() + 1);
            depth.get()
        });
        Self {
            counter: 0.into(),
            depth,
        }
    }

    /// Run a closure with a temporary protection counter, then unprotect its objects.
    ///
    /// Use this in a loop that would otherwise protect more objects than R's protection stack
    /// holds. Inside the closure, only the new counter may protect objects, and references to
    /// its objects cannot escape; copy out the data you need.
    pub fn nested<T>(&self, f: impl FnOnce(&Pc) -> T) -> T {
        let pc = unsafe { Self::__private_new() };
        f(&pc)
    }

    #[inline]
    fn check_active(&self) {
        if PC_DEPTH.with(Cell::get) != self.depth {
            not_innermost()
        }
    }

    /// Protect a raw R object until this counter is dropped.
    ///
    /// # Safety
    /// `sexp` must point to a live R object.
    pub unsafe fn protect(&self, sexp: SEXP) -> SEXP {
        self.check_active();
        unsafe { Rf_protect(sexp) };
        self.counter.set(self.counter.get() + 1);
        sexp
    }

    /// # Safety
    /// `sexp` must point to a live R object of type `T`, and the returned reference must follow the
    /// [aliasing rule](crate#aliasing).
    #[expect(clippy::mut_from_ref)]
    pub unsafe fn protect_and_transmute<T: RObjectVariant>(&self, sexp: SEXP) -> &mut T {
        unsafe { self.protect(sexp).transmute_mut(self) }
    }
}

pub struct R;

impl R {
    /// Whether the caller is on R's main thread.
    ///
    /// Returns `false` before package initialization.
    pub fn is_main_thread() -> bool {
        IS_MAIN_THREAD.with(Cell::get)
    }

    /// Returns R's `NULL` value.
    pub fn null() -> &'static RObject {
        check_main_thread();
        unsafe { R_NilValue.transmute_static() }
    }

    /// Returns R's `NULL` value.
    #[expect(non_snake_case)]
    pub fn NULL() -> &'static RObject {
        Self::null()
    }

    /// Returns R's `TRUE` value for storage mode "integer".
    #[expect(non_snake_case)]
    pub fn TRUE() -> i32 {
        Rboolean_TRUE
    }

    /// Returns R's `FALSE` value for storage mode "integer".
    #[expect(non_snake_case)]
    pub fn FALSE() -> i32 {
        Rboolean_FALSE
    }

    /// Returns R's `NA` value for storage mode "double".
    pub fn na_f64() -> f64 {
        unsafe { R_NaReal }
    }

    /// Returns R's `NA` value for storage mode "integer".
    pub fn na_i32() -> i32 {
        unsafe { R_NaInt }
    }

    /// Returns R's `NA` value for storage mode "logical".
    pub fn na_bool() -> i32 {
        unsafe { R_NaInt }
    }

    /// Returns R's `NaN` value.
    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    /// Returns R's `Inf` value.
    pub fn positive_infinity() -> f64 {
        unsafe { R_PosInf }
    }

    /// Returns R's `-Inf` value.
    pub fn negative_infinity() -> f64 {
        unsafe { R_NegInf }
    }

    /// Checks if an `f64` is R's `NA` value.
    pub fn is_na_f64(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    /// Checks if an `i32` is R's `NA` value.
    pub fn is_na_i32(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    /// Checks if a logical value (stored by R in an `i32`) is R's `NA` value.
    pub fn is_na_bool(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    /// Checks if a logical value (stored as an `i32`) can be evaluated as R's `TRUE`.
    pub fn is_true(x: i32) -> bool {
        x != Rboolean_FALSE && !Self::is_na_bool(x)
    }

    /// Convert a `bool` into a logical value (stored as an `i32`).
    pub fn as_logical(x: bool) -> i32 {
        if x { Rboolean_TRUE } else { Rboolean_FALSE }
    }

    /// Checks if an `f64` is R's `NaN` value.
    pub fn is_nan(x: f64) -> bool {
        unsafe { R_IsNaN(x) != 0 }
    }

    /// Checks if an `f64` is considered finite in R.
    pub fn is_finite(x: f64) -> bool {
        unsafe { R_finite(x) != 0 }
    }

    /// Checks if an `f64` is considered to be positive infinity in R.
    pub fn is_positive_infinity(x: f64) -> bool {
        unsafe { x == R_PosInf }
    }

    /// Checks if an `f64` is considered to be negative infinity in R.
    pub fn is_negative_infinity(x: f64) -> bool {
        unsafe { x == R_NegInf }
    }

    /// Generate random bytes using R's random number generator.
    ///
    /// This can be used to seed a random number generator that is native to Rust.
    pub fn random_bytes<const LENGTH: usize>() -> [u8; LENGTH] {
        check_main_thread();
        unsafe {
            let m = (u8::MAX as f64) + 1.0;
            let mut bytes: [u8; LENGTH] = [0; LENGTH];
            GetRNGstate();
            for x in bytes.iter_mut() {
                *x = R_unif_index(m) as u8;
            }
            PutRNGstate();
            bytes
        }
    }

    /// Flush R's console.
    pub fn flush_console() {
        check_main_thread();
        extern "C" fn flush_console_fn(_: *mut c_void) {
            unsafe { R_FlushConsole() };
        }
        toplevel_exec(flush_console_fn, std::ptr::null_mut());
    }

    /// Check if the user has attempted to interrupt the execution.
    pub fn check_user_interrupt() -> bool {
        check_main_thread();
        extern "C" fn check_interrupt_fn(_: *mut c_void) {
            unsafe { R_CheckUserInterrupt() };
        }
        !toplevel_exec(check_interrupt_fn, std::ptr::null_mut())
    }
}

/// An enumeration of the supported variants of R objects.
pub enum RObjectEnum<'a> {
    RObject(&'a RObject),
    RScalar(&'a RScalar),
    RVector(&'a RVector),
    RMatrix(&'a RMatrix),
    RArray(&'a RArray),
    RSymbol(&'a RSymbol),
    RList(&'a RList),
    RDataFrame(&'a RDataFrame),
    RFunction(&'a RFunction),
    RExternalPtr(&'a RExternalPtr),
    RError(&'a RError),
}

impl RObject {
    // Convert to an enumeration of the supported variants.
    pub fn enumerate(&self) -> RObjectEnum<'_> {
        if self.is_vector() {
            let s: &RVector = unsafe { self.transmute() };
            if s.is_scalar() {
                RObjectEnum::RScalar(unsafe { s.transmute() })
            } else {
                RObjectEnum::RVector(s)
            }
        } else if self.is_matrix() {
            RObjectEnum::RMatrix(unsafe { self.transmute() })
        } else if self.is_array() {
            RObjectEnum::RArray(unsafe { self.transmute() })
        } else if self.is_symbol() {
            RObjectEnum::RSymbol(unsafe { self.transmute() })
        } else if self.is_list() {
            RObjectEnum::RList(unsafe { self.transmute() })
        } else if self.is_data_frame() {
            RObjectEnum::RDataFrame(unsafe { self.transmute() })
        } else if self.is_function() {
            RObjectEnum::RFunction(unsafe { self.transmute() })
        } else if self.is_external_ptr() {
            RObjectEnum::RExternalPtr(unsafe { self.transmute() })
        } else {
            RObjectEnum::RObject(self)
        }
    }

    /// Borrow a raw R object without protecting it.
    ///
    /// # Safety
    /// Call on R's main thread. `sexp` must remain a live R object for the returned lifetime.
    pub unsafe fn from_sexp<A>(sexp: SEXP, anchor: &A) -> &Self {
        unsafe { sexp.transmute(anchor) }
    }

    /// Mutably borrow a raw R object without protecting it.
    ///
    /// # Safety
    /// Call on R's main thread. `sexp` must remain a live R object for the returned lifetime, and
    /// the reference must follow the [aliasing rule](crate#aliasing).
    #[expect(clippy::mut_from_ref)]
    pub unsafe fn from_sexp_mut<A>(sexp: SEXP, anchor: &A) -> &mut Self {
        unsafe { sexp.transmute_mut(anchor) }
    }

    /// Mutably borrow an argument from R, first copying it if it is referenced, as R does.
    #[doc(hidden)]
    #[expect(clippy::mut_from_ref)]
    pub unsafe fn __private_from_arg_mut<'a>(sexp: SEXP, name: &str, pc: &'a Pc) -> &'a mut Self {
        unsafe {
            if let Some(description) = uncopyable_description(sexp) {
                crate::stop!(
                    "'{name}' is expected to be an object that R can copy, but it is {description}."
                );
            }
            // `.Call` does not count its own reference, so any reference may be the caller's.
            if NO_REFERENCES(sexp) == 0 {
                let Ok(copy) = duplicate_for_mutation(sexp, true) else {
                    crate::stop!("'{name}' is expected to be an object that R can copy.");
                };
                pc.protect(copy).transmute_mut(pc)
            } else {
                sexp.transmute_mut(pc)
            }
        }
    }

    /// Check an argument that is modified in place: it may not be the same object as another
    /// argument that is borrowed, nor `NULL` unless it is optional.
    #[doc(hidden)]
    pub unsafe fn __private_check_in_place(
        sexp: SEXP,
        name: &str,
        optional: bool,
        others: &[(SEXP, &str)],
    ) {
        if unsafe { Rf_isNull(sexp) } != 0 {
            if optional {
                return;
            }
            crate::stop!(
                "'{name}' is expected to be an object that can be modified in place, but it is NULL."
            );
        }
        for (other, other_name) in others {
            if *other == sexp {
                crate::stop!("'{name}' and '{other_name}' are expected to be different objects.");
            }
        }
    }

    /// Returns an `Option` which equals `None` if the [`RObject`] reference is null.
    pub fn as_option(&self) -> Option<&Self> {
        if self.is_null() { None } else { Some(self) }
    }

    /// Attempts to recharacterize as a reference to an [`RScalar`] (i.e., a vector of length 1).
    pub fn as_scalar(&self) -> Result<&RScalar, &'static str> {
        let s = self.as_vector()?;
        if s.is_scalar() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a scalar.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RScalar`] (i.e., a vector of length 1).
    pub fn as_scalar_mut(&mut self) -> Result<&mut RScalar, &'static str> {
        let s = self.as_vector()?;
        if s.is_scalar() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a scalar.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RVector`].
    pub fn as_vector(&self) -> Result<&RVector, &'static str> {
        if self.is_vector() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a vector.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RVector`].
    pub fn as_vector_mut(&mut self) -> Result<&mut RVector, &'static str> {
        if self.is_vector() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a vector.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RMatrix`].
    pub fn as_matrix(&self) -> Result<&RMatrix, &'static str> {
        if self.is_matrix() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a matrix.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RMatrix`].
    pub fn as_matrix_mut(&mut self) -> Result<&mut RMatrix, &'static str> {
        if self.is_matrix() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a matrix.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RArray`].
    pub fn as_array(&self) -> Result<&RArray, &'static str> {
        if self.is_array() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not an array.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RArray`].
    pub fn as_array_mut(&mut self) -> Result<&mut RArray, &'static str> {
        if self.is_array() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not an array.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RList`].
    pub fn as_list(&self) -> Result<&RList, &'static str> {
        if self.is_list() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a list.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RList`].
    pub fn as_list_mut(&mut self) -> Result<&mut RList, &'static str> {
        if self.is_list() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a list.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RDataFrame`].
    pub fn as_data_frame(&self) -> Result<&RDataFrame, &'static str> {
        if self.is_data_frame() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a data frame.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RDataFrame`].
    pub fn as_data_frame_mut(&mut self) -> Result<&mut RDataFrame, &'static str> {
        if self.is_data_frame() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a data frame.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RFunction`].
    pub fn as_function(&self) -> Result<&RFunction, &'static str> {
        if self.is_function() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a function.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RFunction`].
    pub fn as_function_mut(&mut self) -> Result<&mut RFunction, &'static str> {
        if self.is_function() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a function.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RExternalPtr`].
    pub fn as_external_ptr(&self) -> Result<&RExternalPtr, &'static str> {
        if self.is_external_ptr() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not an external pointer.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RExternalPtr`].
    pub fn as_external_ptr_mut(&mut self) -> Result<&mut RExternalPtr, &'static str> {
        if self.is_external_ptr() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not an external pointer.")
        }
    }

    /// Attempts to recharacterize as a reference to an [`RSymbol`].
    pub fn as_symbol(&self) -> Result<&RSymbol, &'static str> {
        if self.is_symbol() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a symbol.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RSymbol`].
    pub fn as_symbol_mut(&mut self) -> Result<&mut RSymbol, &'static str> {
        if self.is_symbol() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a symbol.")
        }
    }

    /// Check it is R's `NULL` value.
    pub fn is_null(&self) -> bool {
        unsafe { Rf_isNull(self.sexp()) != 0 }
    }

    /// Check if it can be interpreted as a vector in R.
    pub fn is_vector(&self) -> bool {
        unsafe { Rf_isVectorAtomic(self.sexp()) != 0 }
    }

    /// Check if it can be interpreted as a matrix in R.
    pub fn is_matrix(&self) -> bool {
        unsafe { Rf_isMatrix(self.sexp()) != 0 }
    }

    /// Check if it can be interpreted as an array in R.
    pub fn is_array(&self) -> bool {
        unsafe { Rf_isArray(self.sexp()) != 0 }
    }

    /// Check if it can be interpreted as a symbol in R.
    pub fn is_symbol(&self) -> bool {
        unsafe { TYPEOF(self.sexp()) == SYMSXP as i32 }
    }

    /// Check if it can be interpreted as a list in R.
    pub fn is_list(&self) -> bool {
        unsafe { Rf_isVectorList(self.sexp()) != 0 }
    }

    /// Check if it can be interpreted as a data frame in R.
    pub fn is_data_frame(&self) -> bool {
        if unsafe { TYPEOF(self.sexp()) } != VECSXP as i32 {
            return false;
        }
        unsafe { Rf_inherits(self.sexp(), c"data.frame".as_ptr()) != 0 }
    }

    /// Check if it can be interpreted as a function in R.
    pub fn is_function(&self) -> bool {
        unsafe { Rf_isFunction(self.sexp()) != 0 }
    }

    /// Check if it can be interpreted as an external pointer in R.
    pub fn is_external_ptr(&self) -> bool {
        unsafe { TYPEOF(self.sexp()) == EXTPTRSXP as i32 }
    }
}

impl RError {
    /// Define a new R error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use the [`stop`] macro.
    #[expect(clippy::mut_from_ref)]
    pub fn new<'a>(message: &str, pc: &'a Pc) -> &'a mut Self {
        let list = RList::with_names(&["message", "calls"], pc);
        let _ = list.set(0, message);
        list.set_class(&["error", "condition"]);
        unsafe { list.transmute_mut() }
    }
}

impl RSymbol {
    /// Define a new symbol.
    pub fn new(x: &CStr) -> &'static Self {
        check_main_thread();
        // Doesn't need protection because R's garbage collection does not collect symbols.
        unsafe { Rf_install(x.as_ptr()).transmute_static() }
    }

    /// Define a new symbol from a string.
    pub fn from(s: &str) -> Result<&Self, NulError> {
        check_main_thread();
        CString::new(s).map(|cstr| unsafe { Rf_install(cstr.as_ptr()).transmute_static() })
    }

    /// Get R's "dim" symbol.
    pub fn dim() -> &'static Self {
        check_main_thread();
        unsafe { R_DimSymbol.transmute_static() }
    }

    /// Get R's "names" symbol.
    pub fn names() -> &'static Self {
        check_main_thread();
        unsafe { R_NamesSymbol.transmute_static() }
    }

    /// Get R's "rownames" symbol.
    pub fn rownames() -> &'static Self {
        check_main_thread();
        unsafe { R_RowNamesSymbol.transmute_static() }
    }

    /// Get R's "dimnames" symbol.
    pub fn dimnames() -> &'static Self {
        check_main_thread();
        unsafe { R_DimNamesSymbol.transmute_static() }
    }

    /// Get R's "class" symbol.
    pub fn class() -> &'static Self {
        check_main_thread();
        unsafe { R_ClassSymbol.transmute_static() }
    }
}

/// These methods evaluate the function in the frame of the R function that called `.Call`. If the
/// function uses `parent.frame()`, call `.Call` directly in that R function's body, not inside
/// another R function such as `tryCatch()`.
impl RFunction {
    fn eval(expression: SEXP, pc: &Pc) -> Result<&RObject, i32> {
        let expression = unsafe { pc.protect(expression) };
        // Quote arguments that R would evaluate, so R passes them as data.
        unsafe {
            // The base function 'quote' is always reachable, so it needs no protection.
            let quote = Rf_eval(Rf_install(c"quote".as_ptr()), R_BaseEnv);
            let mut cell = CDR(expression);
            while cell != R_NilValue {
                let arg = CAR(cell);
                if matches!(TYPEOF(arg) as u32, SYMSXP | LANGSXP | PROMSXP | BCODESXP) {
                    SETCAR(cell, Rf_lang2(quote, arg));
                }
                cell = CDR(cell);
            }
        }
        let mut p_out_error: i32 = 0;
        // R_GetCurrentEnv is experimental API, but R_GlobalEnv would hide the calling R function
        // from parent.frame(), unlike base R's callbacks.
        let sexp = unsafe {
            R_tryEval(
                expression,
                R_GetCurrentEnv(),
                std::ptr::from_mut(&mut p_out_error),
            )
        };
        // An R error inside the call may have jumped over nested counters.
        PC_DEPTH.with(|depth| depth.set(pc.depth));
        match p_out_error {
            0 => Ok(unsafe { pc.protect(sexp).transmute(pc) }),
            e => Err(e),
        }
    }

    /// Evaluate a function with no parameters.
    ///
    /// # Safety
    /// The callback may keep its arguments or return an existing object; see the
    /// [aliasing rule](crate#aliasing).
    pub unsafe fn call0<'a>(&self, pc: &'a Pc) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang1(self.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 1 parameter.
    ///
    /// # Safety
    /// See [`Self::call0`].
    pub unsafe fn call1<'a>(
        &self,
        arg1: &impl RObjectVariant,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang2(self.sexp(), arg1.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 2 parameters.
    ///
    /// # Safety
    /// See [`Self::call0`].
    pub unsafe fn call2<'a>(
        &self,
        arg1: &impl RObjectVariant,
        arg2: &impl RObjectVariant,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang3(self.sexp(), arg1.sexp(), arg2.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 3 parameters.
    ///
    /// # Safety
    /// See [`Self::call0`].
    pub unsafe fn call3<'a>(
        &self,
        arg1: &impl RObjectVariant,
        arg2: &impl RObjectVariant,
        arg3: &impl RObjectVariant,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang4(self.sexp(), arg1.sexp(), arg2.sexp(), arg3.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 4 parameters.
    ///
    /// # Safety
    /// See [`Self::call0`].
    pub unsafe fn call4<'a>(
        &self,
        arg1: &impl RObjectVariant,
        arg2: &impl RObjectVariant,
        arg3: &impl RObjectVariant,
        arg4: &impl RObjectVariant,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe {
            Rf_lang5(
                self.sexp(),
                arg1.sexp(),
                arg2.sexp(),
                arg3.sexp(),
                arg4.sexp(),
            )
        };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 5 parameters.
    ///
    /// # Safety
    /// See [`Self::call0`].
    pub unsafe fn call5<'a>(
        &self,
        arg1: &impl RObjectVariant,
        arg2: &impl RObjectVariant,
        arg3: &impl RObjectVariant,
        arg4: &impl RObjectVariant,
        arg5: &impl RObjectVariant,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe {
            Rf_lang6(
                self.sexp(),
                arg1.sexp(),
                arg2.sexp(),
                arg3.sexp(),
                arg4.sexp(),
                arg5.sexp(),
            )
        };
        Self::eval(expression, pc)
    }
}

impl<T> RScalar<T> {
    /// Recharacterize as a reference to an [`RVector`].
    pub fn as_vector(&self) -> &RVector<T> {
        unsafe { self.transmute() }
    }

    /// Recharacterize as a mutable reference to an [`RVector`].
    pub fn as_vector_mut(&mut self) -> &mut RVector<T> {
        unsafe { self.transmute_mut() }
    }

    /// Interpret as an `f64`.
    pub fn f64(&self) -> f64 {
        unsafe { Rf_asReal(self.sexp()) }
    }

    /// Interpret as an `i32`. A double must be a whole number.
    pub fn i32(&self) -> Result<i32, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers.")
            } else {
                Ok(x)
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            match whole_number(y) {
                // R uses i32::MIN for NA, so it is not a valid value.
                Ok(i32::MIN) => Err("Less than the minimum value."),
                result => result,
            }
        } else if self.is_u8() {
            Ok(unsafe { Rf_asInteger(self.sexp()) })
        } else if self.is_bool() {
            let y = unsafe { Rf_asLogical(self.sexp()) };
            if y == i32::MIN {
                Err("Equals R's NA for logical.")
            } else {
                Ok(y)
            }
        } else {
            Err("Unsupported R type.")
        }
    }

    /// Attempt to interpret as a `usize`. A double must be a whole number.
    pub fn usize(&self) -> Result<usize, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers.")
            } else if x < 0 {
                Err("Negative value not expected.")
            } else {
                usize::try_from(x).map_err(|_| "Cannot convert to usize.")
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            whole_number(y)
        } else if self.is_u8() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            usize::try_from(x).map_err(|_| "Cannot convert to usize.")
        } else if self.is_bool() {
            let x = unsafe { Rf_asLogical(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for logical.")
            } else {
                usize::try_from(x).map_err(|_| "Cannot convert to usize.")
            }
        } else {
            Err("Unsupported R type.")
        }
    }

    /// Attempt to interpret as a `u8`. A double must be a whole number.
    pub fn u8(&self) -> Result<u8, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers.")
            } else {
                u8::try_from(x).map_err(|_| "Cannot convert to u8.")
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            whole_number(y)
        } else if self.is_u8() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            u8::try_from(x).map_err(|_| "Cannot convert to u8.")
        } else if self.is_bool() {
            let x = unsafe { Rf_asLogical(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for logical.")
            } else {
                u8::try_from(x).map_err(|_| "Cannot convert to u8.")
            }
        } else {
            Err("Unsupported R type.")
        }
    }

    /// Attempt to interpret as a `bool`.
    pub fn bool(&self) -> Result<bool, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers.")
            } else {
                Ok(R::is_true(x))
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if R::is_na_f64(y) {
                Err("Equals R's NA for doubles.")
            } else if R::is_nan(y) {
                Err("Equals R's NaN.")
            } else {
                Ok(y != 0.0)
            }
        } else if self.is_u8() {
            Ok(R::is_true(unsafe { Rf_asInteger(self.sexp()) }))
        } else if self.is_bool() {
            let y = unsafe { Rf_asLogical(self.sexp()) };
            if y == i32::MIN {
                Err("Equals R's NA for logical.")
            } else {
                Ok(R::is_true(y))
            }
        } else {
            Err("Unsupported R type.")
        }
    }

    /// Attempt to interpret as a string, converting from other storage modes as R does.
    ///
    /// An error is returned for `NA` and for strings that cannot be represented in UTF-8.
    pub fn str<'a>(&'a self, pc: &'a Pc) -> Result<&'a str, &'static str> {
        self.to_char(pc).get()
    }

    /// Check if RObject can be interpreted as an NA value in R.
    pub fn is_na(&self) -> bool {
        if self.is_f64() {
            unsafe { R_IsNA(Rf_asReal(self.sexp())) != 0 }
        } else if self.is_i32() {
            unsafe { Rf_asInteger(self.sexp()) == R::na_i32() }
        } else if self.is_bool() {
            unsafe { Rf_asLogical(self.sexp()) == R::na_bool() }
        } else if self.is_char() {
            unsafe { Rf_asChar(self.sexp()) == R_NaString }
        } else {
            false
        }
    }

    /// Check if it equals R's `NaN` value.
    pub fn is_nan(&self) -> bool {
        if self.is_f64() {
            unsafe { R_IsNaN(Rf_asReal(self.sexp())) != 0 }
        } else {
            false
        }
    }

    /// Check if it can be interpreted as a finite value in R.
    pub fn is_finite(&self) -> bool {
        if self.is_f64() {
            unsafe { R_finite(Rf_asReal(self.sexp())) != 0 }
        } else {
            false
        }
    }

    /// Check if it can be interpreted as positive infinity in R.
    pub fn is_positive_infinity(&self) -> bool {
        if self.is_f64() {
            unsafe { Rf_asReal(self.sexp()) == R_PosInf }
        } else {
            false
        }
    }

    /// Check if it can be interpreted as negative infinity in R.
    pub fn is_negative_infinity(&self) -> bool {
        if self.is_f64() {
            unsafe { Rf_asReal(self.sexp()) == R_NegInf }
        } else {
            false
        }
    }
}

macro_rules! rscalar_constructor {
    ($tipe:ty, $code:expr) => {
        impl RScalarConstructor<$tipe> for RScalar<$tipe> {
            fn from_value(value: $tipe, pc: &Pc) -> &mut Self {
                unsafe { pc.protect_and_transmute($code(value)) }
            }
        }
    };
}

rscalar_constructor!(f64, Rf_ScalarReal);
rscalar_constructor!(i32, Rf_ScalarInteger);
rscalar_constructor!(u8, Rf_ScalarRaw);

impl RScalarConstructor<bool> for RScalar<bool> {
    fn from_value(value: bool, pc: &Pc) -> &mut Self {
        // Rf_ScalarLogical returns R's shared TRUE/FALSE constants.
        unsafe { RVector::<bool>::from_value(value, 1, pc).transmute_mut() }
    }
}

impl RScalarConstructor<&str> for RScalar<char> {
    fn from_value<'a>(value: &str, pc: &'a Pc) -> &'a mut Self {
        unsafe { pc.protect_and_transmute(Rf_ScalarString(pc.protect(charsxp_from_str(value)))) }
    }
}

macro_rules! rconvertable {
    ($name:ident) => {
        impl<T> $name<T> {
            /// Attempt to recharacterize as R's storage mode "double".
            pub fn as_f64(&self) -> Result<&$name<f64>, &'static str> {
                if self.is_f64() {
                    Ok(unsafe { self.transmute() })
                } else {
                    Err("Not of storage mode 'double'.")
                }
            }

            /// Attempt to recharacterize as R's storage mode "double".
            pub fn as_f64_mut(&mut self) -> Result<&mut $name<f64>, &'static str> {
                if self.is_f64() {
                    Ok(unsafe { self.transmute_mut() })
                } else {
                    Err("Not of storage mode 'double'.")
                }
            }

            /// Checks whether it uses R's storage mode "double".
            pub fn is_f64(&self) -> bool {
                unsafe { Rf_isReal(self.sexp()) != 0 }
            }

            /// Attempt to coerce to R's storage mode "double".
            pub fn to_f64<'a>(&'a self, pc: &'a Pc) -> &'a $name<f64> {
                if self.is_f64() {
                    unsafe { self.transmute() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), REALSXP)) }
                }
            }

            /// Attempt to coerce to R's storage mode "double".
            pub fn to_f64_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut $name<f64> {
                if self.is_f64() {
                    unsafe { self.transmute_mut() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), REALSXP)) }
                }
            }

            /// Attempt to recharacterize as R's storage mode "integer".
            pub fn as_i32(&self) -> Result<&$name<i32>, &'static str> {
                if self.is_i32() {
                    Ok(unsafe { self.transmute() })
                } else {
                    Err("Not of storage mode 'integer'.")
                }
            }

            /// Attempt to recharacterize as R's storage mode "integer".
            pub fn as_i32_mut(&mut self) -> Result<&mut $name<i32>, &'static str> {
                if self.is_i32() {
                    Ok(unsafe { self.transmute_mut() })
                } else {
                    Err("Not of storage mode 'integer'.")
                }
            }

            /// Checks whether it uses R's storage mode "integer".
            pub fn is_i32(&self) -> bool {
                unsafe { Rf_isInteger(self.sexp()) != 0 }
            }

            /// Attempt to coerce to R's storage mode "integer".
            pub fn to_i32<'a>(&'a self, pc: &'a Pc) -> &'a $name<i32> {
                if self.is_i32() {
                    unsafe { self.transmute() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), INTSXP)) }
                }
            }

            /// Attempt to coerce to R's storage mode "integer".
            pub fn to_i32_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut $name<i32> {
                if self.is_i32() {
                    unsafe { self.transmute_mut() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), INTSXP)) }
                }
            }

            /// Attempt to recharacterize as R's storage mode "raw".
            pub fn as_u8(&self) -> Result<&$name<u8>, &'static str> {
                if self.is_u8() {
                    Ok(unsafe { self.transmute() })
                } else {
                    Err("Not of storage mode 'raw'.")
                }
            }

            /// Attempt to recharacterize as R's storage mode "raw".
            pub fn as_u8_mut(&mut self) -> Result<&mut $name<u8>, &'static str> {
                if self.is_u8() {
                    Ok(unsafe { self.transmute_mut() })
                } else {
                    Err("Not of storage mode 'raw'.")
                }
            }

            /// Checks whether it uses R's storage mode "raw".
            pub fn is_u8(&self) -> bool {
                unsafe { TYPEOF(self.sexp()) == RAWSXP as i32 }
            }

            /// Attempt to coerce to R's storage mode "raw".
            pub fn to_u8<'a>(&'a self, pc: &'a Pc) -> &'a $name<u8> {
                if self.is_u8() {
                    unsafe { self.transmute() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), RAWSXP)) }
                }
            }

            /// Attempt to coerce to R's storage mode "raw".
            pub fn to_u8_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut $name<u8> {
                if self.is_u8() {
                    unsafe { self.transmute_mut() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), RAWSXP)) }
                }
            }

            /// Attempt to recharacterize as R's storage mode "logical".
            pub fn as_bool(&self) -> Result<&$name<bool>, &'static str> {
                if self.is_bool() {
                    Ok(unsafe { self.transmute() })
                } else {
                    Err("Not of storage mode 'logical'.")
                }
            }

            /// Attempt to recharacterize as R's storage mode "logical".
            pub fn as_bool_mut(&mut self) -> Result<&mut $name<bool>, &'static str> {
                if self.is_bool() {
                    Ok(unsafe { self.transmute_mut() })
                } else {
                    Err("Not of storage mode 'logical'.")
                }
            }

            /// Checks whether it uses R's storage mode "logical".
            pub fn is_bool(&self) -> bool {
                unsafe { Rf_isLogical(self.sexp()) != 0 }
            }

            /// Attempt to coerce to R's storage mode "logical".
            pub fn to_bool<'a>(&'a self, pc: &'a Pc) -> &'a $name<bool> {
                if self.is_bool() {
                    unsafe { self.transmute() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), LGLSXP)) }
                }
            }

            /// Attempt to coerce to R's storage mode "logical".
            pub fn to_bool_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut $name<bool> {
                if self.is_bool() {
                    unsafe { self.transmute_mut() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), LGLSXP)) }
                }
            }

            /// Attempt to recharacterize as R's storage mode "character".
            pub fn as_char(&self) -> Result<&$name<char>, &'static str> {
                if self.is_char() {
                    Ok(unsafe { self.transmute() })
                } else {
                    Err("Not of storage mode 'character'.")
                }
            }

            /// Attempt to recharacterize as R's storage mode "character".
            pub fn as_char_mut(&mut self) -> Result<&mut $name<char>, &'static str> {
                if self.is_char() {
                    Ok(unsafe { self.transmute_mut() })
                } else {
                    Err("Not of storage mode 'character'.")
                }
            }

            /// Checks whether it uses R's storage mode "character".
            pub fn is_char(&self) -> bool {
                unsafe { Rf_isString(self.sexp()) != 0 }
            }

            /// Attempt to coerce to R's storage mode "character".
            pub fn to_char<'a>(&'a self, pc: &'a Pc) -> &'a $name<char> {
                if self.is_char() {
                    unsafe { self.transmute() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), STRSXP)) }
                }
            }

            /// Attempt to coerce to R's storage mode "character".
            pub fn to_char_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut $name<char> {
                if self.is_char() {
                    unsafe { self.transmute_mut() }
                } else {
                    unsafe { pc.protect_and_transmute(Rf_coerceVector(self.sexp(), STRSXP)) }
                }
            }
        }
    };
}

rconvertable!(RScalar);
rconvertable!(RVector);
rconvertable!(RMatrix);
rconvertable!(RArray);

macro_rules! rsliceable {
    ($name:ident, $tipe:ty, $tipe2:ty, $ptr:expr) => {
        impl RSliceable<$tipe2> for $name<$tipe> {
            fn slice(&self) -> &[$tipe2] {
                let len = self.len();
                // R can use an unaligned sentinel for an empty vector's data.
                if len == 0 {
                    &[]
                } else {
                    unsafe { std::slice::from_raw_parts($ptr(self.sexp()), len) }
                }
            }

            fn slice_mut(&mut self) -> &mut [$tipe2] {
                let len = self.len();
                if len == 0 {
                    &mut []
                } else {
                    unsafe { std::slice::from_raw_parts_mut($ptr(self.sexp()), len) }
                }
            }
        }
    };
}

rsliceable!(RScalar, f64, f64, REAL);
rsliceable!(RScalar, i32, i32, INTEGER);
rsliceable!(RScalar, u8, u8, RAW);
rsliceable!(RScalar, bool, i32, LOGICAL);
rsliceable!(RVector, f64, f64, REAL);
rsliceable!(RVector, i32, i32, INTEGER);
rsliceable!(RVector, u8, u8, RAW);
rsliceable!(RVector, bool, i32, LOGICAL);
rsliceable!(RMatrix, f64, f64, REAL);
rsliceable!(RMatrix, i32, i32, INTEGER);
rsliceable!(RMatrix, u8, u8, RAW);
rsliceable!(RMatrix, bool, i32, LOGICAL);
rsliceable!(RArray, f64, f64, REAL);
rsliceable!(RArray, i32, i32, INTEGER);
rsliceable!(RArray, u8, u8, RAW);
rsliceable!(RArray, bool, i32, LOGICAL);

macro_rules! rscalar_getset {
    ($tipe:ty, $tipe2:ty, $get:expr, $set:expr) => {
        impl RGetSet0<$tipe2> for RScalar<$tipe> {
            fn get(&self) -> $tipe2 {
                unsafe { $get(self.sexp(), 0) }
            }

            fn set(&mut self, value: $tipe2) {
                unsafe { $set(self.sexp(), 0, value) }
            }
        }
    };
}

rscalar_getset!(f64, f64, REAL_ELT, SET_REAL_ELT);
rscalar_getset!(i32, i32, INTEGER_ELT, SET_INTEGER_ELT);
rscalar_getset!(u8, u8, RAW_ELT, SET_RAW_ELT);

impl RGetSet0<bool> for RScalar<bool> {
    fn get(&self) -> bool {
        R::is_true(unsafe { LOGICAL_ELT(self.sexp(), 0) })
    }

    fn set(&mut self, value: bool) {
        unsafe { SET_LOGICAL_ELT(self.sexp(), 0, R::as_logical(value)) }
    }
}

impl RScalar<char> {
    /// Get the value of a scalar.
    ///
    /// An error is returned for `NA` and for strings that cannot be represented in UTF-8.
    pub fn get(&self) -> Result<&str, &'static str> {
        charsxp_as_str(unsafe { STRING_ELT(self.sexp(), 0) }, self)
    }

    /// Set the value of a scalar.
    pub fn set(&mut self, value: &str) {
        unsafe { SET_STRING_ELT(self.sexp(), 0, charsxp_from_str(value)) }
    }
}

macro_rules! rvector {
    ($tipe:ty, $code:expr, $ptr:expr, $get:expr, $set:expr) => {
        impl RFromIterator<$tipe> for RVector<$tipe> {
            fn from_iter1<T>(iter: T, pc: &Pc) -> &mut Self
            where
                T: IntoIterator<Item = $tipe> + ExactSizeIterator,
            {
                let len = iter.len();
                unsafe {
                    let vec = pc.protect(Rf_allocVector($code, len.try_into().stop_str(TOO_LONG)));
                    write_iter($ptr(vec), len, iter.into_iter());
                    vec.transmute_mut(pc)
                }
            }

            fn from_iter2<'a, 'b, T>(iter: T, pc: &'a Pc) -> &'a mut Self
            where
                T: IntoIterator<Item = &'b $tipe> + ExactSizeIterator,
            {
                let len = iter.len();
                unsafe {
                    let vec = pc.protect(Rf_allocVector($code, len.try_into().stop_str(TOO_LONG)));
                    write_iter($ptr(vec), len, iter.into_iter().copied());
                    vec.transmute_mut(pc)
                }
            }
        }

        impl RGetSet1<$tipe> for RVector<$tipe> {
            fn get(&self, index: usize) -> Result<$tipe, &'static str> {
                if index < self.len() {
                    Ok(unsafe { $get(self.sexp(), index.try_into().unwrap()) })
                } else {
                    Err("Index out of bounds.")
                }
            }

            fn set(&mut self, index: usize, value: $tipe) -> Result<(), &'static str> {
                if index < self.len() {
                    unsafe { $set(self.sexp(), index.try_into().unwrap(), value) };
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }
        }

        impl RVectorConstructors<$tipe> for RVector<$tipe> {
            fn new(length: usize, pc: &Pc) -> &mut Self {
                Self::from_value(0 as $tipe, length, pc)
            }

            unsafe fn new_uninit(length: usize, pc: &Pc) -> &mut Self {
                unsafe {
                    pc.protect_and_transmute(Rf_allocVector(
                        $code,
                        length.try_into().stop_str(TOO_LONG),
                    ))
                }
            }

            fn from_value(value: $tipe, length: usize, pc: &Pc) -> &mut Self {
                unsafe {
                    let vec =
                        pc.protect(Rf_allocVector($code, length.try_into().stop_str(TOO_LONG)));
                    fill_uninit($ptr(vec), length, value);
                    vec.transmute_mut(pc)
                }
            }

            fn from_array<const N: usize>(slice: [$tipe; N], pc: &Pc) -> &mut Self {
                Self::from_slice(&slice, pc)
            }

            fn from_slice<'a>(slice: &[$tipe], pc: &'a Pc) -> &'a mut Self {
                unsafe {
                    let vec = pc.protect(Rf_allocVector(
                        $code,
                        slice.len().try_into().stop_str(TOO_LONG),
                    ));
                    if !slice.is_empty() {
                        std::ptr::copy_nonoverlapping(slice.as_ptr(), $ptr(vec), slice.len());
                    }
                    vec.transmute_mut(pc)
                }
            }
        }
    };
}

rvector!(f64, REALSXP, REAL, REAL_ELT, SET_REAL_ELT);
rvector!(i32, INTSXP, INTEGER, INTEGER_ELT, SET_INTEGER_ELT);
rvector!(u8, RAWSXP, RAW, RAW_ELT, SET_RAW_ELT);

impl RFromIterator<bool> for RVector<bool> {
    fn from_iter1<T>(iter: T, pc: &Pc) -> &mut Self
    where
        T: IntoIterator<Item = bool> + ExactSizeIterator,
    {
        let len = iter.len();
        unsafe {
            let vec = pc.protect(Rf_allocVector(LGLSXP, len.try_into().stop_str(TOO_LONG)));
            write_iter(LOGICAL(vec), len, iter.into_iter().map(R::as_logical));
            vec.transmute_mut(pc)
        }
    }

    fn from_iter2<'a, 'b, T>(iter: T, pc: &'a Pc) -> &'a mut Self
    where
        T: IntoIterator<Item = &'b bool> + ExactSizeIterator,
    {
        let len = iter.len();
        unsafe {
            let vec = pc.protect(Rf_allocVector(LGLSXP, len.try_into().stop_str(TOO_LONG)));
            write_iter(
                LOGICAL(vec),
                len,
                iter.into_iter().map(|x| R::as_logical(*x)),
            );
            vec.transmute_mut(pc)
        }
    }
}

impl RGetSet1<bool> for RVector<bool> {
    /// Get the value at a certain index.
    fn get(&self, index: usize) -> Result<bool, &'static str> {
        if index < self.len() {
            let value = unsafe { LOGICAL_ELT(self.sexp(), index.try_into().unwrap()) };
            Ok(R::is_true(value))
        } else {
            Err("Index out of bounds.")
        }
    }

    /// Set the value at a certain index.
    fn set(&mut self, index: usize, value: bool) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe {
                SET_LOGICAL_ELT(self.sexp(), index.try_into().unwrap(), R::as_logical(value))
            };
            Ok(())
        } else {
            Err("Index out of bounds.")
        }
    }
}

impl RVector<bool> {
    /// Get the value at a certain index as R stores it in an `i32`.
    pub fn get_i32(&self, index: usize) -> Result<i32, &'static str> {
        if index < self.len() {
            Ok(unsafe { LOGICAL_ELT(self.sexp(), index.try_into().unwrap()) })
        } else {
            Err("Index out of bounds.")
        }
    }

    /// Set the value at a certain index as R stores it in an `i32`.
    pub fn set_i32(&mut self, index: usize, value: i32) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { SET_LOGICAL_ELT(self.sexp(), index.try_into().unwrap(), value) };
            Ok(())
        } else {
            Err("Index out of bounds.")
        }
    }
}

impl RVectorConstructors<bool> for RVector<bool> {
    fn new(length: usize, pc: &Pc) -> &mut Self {
        Self::from_value(false, length, pc)
    }

    unsafe fn new_uninit(length: usize, pc: &Pc) -> &mut Self {
        unsafe {
            pc.protect_and_transmute(Rf_allocVector(LGLSXP, length.try_into().stop_str(TOO_LONG)))
        }
    }

    fn from_value(value: bool, length: usize, pc: &Pc) -> &mut Self {
        unsafe {
            let vec = pc.protect(Rf_allocVector(LGLSXP, length.try_into().stop_str(TOO_LONG)));
            fill_uninit(LOGICAL(vec), length, R::as_logical(value));
            vec.transmute_mut(pc)
        }
    }

    fn from_array<const N: usize>(slice: [bool; N], pc: &Pc) -> &mut Self {
        Self::from_iter2(slice.iter(), pc)
    }

    fn from_slice<'a>(slice: &[bool], pc: &'a Pc) -> &'a mut Self {
        Self::from_iter2(slice.iter(), pc)
    }
}

impl RVector<char> {
    /// Get the value at a certain index in the R object.
    ///
    /// An error is returned for `NA` and for strings that cannot be represented in UTF-8.
    pub fn get(&self, index: usize) -> Result<&str, &'static str> {
        if index < self.len() {
            self.get_unchecked(index)
        } else {
            Err("Index out of bounds.")
        }
    }

    /// Check whether the value at a certain index is `NA`.
    pub fn is_na(&self, index: usize) -> Result<bool, &'static str> {
        if index < self.len() {
            Ok(unsafe { STRING_ELT(self.sexp(), index.try_into().unwrap()) == R_NaString })
        } else {
            Err("Index out of bounds.")
        }
    }

    /// Set the value at a certain index in the R object.
    pub fn set(&mut self, index: usize, value: &str) -> Result<(), &'static str> {
        if index < self.len() {
            self.set_unchecked(index, value);
            Ok(())
        } else {
            Err("Index out of bounds.")
        }
    }

    fn get_unchecked(&self, index: usize) -> Result<&str, &'static str> {
        let sexp = unsafe { STRING_ELT(self.sexp(), index.try_into().unwrap()) };
        charsxp_as_str(sexp, self)
    }

    fn set_unchecked(&mut self, index: usize, value: &str) {
        // Doesn't need protection because it's immediately set to a protected object.
        let sexp = charsxp_from_str(value);
        unsafe { SET_STRING_ELT(self.sexp(), index.try_into().unwrap(), sexp) };
    }
}

impl RVectorConstructors<&str> for RVector<char> {
    fn new(length: usize, pc: &Pc) -> &mut Self {
        unsafe {
            pc.protect_and_transmute(Rf_allocVector(STRSXP, length.try_into().stop_str(TOO_LONG)))
        }
    }

    // R always initializes the elements of a character vector.
    unsafe fn new_uninit(length: usize, pc: &Pc) -> &mut Self {
        Self::new(length, pc)
    }

    fn from_value<'a>(value: &str, length: usize, pc: &'a Pc) -> &'a mut Self {
        let length_i32 = length.try_into().stop_str(TOO_LONG);
        let vec = unsafe { pc.protect(Rf_allocVector(STRSXP, length_i32)) };
        if length_i32 > 0 {
            let element = charsxp_from_str(value);
            for index in 0..length_i32 {
                unsafe { SET_STRING_ELT(vec, index, element) };
            }
        }
        unsafe { vec.transmute_mut(pc) }
    }

    fn from_array<'a, const N: usize>(array: [&str; N], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(array.len(), pc);
        for (index, value) in array.iter().enumerate() {
            result.set_unchecked(index, value)
        }
        result
    }

    fn from_slice<'a>(slice: &[&str], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(slice.len(), pc);
        for (index, value) in slice.iter().enumerate() {
            result.set_unchecked(index, value)
        }
        result
    }
}

macro_rules! rmatrix {
    ($tipe:ty, $code:expr, $ptr:expr, $get:expr, $set:expr) => {
        impl RGetSet2<$tipe> for RMatrix<$tipe> {
            /// Get the value at a certain row and column.
            fn get(&self, row: usize, col: usize) -> Result<$tipe, &'static str> {
                let index = self.index(row, col, None);
                if index < self.len() {
                    Ok(unsafe { $get(self.sexp(), index.try_into().unwrap()) })
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Set the value at a certain row and column.
            fn set(&mut self, row: usize, col: usize, value: $tipe) -> Result<(), &'static str> {
                let index = self.index(row, col, None);
                if index < self.len() {
                    unsafe { $set(self.sexp(), index.try_into().unwrap(), value) };
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }
        }

        impl RMatrixConstructors<$tipe> for RMatrix<$tipe> {
            fn new(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
                Self::from_value(0 as $tipe, nrow, ncol, pc)
            }

            unsafe fn new_uninit(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
                unsafe {
                    pc.protect_and_transmute(Rf_allocMatrix(
                        $code,
                        nrow.try_into().stop_str(TOO_LONG),
                        ncol.try_into().stop_str(TOO_LONG),
                    ))
                }
            }

            fn from_value(value: $tipe, nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
                unsafe {
                    let matrix = pc.protect(Rf_allocMatrix(
                        $code,
                        nrow.try_into().stop_str(TOO_LONG),
                        ncol.try_into().stop_str(TOO_LONG),
                    ));
                    fill_uninit($ptr(matrix), Rf_xlength(matrix).try_into().unwrap(), value);
                    matrix.transmute_mut(pc)
                }
            }
        }
    };
}

rmatrix!(f64, REALSXP, REAL, REAL_ELT, SET_REAL_ELT);
rmatrix!(i32, INTSXP, INTEGER, INTEGER_ELT, SET_INTEGER_ELT);
rmatrix!(u8, RAWSXP, RAW, RAW_ELT, SET_RAW_ELT);

impl<T> RMatrix<T> {
    /// Return the number of rows in the RMatrix.
    pub fn nrow(&self) -> usize {
        unsafe { Rf_nrows(self.sexp()).try_into().unwrap() }
    }

    /// Returns the number of columns in the RMatrix.
    pub fn ncol(&self) -> usize {
        unsafe { Rf_ncols(self.sexp()).try_into().unwrap() }
    }

    /// Return the dimensions of the RMatrix.
    pub fn dim(&self) -> [usize; 2] {
        [self.nrow(), self.ncol()]
    }

    /// Transpose the matrix.
    #[expect(clippy::mut_from_ref)]
    pub fn transpose<'a>(&self, pc: &'a Pc) -> &'a mut Self {
        let transposed = self.clone(pc);
        let mut dim = transposed.dim();
        dim.swap(0, 1);
        let dim2: [i32; 2] = [dim[0].try_into().unwrap(), dim[1].try_into().unwrap()];
        transposed.set_attribute(RSymbol::dim(), &dim2[..]);
        unsafe { Rf_copyMatrix(transposed.sexp(), self.sexp(), Rboolean_TRUE) };
        transposed
    }

    /// Manipulate the matrix in place to be a vector by dropping the `dim` attribute.
    pub fn to_vector_mut(&mut self) -> &mut RVector<T> {
        unsafe {
            Rf_setAttrib(self.sexp(), R_DimSymbol, R_NilValue);
            self.transmute_mut()
        }
    }

    /// Get the linear index of a certain row and column.
    pub fn index(&self, row: usize, col: usize, nrow: Option<usize>) -> usize {
        let nrow = nrow.unwrap_or_else(|| self.nrow());
        nrow * col + row
    }

    /// Get the dimnames of a matrix.
    pub fn get_dimnames(&self) -> &RList {
        unsafe { Rf_getAttrib(self.sexp(), R_DimNamesSymbol).transmute(self) }
    }

    /// Set the dimnames of a matrix. `None` leaves a dimension unnamed.
    pub fn set_dimnames(&mut self, dimnames: &[Option<&[&str]>]) -> Result<(), &'static str> {
        if dimnames.len() != 2 {
            return Err("Dimnames must have two elements.");
        }
        unsafe {
            let pc = Pc::__private_new();
            self.set_dimnames_object(dimnames_to_r(dimnames, &pc))
        }
    }

    /// Set the dimnames of a matrix to an existing R list.
    ///
    /// # Safety
    /// `dimnames` is stored without copying; see the [aliasing rule](crate#aliasing).
    pub unsafe fn set_dimnames_object(&mut self, dimnames: &RList) -> Result<(), &'static str> {
        match dimnames.get(0) {
            Ok(rownames) if rownames.is_null() => {}
            Ok(rownames) => match rownames.as_vector() {
                Ok(rownames) => {
                    if rownames.len() != self.nrow() {
                        return Err("Row names do not match the number of rows.");
                    }
                }
                Err(_) => {
                    return Err("Row names must be a character vector or NULL.");
                }
            },
            Err(_) => return Err("No row names element found."),
        };
        match dimnames.get(1) {
            Ok(colnames) if colnames.is_null() => {}
            Ok(colnames) => match colnames.as_vector() {
                Ok(colnames) => {
                    if colnames.len() != self.ncol() {
                        return Err("Column names do not match the number of columns.");
                    }
                }
                Err(_) => {
                    return Err("Column names must be a character vector or NULL.");
                }
            },
            Err(_) => return Err("No column names element found."),
        };
        unsafe {
            Rf_dimnamesgets(self.sexp(), dimnames.sexp());
        }
        Ok(())
    }
}

macro_rules! rarray {
    ($tipe:ty, $code:expr, $ptr:expr, $get:expr, $set:expr) => {
        impl RGetSetN<$tipe> for RArray<$tipe> {
            /// Get the value at a certain index.
            fn get(&self, index: &[usize]) -> Result<$tipe, &'static str> {
                let index = self.index(index, None);
                if index < self.len() {
                    Ok(unsafe { $get(self.sexp(), index.try_into().unwrap()) })
                } else {
                    Err("Index out of bounds.")
                }
            }
            /// Set the value at a certain index.
            fn set(&mut self, index: &[usize], value: $tipe) -> Result<(), &'static str> {
                let index = self.index(index, None);
                if index < self.len() {
                    unsafe { $set(self.sexp(), index.try_into().unwrap(), value) };
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }
        }

        impl RArrayConstructors<$tipe> for RArray<$tipe> {
            fn new<'a>(dim: &[usize], pc: &'a Pc) -> &'a mut Self {
                Self::from_value(0 as $tipe, dim, pc)
            }

            unsafe fn new_uninit<'a>(dim: &[usize], pc: &'a Pc) -> &'a mut Self {
                let dim = dim
                    .iter()
                    .map(|x| i32::try_from(*x).stop_str(TOO_LONG))
                    .to_r(pc)
                    .sexp();
                unsafe { pc.protect_and_transmute(Rf_allocArray($code, dim)) }
            }

            fn from_value<'a>(value: $tipe, dim: &[usize], pc: &'a Pc) -> &'a mut Self {
                let dim = dim
                    .iter()
                    .map(|x| i32::try_from(*x).stop_str(TOO_LONG))
                    .to_r(pc)
                    .sexp();
                unsafe {
                    let array = pc.protect(Rf_allocArray($code, dim));
                    fill_uninit($ptr(array), Rf_xlength(array).try_into().unwrap(), value);
                    array.transmute_mut(pc)
                }
            }
        }
    };
}

rarray!(f64, REALSXP, REAL, REAL_ELT, SET_REAL_ELT);
rarray!(i32, INTSXP, INTEGER, INTEGER_ELT, SET_INTEGER_ELT);
rarray!(u8, RAWSXP, RAW, RAW_ELT, SET_RAW_ELT);

impl<T> RArray<T> {
    /// Return the dimensions of the RArray.
    pub fn dim(&self) -> Vec<usize> {
        let d: &RVector<i32> = unsafe { Rf_getAttrib(self.sexp(), R_DimSymbol).transmute(self) };
        d.slice().iter().map(|&x| x.try_into().unwrap()).collect()
    }

    /// Manipulate the matrix in place to be a vector by dropping the `dim` attribute.
    pub fn to_vector_mut(&mut self) -> &mut RVector<T> {
        unsafe {
            Rf_setAttrib(self.sexp(), R_DimSymbol, R_NilValue);
            self.transmute_mut()
        }
    }

    /// Get the linear index of a value from its position in each dimension.
    pub fn index(&self, index: &[usize], dim: Option<&[usize]>) -> usize {
        let mut dim_holder: Vec<_> = vec![];
        let dim = dim.unwrap_or_else(|| {
            dim_holder = self.dim();
            &dim_holder[..]
        });
        let coef = std::iter::once(&1)
            .chain(dim[..dim.len() - 1].iter())
            .scan(1, |prod, d| {
                *prod *= *d;
                Some(*prod)
            });
        let mut i = 0;
        for (x, y) in coef.zip(index.iter()) {
            i += x * (*y);
        }
        i
    }

    /// Get the dimnames of an array.
    pub fn get_dimnames(&self) -> &RList {
        unsafe { Rf_getAttrib(self.sexp(), R_DimNamesSymbol).transmute(self) }
    }

    /// Set the dimnames of an array. `None` leaves a dimension unnamed.
    pub fn set_dimnames(&mut self, dimnames: &[Option<&[&str]>]) -> Result<(), String> {
        unsafe {
            let pc = Pc::__private_new();
            self.set_dimnames_object(dimnames_to_r(dimnames, &pc))
        }
    }

    /// Set the dimnames of an array to an existing R list.
    ///
    /// # Safety
    /// `dimnames` is stored without copying; see the [aliasing rule](crate#aliasing).
    pub unsafe fn set_dimnames_object(&mut self, dimnames: &RList) -> Result<(), String> {
        let dim = self.dim();
        if dimnames.len() != dim.len() {
            return Err(format!(
                "Length of dimnames is {} whereas the dim is of length {}.",
                dimnames.len(),
                dim.len()
            ));
        }
        for (i, &len) in dim.iter().enumerate() {
            let names = dimnames.get(i).unwrap();
            if names.is_null() {
                continue;
            }
            match names.as_vector() {
                Ok(names) => {
                    if names.len() != len {
                        return Err(format!(
                            "Element {i} of the dimnames list has length {}, but the corresponding dimension is {len}.",
                            names.len()
                        ));
                    }
                }
                Err(_) => {
                    return Err(format!(
                        "Element {i} of the dimnames list must be a character vector or NULL."
                    ));
                }
            }
        }
        unsafe {
            Rf_dimnamesgets(self.sexp(), dimnames.sexp());
        }
        Ok(())
    }
}

pub struct RListMap<'a> {
    unused_counter: usize,
    used: Vec<bool>,
    robj: &'a RList,
    map: HashMap<&'a str, usize>,
}

impl RListMap<'_> {
    /// Find an RObject in the list based on its name.
    pub fn get(&mut self, name: &str) -> Result<&RObject, String> {
        let Some(index) = self.map.get(name) else {
            return Err(format!("'{name}' not found."));
        };
        if !self.used[*index] {
            self.unused_counter -= 1;
            self.used[*index] = true;
        }
        Ok(self.robj.get(*index)?)
    }

    /// Check if every element in the R list has been used.
    pub fn exhaustive(&self) -> Result<(), String> {
        if self.unused_counter != 0 {
            return Err(format!(
                "Unrecognized elements in list:\n    {}.",
                self.unused_elements().join("\n    ")
            ));
        }
        Ok(())
    }

    /// Return the number of unused RObjects in the map.
    pub fn unused_counter(&self) -> usize {
        self.unused_counter
    }

    /// Return the names of all unused RObjects in the map.
    pub fn unused_elements(&self) -> Vec<&str> {
        let names = self.robj.get_names();
        (0..self.used.len())
            .filter(|&index| !self.used[index])
            .map(|index| names.get(index).unwrap_or("NA"))
            .collect()
    }
}

macro_rules! rlistlike {
    ($name:ident) => {
        impl $name {
            /// Get the value at a certain index in the R list.
            /// ALTREP lists must be copied to an ordinary list before borrowing elements.
            pub fn get(&self, index: usize) -> Result<&RObject, &'static str> {
                // ALTREP elements need not remain reachable through the list.
                if unsafe { ALTREP(self.sexp()) } != 0 {
                    return Err(
                        "Cannot borrow ALTREP list elements; copy to an ordinary list first.",
                    );
                }
                if index < self.len() {
                    Ok(unsafe {
                        VECTOR_ELT(self.sexp(), index.try_into().unwrap()).transmute(self)
                    })
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Get the value at a certain index in the R list, first copying it if it is shared.
            /// Returns an error for objects R cannot copy, such as external pointers and symbols.
            /// ALTREP lists must be copied to an ordinary list before borrowing elements.
            pub fn get_mut(&mut self, index: usize) -> Result<&mut RObject, &'static str> {
                if unsafe { ALTREP(self.sexp()) } != 0 {
                    return Err(
                        "Cannot borrow ALTREP list elements; copy to an ordinary list first.",
                    );
                }
                if index < self.len() {
                    let index = index.try_into().unwrap();
                    unsafe {
                        let mut value = VECTOR_ELT(self.sexp(), index);
                        check_mutable(value)?;
                        if MAYBE_SHARED(value) != 0 {
                            value = duplicate_for_mutation(value, true)?;
                            SET_VECTOR_ELT(self.sexp(), index, value);
                        }
                        Ok(value.transmute_mut(self))
                    }
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Convert a Rust value, such as a scalar, slice, or iterator, to a new R object and
            /// store it at an index.
            pub fn set<T: RObjectVariant, M>(
                &mut self,
                index: usize,
                value: impl IntoR<T, M>,
            ) -> Result<(), &'static str> {
                self.set_with_pc(index, |pc| value.into_r(pc))
            }

            /// Store an existing R object at an index.
            ///
            /// # Safety
            /// `value` is stored without copying; see the [aliasing rule](crate#aliasing).
            pub unsafe fn set_object(
                &mut self,
                index: usize,
                value: &impl RObjectVariant,
            ) -> Result<(), &'static str> {
                if index < self.len() {
                    unsafe { SET_VECTOR_ELT(self.sexp(), index.try_into().unwrap(), value.sexp()) };
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Store the object that `x` builds with a temporary protection counter.
            ///
            /// The counter is released once the object is stored, so filling a large list does not
            /// overflow R's PROTECT stack. `x` can return only an object it creates from the counter,
            /// or a `'static` object such as `NULL` or a symbol, so nothing else references it.
            pub fn set_with_pc<T: RObjectVariant, F: FnOnce(&Pc) -> &T>(
                &mut self,
                index: usize,
                x: F,
            ) -> Result<(), &'static str> {
                if index < self.len() {
                    let pc = unsafe { Pc::__private_new() };
                    unsafe {
                        SET_VECTOR_ELT(self.sexp(), index.try_into().unwrap(), x(&pc).sexp())
                    };
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Fill every element as [`Self::set_with_pc`] does, calling `x` once per element.
            pub fn set_loop_with_pc<T: RObjectVariant, F: FnMut(&Pc) -> &T>(&mut self, mut x: F) {
                for i in 0..self.len() {
                    let pc = unsafe { Pc::__private_new() };
                    unsafe { SET_VECTOR_ELT(self.sexp(), i.try_into().unwrap(), x(&pc).sexp()) };
                }
            }

            /// Get a value in an R list based on its key.
            pub fn get_by_key(&self, key: impl AsRef<str>) -> Result<&RObject, String> {
                let names = self.get_names();
                for i in 0..names.len() {
                    if names.get(i) == Ok(key.as_ref()) {
                        return Ok(self.get(i)?);
                    }
                }
                Err(format!("Could not find '{}' in the list.", key.as_ref()))
            }

            /// Get a value in an R list based on its key.
            pub fn get_mut_by_key(&mut self, key: impl AsRef<str>) -> Result<&mut RObject, String> {
                let names = self.get_names();
                for i in 0..names.len() {
                    if names.get(i) == Ok(key.as_ref()) {
                        return Ok(self.get_mut(i)?);
                    }
                }
                Err(format!("Could not find '{}' in the list.", key.as_ref()))
            }

            /// Convert the list into an [`RListMap`]
            ///
            /// This allows Rust's [`HashMap`] methods to be used on the contents
            /// of the list, while still retaining the original list within
            /// the RListMap struct in the robj field.
            pub fn make_map(&self) -> RListMap<'_> {
                let mut map = HashMap::new();
                let names = self.get_names();
                let len = names.len();
                for i in 0..len {
                    // An element whose name is NA cannot be looked up, so it stays unused.
                    if let Ok(name) = names.get(i) {
                        map.insert(name, i);
                    }
                }
                RListMap {
                    unused_counter: len,
                    used: vec![false; len],
                    robj: unsafe { self.transmute() },
                    map,
                }
            }
        }
    };
}

rlistlike!(RList);
rlistlike!(RDataFrame);

impl RList {
    #[expect(clippy::mut_from_ref)]
    pub fn new(length: usize, pc: &Pc) -> &mut Self {
        unsafe {
            pc.protect_and_transmute(Rf_allocVector(VECSXP, length.try_into().stop_str(TOO_LONG)))
        }
    }

    #[expect(clippy::mut_from_ref)]
    pub fn with_names<'a>(names: &[&str], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(names.len(), pc);
        unsafe {
            Rf_namesgets(result.sexp(), names.to_r(pc).sexp());
        }
        result
    }
}

impl RExternalPtr {
    /// Store an owned Rust value in an R external pointer.
    ///
    /// R drops the value when the external pointer is collected.
    /// Its destructor must not panic or raise an R error.
    #[expect(clippy::mut_from_ref)]
    pub fn encode<'a, T: 'static>(x: T, tag: &str, pc: &'a Pc) -> &'a mut Self {
        // The tag is new, so nothing else references it.
        unsafe { Self::encode_full(x, tag.to_r(pc), true, pc) }
    }

    /// Store an owned Rust value with an R object as its tag.
    ///
    /// If `managed_by_r` is false, reclaim the value with [`Self::decode_val`].
    ///
    /// # Safety
    /// `tag` is stored without copying; see the [aliasing rule](crate#aliasing).
    #[expect(clippy::mut_from_ref)]
    pub unsafe fn encode_full<'a, T: 'static>(
        x: T,
        tag: &impl RObjectVariant,
        managed_by_r: bool,
        pc: &'a Pc,
    ) -> &'a mut Self {
        unsafe {
            let ptr = Box::into_raw(Box::new(x));
            let sexp = pc.protect(R_MakeExternalPtr(
                ptr as *mut c_void,
                tag.sexp(),
                R_NilValue,
            ));
            if managed_by_r {
                unsafe extern "C" fn free<S>(sexp: SEXP) {
                    unsafe {
                        let addr = R_ExternalPtrAddr(sexp);
                        if addr.is_null() {
                            return;
                        }
                        let _ = Box::from_raw(addr as *mut S);
                        R_ClearExternalPtr(sexp);
                    }
                }
                Rf_setAttrib(sexp, R_AtsignSymbol, R_AtsignSymbol);
                R_RegisterCFinalizerEx(sexp, Some(free::<T>), 0);
            }
            sexp.transmute_mut(pc)
        }
    }

    /// Restore a null external pointer using its tag.
    ///
    /// # Safety
    /// Any existing finalizers must accept a `Box<T>` allocation. No other alias may replace or
    /// free the address while it is borrowed.
    pub unsafe fn reencode<T: 'static, F: FnOnce(&RObject) -> T>(&mut self, f: F) {
        unsafe {
            if self.is_null() {
                let ptr = Box::into_raw(Box::new(f(self.tag())));
                R_SetExternalPtrAddr(self.sexp(), ptr as *mut c_void);
                if Rf_getAttrib(self.sexp(), R_AtsignSymbol) == R_AtsignSymbol {
                    unsafe extern "C" fn free<S>(sexp: SEXP) {
                        unsafe {
                            let addr = R_ExternalPtrAddr(sexp);
                            if addr.is_null() {
                                return;
                            }
                            let _ = Box::from_raw(addr as *mut S);
                            R_ClearExternalPtr(sexp);
                        }
                    }
                    Rf_setAttrib(self.sexp(), R_AtsignSymbol, R_AtsignSymbol);
                    R_RegisterCFinalizerEx(self.sexp(), Some(free::<T>), 0);
                }
            }
        }
    }

    /// Check if an external pointer is managed by R.
    pub fn is_managed_by_r(&self) -> bool {
        unsafe { Rf_getAttrib(self.sexp(), R_AtsignSymbol) == R_AtsignSymbol }
    }

    /// Check if an external pointer is null.
    pub fn is_null(&self) -> bool {
        self.address().is_null()
    }

    /// Move an R external pointer to a Rust object.
    ///
    /// Takes ownership of a value stored by [`Self::encode_full`] with `managed_by_r = false`.
    ///
    /// # Safety
    /// A non-null, unmanaged address must come from `Box<T>`, with no other owner or outstanding references.
    pub unsafe fn decode_val<T>(&mut self) -> Result<T, &'static str> {
        if self.is_managed_by_r() {
            return Err("External pointer is managed by R.");
        }
        unsafe {
            let addr = R_ExternalPtrAddr(self.sexp());
            if addr.is_null() {
                return Err("External pointer was already decoded by value.");
            }
            R_ClearExternalPtr(self.sexp());
            Ok(*Box::from_raw(addr as *mut T))
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer.
    ///
    /// # Safety
    /// The address must point to a live `T` that remains valid and is not mutated for the borrow.
    pub unsafe fn decode_ref<T>(&self) -> &T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// # Safety
    /// As for [`Self::decode_ref`], for every use of the reference. Keep this external pointer
    /// reachable from R meanwhile, or its finalizer may free the value.
    pub unsafe fn decode_ref_static<T>(&self) -> &'static T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer.
    ///
    /// # Safety
    /// The address must point to a live `T` with exclusive access for the borrow, including through
    /// other R references to this external pointer.
    pub unsafe fn decode_mut<T>(&mut self) -> &mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// # Safety
    /// As for [`Self::decode_mut`], for every use of the reference. Keep this external pointer
    /// reachable from R meanwhile, or its finalizer may free the value.
    pub unsafe fn decode_mut_static<T>(&mut self) -> &'static mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Get the memory address of the external pointer.
    pub fn address(&self) -> *mut c_void {
        unsafe { R_ExternalPtrAddr(self.sexp()) }
    }

    /// Replace the memory address of the external pointer.
    ///
    /// # Safety
    /// `addr` must stay valid while the pointer is used and suit any registered finalizer. The
    /// previous allocation is not freed.
    pub unsafe fn set_address(&mut self, addr: &mut c_void) {
        unsafe { R_SetExternalPtrAddr(self.sexp(), addr) }
    }

    /// Register the external pointer to be finalized.
    ///
    /// This allows the object to perform cleanup actions when no longer referenced in R.
    ///
    pub fn register_finalizer(&self, func: extern "C" fn(sexp: SEXP)) -> Result<(), &'static str> {
        if self.is_managed_by_r() {
            return Err("External pointer is managed by R.");
        }
        unsafe {
            R_RegisterCFinalizerEx(self.sexp(), Some(func), 0);
            Ok(())
        }
    }

    /// Get tag for an R external pointer.
    ///
    /// This method gets the tag associated with an R external pointer, which was set by [`Self::encode`] or [`Self::encode_full`].
    ///
    pub fn tag(&self) -> &RObject {
        unsafe { R_ExternalPtrTag(self.sexp()).transmute(self) }
    }

    /// Get &str tag for an R external pointer.
    ///
    /// This method gets the &str tag associated with an R external pointer, which was set by [`Self::encode`] or [`Self::encode_full`].
    /// It turns the empty string "" if the tag is not a character vector of length one.
    ///
    pub fn tag_str(&self) -> &str {
        let tag = self.tag();
        let Ok(tag) = tag.as_scalar() else {
            return "";
        };
        let Ok(tag) = tag.as_char() else {
            return "";
        };
        tag.get().unwrap_or("")
    }
}

// Conversions

/// Create a variable in Rust from a reference to an R object.
pub trait FromR<T: RObjectVariant, U> {
    /// Create a new object in Rust from a reference to an R object.
    fn from_r(x: &T, pc: &Pc) -> Result<Self, U>
    where
        Self: Sized;
}

/// Create a mutable reference to an R object from a Rust object.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a Rust value that can be converted to a new R object",
    label = "expected a Rust value, such as a number, a string, or a slice",
    note = "to store an R object, build it inside `set_with_pc` or `set_attribute_with_pc`"
)]
pub trait ToR<T: RObjectVariant> {
    /// Create a new R object.
    #[expect(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut T;
}

/// Create a mutable reference to an R object from a Rust object.
pub trait ToR1<T: RObjectVariant> {
    /// Convert to an R object.
    #[expect(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut T;
}

/// Create a mutable reference to an R object from a Rust object.
pub trait ToR2<T: RObjectVariant> {
    /// Convert to an R object.
    #[expect(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut T;
}

/// A Rust value that converts to a new R object through [`ToR`], [`ToR1`], or [`ToR2`].
///
/// This lets one method accept all three. `Marker` only records which conversion applies and is
/// always inferred.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a Rust value that can be converted to a new R object",
    label = "expected a Rust value, such as a number, a string, a slice, or an iterator",
    note = "if this is the result of `to_r`, pass the Rust value itself",
    note = "to store an R object, build it inside `set_with_pc` or `set_attribute_with_pc`"
)]
pub trait IntoR<T: RObjectVariant, Marker> {
    /// Convert to an R object.
    #[expect(clippy::mut_from_ref)]
    fn into_r(self, pc: &Pc) -> &mut T;
}

#[doc(hidden)]
pub struct ViaToR;

#[doc(hidden)]
pub struct ViaToR1;

#[doc(hidden)]
pub struct ViaToR2;

impl<T: RObjectVariant, S: ToR<T>> IntoR<T, ViaToR> for S {
    fn into_r(self, pc: &Pc) -> &mut T {
        ToR::to_r(self, pc)
    }
}

impl<T: RObjectVariant, S: ToR1<T>> IntoR<T, ViaToR1> for S {
    fn into_r(self, pc: &Pc) -> &mut T {
        ToR1::to_r(self, pc)
    }
}

impl<T: RObjectVariant, S: ToR2<T>> IntoR<T, ViaToR2> for S {
    fn into_r(self, pc: &Pc) -> &mut T {
        ToR2::to_r(self, pc)
    }
}

/// Create a reference to an R object from a reference to Rust object.
pub trait ToRRef<T: RObjectVariant> {
    /// Convert to an R object.
    #[expect(clippy::mut_from_ref)]
    fn to_r<'a>(&self, pc: &'a Pc) -> &'a mut T;
}

/// Create an R object from a Rust object.
pub trait ToRNoMut<T: RObjectVariant> {
    /// Convert to an R object.
    fn to_r<'a>(self, pc: &'a Pc) -> &'a T
    where
        Self: 'a;
}

impl ToRNoMut<RObject> for () {
    fn to_r<'a>(self, pc: &'a Pc) -> &'a RObject
    where
        Self: 'a,
    {
        unsafe { R_NilValue.transmute::<RObject, Pc>(pc) }
    }
}

impl<T: RObjectVariant> ToRNoMut<RObject> for &T {
    fn to_r<'a>(self, _pc: &'a Pc) -> &'a RObject
    where
        Self: 'a,
    {
        self.as_robject()
    }
}

macro_rules! to_rscalar {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a> ToR<RScalar<$tipe>> for $tipe2 {
            fn to_r(self, pc: &Pc) -> &mut RScalar<$tipe> {
                RScalar::from_value(self, pc)
            }
        }
    };
}

to_rscalar!(f64, f64);
to_rscalar!(i32, i32);
to_rscalar!(u8, u8);
to_rscalar!(bool, bool);
to_rscalar!(char, &str);

macro_rules! to_rvector {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a> ToR<RVector<$tipe>> for &[$tipe2] {
            fn to_r(self, pc: &Pc) -> &mut RVector<$tipe> {
                RVector::from_slice(self, pc)
            }
        }
    };
}

to_rvector!(f64, f64);
to_rvector!(i32, i32);
to_rvector!(u8, u8);
to_rvector!(bool, bool);
to_rvector!(char, &str);

macro_rules! to_rvector_iter {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a, T: Iterator<Item = $tipe2> + ExactSizeIterator> ToR1<RVector<$tipe>> for T {
            fn to_r(self, pc: &Pc) -> &mut RVector<$tipe> {
                RVector::from_iter1(self, pc)
            }
        }

        impl<'a, T: Iterator<Item = &'a $tipe2> + ExactSizeIterator> ToR2<RVector<$tipe>> for T {
            fn to_r(self, pc: &Pc) -> &mut RVector<$tipe> {
                RVector::from_iter2(self, pc)
            }
        }
    };
}

to_rvector_iter!(f64, f64);
to_rvector_iter!(i32, i32);
to_rvector_iter!(u8, u8);
to_rvector_iter!(bool, bool);

/// Print to the R console.
///
/// This is an implementation detail and *should not* be called directly!
/// This returns `true` if the print statement swallowed a user interrupt.
/// R checks for user interrupt every 100 print statements.
/// See the `Rvprintf` function in `printutils.c` of R's source.
///
#[doc(hidden)]
pub fn __private_print(x: &str, newline: bool, use_stdout: bool) -> bool {
    check_main_thread();
    #[repr(C)]
    struct DummyFat {
        len: usize,
        ptr: *const c_char,
        newline: bool,
        use_stdout: bool,
    }
    let mut y = DummyFat {
        len: x.len(),
        ptr: x.as_ptr() as *const c_char,
        newline,
        use_stdout,
    };
    let y_ptr = std::ptr::from_mut(&mut y).cast::<c_void>();
    extern "C" fn print_fn(y_ptr: *mut c_void) {
        unsafe {
            let y_ptr = y_ptr as *mut DummyFat;
            let f = if (*y_ptr).use_stdout {
                Rprintf
            } else {
                REprintf
            };
            if (*y_ptr).newline {
                f(
                    c"%.*s\n".as_ptr() as *const c_char,
                    (*y_ptr).len,
                    (*y_ptr).ptr,
                );
            } else {
                f(
                    c"%.*s".as_ptr() as *const c_char,
                    (*y_ptr).len,
                    (*y_ptr).ptr,
                );
            }
        }
    }
    !toplevel_exec(print_fn, y_ptr)
}

/// Macro is called at the start of the 'src/rust/src/lib.rs' file.
///
/// This directs the Roxido Framework to automatically
/// generate code to register functions decorated with the `roxido` attribute
/// so that they are callable using R's `.Call` function. That is, this macro
/// automatically implements for the developer what is described in Section 5.4
/// Registering Native Routines in "Writing R Extensions". Rust code also be
/// organized in modules under `src/rust/src/`, but there is only one use of the
/// `roxido_registration` macro in the `src/rust/src/lib.rs` file.
#[macro_export]
macro_rules! roxido_registration {
    () => {
        mod registration {
            include!(concat!(env!("OUT_DIR"), "/registration.rs"));
        }
    };
}

/// Print like Rust's usual [`print!`] macro, except output goes to R's output stream.
#[macro_export]
macro_rules! rprint {
    ($($arg:tt)*) => {
        $crate::__private_print(&format!($($arg)*), false, true)
    };
}

/// Print like Rust's usual [`println!`] macro, except output goes to R's output stream.
#[macro_export]
macro_rules! rprintln {
    () => {
        $crate::__private_print("", true, true)
    };
    ($($arg:tt)*) => {
        $crate::__private_print(&format!($($arg)*), true, true)
    };
}

/// Print like Rust's usual [`eprint!`] macro, except output goes to R's error stream.
#[macro_export]
macro_rules! reprint {
    ($($arg:tt)*) => {
        $crate::__private_print(&format!($($arg)*), false, false)
    };
}

/// Print like Rust's usual [`eprintln!`] macro, except output goes to R's error stream.
#[macro_export]
macro_rules! reprintln {
    () => {
        $crate::__private_print("", true, false)
    };
    ($($arg:tt)*) => {
        $crate::__private_print(&format!($($arg)*), true, false)
    };
}

use std::fmt::Display;

#[doc(hidden)]
pub struct RStopHelper(pub String);

/// Throw an R error using semantics like Rust's [`panic`] macro.
///
/// Set the `RUST_BACKTRACE` environment variable to see a stack trace.
#[macro_export]
macro_rules! stop {
    () => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                panic!("Panic in stop!() due to RUST_BACKTRACE environment variable.")
            },
            Err(_) => {
                std::panic::panic_any($crate::RStopHelper(String::new()))
            }
        }
    };
    ($fmt_string:expr) => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                let mut msg = String::new();
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable: ");
                msg.push_str(&format!($fmt_string));
                panic!("{msg}");
            },
            Err(_) => {
                std::panic::panic_any($crate::RStopHelper(format!($fmt_string)))
            }
        }
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                let mut msg = String::new();
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable: ");
                msg.push_str(&format!($fmt_string, $($arg),*));
                panic!("{msg}");
            },
            Err(_) => {
                std::panic::panic_any($crate::RStopHelper(format!($fmt_string, $($arg),*)))
            }
        }
    }
}

/// Error handling functionality.
///
/// This trait provides the `stop` family of methods, which have similar
/// functionality to the `unwrap` methods on [`Result`] and [`Option`],
/// but throw an R error on failure.
pub trait UnwrapOrStop<T> {
    /// Throw an R error.
    ///
    /// Unwrap to obtain an R object or, on failure, throw an R error.
    fn stop(self) -> T;

    /// Unwrap to obtain an R object or, on failure, throw an R error with the provided message.
    fn stop_str(self, msg: &str) -> T;

    /// Unwrap to obtain an R object or, on failure, throw an R error with the message provided by a closure.
    fn stop_closure(self, msg: impl FnMut() -> String) -> T;
}

impl<T, S: Display> UnwrapOrStop<T> for Result<T, S> {
    fn stop(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => stop!("{}", e),
        }
    }
    fn stop_str(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg),
        }
    }
    fn stop_closure<'a>(self, mut msg: impl FnMut() -> String) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg()),
        }
    }
}

impl<T> UnwrapOrStop<T> for Option<T> {
    fn stop(self) -> T {
        match self {
            Some(t) => t,
            None => stop!("A required value is missing."),
        }
    }
    fn stop_str(self, msg: &str) -> T {
        match self {
            Some(t) => t,
            None => stop!("{}", msg),
        }
    }
    fn stop_closure<'a>(self, mut msg: impl FnMut() -> String) -> T {
        match self {
            Some(t) => t,
            None => stop!("{}", msg()),
        }
    }
}

/// Record R's main thread and silence `stop!()` panics. Called by the package loader or rustfn.
///
/// # Safety
/// Call on R's main thread.
#[doc(hidden)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __private_init() -> SEXP {
    if IS_MAIN_THREAD.with(|x| x.replace(true)) {
        return unsafe { R_NilValue };
    }
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        if panic_info.payload().downcast_ref::<RStopHelper>().is_none() {
            default_panic(panic_info);
        }
    }));
    unsafe { R_NilValue }
}
