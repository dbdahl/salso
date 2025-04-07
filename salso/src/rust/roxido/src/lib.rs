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
//! of them. Any function with the roxido attribute must return an [RObject],
//! and may only take parameters of [RObject] type.
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
use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr, CString, NulError};
use std::marker::PhantomData;

trait SEXPMethods {
    /// # Safety
    /// Expert use only.
    unsafe fn transmute<T: RObjectVariant, A>(self, _anchor: &A) -> &T;

    /// # Safety
    /// Expert use only.
    #[allow(clippy::mut_from_ref)]
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

/// Trait for methods shared by all R objects.
pub trait RObjectVariant: Sized {
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
    /// Expert use only.
    unsafe fn transmute<T: RObjectVariant>(&self) -> &T
    where
        Self: Sized,
    {
        self.sexp().transmute(self)
    }

    /// # Safety
    /// Expert use only.
    unsafe fn transmute_mut<T: RObjectVariant>(&mut self) -> &mut T
    where
        Self: Sized,
    {
        self.sexp().transmute_mut(self)
    }

    /// Duplicate an R object.
    ///
    /// Multiple symbols may be bound to the same object, so if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy first.
    ///
    #[allow(clippy::mut_from_ref)]
    fn clone<'a>(&self, pc: &'a Pc) -> &'a mut Self {
        unsafe { pc.protect_and_transmute(Rf_duplicate(self.sexp())) }
    }

    /// Get the class of an R object.
    fn get_class(&self) -> &RVector<char> {
        unsafe { Rf_getAttrib(self.sexp(), RSymbol::class().sexp()).transmute(self) }
    }

    /// Set the class of an R object.
    fn set_class(&mut self, names: &RVector<char>) {
        unsafe {
            Rf_classgets(self.sexp(), names.sexp());
        }
    }

    /// Get an attribute of an R object.
    fn get_attribute(&self, which: &RSymbol) -> &RObject {
        unsafe { Rf_getAttrib(self.sexp(), which.sexp()).transmute(self) }
    }

    /// Set an attribute of an R object.
    fn set_attribute(&mut self, which: &RSymbol, value: &impl RObjectVariant) {
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
    fn set_names(&mut self, names: &RVector<char>) -> Result<(), &'static str> {
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
    #[allow(clippy::mut_from_ref)]
    fn from_value(value: T, pc: &Pc) -> &mut Self;
}

/// Methods for creating an R object from an iterator.
pub trait RFromIterator<T> {
    #[allow(clippy::mut_from_ref)]
    fn from_iter1<S>(iter: S, pc: &Pc) -> &mut Self
    where
        S: IntoIterator<Item = T> + ExactSizeIterator;

    #[allow(clippy::mut_from_ref)]
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
    #[allow(clippy::mut_from_ref)]
    fn new(length: usize, pc: &Pc) -> &mut Self;

    #[allow(clippy::mut_from_ref)]
    fn from_value(value: T, length: usize, pc: &Pc) -> &mut Self;

    #[allow(clippy::mut_from_ref)]
    fn from_array<const N: usize>(slice: [T; N], pc: &Pc) -> &mut Self;

    #[allow(clippy::mut_from_ref)]
    fn from_slice<'a>(slice: &[T], pc: &'a Pc) -> &'a mut Self;
}

/// Methods for creating a new matrix in R.
pub trait RMatrixConstructors<T> {
    #[allow(clippy::mut_from_ref)]
    fn new(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self;

    #[allow(clippy::mut_from_ref)]
    fn from_value(value: T, nrow: usize, ncol: usize, pc: &Pc) -> &mut Self;
}

/// Methods for creating a new array in R.
pub trait RArrayConstructors<T> {
    #[allow(clippy::mut_from_ref)]
    fn new<'a>(dim: &[usize], pc: &'a Pc) -> &'a mut Self;

    #[allow(clippy::mut_from_ref)]
    fn from_value<'a>(value: T, dim: &[usize], pc: &'a Pc) -> &'a mut Self;
}

static TOO_LONG: &str = "Could not fit usize into i32.";

fn charsxp_from_str(x: &str) -> SEXP {
    unsafe {
        Rf_mkCharLenCE(
            x.as_ptr() as *const c_char,
            x.len().try_into().stop_str(TOO_LONG),
            cetype_t_CE_UTF8,
        )
    }
}

fn charsxp_as_str<T>(sexp: SEXP, _anchor: &T) -> Result<&str, &'static str> {
    let c_str = unsafe { CStr::from_ptr(R_CHAR(sexp) as *const c_char) };
    c_str.to_str().map_err(|_| "Not valid UTF8.")
}

macro_rules! robject_variant {
    ($name:ident) => {
        #[repr(C)]
        pub struct $name {
            sexprec: SEXPREC,
        }
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
            pub sexprec: SEXPREC,
            rtype: PhantomData<RMode>,
        }
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

pub struct Pc {
    counter: std::cell::RefCell<i32>,
}

impl Default for Pc {
    fn default() -> Self {
        Self::__private_new()
    }
}

impl Drop for Pc {
    fn drop(&mut self) {
        let count = *self.counter.borrow();
        if count > 0 {
            unsafe { Rf_unprotect(count) };
        }
    }
}

impl Pc {
    /// Allocate a new protection counter.
    ///
    /// This is an implementation detail and *should not* be called directly!
    /// Functions defined with the `roxido` macro already have an instance of this structure named
    /// `pc`, so this function is generally not needed.
    ///
    #[doc(hidden)]
    pub fn __private_new() -> Self {
        Self { counter: 0.into() }
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn protect(&self, sexp: SEXP) -> SEXP {
        unsafe { Rf_protect(sexp) };
        let mut counter = self.counter.borrow_mut();
        *counter += 1;
        sexp
    }

    /// # Safety
    /// Expert use only.
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn protect_and_transmute<T: RObjectVariant>(&self, sexp: SEXP) -> &mut T {
        let sexp = self.protect(sexp);
        unsafe { sexp.transmute_mut(self) }
    }
}

pub struct R;

impl R {
    /// Returns R's `NULL` value.
    pub fn null() -> &'static RObject {
        unsafe { R_NilValue.transmute_static() }
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
        if x {
            Rboolean_TRUE
        } else {
            Rboolean_FALSE
        }
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
        unsafe { R_FlushConsole() };
    }

    /// Check if the user has attempted to interrupt the execution.
    pub fn check_user_interrupt() -> bool {
        extern "C" fn check_interrupt_fn(_: *mut c_void) {
            unsafe { R_CheckUserInterrupt() };
        }
        unsafe { R_ToplevelExec(Some(check_interrupt_fn), std::ptr::null_mut()) == 0 }
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
    pub fn enumerate(&self) -> RObjectEnum {
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

    /// # Safety
    /// Expert use only.
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub unsafe fn from_sexp<A>(sexp: SEXP, anchor: &A) -> &Self {
        unsafe { sexp.transmute(anchor) }
    }

    /// # Safety
    /// Expert use only.
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn from_sexp_mut<A>(sexp: SEXP, anchor: &A) -> &mut Self {
        unsafe { sexp.transmute_mut(anchor) }
    }

    /// Returns an `Option` which equals `None` if the [`RObject`] reference is null.
    pub fn as_option(&self) -> Option<&Self> {
        if self.is_null() {
            None
        } else {
            Some(self)
        }
    }

    /// Attempts to recharacterize as a reference to an [`RScalar`] (i.e., a vector of length 1).
    pub fn as_scalar(&self) -> Result<&RScalar, &'static str> {
        let s = self.as_vector()?;
        if s.is_scalar() {
            Ok(unsafe { self.transmute() })
        } else {
            Err("Not a vector.")
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

    /// Attempts to recharacterize as a reference to an ]`RMatrix`].
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
            Err("Not a vector.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RArray`].
    pub fn as_array_mut(&mut self) -> Result<&mut RArray, &'static str> {
        if self.is_array() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not a vector.")
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
            Err("Not an external pointer.")
        }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RSymbol`].
    pub fn as_symbol_mut(&mut self) -> Result<&mut RSymbol, &'static str> {
        if self.is_symbol() {
            Ok(unsafe { self.transmute_mut() })
        } else {
            Err("Not an external pointer.")
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
        let cstr = CString::new("data.frame").unwrap();
        unsafe { Rf_inherits(self.sexp(), cstr.as_ptr()) != 0 }
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
    #[allow(clippy::mut_from_ref)]
    pub fn new<'a>(message: &str, pc: &'a Pc) -> &'a mut Self {
        let list = RList::with_names(&["message", "calls"], pc);
        let _ = list.set(0, message.to_r(pc));
        let _ = list.set(1, R::null());
        list.set_class(["error", "condition"].to_r(pc));
        unsafe { list.transmute_mut() }
    }
}

impl RSymbol {
    /// Define a new symbol.
    pub fn new(x: &CStr) -> &'static Self {
        // Doesn't need protection because R's garbage collection does not collect symbols.
        unsafe { Rf_install(x.as_ptr()).transmute_static() }
    }

    /// Define a new symbol from a string.
    pub fn from(s: &str) -> Result<&Self, NulError> {
        CString::new(s).map(|cstr| unsafe { Rf_install(cstr.as_ptr()).transmute_static() })
    }

    /// Get R's "dim" symbol.
    pub fn dim() -> &'static Self {
        unsafe { R_DimSymbol.transmute_static() }
    }

    /// Get R's "names" symbol.
    pub fn names() -> &'static Self {
        unsafe { R_NamesSymbol.transmute_static() }
    }

    /// Get R's "rownames" symbol.
    pub fn rownames() -> &'static Self {
        unsafe { R_RowNamesSymbol.transmute_static() }
    }

    /// Get R's "dimnames" symbol.
    pub fn dimnames() -> &'static Self {
        unsafe { R_DimNamesSymbol.transmute_static() }
    }

    /// Get R's "class" symbol.
    pub fn class() -> &'static Self {
        unsafe { R_ClassSymbol.transmute_static() }
    }
}

impl RFunction {
    fn eval(expression: SEXP, pc: &Pc) -> Result<&RObject, i32> {
        let expression = pc.protect(expression);
        let mut p_out_error: i32 = 0;
        let sexp = pc.protect(unsafe {
            R_tryEval(expression, R_GetCurrentEnv(), &mut p_out_error as *mut i32)
        });
        match p_out_error {
            0 => Ok(unsafe { sexp.transmute(pc) }),
            e => Err(e),
        }
    }

    /// Evaluate a function with no parameters.
    pub fn call0<'a>(&self, pc: &'a Pc) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang1(self.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 1 parameter.
    pub fn call1<'a>(&self, arg1: &impl RObjectVariant, pc: &'a Pc) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang2(self.sexp(), arg1.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 2 parameters.
    pub fn call2<'a>(
        &self,
        arg1: &impl RObjectVariant,
        arg2: &impl RObjectVariant,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang3(self.sexp(), arg1.sexp(), arg2.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 3 parameters.
    pub fn call3<'a>(
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
    pub fn call4<'a>(
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
    pub fn call5<'a>(
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
    /// Attempts to recharacterize as a reference to an [`RVector`].
    pub fn as_vector(&self) -> &RVector<T> {
        unsafe { self.transmute() }
    }

    /// Attempts to recharacterize as a mutable reference to an [`RVector`].
    pub fn as_vector_mut(&mut self) -> &mut RVector<T> {
        unsafe { self.transmute_mut() }
    }

    /// Interpret as an `f64`.
    pub fn f64(&self) -> f64 {
        unsafe { Rf_asReal(self.sexp()) }
    }

    /// Interpret as an `i32`.
    pub fn i32(&self) -> Result<i32, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("i32 equals R's NA for integers.")
            } else {
                Ok(x)
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if y > f64::from(i32::MAX) {
                Err("Greater than maximum integer value.")
            } else if y < f64::from(i32::MIN) {
                Err("Less than minimum integer value.")
            } else if y == f64::from(i32::MIN) {
                Err("Equals R's NA for integers.")
            } else if y.is_nan() {
                Err("Equals R's NaN.")
            } else {
                Ok(y.round() as i32)
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

    /// Attempt to interpret as an `usize`.
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
            if y < 0.0 {
                Err("Negative value not expected.")
            } else {
                let z = y as usize;
                if z as f64 == y {
                    Ok(z)
                } else {
                    Err("Cannot convert to usize.")
                }
            }
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

    /// Attempt to interpret as an `u8`.
    pub fn u8(&self) -> Result<u8, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            u8::try_from(x).map_err(|_| "Cannot convert to u8.")
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if y < 0.0 {
                Err("Negative value not expected.")
            } else {
                let z = y as u8;
                if z as f64 == y {
                    Ok(z)
                } else {
                    Err("Cannot convert to u8.")
                }
            }
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

    /// Attempt to interpret as an `bool`.
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

    /// Attempt to interpret as an a string.
    pub fn str<'a>(&'a self, pc: &'a Pc) -> &'a str {
        let s = self.to_char(pc);
        s.get().unwrap()
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
            #[allow(clippy::mut_from_ref)]
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
    #[allow(clippy::mut_from_ref)]
    fn from_value(value: bool, pc: &Pc) -> &mut Self {
        unsafe { pc.protect_and_transmute(Rf_ScalarLogical(R::as_logical(value))) }
    }
}

impl RScalarConstructor<&str> for RScalar<char> {
    #[allow(clippy::mut_from_ref)]
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

            /// Checks if its using R's storage model "double".
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

            /// Checks if its using R's storage model "integer".
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

            /// Checks if its using R's storage model "raw".
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

            /// Checks if its using R's storage model "logical".
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

            /// Checks if its using R's storage model "character".
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
                unsafe { std::slice::from_raw_parts($ptr(self.sexp()), self.len()) }
            }

            fn slice_mut(&mut self) -> &mut [$tipe2] {
                unsafe { std::slice::from_raw_parts_mut($ptr(self.sexp()), self.len()) }
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
                let result = Self::new(iter.len(), pc);
                let slice = result.slice_mut();
                for (s, d) in slice.iter_mut().zip(iter) {
                    *s = d;
                }
                result
            }

            fn from_iter2<'a, 'b, T>(iter: T, pc: &'a Pc) -> &'a mut Self
            where
                T: IntoIterator<Item = &'b $tipe> + ExactSizeIterator,
            {
                let result = Self::new(iter.len(), pc);
                let slice = result.slice_mut();
                for (s, d) in slice.iter_mut().zip(iter) {
                    *s = *d;
                }
                result
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
                unsafe {
                    pc.protect_and_transmute(Rf_allocVector(
                        $code,
                        length.try_into().stop_str(TOO_LONG),
                    ))
                }
            }

            fn from_value(value: $tipe, length: usize, pc: &Pc) -> &mut Self {
                let result = Self::new(length, pc);
                let slice = result.slice_mut();
                slice.fill(value);
                result
            }

            fn from_array<const N: usize>(slice: [$tipe; N], pc: &Pc) -> &mut Self {
                let result = Self::new(slice.len(), pc);
                let slice2 = result.slice_mut();
                slice2.copy_from_slice(slice.as_ref());
                result
            }

            fn from_slice<'a>(slice: &[$tipe], pc: &'a Pc) -> &'a mut Self {
                let result = Self::new(slice.len(), pc);
                let slice2 = result.slice_mut();
                slice2.copy_from_slice(slice);
                result
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
        let result = Self::new(iter.len(), pc);
        let slice = result.slice_mut();
        for (s, d) in slice.iter_mut().zip(iter) {
            *s = R::as_logical(d);
        }
        result
    }

    fn from_iter2<'a, 'b, T>(iter: T, pc: &'a Pc) -> &'a mut Self
    where
        T: IntoIterator<Item = &'b bool> + ExactSizeIterator,
    {
        let result = Self::new(iter.len(), pc);
        let slice = result.slice_mut();
        for (s, d) in slice.iter_mut().zip(iter) {
            *s = R::as_logical(*d);
        }
        result
    }
}

impl RGetSet1<bool> for RVector<bool> {
    /// Get the value at a certain index in an $tipe RVector.
    fn get(&self, index: usize) -> Result<bool, &'static str> {
        if index < self.len() {
            let value = unsafe { LOGICAL_ELT(self.sexp(), index.try_into().unwrap()) };
            Ok(R::is_true(value))
        } else {
            Err("Index out of bounds.")
        }
    }

    /// Set the value at a certain index in an $tipe RVector.
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
    /// Get the value at a certain index in an $tipe RVector.
    pub fn get_i32(&self, index: usize) -> Result<i32, &'static str> {
        if index < self.len() {
            Ok(unsafe { LOGICAL_ELT(self.sexp(), index.try_into().unwrap()) })
        } else {
            Err("Index out of bounds.")
        }
    }

    /// Set the value at a certain index in an $tipe RVector.
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
        unsafe {
            pc.protect_and_transmute(Rf_allocVector(LGLSXP, length.try_into().stop_str(TOO_LONG)))
        }
    }

    fn from_value(value: bool, length: usize, pc: &Pc) -> &mut Self {
        let result = Self::new(length, pc);
        let slice = result.slice_mut();
        slice.fill(R::as_logical(value));
        result
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
    pub fn get(&self, index: usize) -> Result<&str, &'static str> {
        if index < self.len() {
            self.get_unchecked(index)
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
    #[allow(clippy::mut_from_ref)]
    fn new(length: usize, pc: &Pc) -> &mut Self {
        unsafe {
            pc.protect_and_transmute(Rf_allocVector(STRSXP, length.try_into().stop_str(TOO_LONG)))
        }
    }

    #[allow(clippy::mut_from_ref)]
    fn from_value<'a>(value: &str, length: usize, pc: &'a Pc) -> &'a mut Self {
        let length_i32 = length.try_into().stop_str(TOO_LONG);
        let vec = pc.protect(unsafe { Rf_allocVector(STRSXP, length_i32) });
        if length_i32 > 0 {
            let element = pc.protect(charsxp_from_str(value));
            unsafe { SET_STRING_ELT(vec, 0, element) };
            for index in 1..length_i32 {
                unsafe { SET_STRING_ELT(vec, index, pc.protect(Rf_duplicate(element))) };
            }
        }
        unsafe { vec.transmute_mut(pc) }
    }

    #[allow(clippy::mut_from_ref)]
    fn from_array<'a, const N: usize>(array: [&str; N], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(array.len(), pc);
        for (index, value) in array.iter().enumerate() {
            result.set_unchecked(index, value)
        }
        result
    }

    #[allow(clippy::mut_from_ref)]
    fn from_slice<'a>(slice: &[&str], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(slice.len(), pc);
        for (index, value) in slice.iter().enumerate() {
            result.set_unchecked(index, value)
        }
        result
    }
}

macro_rules! rmatrix {
    ($tipe:ty, $code:expr, $get:expr, $set:expr) => {
        impl RGetSet2<$tipe> for RMatrix<$tipe> {
            /// Get the value at a certain index in an $tipe RMatrix.
            fn get(&self, row: usize, col: usize) -> Result<$tipe, &'static str> {
                let index = self.index(row, col, None);
                if index < self.len() {
                    Ok(unsafe { $get(self.sexp(), index.try_into().unwrap()) })
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Set the value at a certain index in an $tipe RMatrix.
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
                unsafe {
                    pc.protect_and_transmute(Rf_allocMatrix(
                        $code,
                        nrow.try_into().stop_str(TOO_LONG),
                        ncol.try_into().stop_str(TOO_LONG),
                    ))
                }
            }

            fn from_value(value: $tipe, nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
                let result = Self::new(nrow, ncol, pc);
                let slice = result.slice_mut();
                slice.fill(value);
                result
            }
        }
    };
}

rmatrix!(f64, REALSXP, REAL_ELT, SET_REAL_ELT);
rmatrix!(i32, INTSXP, INTEGER_ELT, SET_INTEGER_ELT);
rmatrix!(u8, RAWSXP, RAW_ELT, SET_RAW_ELT);

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
    #[allow(clippy::mut_from_ref)]
    pub fn transpose<'a>(&self, pc: &'a Pc) -> &'a mut Self {
        let transposed = self.clone(pc);
        let mut dim = transposed.dim();
        dim.swap(0, 1);
        let dim2: [i32; 2] = [dim[0].try_into().unwrap(), dim[1].try_into().unwrap()];
        transposed.set_attribute(RSymbol::dim(), dim2.to_r(pc));
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

    /// Get the value at a certain row and column in the R matrix.
    pub fn index(&self, row: usize, col: usize, nrow: Option<usize>) -> usize {
        let nrow = nrow.unwrap_or_else(|| self.nrow());
        nrow * col + row
    }

    /// Get the dimnames of a matrix.
    pub fn get_dimnames(&self) -> &RVector<char> {
        unsafe { Rf_getAttrib(self.sexp(), R_DimNamesSymbol).transmute(self) }
    }

    /// Set the dimnames of a matrix.
    pub fn set_dimnames(&mut self, dimnames: &RList) -> Result<(), &'static str> {
        match dimnames.get(0) {
            Ok(rownames) => match rownames.as_vector() {
                Ok(rownames) => {
                    if rownames.len() != self.nrow() {
                        return Err("Row names do not match the number of rows.");
                    }
                }
                Err(_) => {
                    return Err("Row names must be a character vector.");
                }
            },
            Err(_) => return Err("No row names element found."),
        };
        match dimnames.get(1) {
            Ok(colnames) => match colnames.as_vector() {
                Ok(colnames) => {
                    if colnames.len() != self.ncol() {
                        return Err("Column names do not match the number of columns.");
                    }
                }
                Err(_) => {
                    return Err("Column names must be a character vector.");
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
    ($tipe:ty, $code:expr, $get:expr, $set:expr) => {
        impl RGetSetN<$tipe> for RArray<$tipe> {
            /// Get the value at a certain index in an $tipe RArray.
            fn get(&self, index: &[usize]) -> Result<$tipe, &'static str> {
                let index = self.index(index, None);
                if index < self.len() {
                    Ok(unsafe { $get(self.sexp(), index.try_into().unwrap()) })
                } else {
                    Err("Index out of bounds.")
                }
            }
            /// Set the value at a certain index in an $tipe RArray.
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
                let dim = dim
                    .iter()
                    .map(|x| i32::try_from(*x).stop_str(TOO_LONG))
                    .to_r(pc)
                    .sexp();
                unsafe { pc.protect_and_transmute(Rf_allocArray($code, dim)) }
            }
            fn from_value<'a>(value: $tipe, dim: &[usize], pc: &'a Pc) -> &'a mut Self {
                let result = Self::new(dim, pc);
                let slice = result.slice_mut();
                slice.fill(value);
                result
            }
        }
    };
}

rarray!(f64, REALSXP, REAL_ELT, SET_REAL_ELT);
rarray!(i32, INTSXP, INTEGER_ELT, SET_INTEGER_ELT);
rarray!(u8, RAWSXP, RAW_ELT, SET_RAW_ELT);

impl<T> RArray<T> {
    /// Return the dimensions of the RMatrix.
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

    /// Get the index of a value based on the row and column number.
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

    /// Get the dimnames of a matrix.
    pub fn get_dimnames(&self) -> &RVector<char> {
        unsafe { Rf_getAttrib(self.sexp(), R_DimNamesSymbol).transmute(self) }
    }

    /// Set the dimnames of a matrix.
    pub fn set_dimnames(&mut self, dimnames: &RList) -> Result<(), String> {
        let dim = self.dim();
        if dimnames.len() != dim.len() {
            return Err(format!(
                "Length of dimnames is {} whereas the dim is of length {}.",
                dimnames.len(),
                dim.len()
            ));
        }
        for (i, &len) in dim.iter().enumerate() {
            match dimnames.get(i).unwrap().as_vector() {
                Ok(names) => {
                    if names.len() != len {
                        return Err(format!("Element {} of the dimnames list has length {}, but the corresponding dimension is {}.", i, names.len(), len));
                    }
                }
                Err(_) => {
                    return Err(format!(
                        "Element {} of the dimnames list must be a character vector.",
                        i
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
            return Err(format!("'{}' not found.", name));
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
        let result = self
            .map
            .iter()
            .filter(|(_, index)| !self.used[**index])
            .map(|(name, _)| *name)
            .take(self.unused_counter);
        result.collect()
    }
}

macro_rules! rlistlike {
    ($name:ident) => {
        impl $name {
            /// Get the value at a certain index in the R list.
            pub fn get(&self, index: usize) -> Result<&RObject, &'static str> {
                if index < self.len() {
                    Ok(unsafe {
                        VECTOR_ELT(self.sexp(), index.try_into().unwrap()).transmute(self)
                    })
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Get the value at a certain index in the R list.
            pub fn get_mut(&mut self, index: usize) -> Result<&mut RObject, &'static str> {
                if index < self.len() {
                    Ok(unsafe {
                        VECTOR_ELT(self.sexp(), index.try_into().unwrap()).transmute_mut(self)
                    })
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Set the value at a certain index in the R list.
            pub fn set(
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

            /// Set the value at a certain index in the R list via a closure with a mutable reference to a local Pc.
            ///
            /// This is slightly less efficient than `set` but is useful to avoid overflowing R's PROTECT stack for
            /// very large lists.
            pub fn set_with_pc<T: RObjectVariant, F: FnOnce(&mut Pc) -> &T>(
                &mut self,
                index: usize,
                x: F,
            ) -> Result<(), &'static str> {
                if index < self.len() {
                    let mut pc = Pc::default();
                    unsafe {
                        SET_VECTOR_ELT(self.sexp(), index.try_into().unwrap(), x(&mut pc).sexp())
                    };
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Set the values in the R list via a closure with a mutable reference to a local Pc.
            ///
            /// This is slightly less efficient than `set` but is useful to avoid overflowing R's PROTECT stack for
            /// very large lists.
            pub fn set_loop_with_pc<T: RObjectVariant, F: FnMut(&mut Pc) -> &T>(
                &mut self,
                index: usize,
                mut x: F,
            ) -> Result<(), &'static str> {
                if index < self.len() {
                    let mut pc = Pc::default();
                    for i in 0..self.len() {
                        unsafe {
                            SET_VECTOR_ELT(self.sexp(), i.try_into().unwrap(), x(&mut pc).sexp())
                        };
                    }
                    Ok(())
                } else {
                    Err("Index out of bounds.")
                }
            }

            /// Get a value in an R list based on its key.
            pub fn get_by_key(&self, key: impl AsRef<str>) -> Result<&RObject, String> {
                let names = self.get_names();
                for i in 0..names.len() {
                    if names.get(i).unwrap() == key.as_ref() {
                        return Ok(self.get(i)?);
                    }
                }
                Err(format!("Could not find '{}' in the list.", key.as_ref()))
            }

            /// Get a value in an R list based on its key.
            pub fn get_mut_by_key(&mut self, key: impl AsRef<str>) -> Result<&mut RObject, String> {
                let names = self.get_names();
                for i in 0..names.len() {
                    if names.get(i).unwrap() == key.as_ref() {
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
            pub fn make_map(&self) -> RListMap {
                let mut map = HashMap::new();
                let names = self.get_names();
                let len = names.len();
                for i in 0..len {
                    map.insert(names.get(i).unwrap(), i);
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
    #[allow(clippy::mut_from_ref)]
    pub fn new(length: usize, pc: &Pc) -> &mut Self {
        unsafe {
            pc.protect_and_transmute(Rf_allocVector(VECSXP, length.try_into().stop_str(TOO_LONG)))
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn with_names<'a>(names: &[&str], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(names.len(), pc);
        unsafe {
            Rf_namesgets(result.sexp(), names.to_r(pc).sexp());
        }
        result
    }
}

impl RExternalPtr {
    /// Move Rust object to an R external pointer.
    ///
    /// This *method* moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RExternalPtr::decode_val`]
    /// unless `managed_By_r` is `true`.
    #[allow(clippy::mut_from_ref)]
    pub fn encode<'a, T>(x: T, tag: &str, pc: &'a Pc) -> &'a mut Self {
        Self::encode_full(x, tag.to_r(pc), true, pc)
    }

    /// Move Rust object to an R external pointer.
    ///
    /// This *method* moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RExternalPtr::decode_val`]
    /// unless `managed_By_r` is `true`.
    #[allow(clippy::mut_from_ref)]
    pub fn encode_full<'a, T>(
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
                    let addr = R_ExternalPtrAddr(sexp);
                    if addr.as_ref().is_none() {
                        return;
                    }
                    let _ = Box::from_raw(addr as *mut S);
                    R_ClearExternalPtr(sexp);
                }
                Rf_setAttrib(sexp, R_AtsignSymbol, R_AtsignSymbol);
                R_RegisterCFinalizerEx(sexp, Some(free::<T>), 0);
            }
            sexp.transmute_mut(pc)
        }
    }

    /// Check if an external pointer is managed by R.
    pub fn is_managed_by_r(&self) -> bool {
        unsafe { Rf_getAttrib(self.sexp(), R_AtsignSymbol) == R_AtsignSymbol }
    }

    /// Move an R external pointer to a Rust object.
    ///
    /// This method moves an R external pointer created by [`RObject::as_external_ptr`] to a Rust object and Rust will then manage its memory.
    ///
    pub fn decode_val<T>(&self) -> Result<T, &'static str> {
        if self.is_managed_by_r() {
            return Err("External pointer is managed by R.");
        }
        unsafe {
            let addr = R_ExternalPtrAddr(self.sexp());
            if addr.as_ref().is_none() {
                return Err("External pointer was already decoded by value.");
            }
            R_ClearExternalPtr(self.sexp());
            Ok(*Box::from_raw(addr as *mut T))
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer.
    ///
    /// This method obtains a reference to a Rust object from an R external pointer created by [`RObject::as_external_ptr`].
    ///
    pub fn decode_ref<T>(&self) -> &T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a reference to a Rust object from an R external pointer created by [`RObject::as_external_ptr`].
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn decode_ref_static<T>(&self) -> &'static T {
        let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
        ptr.as_ref().unwrap()
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer.
    ///
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`RObject::as_external_ptr`].
    pub fn decode_mut<'a, T>(&mut self) -> &'a mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`RObject::as_external_ptr`].
    ///
    /// # Safety
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn decode_mut_static<T>(&mut self) -> &'static mut T {
        let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
        ptr.as_mut().unwrap()
    }

    /// Get the memory address of the external pointer.
    pub fn address(&self) -> *mut c_void {
        unsafe { R_ExternalPtrAddr(self.sexp()) }
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
    /// This method gets the tag associated with an R external pointer, which was set by [`RObject::as_external_ptr`].
    ///
    pub fn tag(&self) -> &RObject {
        unsafe { R_ExternalPtrTag(self.sexp()).transmute(self) }
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
pub trait ToR<T: RObjectVariant> {
    /// Create a new R object.
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut T;
}

/// Create a mutable reference to an R object from a Rust object.
pub trait ToR1<T: RObjectVariant> {
    /// Convert to an R object.
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut T;
}

/// Create a mutable reference to an R object from a Rust object.
pub trait ToR2<T: RObjectVariant> {
    /// Convert to an R object.
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut T;
}

/// Create a reference to an R object from a reference to Rust object.
pub trait ToRRef<T: RObjectVariant> {
    /// Convert to an R object.
    #[allow(clippy::mut_from_ref)]
    fn to_r<'a>(&self, pc: &'a Pc) -> &'a mut T;
}

/// Create an R object from a Rust object.
pub trait ToRNoMut<T: RObjectVariant> {
    /// Convert to an R object.
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &T;
}

impl ToRNoMut<RObject> for () {
    fn to_r(self, pc: &Pc) -> &RObject {
        unsafe { R_NilValue.transmute_mut::<RObject, Pc>(pc) }
    }
}

impl ToRNoMut<RObject> for SEXP {
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    fn to_r(self, pc: &Pc) -> &RObject {
        unsafe { self.transmute::<RObject, Pc>(pc) }
    }
}

impl<T: RObjectVariant> ToRNoMut<RObject> for &T {
    fn to_r(self, pc: &Pc) -> &RObject {
        unsafe { self.sexp().transmute::<RObject, Pc>(pc) }
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
    let y_ptr = &mut y as *mut DummyFat as *mut c_void;
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
    unsafe { R_ToplevelExec(Some(print_fn), y_ptr) == 0 }
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

/// Print like Rust's usual [`print!`] macro, except output goes to R's output steam.
#[macro_export]
macro_rules! rprint {
    ($fmt_string:expr) => {
        __private_print(format!($fmt_string).as_str(), false, true)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        __private_print(format!($fmt_string, $($arg),*).as_str(), false, true)
    }
}

/// Print like Rust's usual [`println!`] macro, except output goes to R's output steam.
#[macro_export]
macro_rules! rprintln {
    () => {
        __private_print("", true, true)
    };
    ($fmt_string:expr) => {
        __private_print(format!($fmt_string).as_str(), true, true)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        __private_print(format!($fmt_string, $($arg),*).as_str(), true, true)
    }
}

/// Print like Rust's usual [`eprint!`] macro, except output goes to R's error steam.
#[macro_export]
macro_rules! reprint {
    ($fmt_string:expr) => {
        __private_print(format!($fmt_string).as_str(), false, false)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        __private_print(format!($fmt_string, $($arg),*).as_str(), false, false)
    }
}

/// Print like Rust's usual [`eprintln!`] macro, except output goes to R's error steam.
#[macro_export]
macro_rules! reprintln {
    () => {
        __private_print("\n", false)
    };
    ($fmt_string:expr) => {
        __private_print(format!($fmt_string).as_str(), true, false)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        __private_print(format!($fmt_string, $($arg),*).as_str(), true, false)
    }
}

use std::fmt::Display;

#[doc(hidden)]
pub struct RStopHelper(pub String);

/// Throw an R error using symmatrics like Rust's [`panic`] macro.
///
/// Set the `RUST_BACKTRACE` environment variable to see a stack trace.
#[macro_export]
#[allow(clippy::crate_in_macro_def)]
macro_rules! stop {
    () => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                panic!("Panic in stop!() due to RUST_BACKTRACE environment variable.")
            },
            Err(_) => {
                std::panic::panic_any(RStopHelper(String::new()))
            }
        }
    };
    ($fmt_string:expr) => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                let mut msg = String::new();
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable: ");
                msg.push_str(&format!($fmt_string));
                panic!("{}", msg);
            },
            Err(_) => {
                std::panic::panic_any(RStopHelper(format!($fmt_string)))
            }
        }
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                let mut msg = String::new();
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable: ");
                msg.push_str(&format!($fmt_string, $($arg),*));
                panic!("{}", msg);
            },
            Err(_) => {
                std::panic::panic_any(RStopHelper(format!($fmt_string, $($arg),*)))
            }
        }
    }
}

/// Error handling functionality.
///
/// This trait provides the `stop` family of methods, which have similar
/// functionality to the `unwrap` methods on [`Result`] and [`Option`],
//  but throw an R error on failure.
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
            None => stop!(),
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

#[doc(hidden)]
#[no_mangle]
pub extern "C" fn __private_set_custom_panic_hook() -> SEXP {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        if panic_info.payload().downcast_ref::<RStopHelper>().is_none() {
            default_panic(panic_info);
        }
    }));
    unsafe { R_NilValue }
}
