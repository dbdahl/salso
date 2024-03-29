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
//! fn convolve2(a: &RObject<RVector>, b: &RObject<RVector>) {
//!     let vec = RObject::<RVector, f64>::from_value(0.0, a.len() + b.len() - 1, pc);
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

pub mod rbindings;

/// A procedural macro to facilitate calling a Rust function from R.
pub use roxido_macro::roxido;

pub use rbindings::SEXP;

use rbindings::*;
use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr};
use std::marker::PhantomData;

static TOO_LONG: &str = "Could not fit usize into i32";

pub struct R;

#[repr(C)]
pub struct RObject<RType = RAnyType, RMode = RUnknown> {
    pub sexprec: SEXPREC,
    rtype: PhantomData<(RType, RMode)>,
}

pub struct Pc {
    counter: std::cell::RefCell<i32>,
}

pub struct RAnyType;

pub struct RUnknown;

pub struct RScalar;

pub struct RVector;

pub struct RMatrix;

pub struct RArray;

pub struct RCharacter;

pub struct RList;

pub struct RDataFrame;

pub struct RFunction;

pub struct RExternalPtr;

pub struct RSymbol;

pub struct RError;

pub trait RHasLength {}
impl RHasLength for RScalar {}
impl RHasLength for RVector {}
impl RHasLength for RMatrix {}
impl RHasLength for RArray {}
impl RHasLength for RList {}

pub trait RAtomic {}
impl RAtomic for RScalar {}
impl RAtomic for RVector {}
impl RAtomic for RMatrix {}
impl RAtomic for RArray {}

pub trait ROneDimensional {}
impl ROneDimensional for RVector {}
impl ROneDimensional for RList {}

impl Default for Pc {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Pc {
    fn drop(&mut self) {
        let count = self.counter.borrow();
        if *self.counter.borrow() > 0 {
            unsafe { Rf_unprotect(*count) };
        }
    }
}

impl Pc {
    /// Allocate a new protection counter.
    ///
    /// Functions defined with the `roxido` macro already have an instance of this structure named
    /// `pc`, so this function is generally not needed.
    ///
    pub fn new() -> Self {
        Self { counter: 0.into() }
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn protect(&self, sexp: SEXP) -> SEXP {
        unsafe { Rf_protect(sexp) };
        let mut counter = self.counter.borrow_mut();
        *counter += 1;
        sexp
    }

    /// This is an implementation detail and *should not* be called directly!
    #[doc(hidden)]
    pub fn transmute_sexp<RTypeTo, RModeTo>(&self, sexp: SEXP) -> &RObject<RTypeTo, RModeTo> {
        unsafe { &*sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    /// This is an implementation detail and *should not* be called directly!
    #[doc(hidden)]
    pub fn transmute_sexp_mut<'a, RTypeTo, RModeTo>(
        &self,
        sexp: SEXP,
    ) -> &'a mut RObject<RTypeTo, RModeTo> {
        unsafe { &mut *sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    /// This is an implementation detail and *should not* be called directly!
    #[doc(hidden)]
    pub fn transmute_sexp_static<RTypeTo, RModeTo>(
        sexp: SEXP,
    ) -> &'static RObject<RTypeTo, RModeTo> {
        unsafe { &*sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }
}

impl R {
    /// Returns an R NULL value.
    pub fn null() -> &'static RObject {
        Pc::transmute_sexp_static(unsafe { R_NilValue })
    }

    /// Returns an R NA value for storage mode "double".
    pub fn na_f64() -> f64 {
        unsafe { R_NaReal }
    }

    /// Returns an R NA value for storage mode "integer".
    pub fn na_i32() -> i32 {
        unsafe { R_NaInt }
    }

    /// Returns an R NA value for storage mode "logical".
    pub fn na_bool() -> i32 {
        unsafe { R_NaInt }
    }

    /// Returns an R NaN value.
    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    /// Returns an R Inf value.
    pub fn positive_infinity() -> f64 {
        unsafe { R_PosInf }
    }

    /// Returns an R -Inf value.
    pub fn negative_infinity() -> f64 {
        unsafe { R_NegInf }
    }

    /// Checks if an f64 can be interpreted as an R NA value.
    pub fn is_na_f64(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    /// Checks if an i32 can be interpreted as an R NA value.
    pub fn is_na_i32(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    /// Checks if a bool can be interpreted as an R NA value.
    pub fn is_na_bool(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    /// Checks if an f64 can be interpreted as an R NaN value.
    pub fn is_nan(x: f64) -> bool {
        unsafe { R_IsNaN(x) != 0 }
    }

    /// Checks if an f64 would be considered finite in R.
    pub fn is_finite(x: f64) -> bool {
        unsafe { R_finite(x) != 0 }
    }

    /// Checks if an f64 would be considered finite in R.
    pub fn is_positive_infinity(x: f64) -> bool {
        unsafe { x == R_PosInf }
    }

    /// Checks if an f64 would be considered finite in R.
    pub fn is_negative_infinity(x: f64) -> bool {
        unsafe { x == R_NegInf }
    }

    /// Generate random bytes using R's RNG.
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

    /// Flush the R console.
    pub fn flush_console() {
        unsafe { R_FlushConsole() };
    }

    /// Check to see if the user has attempted to interrupt the execution.
    pub fn check_user_interrupt() -> bool {
        extern "C" fn check_interrupt_fn(_: *mut c_void) {
            unsafe { R_CheckUserInterrupt() };
        }
        unsafe { R_ToplevelExec(Some(check_interrupt_fn), std::ptr::null_mut()) == 0 }
    }
}

impl<RType, RMode> RObject<RType, RMode> {
    pub fn sexp(&self) -> SEXP {
        self as *const RObject<RType, RMode> as SEXP
    }

    fn transmute_sexp<RTypeTo, RModeTo>(&self, sexp: SEXP) -> &RObject<RTypeTo, RModeTo> {
        unsafe { &*sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    fn transmute_sexp_mut<'a, RTypeTo, RModeTo>(
        &mut self,
        sexp: SEXP,
    ) -> &'a mut RObject<RTypeTo, RModeTo> {
        unsafe { &mut *sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    fn transmute<RTypeTo, RModeTo>(&self) -> &RObject<RTypeTo, RModeTo> {
        unsafe { std::mem::transmute::<&Self, &RObject<RTypeTo, RModeTo>>(self) }
    }

    fn transmute_mut<RTypeTo, RModeTo>(&mut self) -> &mut RObject<RTypeTo, RModeTo> {
        unsafe { std::mem::transmute::<&mut Self, &mut RObject<RTypeTo, RModeTo>>(self) }
    }

    /// Duplicate an object.
    ///
    /// Multiple symbols may be bound to the same object, so if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy first.
    /// E.g, call this method on arguments pass via `.Call` before modifying them.
    ///
    #[allow(clippy::mut_from_ref)]
    pub fn clone<'a>(&self, pc: &'a Pc) -> &'a mut RObject<RType, RMode> {
        let sexp = pc.protect(unsafe { Rf_duplicate(self.sexp()) });
        pc.transmute_sexp_mut(sexp)
    }

    /// Recharacterize an RObject<RType, RMode> as an RObject (i.e., an RObject<RAnyType, RUnknown>).
    pub fn as_unknown(&self) -> &RObject {
        self.transmute()
    }

    /// Returns the result of the is_null method, but as an Option value.
    pub fn as_option(&self) -> Option<&Self> {
        if self.is_null() {
            None
        } else {
            Some(self)
        }
    }

    /// Attempts to recharacterize the RObject as a scalar (i.e., a vector of length 1).
    pub fn as_scalar(&self) -> Result<&RObject<RScalar, RMode>, &'static str> {
        let s = self.as_vector()?;
        if s.is_scalar() {
            Ok(self.transmute())
        } else {
            Err("Not a scalar")
        }
    }

    pub fn as_scalar_mut(&mut self) -> Result<&mut RObject<RScalar, RMode>, &'static str> {
        let s = self.as_vector()?;
        if s.is_scalar() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a scalar")
        }
    }

    pub fn as_vector(&self) -> Result<&RObject<RVector, RMode>, &'static str> {
        if self.is_vector() {
            Ok(self.transmute())
        } else {
            Err("Not a vector")
        }
    }

    pub fn as_vector_mut(&mut self) -> Result<&mut RObject<RVector, RMode>, &'static str> {
        if self.is_vector() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RMatrix>`.
    /// Checks using R's `Rf_isMatrix` function.
    pub fn as_matrix(&self) -> Result<&RObject<RMatrix, RMode>, &'static str> {
        if self.is_matrix() {
            Ok(self.transmute())
        } else {
            Err("Not a matrix")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RMatrix>`.
    /// Checks using R's `Rf_isMatrix` function.
    pub fn as_matrix_mut(&mut self) -> Result<&mut RObject<RMatrix, RMode>, &'static str> {
        if self.is_matrix() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a matrix")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RArray>`.
    /// Checks using R's `Rf_isArray` function.
    pub fn as_array(&self) -> Result<&RObject<RArray, RMode>, &'static str> {
        if self.is_array() {
            Ok(self.transmute())
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RArray>`.
    /// Checks using R's `Rf_isArray` function.
    pub fn as_array_mut(&mut self) -> Result<&mut RObject<RArray, RMode>, &'static str> {
        if self.is_array() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RVector, RList>`.
    /// Checks using R's `Rf_isVectorList` function.
    pub fn as_list(&self) -> Result<&RObject<RList>, &'static str> {
        if self.is_list() {
            Ok(self.transmute())
        } else {
            Err("Not a list")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RVector, RList>`.
    /// Checks using R's `Rf_isVectorList` function.
    pub fn as_list_mut(&mut self) -> Result<&mut RObject<RList>, &'static str> {
        if self.is_list() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a list")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RVector, RDataFrame>`.
    /// Checks using R's `Rf_isFrame` function.
    pub fn as_data_frame(&self) -> Result<&RObject<RList, RDataFrame>, &'static str> {
        if self.is_data_frame() {
            Ok(self.transmute())
        } else {
            Err("Not a data frame")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RVector, RDataFrame>`.
    /// Checks using R's `Rf_isFrame` function.
    pub fn as_data_frame_mut(&mut self) -> Result<&mut RObject<RList, RDataFrame>, &'static str> {
        if self.is_data_frame() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a data frame")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RFunction>`.
    /// Checks using R's `Rf_isFunction` function.
    pub fn as_function(&self) -> Result<&RObject<RFunction>, &'static str> {
        if self.is_function() {
            Ok(self.transmute())
        } else {
            Err("Not a function")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RFunction>`.
    /// Checks using R's `Rf_isFunction` function.
    pub fn as_function_mut(&mut self) -> Result<&mut RObject<RFunction>, &'static str> {
        if self.is_function() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a function")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RExternalPtr>`.
    /// Uses the SEXP type to determine if this is possible.
    pub fn as_external_ptr(&self) -> Result<&RObject<RExternalPtr>, &'static str> {
        if self.is_external_ptr() {
            Ok(self.transmute())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RExternalPtr>`.
    /// Uses the SEXP type to determine if this is possible.
    pub fn as_external_ptr_mut(&mut self) -> Result<&mut RObject<RExternalPtr>, &'static str> {
        if self.is_external_ptr() {
            Ok(self.transmute_mut())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RSymbol>`.
    /// Uses the SEXP type to determine if this is possible.
    pub fn as_symbol(&self) -> Result<&RObject<RSymbol>, &'static str> {
        if self.is_symbol() {
            Ok(self.transmute())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if appropriate to characterize as an `RObject<RSymbol>`.
    /// Uses the SEXP type to determine if this is possible.
    pub fn as_symbol_mut(&mut self) -> Result<&mut RObject<RSymbol>, &'static str> {
        if self.is_symbol() {
            Ok(self.transmute_mut())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if RObject can be interpreted as the NULL value in R.
    pub fn is_null(&self) -> bool {
        unsafe { Rf_isNull(self.sexp()) != 0 }
    }

    pub fn is_vector(&self) -> bool {
        unsafe { Rf_isVectorAtomic(self.sexp()) != 0 }
    }

    pub fn is_matrix(&self) -> bool {
        unsafe { Rf_isMatrix(self.sexp()) != 0 }
    }

    pub fn is_array(&self) -> bool {
        unsafe { Rf_isArray(self.sexp()) != 0 }
    }

    pub fn is_list(&self) -> bool {
        unsafe { Rf_isVectorList(self.sexp()) != 0 }
    }

    pub fn is_data_frame(&self) -> bool {
        unsafe { Rf_isFrame(self.sexp()) != 0 }
    }

    pub fn is_function(&self) -> bool {
        unsafe { Rf_isFunction(self.sexp()) != 0 }
    }

    pub fn is_external_ptr(&self) -> bool {
        unsafe { TYPEOF(self.sexp()) == EXTPTRSXP as i32 }
    }

    pub fn is_symbol(&self) -> bool {
        unsafe { TYPEOF(self.sexp()) == SYMSXP as i32 }
    }

    /// Set the class or classes of the data for an RObject.
    pub fn set_class(&mut self, names: &RObject<RVector, RCharacter>) {
        unsafe {
            Rf_classgets(self.sexp(), names.sexp());
        }
    }

    /// Set an attribute.
    pub fn set_attribute<RTypeValue, RModeValue>(
        &mut self,
        which: &RObject<RSymbol>,
        value: &RObject<RTypeValue, RModeValue>,
    ) {
        unsafe {
            Rf_setAttrib(self.sexp(), which.sexp(), value.sexp());
        }
    }

    /// Get the class or classes of the data in an RObject.
    pub fn get_class(&self) -> &RObject<RVector, RCharacter> {
        self.transmute_sexp(unsafe {
            Rf_getAttrib(self.sexp(), RObject::<RSymbol>::class().sexp())
        })
    }

    /// Get an attribute.
    pub fn get_attribute(&self, which: &RObject<RSymbol>) -> &RObject {
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), which.sexp()) })
    }
}

impl RObject<RError> {
    /// Define a new error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use `stop!`.
    ///
    pub fn new<'a>(message: &str, pc: &'a Pc) -> &'a mut Self {
        let list = RObject::<RList>::with_names(["message", "calls"], pc);
        let _ = list.set(0, message.to_r(pc));
        let _ = list.set(1, R::null());
        list.set_class(["error", "condition"].to_r(pc));
        list.transmute_mut()
    }
}

impl RObject<RSymbol> {
    /// Define a new symbol.
    #[allow(clippy::mut_from_ref)]
    pub fn new<'a>(x: &str, pc: &'a Pc) -> &'a mut Self {
        let sexp = pc.protect(unsafe {
            Rf_mkCharLenCE(
                x.as_ptr() as *const c_char,
                x.len().try_into().stop_str(TOO_LONG),
                cetype_t_CE_UTF8,
            )
        });
        let sexp = pc.protect(unsafe { Rf_installChar(sexp) });
        pc.transmute_sexp_mut(sexp)
    }

    /// Get R's "dim" symbol.
    pub fn dim() -> &'static Self {
        Pc::transmute_sexp_static(unsafe { R_DimSymbol })
    }

    /// Get R's "names" symbol.
    pub fn names() -> &'static Self {
        Pc::transmute_sexp_static(unsafe { R_NamesSymbol })
    }

    /// Get R's "rownames" symbol.
    pub fn rownames() -> &'static Self {
        Pc::transmute_sexp_static(unsafe { R_RowNamesSymbol })
    }

    /// Get R's "dimnames" symbol.
    pub fn dimnames() -> &'static Self {
        Pc::transmute_sexp_static(unsafe { R_DimNamesSymbol })
    }

    /// Get R's "class" symbol.
    pub fn class() -> &'static Self {
        Pc::transmute_sexp_static(unsafe { R_ClassSymbol })
    }
}

impl<RType: RHasLength, RMode> RObject<RType, RMode> {
    /// Returns the length of the RObject.
    pub fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.sexp()) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    /// Checks to see if the RObject is empty.
    pub fn is_empty(&self) -> bool {
        unsafe { Rf_xlength(self.sexp()) == 0 }
    }

    /// Checks to see if the RObject is a scalar (has a length of 1).
    pub fn is_scalar(&self) -> bool {
        unsafe { Rf_xlength(self.sexp()) == 1 }
    }
}

impl<RType: RAtomic + RHasLength, RMode> RObject<RType, RMode> {
    fn slice_engine<U>(&self, data: *mut U) -> &[U] {
        unsafe { std::slice::from_raw_parts_mut(data, self.len()) }
    }

    fn slice_mut_engine<U>(&mut self, data: *mut U) -> &mut [U] {
        unsafe { std::slice::from_raw_parts_mut(data, self.len()) }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_f64(&self) -> Result<&RObject<RType, f64>, &'static str> {
        if self.is_f64() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'double'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_f64_mut(&mut self) -> Result<&mut RObject<RType, f64>, &'static str> {
        if self.is_f64() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'double'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_f64(&self) -> bool {
        unsafe { Rf_isReal(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_f64<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, f64> {
        if self.is_f64() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), REALSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_f64_mut(&mut self, pc: &Pc) -> &mut RObject<RType, f64> {
        if self.is_f64() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), REALSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_i32(&self) -> Result<&RObject<RType, i32>, &'static str> {
        if self.is_i32() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'integer'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_i32_mut(&mut self) -> Result<&mut RObject<RType, i32>, &'static str> {
        if self.is_i32() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'integer'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_i32(&self) -> bool {
        unsafe { Rf_isInteger(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_i32<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, i32> {
        if self.is_i32() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), INTSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_i32_mut(&mut self, pc: &Pc) -> &mut RObject<RType, i32> {
        if self.is_i32() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), INTSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_u8(&self) -> Result<&RObject<RType, u8>, &'static str> {
        if self.is_u8() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'raw'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_u8_mut(&mut self) -> Result<&mut RObject<RType, u8>, &'static str> {
        if self.is_u8() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'raw'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_u8(&self) -> bool {
        unsafe { TYPEOF(self.sexp()) == RAWSXP as i32 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_u8<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, u8> {
        if self.is_u8() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), RAWSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_u8_mut(&mut self, pc: &Pc) -> &mut RObject<RType, u8> {
        if self.is_u8() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), RAWSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_bool(&self) -> Result<&RObject<RType, bool>, &'static str> {
        if self.is_bool() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'logical'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_bool_mut(&mut self) -> Result<&mut RObject<RType, bool>, &'static str> {
        if self.is_bool() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'logical'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_bool(&self) -> bool {
        unsafe { Rf_isLogical(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_bool<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, bool> {
        if self.is_bool() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), LGLSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_bool_mut(&mut self, pc: &Pc) -> &mut RObject<RType, bool> {
        if self.is_bool() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), LGLSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_character(&self) -> Result<&RObject<RType, RCharacter>, &'static str> {
        if self.is_character() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'character'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn as_character_mut(&mut self) -> Result<&mut RObject<RType, RCharacter>, &'static str> {
        if self.is_character() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'character'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_character(&self) -> bool {
        unsafe { Rf_isString(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_character<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, RCharacter> {
        if self.is_character() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), STRSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_character_mut(&mut self, pc: &Pc) -> &mut RObject<RType, RCharacter> {
        if self.is_character() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), STRSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }
}

impl<RMode> RObject<RVector, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn new_engine(code: u32, length: usize, pc: &Pc) -> &mut Self {
        let sexp =
            pc.protect(unsafe { Rf_allocVector(code, length.try_into().stop_str(TOO_LONG)) });
        pc.transmute_sexp_mut(sexp)
    }
}

macro_rules! rslice {
    ($tipe:ty, $tipe2:ty, $code:expr) => {
        impl<RType: RAtomic + RHasLength> RObject<RType, $tipe> {
            /// Returns a slice of the data structure.
            pub fn slice(&self) -> &[$tipe2] {
                self.slice_engine(unsafe { $code(self.sexp()) })
            }

            /// Returns a slice of the data structure.
            pub fn slice_mut(&mut self) -> &mut [$tipe2] {
                self.slice_mut_engine(unsafe { $code(self.sexp()) })
            }
        }
    };
}

rslice!(f64, f64, REAL);
rslice!(i32, i32, INTEGER);
rslice!(u8, u8, RAW);
rslice!(bool, i32, LOGICAL);

impl<RType> RObject<RArray, RType> {
    #[allow(clippy::mut_from_ref)]
    fn new_engine<'a>(code: u32, dim: &[usize], pc: &'a Pc) -> &'a mut RObject<RArray, RType> {
        let d = dim.to_r(pc);
        pc.transmute_sexp_mut(pc.protect(unsafe { Rf_allocArray(code, d.sexp()) }))
    }

    /// Returns the dimensions of the RArray.
    pub fn dim(&self) -> Vec<usize> {
        let d =
            self.transmute_sexp::<RVector, i32>(unsafe { Rf_getAttrib(self.sexp(), R_DimSymbol) });
        d.slice().iter().map(|&x| x.try_into().unwrap()).collect()
    }

    // Create a new vector from a matrix.
    /// Convert an RArray to a Vector.
    pub fn to_vector(&mut self) -> &mut RObject<RVector, RType> {
        unsafe { Rf_setAttrib(self.sexp(), R_DimSymbol, R_NilValue) };
        self.transmute_mut()
    }
}

macro_rules! rarray {
    ($tipe:ty, $code:expr) => {
        impl RObject<RArray, $tipe> {
            pub fn new<'a>(dim: &[usize], pc: &'a Pc) -> &'a mut Self {
                Self::new_engine($code, dim, pc)
            }
        }
    };
}

rarray!(f64, REALSXP);
rarray!(i32, INTSXP);
rarray!(u8, RAWSXP);
rarray!(bool, LGLSXP);
rarray!(RCharacter, STRSXP);

impl RObject<RFunction> {
    fn eval(expression: SEXP, pc: &Pc) -> Result<&RObject, i32> {
        let expression = pc.protect(expression);
        let mut p_out_error: i32 = 0;
        let sexp = pc.protect(unsafe {
            R_tryEval(expression, R_GetCurrentEnv(), &mut p_out_error as *mut i32)
        });
        match p_out_error {
            0 => Ok(pc.transmute_sexp(sexp)),
            e => Err(e),
        }
    }

    /// Evaluate a function with 0 parameters.
    pub fn call0<'a>(&self, pc: &'a Pc) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang1(self.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 1 parameter.
    pub fn call1<'a, T1, M1>(
        &self,
        arg1: &RObject<T1, M1>,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang2(self.sexp(), arg1.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 2 parameters.
    pub fn call2<'a, T1, M1, T2, M2>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang3(self.sexp(), arg1.sexp(), arg2.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 3 parameters.
    pub fn call3<'a, T1, M1, T2, M2, T3, M3>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T3, M3>,
        pc: &'a Pc,
    ) -> Result<&'a RObject, i32> {
        let expression = unsafe { Rf_lang4(self.sexp(), arg1.sexp(), arg2.sexp(), arg3.sexp()) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 4 parameters.
    pub fn call4<'a, T1, M1, T2, M2, T3, M3, T4, M4>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T3, M3>,
        arg4: &RObject<T4, M4>,
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
    pub fn call5<'a, T1, M1, T2, M2, T3, M3, T4, M4, T5, M5>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T3, M3>,
        arg4: &RObject<T4, M4>,
        arg5: &RObject<T5, M5>,
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

impl<RMode> RObject<RScalar, RMode> {
    /// Check if appropriate to characterize as an f64.
    pub fn f64(&self) -> f64 {
        unsafe { Rf_asReal(self.sexp()) }
    }

    /// Check if appropriate to characterize as an i32.
    pub fn i32(&self) -> Result<i32, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("i32 equals R's NA for integers")
            } else {
                Ok(x)
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if y > f64::from(i32::MAX) {
                Err("Greater than maximum integer value")
            } else if y < f64::from(i32::MIN) {
                Err("Less than minimum integer value")
            } else if y == f64::from(i32::MIN) {
                Err("Equals R's NA for integers")
            } else if y.is_nan() {
                Err("Equals R's NaN")
            } else {
                Ok(y.round() as i32)
            }
        } else if self.is_u8() {
            Ok(unsafe { Rf_asInteger(self.sexp()) })
        } else if self.is_bool() {
            let y = unsafe { Rf_asLogical(self.sexp()) };
            if y == i32::MIN {
                Err("Equals R's NA for logical")
            } else {
                Ok(y)
            }
        } else {
            Err("Unsupported R type")
        }
    }

    /// Check if appropriate to characterize as a usize.
    pub fn usize(&self) -> Result<usize, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers")
            } else if x < 0 {
                Err("Negative value not expected")
            } else {
                usize::try_from(x).map_err(|_| "Cannot convert to usize")
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if y < 0.0 {
                Err("Negative value not expected")
            } else {
                let z = y as usize;
                if z as f64 == y {
                    Ok(z)
                } else {
                    Err("Cannot convert to usize")
                }
            }
        } else if self.is_u8() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            usize::try_from(x).map_err(|_| "Cannot convert to usize")
        } else if self.is_bool() {
            let x = unsafe { Rf_asLogical(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for logical")
            } else {
                usize::try_from(x).map_err(|_| "Cannot convert to usize")
            }
        } else {
            Err("Unsupported R type")
        }
    }

    /// Check if appropriate to characterize as a u8.
    pub fn u8(&self) -> Result<u8, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            u8::try_from(x).map_err(|_| "Cannot convert to u8")
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if y < 0.0 {
                Err("Negative value not expected")
            } else {
                let z = y as u8;
                if z as f64 == y {
                    Ok(z)
                } else {
                    Err("Cannot convert to u8")
                }
            }
        } else if self.is_u8() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            u8::try_from(x).map_err(|_| "Cannot convert to u8")
        } else if self.is_bool() {
            let x = unsafe { Rf_asLogical(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for logical")
            } else {
                u8::try_from(x).map_err(|_| "Cannot convert to u8")
            }
        } else {
            Err("Unsupported R type")
        }
    }

    /// Check if appropriate to characterize as a bool.
    pub fn bool(&self) -> Result<bool, &'static str> {
        if self.is_i32() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers")
            } else {
                Ok(x != 0)
            }
        } else if self.is_f64() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if R::is_na_f64(y) {
                Err("Equals R's NA for doubles")
            } else if R::is_nan(y) {
                Err("Equals R's NaN")
            } else {
                Ok(y != 0.0)
            }
        } else if self.is_u8() {
            Ok(unsafe { Rf_asInteger(self.sexp()) } != 0)
        } else if self.is_bool() {
            let y = unsafe { Rf_asLogical(self.sexp()) };
            if y == i32::MIN {
                Err("Equals R's NA for logical")
            } else {
                Ok(y != 0)
            }
        } else {
            Err("Unsupported R type")
        }
    }

    /// Check if appropriate to characterize as a str reference.
    pub fn str<'a>(&self, pc: &'a Pc) -> &'a str {
        let s: &RObject<RVector, RCharacter> = self.to_character(pc).transmute();
        s.get(0).unwrap()
    }

    /// Check if RObject can be interpreted as an NA value in R.
    pub fn is_na(&self) -> bool {
        if self.is_f64() {
            unsafe { R_IsNA(Rf_asReal(self.sexp())) != 0 }
        } else if self.is_i32() {
            unsafe { Rf_asInteger(self.sexp()) == R::na_i32() }
        } else if self.is_bool() {
            unsafe { Rf_asLogical(self.sexp()) == R::na_bool() }
        } else if self.is_character() {
            unsafe { Rf_asChar(self.sexp()) == R_NaString }
        } else {
            false
        }
    }

    /// Check if RObject can be interpreted as an NaN value in R.
    pub fn is_nan(&self) -> bool {
        if self.is_f64() {
            unsafe { R_IsNaN(Rf_asReal(self.sexp())) != 0 }
        } else {
            false
        }
    }

    pub fn is_finite(&self) -> bool {
        if self.is_f64() {
            unsafe { R_finite(Rf_asReal(self.sexp())) != 0 }
        } else {
            false
        }
    }

    pub fn is_positive_infinity(&self) -> bool {
        if self.is_f64() {
            unsafe { Rf_asReal(self.sexp()) == R_PosInf }
        } else {
            false
        }
    }

    pub fn is_negative_infinity(&self) -> bool {
        if self.is_f64() {
            unsafe { Rf_asReal(self.sexp()) == R_NegInf }
        } else {
            false
        }
    }
}

macro_rules! rscalar {
    ($tipe:ty, $tipe2:ty, $code:expr, $code2:expr, $code3:expr) => {
        impl RObject<RScalar, $tipe> {
            #[allow(clippy::mut_from_ref)]
            pub fn from_value(value: $tipe2, pc: &Pc) -> &mut Self {
                pc.transmute_sexp_mut(pc.protect(unsafe { $code(value) }))
            }

            /// Get the value at a certain index in an $tipe RVector.
            pub fn get(&self) -> $tipe2 {
                unsafe { $code2(self.sexp(), 0) }
            }

            /// Set the value at a certain index in an $tipe RVector.
            pub fn set(&mut self, value: $tipe2) {
                unsafe { $code3(self.sexp(), 0, value) }
            }
        }
    };
}

rscalar!(f64, f64, Rf_ScalarReal, REAL_ELT, SET_REAL_ELT);
rscalar!(i32, i32, Rf_ScalarInteger, INTEGER_ELT, SET_INTEGER_ELT);
rscalar!(u8, u8, Rf_ScalarRaw, RAW_ELT, SET_RAW_ELT);
rscalar!(bool, i32, Rf_ScalarLogical, LOGICAL_ELT, SET_LOGICAL_ELT);

impl RObject<RScalar, bool> {
    /// Get the value at a certain index in a logical RVector.
    pub fn get_bool(&self) -> bool {
        unsafe { LOGICAL_ELT(self.sexp(), 0) == Rboolean_TRUE as i32 }
    }

    /// Set the value at a certain index in a logical RVector.
    pub fn set_bool(&mut self, value: bool) {
        unsafe {
            SET_LOGICAL_ELT(
                self.sexp(),
                0,
                if value {
                    Rboolean_TRUE as i32
                } else {
                    Rboolean_FALSE as i32
                },
            )
        }
    }
}

impl RObject<RScalar, RCharacter> {
    #[allow(clippy::mut_from_ref)]
    pub fn from_value<'a>(value: &str, pc: &'a Pc) -> &'a mut Self {
        let sexp = unsafe {
            Rf_ScalarString(Rf_mkCharLenCE(
                value.as_ptr() as *const c_char,
                value.len().try_into().stop_str(TOO_LONG),
                cetype_t_CE_UTF8,
            ))
        };
        pc.transmute_sexp_mut(pc.protect(sexp))
    }

    /// Get the value at a certain index in an $tipe RVector.
    pub fn get(&self) -> &str {
        self.transmute::<RVector, RCharacter>().get(0).unwrap()
    }

    /// Set the value at a certain index in an $tipe RVector.
    pub fn set(&mut self, value: &str) {
        let _ = self.transmute_mut::<RVector, RCharacter>().set(0, value);
    }
}

impl<RType: ROneDimensional + RHasLength, RMode> RObject<RType, RMode> {
    fn get_engine<T>(
        &self,
        index: usize,
        f: unsafe extern "C" fn(SEXP, isize) -> T,
    ) -> Result<T, &'static str> {
        if index < self.len() {
            Ok(unsafe { f(self.sexp(), index.try_into().unwrap()) })
        } else {
            Err("Index out of bounds")
        }
    }

    fn get_mut_engine<T>(
        &mut self,
        index: usize,
        f: unsafe extern "C" fn(SEXP, isize) -> T,
    ) -> Result<T, &'static str> {
        if index < self.len() {
            Ok(unsafe { f(self.sexp(), index.try_into().unwrap()) })
        } else {
            Err("Index out of bounds")
        }
    }

    /// Get names of values in a RVector.
    pub fn get_names(&self) -> &RObject<RVector, RCharacter> {
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), R_NamesSymbol) })
    }

    fn set_engine<T>(
        &mut self,
        index: usize,
        value: T,
        f: unsafe extern "C" fn(SEXP, isize, T),
    ) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { f(self.sexp(), index.try_into().unwrap(), value) }
            Ok(())
        } else {
            Err("Index out of bounds")
        }
    }

    /// Set names of values in a RVector.
    pub fn set_names(&mut self, names: &RObject<RVector, RCharacter>) -> Result<(), &'static str> {
        if unsafe { Rf_length(names.sexp()) != Rf_length(self.sexp()) } {
            return Err("Length of names is not correct");
        }
        unsafe {
            Rf_namesgets(self.sexp(), names.sexp());
        }
        Ok(())
    }
}

macro_rules! rvector {
    ($tipe:ty, $tipe2:ty, $code:expr, $code2:expr, $code3:expr) => {
        impl RObject<RVector, $tipe> {
            pub fn new(length: usize, pc: &Pc) -> &mut Self {
                Self::new_engine($code, length, pc)
            }

            pub fn from_value(value: $tipe2, length: usize, pc: &Pc) -> &mut Self {
                let result = Self::new(length, pc);
                let slice = result.slice_mut();
                slice.fill(value);
                result
            }

            pub fn from_slice<'a>(slice: &[$tipe2], pc: &'a Pc) -> &'a mut Self {
                let result = Self::new(slice.len(), pc);
                let slice2 = result.slice_mut();
                slice2.copy_from_slice(slice);
                result
            }

            pub fn from_iter<T>(iter: T, pc: &Pc) -> &mut Self
            where
                T: IntoIterator<Item = $tipe2> + ExactSizeIterator,
            {
                let result = Self::new(iter.len(), pc);
                let slice = result.slice_mut();
                for (s, d) in slice.iter_mut().zip(iter) {
                    *s = d;
                }
                result
            }

            /// Get the value at a certain index in an $tipe RVector.
            pub fn get(&self, index: usize) -> Result<$tipe2, &'static str> {
                self.get_engine(index, $code2)
            }

            /// Set the value at a certain index in an $tipe RVector.
            pub fn set(&mut self, index: usize, value: $tipe2) -> Result<(), &'static str> {
                self.set_engine(index, value, $code3)
            }
        }
    };
}

rvector!(f64, f64, REALSXP, REAL_ELT, SET_REAL_ELT);
rvector!(i32, i32, INTSXP, INTEGER_ELT, SET_INTEGER_ELT);
rvector!(u8, u8, RAWSXP, RAW_ELT, SET_RAW_ELT);
rvector!(bool, i32, LGLSXP, LOGICAL_ELT, SET_LOGICAL_ELT);

impl RObject<RVector, bool> {
    /// Get the value at a certain index in a logical RVector.
    pub fn get_bool(&self, index: usize) -> Result<bool, &'static str> {
        self.get_engine(index, LOGICAL_ELT).map(|x| x != 0)
    }

    /// Set the value at a certain index in a logical RVector.
    pub fn set_bool(&mut self, index: usize, value: bool) -> Result<(), &'static str> {
        let value = if value {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }
}

impl RObject<RVector, RCharacter> {
    pub fn new(length: usize, pc: &Pc) -> &mut Self {
        Self::new_engine(STRSXP, length, pc)
    }

    pub fn from_slice<'a>(slice: &[&str], pc: &'a Pc) -> &'a mut Self {
        let vector = RObject::<RVector, RCharacter>::new(slice.len(), pc);
        for (i, v) in slice.iter().enumerate() {
            let _ = vector.set(i, v);
        }
        vector
    }

    pub fn from_iter<'a, T>(iter: T, pc: &'a Pc) -> &'a mut Self
    where
        T: Iterator<Item = &'a str> + ExactSizeIterator,
    {
        let vector = RObject::<RVector, RCharacter>::new(iter.len(), pc);
        for (i, v) in iter.enumerate() {
            let _ = vector.set(i, v);
        }
        vector
    }

    /// Get the value at a certain index in a character RVector.
    pub fn get<'a>(&self, index: usize) -> Result<&'a str, &'static str> {
        match self.get_engine(index, STRING_ELT) {
            Ok(sexp) => {
                let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
                c_str.to_str().map_err(|_| "Not valid UTF8")
            }
            Err(e) => Err(e),
        }
    }

    /// Set the value at a certain index in a character RVector.
    pub fn set(&mut self, index: usize, value: &str) -> Result<(), &'static str> {
        let len = value.len().try_into().map_err(|_| TOO_LONG)?;
        let value =
            unsafe { Rf_mkCharLenCE(value.as_ptr() as *const c_char, len, cetype_t_CE_UTF8) };
        self.set_engine(index, value, SET_STRING_ELT)
    }

    /// Set the value at a certain index in a character RVector to NA.
    pub fn set_na(&mut self, index: usize) -> Result<(), &'static str> {
        self.set_engine(index, unsafe { R_NaString }, SET_STRING_ELT)
    }
}

impl<RMode> RObject<RMatrix, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn new_engine(code: u32, nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
        let sexp = pc.protect(unsafe {
            Rf_allocMatrix(
                code,
                nrow.try_into().stop_str(TOO_LONG),
                ncol.try_into().stop_str(TOO_LONG),
            )
        });
        pc.transmute_sexp_mut(sexp)
    }

    /// Returns the number of rows in the RMatrix.
    pub fn nrow(&self) -> usize {
        unsafe { Rf_nrows(self.sexp()).try_into().unwrap() }
    }

    /// Returns the number of columns in the RMatrix.
    pub fn ncol(&self) -> usize {
        unsafe { Rf_ncols(self.sexp()).try_into().unwrap() }
    }

    /// Returns the dimensions of the RMatrix.
    pub fn dim(&self) -> [usize; 2] {
        [self.nrow(), self.ncol()]
    }

    /// Transpose the matrix.
    #[allow(clippy::mut_from_ref)]
    pub fn transpose<'a>(&self, pc: &'a Pc) -> &'a mut RObject<RMatrix, RMode> {
        let transposed = self.clone(pc);
        let mut dim = transposed.dim();
        dim.swap(0, 1);
        transposed.set_attribute(RObject::<RSymbol>::dim(), dim.to_r(pc));
        unsafe { Rf_copyMatrix(transposed.sexp(), self.sexp(), Rboolean_TRUE) };
        transposed
    }

    /// Manipulates the matrix in place to be a vector by dropping the `dim` attribute.
    pub fn to_vector(&mut self) -> &mut RObject<RVector, RMode> {
        unsafe { Rf_setAttrib(self.sexp(), R_DimSymbol, R_NilValue) };
        self.transmute_mut()
    }

    /// Get the index of a value based on the row and column number.
    pub fn index(&self, (i, j): (usize, usize)) -> usize {
        let nrow = self.nrow();
        nrow * j + i
    }

    /// Get the dimnames of a matrix.
    pub fn get_dimnames(&self) -> &RObject<RVector, RCharacter> {
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), R_DimNamesSymbol) })
    }

    /// Set the dimnames of a matrix.
    pub fn set_dimnames(&mut self, dimnames: &RObject<RList>) -> Result<(), &'static str> {
        match dimnames.get(0) {
            Ok(rownames) => match rownames.as_vector() {
                Ok(rownames) => {
                    if rownames.len() != self.nrow() {
                        return Err("Row names do not match the number of rows");
                    }
                }
                Err(_) => {
                    return Err("Row names must be a character vector");
                }
            },
            Err(_) => return Err("No row names element found"),
        };
        match dimnames.get(1) {
            Ok(colnames) => match colnames.as_vector() {
                Ok(colnames) => {
                    if colnames.len() != self.ncol() {
                        return Err("Column names do not match the number of columns");
                    }
                }
                Err(_) => {
                    return Err("Column names must be a character vector");
                }
            },
            Err(_) => return Err("No column names element found"),
        };
        unsafe {
            Rf_dimnamesgets(self.sexp(), dimnames.sexp());
        }
        Ok(())
    }
}

macro_rules! rmatrix {
    ($tipe:ty, $tipe2:ty, $code:expr) => {
        impl RObject<RMatrix, $tipe> {
            pub fn new(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
                Self::new_engine($code, nrow, ncol, pc)
            }

            pub fn from_value(value: $tipe2, nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
                let result = Self::new(nrow, ncol, pc);
                let slice = result.slice_mut();
                slice.fill(value);
                result
            }

            pub fn from_slice<'a>(
                slice: &[$tipe2],
                nrow: usize,
                pc: &'a Pc,
            ) -> Result<&'a mut Self, &'static str> {
                if nrow == 0 && slice.len() == 0 {
                    return Ok(Self::new(0, 0, pc));
                }
                let ncol = slice.len() / nrow;
                if nrow * ncol != slice.len() {
                    return Err("Slice length is not divisible by 'nrow'");
                }
                let result = Self::new(nrow, ncol, pc);
                let slice2 = result.slice_mut();
                slice2.copy_from_slice(slice);
                Ok(result)
            }

            pub fn from_iter<T>(iter: T, nrow: usize, pc: &Pc) -> Result<&mut Self, &'static str>
            where
                T: IntoIterator<Item = $tipe2> + ExactSizeIterator,
            {
                if nrow == 0 && iter.len() == 0 {
                    return Ok(Self::new(0, 0, pc));
                }
                let ncol = iter.len() / nrow;
                if nrow * ncol != iter.len() {
                    return Err("Iterator length is not divisible by 'nrow'");
                }
                let result = Self::new(nrow, ncol, pc);
                let slice = result.slice_mut();
                for (s, d) in slice.iter_mut().zip(iter) {
                    *s = d;
                }
                Ok(result)
            }

            /// Get the value at a certain index in a double RMatrix.
            pub fn get(&self, index: (usize, usize)) -> Result<$tipe2, &'static str> {
                self.transmute::<RVector, $tipe>().get(self.index(index))
            }

            /// Set the value at a certain index in a double RMatrix.
            pub fn set(
                &mut self,
                index: (usize, usize),
                value: $tipe2,
            ) -> Result<(), &'static str> {
                let index = self.index(index);
                self.transmute_mut::<RVector, $tipe>().set(index, value)
            }
        }
    };
}

rmatrix!(f64, f64, REALSXP);
rmatrix!(i32, i32, INTSXP);
rmatrix!(u8, u8, RAWSXP);
rmatrix!(bool, i32, LGLSXP);

impl RObject<RMatrix, bool> {
    /// Get the value at a certain index in a logical RMatrix as an i32.
    pub fn get_bool(&self, index: (usize, usize)) -> Result<bool, &'static str> {
        self.transmute::<RVector, bool>()
            .get_bool(self.index(index))
    }

    /// Set the value at a certain index in a logical RMatrix an an i32.
    pub fn set_bool(&mut self, index: (usize, usize), value: bool) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, bool>().set_bool(index, value)
    }
}

impl RObject<RMatrix, RCharacter> {
    pub fn new(nrow: usize, ncol: usize, pc: &Pc) -> &mut Self {
        Self::new_engine(STRSXP, nrow, ncol, pc)
    }

    pub fn from_value<'a>(value: &str, nrow: usize, ncol: usize, pc: &'a Pc) -> &'a mut Self {
        let len = nrow * ncol;
        let vector = RObject::<RVector, RCharacter>::new(len, pc);
        for i in 0..len {
            let _ = vector.set(i, value);
        }
        vector.set_attribute(RObject::<RSymbol>::dim(), [nrow, ncol].to_r(pc));
        vector.transmute_mut::<RMatrix, RCharacter>()
    }

    pub fn from_slice<'a>(
        slice: &[&str],
        nrow: usize,
        pc: &'a Pc,
    ) -> Result<&'a mut Self, &'static str> {
        if nrow == 0 && slice.is_empty() {
            return Ok(Self::new(0, 0, pc));
        }
        let ncol = slice.len() / nrow;
        if nrow * ncol != slice.len() {
            return Err("Slice length is not divisible by 'nrow'");
        }
        let vector = RObject::<RVector, RCharacter>::new(slice.len(), pc);
        for (i, v) in slice.iter().enumerate() {
            let _ = vector.set(i, v);
        }
        vector.set_attribute(RObject::<RSymbol>::dim(), [nrow, ncol].to_r(pc));
        Ok(vector.transmute_mut::<RMatrix, RCharacter>())
    }

    pub fn from_iter<'a, T>(iter: T, nrow: usize, pc: &'a Pc) -> Result<&'a mut Self, &'static str>
    where
        T: Iterator<Item = &'a str> + ExactSizeIterator,
    {
        if nrow == 0 && iter.len() == 0 {
            return Ok(Self::new(0, 0, pc));
        }
        let ncol = iter.len() / nrow;
        if nrow * ncol != iter.len() {
            return Err("Iterator length is not divisible by 'nrow'");
        }
        let vector = RObject::<RVector, RCharacter>::new(iter.len(), pc);
        for (i, v) in iter.enumerate() {
            let _ = vector.set(i, v);
        }
        vector.set_attribute(RObject::<RSymbol>::dim(), [nrow, ncol].to_r(pc));
        Ok(vector.transmute_mut::<RMatrix, RCharacter>())
    }

    /// Get the value at a certain index in a character RMatrix.
    pub fn get(&self, index: (usize, usize)) -> Result<&str, &'static str> {
        self.transmute::<RVector, RCharacter>()
            .get(self.index(index))
    }

    /// Set the value at a certain index in a character RMatrix.
    pub fn set(&mut self, index: (usize, usize), value: &str) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, RCharacter>()
            .set(index, value)
    }
}

pub struct RListMap<'a, RMode> {
    unused_counter: usize,
    used: Vec<bool>,
    robj: &'a RObject<RList, RMode>,
    map: HashMap<&'a str, usize>,
}

impl<RMode> RListMap<'_, RMode> {
    /// Find an RObject in the list based on its name.
    pub fn get(&mut self, name: &str) -> Result<&RObject, String> {
        let Some(index) = self.map.get(name) else {
            return Err(format!("'{}' not found", name));
        };
        if !self.used[*index] {
            self.unused_counter -= 1;
            self.used[*index] = true;
        }
        Ok(self.robj.get(*index)?)
    }

    /// Check to see if every RObject in the map has been used.
    pub fn exhaustive(&self) -> Result<(), String> {
        if self.unused_counter != 0 {
            return Err(format!(
                "Unrecognized elements in list:\n    {}",
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

impl RObject<RList> {
    #[allow(clippy::mut_from_ref)]
    pub fn new(length: usize, pc: &Pc) -> &mut Self {
        let sexp =
            pc.protect(unsafe { Rf_allocVector(VECSXP, length.try_into().stop_str(TOO_LONG)) });
        pc.transmute_sexp_mut(sexp)
    }

    #[allow(clippy::mut_from_ref)]
    pub fn with_names<'a, const N: usize>(names: [&str; N], pc: &'a Pc) -> &'a mut Self {
        let result = Self::new(names.len(), pc);
        unsafe {
            Rf_namesgets(result.sexp(), names.to_r(pc).sexp());
        }
        result
    }
}

impl<RMode> RObject<RList, RMode> {
    /// Get the value at a certain index in a RList.
    pub fn get(&self, index: usize) -> Result<&RObject, &'static str> {
        self.get_engine(index, VECTOR_ELT)
            .map(|sexp| self.transmute_sexp(sexp))
    }

    /// Get the value at a certain index in a RList.
    pub fn get_mut(&mut self, index: usize) -> Result<&mut RObject, &'static str> {
        self.get_mut_engine(index, VECTOR_ELT)
            .map(|sexp| self.transmute_sexp_mut(sexp))
    }

    /// Get a value from the RList based on its key.
    pub fn get_by_key(&self, key: impl AsRef<str>) -> Result<&RObject, String> {
        let names = self.get_names();
        for i in 0..names.len() {
            if names.get(i).unwrap() == key.as_ref() {
                return Ok(self.get(i)?);
            }
        }
        Err(format!("Could not find '{}' in the list", key.as_ref()))
    }

    /// Get a value from the RList based on its key.
    pub fn get_mut_by_key(&mut self, key: impl AsRef<str>) -> Result<&mut RObject, String> {
        let names = self.get_names();
        for i in 0..names.len() {
            if names.get(i).unwrap() == key.as_ref() {
                return Ok(self.get_mut(i)?);
            }
        }
        Err(format!("Could not find '{}' in the list", key.as_ref()))
    }

    /// Convert the list into an [RListMap]
    ///
    /// This allows Rust HashMap methods to be used on the contents
    /// of the list, while still retaining the original list within
    /// the RListMap struct in the robj field.
    pub fn make_map(&self) -> RListMap<RMode> {
        let mut map = HashMap::new();
        let names = self.get_names();
        let len = names.len();
        for i in 0..len {
            map.insert(names.get(i).unwrap(), i);
        }
        RListMap {
            unused_counter: len,
            used: vec![false; len],
            robj: self,
            map,
        }
    }

    /// Set the value at a certain index in an RList.
    pub fn set<RTypeValue, RModeValue>(
        &mut self,
        index: usize,
        value: &RObject<RTypeValue, RModeValue>,
    ) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { SET_VECTOR_ELT(self.sexp(), index.try_into().unwrap(), value.sexp()) };
            Ok(())
        } else {
            Err("Index out of bounds")
        }
    }

    /// Convert an RList to an RDataFrame.
    pub fn to_data_frame<'a>(
        &'a mut self,
        names: &RObject<RVector, RCharacter>,
        rownames: &RObject<RVector, RCharacter>,
        pc: &Pc,
    ) -> Result<&'a mut RObject<RList, RDataFrame>, &'static str> {
        if names.len() != self.len() {
            return Err("Length of names is not correct");
        }
        let mut nrow = -1;
        for i in 0..self.len() {
            let x = self.get(i).unwrap();
            if !x.is_vector() {
                return Err("Expected an atomic vector... Have you set the list elements yet?");
            }
            let len = unsafe { Rf_xlength(x.sexp()) };
            if i == 0 {
                nrow = len;
            } else if len != nrow {
                return Err("Inconsistent number of rows among list elements");
            }
        }
        if rownames.len() != nrow as usize {
            return Err("Length of row names is not correct");
        }
        self.set_names(names)?;
        unsafe { Rf_setAttrib(self.sexp(), R_RowNamesSymbol, rownames.sexp()) };
        self.set_class(["data.frame"].to_r(pc));
        Ok(self.transmute_mut())
    }
}

impl RObject<RList, RDataFrame> {
    /// Get the row names of a RDataFrame.
    pub fn get_rownames(&self) -> &RObject<RVector, RCharacter> {
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), R_RowNamesSymbol) })
    }

    /// Set the row names of a RDataFrame.
    pub fn set_rownames(
        &mut self,
        rownames: &RObject<RVector, RCharacter>,
    ) -> Result<(), &'static str> {
        if unsafe { Rf_length(rownames.sexp()) != Rf_length(self.sexp()) } {
            return Err("Length of row names is not correct");
        }
        unsafe { Rf_setAttrib(self.sexp(), R_RowNamesSymbol, rownames.sexp()) };
        Ok(())
    }
}

impl RObject<RExternalPtr> {
    /// Move Rust object to an R external pointer.
    ///
    /// This *method* moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RObject::decode_val`].
    ///
    #[allow(clippy::mut_from_ref)]
    pub fn encode<'a, T>(x: T, tag: &str, pc: &'a Pc) -> &'a mut Self {
        Self::encode_full(x, tag.to_r(pc), true, pc)
    }

    /// Move Rust object to an R external pointer.
    ///
    /// This *method* moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RObject::decode_val`].
    ///
    #[allow(clippy::mut_from_ref)]
    pub fn encode_full<'a, T, RType, RMode>(
        x: T,
        tag: &RObject<RType, RMode>,
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
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if an external pointer is managed by R.
    pub fn is_managed_by_r(&self) -> bool {
        unsafe { Rf_getAttrib(self.sexp(), R_AtsignSymbol) == R_AtsignSymbol }
    }

    /// Move an R external pointer to a Rust object.
    ///
    /// This method moves an R external pointer created by [`RObject::external_ptr`] to a Rust object and Rust will then manage its memory.
    ///
    pub fn decode_val<T>(&self) -> Result<T, &'static str> {
        if self.is_managed_by_r() {
            return Err("External pointer is managed by R");
        }
        unsafe {
            let addr = R_ExternalPtrAddr(self.sexp());
            if addr.as_ref().is_none() {
                return Err("External pointer was already decoded by value");
            }
            R_ClearExternalPtr(self.sexp());
            Ok(*Box::from_raw(addr as *mut T))
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer.
    ///
    /// This method obtains a reference to a Rust object from an R external pointer created by [`RObject::external_ptr`].
    ///
    pub fn decode_ref<T>(&self) -> &T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a reference to a Rust object from an R external pointer created by [`RObject::external_ptr`].
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
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`RObject::external_ptr`].
    ///
    pub fn decode_mut<'a, T>(&mut self) -> &'a mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`RObject::external_ptr`].
    ///
    /// # Safety
    ///
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
            return Err("External pointer is managed by R");
        }
        unsafe {
            R_RegisterCFinalizerEx(self.sexp(), Some(func), 0);
            Ok(())
        }
    }

    /// Get tag for an R external pointer.
    ///
    /// This method gets the tag associated with an R external pointer, which was set by [`Self::external_ptr`].
    ///
    pub fn tag(&self) -> &RObject {
        self.transmute_sexp(unsafe { R_ExternalPtrTag(self.sexp()) })
    }
}

// Conversions

pub trait FromR<RType, RMode, U> {
    fn from_r(x: &RObject<RType, RMode>, pc: &Pc) -> Result<Self, U>
    where
        Self: Sized;
}

/// Trait for converting objects to RObjects.
pub trait ToR<'a, RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RType, RMode>;
}

/// Trait for converting iterators to RObjects.
pub trait ToR2<RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut RObject<RType, RMode>;
}

/// Trait for converting iterators to RObjects.
pub trait ToR3<RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut RObject<RType, RMode>;
}

// scalars
macro_rules! r_from_scalar {
    ($tipe:ty, $tipe2:ty, $code:expr) => {
        impl<'a> ToR<'a, RScalar, $tipe> for $tipe2 {
            fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RScalar, $tipe> {
                #[allow(clippy::redundant_closure_call)]
                RObject::<RScalar, $tipe>::from_value($code(*self), pc)
            }
        }
    };
}

r_from_scalar!(f64, f64, |x| x);
r_from_scalar!(i32, i32, |x| x);
r_from_scalar!(u8, u8, |x| x);
r_from_scalar!(bool, bool, |x: bool| if x {
    Rboolean_TRUE as i32
} else {
    Rboolean_FALSE as i32
});
r_from_scalar!(i32, usize, |x: usize| x.try_into().stop_str(TOO_LONG));

impl<'a> ToR<'a, RScalar, RCharacter> for &str {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RScalar, RCharacter> {
        RObject::<RScalar, RCharacter>::from_value(self, pc)
    }
}

// slices
macro_rules! r_from_slice {
    ($tipe:ty) => {
        impl<'a> ToR<'a, RVector, $tipe> for &[$tipe] {
            fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, $tipe> {
                RObject::<RVector, $tipe>::from_slice(self, pc)
            }
        }
    };
}

r_from_slice!(f64);
r_from_slice!(i32);
r_from_slice!(u8);

impl<'a> ToR<'a, RVector, bool> for &[bool] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, bool> {
        RObject::<RVector, bool>::from_iter(
            self.iter().map(|x| {
                if *x {
                    Rboolean_TRUE as i32
                } else {
                    Rboolean_FALSE as i32
                }
            }),
            pc,
        )
    }
}

impl<'a> ToR<'a, RVector, i32> for &[usize] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        RObject::<RVector, i32>::from_iter(self.iter().map(|x| (*x).try_into().unwrap()), pc)
    }
}

impl<'a> ToR<'a, RVector, RCharacter> for &[&str] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, RCharacter> {
        let result = RObject::<RVector, RCharacter>::new(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

// vectors
macro_rules! r_from_vector {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a> ToR<'a, RVector, $tipe> for Vec<$tipe2> {
            fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, $tipe> {
                (&self[..]).to_r(pc)
            }
        }
    };
}

r_from_vector!(f64, f64);
r_from_vector!(i32, i32);
r_from_vector!(u8, u8);
r_from_vector!(bool, bool);
r_from_vector!(i32, usize);
r_from_vector!(RCharacter, &str);

// arrays
macro_rules! r_from_array {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a, const N: usize> ToR<'a, RVector, $tipe> for [$tipe2; N] {
            fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, $tipe> {
                self.as_ref().to_r(pc)
            }
        }
    };
}

r_from_array!(f64, f64);
r_from_array!(i32, i32);
r_from_array!(u8, u8);
r_from_array!(bool, bool);
r_from_array!(i32, usize);
r_from_array!(RCharacter, &str);

macro_rules! r_from_iter2 {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a, T: IntoIterator<Item = &'a $tipe2> + ExactSizeIterator> ToR2<RVector, $tipe>
            for T
        {
            fn to_r(self, pc: &Pc) -> &mut RObject<RVector, $tipe> {
                let result = RObject::<RVector, $tipe>::new(self.len(), pc);
                let slice = result.slice_mut();
                for (to, from) in slice.iter_mut().zip(self) {
                    *to = *from;
                }
                result
            }
        }
    };
}

r_from_iter2!(f64, f64);
r_from_iter2!(i32, i32);
r_from_iter2!(u8, u8);

macro_rules! r_from_iter3 {
    ($tipe:ty, $tipe2:ty) => {
        impl<'a, T: IntoIterator<Item = $tipe2> + ExactSizeIterator> ToR3<RVector, $tipe> for T {
            fn to_r(self, pc: &Pc) -> &mut RObject<RVector, $tipe> {
                let result = RObject::<RVector, $tipe>::new(self.len(), pc);
                let slice = result.slice_mut();
                for (to, from) in slice.iter_mut().zip(self) {
                    *to = from;
                }
                result
            }
        }
    };
}

r_from_iter3!(f64, f64);
r_from_iter3!(i32, i32);
r_from_iter3!(u8, u8);

// &RObject and SEXP
impl<'a, RType, RMode> ToR<'a, RAnyType, RUnknown> for RObject<RType, RMode> {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject {
        pc.transmute_sexp_mut(self.sexp())
    }
}

impl<'a> ToR<'a, RAnyType, RUnknown> for SEXP {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RAnyType, RUnknown> {
        pc.transmute_sexp_mut(*self)
    }
}

impl<'a> ToR<'a, RAnyType, RUnknown> for () {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RAnyType, RUnknown> {
        pc.transmute_sexp_mut(unsafe { R_NilValue })
    }
}

/// Print to the R console.
///
/// This is an implementation detail and *should not* be called directly!
/// This returns `true` if the print statement swallowed a user interrupt.
/// R checks for user interrupt every 100 print statements.
/// See the `Rvprintf` function in `printutils.c` of R's source.
///
#[doc(hidden)]
pub fn _print(x: &str, use_stdout: bool) -> bool {
    #[repr(C)]
    struct DummyFat {
        len: usize,
        ptr: *const c_char,
        use_stdout: bool,
    }
    let mut y = DummyFat {
        len: x.len(),
        ptr: x.as_ptr() as *const c_char,
        use_stdout,
    };
    let y_ptr = &mut y as *mut DummyFat as *mut c_void;
    extern "C" fn print_fn(y_ptr: *mut c_void) {
        unsafe {
            let y_ptr = y_ptr as *mut DummyFat;
            if (*y_ptr).use_stdout {
                Rprintf(
                    b"%.*s\0".as_ptr() as *const c_char,
                    (*y_ptr).len,
                    (*y_ptr).ptr,
                );
            } else {
                REprintf(
                    b"%.*s\0".as_ptr() as *const c_char,
                    (*y_ptr).len,
                    (*y_ptr).ptr,
                );
            }
        }
    }
    unsafe { R_ToplevelExec(Some(print_fn), y_ptr) == 0 }
}

/// Just like Rust's usual `print!` macro, except output goes to the R console.
#[macro_export]
macro_rules! rprint {
    ($fmt_string:expr) => {
        _print(format!($fmt_string).as_str(), true)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        _print(format!($fmt_string, $($arg),*).as_str(), true)
    }
}

/// Just like Rust's usual `println!` macro, except output goes to the R console.
#[macro_export]
macro_rules! rprintln {
    () => {
        _print("\n", true)
    };
    ($fmt_string:expr) => {
        _print(format!(concat!($fmt_string,"\n")).as_str(), true)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        _print(format!(concat!($fmt_string,"\n"), $($arg),*).as_str(), true)
    }
}

/// Just like Rust's usual `eprint!` macro, except output goes to the R console.
#[macro_export]
macro_rules! reprint {
    ($fmt_string:expr) => {
        _print(format!($fmt_string).as_str(), false)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        _print(format!($fmt_string, $($arg),*).as_str(), false)
    }
}

/// Just like Rust's usual `eprintln!` macro, except output goes to the R console.
#[macro_export]
macro_rules! reprintln {
    () => {
        _print("\n", false)
    };
    ($fmt_string:expr) => {
        _print(format!(concat!($fmt_string,"\n")).as_str(), false)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        _print(format!(concat!($fmt_string,"\n"), $($arg),*).as_str(), false)
    }
}

use std::fmt::Display;

#[doc(hidden)]
pub struct RStopHelper(pub String);

/// Throw an R error.
#[macro_export]
#[allow(clippy::crate_in_macro_def)]
macro_rules! stop {
    () => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                panic!("Panic in stop!() due to RUST_BACKTRACE environment variable")
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
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable... ");
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
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable... ");
                msg.push_str(&format!($fmt_string, $($arg),*));
                panic!("{}", msg);
            },
            Err(_) => {
                std::panic::panic_any(RStopHelper(format!($fmt_string, $($arg),*)))
            }
        }
    }
}

pub trait UnwrapOrStop<T> {
    fn stop(self) -> T;
    fn stop_str(self, msg: &str) -> T;
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
pub extern "C" fn set_custom_panic_hook() -> SEXP {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        if panic_info.payload().downcast_ref::<RStopHelper>().is_none() {
            default_panic(panic_info);
        }
    }));
    unsafe { R_NilValue }
}
