//! Extension Framework for R using Rust

//#![allow(dead_code)]

// Helpful resources:
//   https://cran.r-project.org/doc/manuals/r-release/R-ints.html
//   https://svn.r-project.org/R/trunk/src/include/Rinternals.h
//   https://github.com/hadley/r-internals
//   https://www.tidyverse.org/blog/2019/05/resource-cleanup-in-c-and-the-r-api
//   https://github.com/wch/r-source

use crate::rbindings::*;

use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr};
use std::marker::PhantomData;

pub struct Pc {
    counter: std::cell::RefCell<i32>,
}

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

pub struct RAnyType;

pub struct RScalar;

pub struct RVector;

pub struct RMatrix;

pub struct RArray;

pub struct RFunction;

pub struct RExternalPtr;

pub struct RSymbol;

pub struct RUnknown;

pub struct RCharacter;

pub struct RList;

pub struct RDataFrame;

pub trait HasLength {}
impl HasLength for RScalar {}
impl HasLength for RVector {}
impl HasLength for RMatrix {}
impl HasLength for RArray {}

pub trait Atomic {}
impl Atomic for f64 {}
impl Atomic for i32 {}
impl Atomic for u8 {}
impl Atomic for bool {}
impl Atomic for RCharacter {}
impl Atomic for RUnknown {}

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
    fn protect(&self, sexp: SEXP) -> SEXP {
        unsafe { Rf_protect(sexp) };
        let mut counter = self.counter.borrow_mut();
        *counter += 1;
        sexp
    }

    pub fn transmute_sexp<RTypeTo, RModeTo>(&self, sexp: SEXP) -> &RObject<RTypeTo, RModeTo> {
        unsafe { &*sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    pub fn transmute_sexp_mut<'a, RTypeTo, RModeTo>(
        &self,
        sexp: SEXP,
    ) -> &'a mut RObject<RTypeTo, RModeTo> {
        unsafe { &mut *sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    pub fn transmute_sexp_static<RTypeTo, RModeTo>(
        sexp: SEXP,
    ) -> &'static RObject<RTypeTo, RModeTo> {
        unsafe { &*sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    /// Create a new scalar (i.e., a vector of length 1) of storage mode "double".
    #[allow(clippy::mut_from_ref)]
    pub fn new_scalar_double(&self, x: f64) -> &mut RObject<RScalar, f64> {
        let sexp = self.protect(unsafe { Rf_ScalarReal(x) });
        self.transmute_sexp_mut(sexp)
    }

    /// Create a new scalar (i.e., a vector of length 1) of storage mode "integer".
    #[allow(clippy::mut_from_ref)]
    pub fn new_scalar_integer(&self, x: i32) -> &mut RObject<RScalar, i32> {
        let sexp = self.protect(unsafe { Rf_ScalarInteger(x) });
        self.transmute_sexp_mut(sexp)
    }

    /// Create a new scalar (i.e., a vector of length 1) of storage mode "raw".
    #[allow(clippy::mut_from_ref)]
    pub fn new_scalar_raw(&self, x: u8) -> &mut RObject<RScalar, u8> {
        let sexp = self.protect(unsafe { Rf_ScalarRaw(x) });
        self.transmute_sexp_mut(sexp)
    }

    /// Create a new scalar (i.e., a vector of length 1) of storage mode "logical".
    #[allow(clippy::mut_from_ref)]
    pub fn new_scalar_logical(&self, x: bool) -> &mut RObject<RScalar, bool> {
        let sexp = self.protect(unsafe { Rf_ScalarLogical(if x { 1 } else { 0 }) });
        self.transmute_sexp_mut(sexp)
    }

    /// Create a new scalar (i.e., a vector of length 1) of storage mode "character".
    #[allow(clippy::mut_from_ref)]
    pub fn new_scalar_character(&self, x: &str) -> &mut RObject<RScalar, RCharacter> {
        let sexp = unsafe {
            Rf_ScalarString(Rf_mkCharLenCE(
                x.as_ptr() as *const c_char,
                x.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            ))
        };
        self.transmute_sexp_mut(self.protect(sexp))
    }

    fn new_vector<'a, RMode>(&self, code: u32, length: usize) -> &'a mut RObject<RVector, RMode> {
        let sexp = self.protect(unsafe { Rf_allocVector(code, length.try_into().unwrap()) });
        self.transmute_sexp_mut(sexp)
    }

    /// Create a new vector of storage mode "double".
    pub fn new_vector_double(&self, length: usize) -> &mut RObject<RVector, f64> {
        self.new_vector(REALSXP, length)
    }

    /// Create a new vector of type storage mode "integer".
    pub fn new_vector_integer(&self, length: usize) -> &mut RObject<RVector, i32> {
        self.new_vector(INTSXP, length)
    }

    /// Create a new vector of storage mode "raw".
    pub fn new_vector_raw(&self, length: usize) -> &mut RObject<RVector, u8> {
        self.new_vector(RAWSXP, length)
    }

    /// Create a new vector of storage mode "logical".
    pub fn new_vector_logical(&self, length: usize) -> &mut RObject<RVector, bool> {
        self.new_vector(LGLSXP, length)
    }

    /// Create a new vector of storage mode "character".
    pub fn new_vector_character(&self, length: usize) -> &mut RObject<RVector, RCharacter> {
        self.new_vector(STRSXP, length)
    }

    fn new_matrix<'a, RMode>(
        &self,
        code: u32,
        nrow: usize,
        ncol: usize,
    ) -> &'a mut RObject<RMatrix, RMode> {
        let sexp = self.protect(unsafe {
            Rf_allocMatrix(code, nrow.try_into().unwrap(), ncol.try_into().unwrap())
        });
        self.transmute_sexp_mut(sexp)
    }

    /// Create a new matrix of storage mode "double".
    pub fn new_matrix_double(&self, nrow: usize, ncol: usize) -> &mut RObject<RMatrix, f64> {
        self.new_matrix(REALSXP, nrow, ncol)
    }

    /// Create a new matrix of storage mode "integer".
    pub fn new_matrix_integer(&self, nrow: usize, ncol: usize) -> &mut RObject<RMatrix, i32> {
        self.new_matrix(INTSXP, nrow, ncol)
    }

    /// Create a new matrix of storage mode "raw".
    pub fn new_matrix_raw(&self, nrow: usize, ncol: usize) -> &mut RObject<RMatrix, u8> {
        self.new_matrix(RAWSXP, nrow, ncol)
    }

    /// Create a new matrix of storage mode "logical".
    pub fn new_matrix_logical(&self, nrow: usize, ncol: usize) -> &mut RObject<RMatrix, bool> {
        self.new_matrix(LGLSXP, nrow, ncol)
    }

    /// Create a new matrix of storage mode "character".
    pub fn new_matrix_character(
        &self,
        nrow: usize,
        ncol: usize,
    ) -> &mut RObject<RMatrix, RCharacter> {
        self.new_matrix(STRSXP, nrow, ncol)
    }

    #[allow(clippy::mut_from_ref)]
    fn new_array<T>(&self, code: u32, dim: &[usize]) -> &mut RObject<RArray, T> {
        let d = dim.iter().map(|x| i32::try_from(*x).unwrap()).to_r(self);
        self.transmute_sexp_mut(self.protect(unsafe { Rf_allocArray(code, d.sexp()) }))
    }

    /// Create a new array of storage mode "double".
    pub fn new_array_double(&self, dim: &[usize]) -> &mut RObject<RArray, f64> {
        self.new_array::<f64>(REALSXP, dim)
    }

    /// Create a new array of storage mode "integer".
    pub fn new_array_integer(&self, dim: &[usize]) -> &mut RObject<RArray, i32> {
        self.new_array::<i32>(INTSXP, dim)
    }

    /// Create a new array of storage mode "raw".
    pub fn new_array_raw(&self, dim: &[usize]) -> &mut RObject<RArray, u8> {
        self.new_array::<u8>(RAWSXP, dim)
    }

    /// Create a new array of storage mode "logical".
    pub fn new_array_logical(&self, dim: &[usize]) -> &mut RObject<RArray, bool> {
        self.new_array::<bool>(LGLSXP, dim)
    }

    /// Create a new array of storage mode "character".
    pub fn new_array_character(&self, dim: &[usize]) -> &mut RObject<RArray, RCharacter> {
        self.new_array::<RCharacter>(STRSXP, dim)
    }

    /// Create a new list.
    pub fn new_list(&self, length: usize) -> &mut RObject<RVector, RList> {
        self.new_vector(VECSXP, length)
    }

    /// Define a new error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use `stop!`.
    ///
    pub fn new_error(&self, message: &str) -> &RObject {
        let list = self.new_list(2);
        let _ = list.set(0, message.to_r(self));
        let _ = list.set(1, Self::null());
        let _ = list.set_names(["message", "calls"].to_r(self));
        list.set_class(["error", "condition"].to_r(self));
        list.transmute()
    }

    /// Define a new symbol.
    #[allow(clippy::mut_from_ref)]
    pub fn new_symbol(&self, x: &str) -> &mut RObject<RSymbol> {
        let sexp = self.protect(unsafe {
            Rf_mkCharLenCE(
                x.as_ptr() as *const c_char,
                x.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            )
        });
        let sexp = self.protect(unsafe { Rf_installChar(sexp) });
        self.transmute_sexp_mut(sexp)
    }

    /// Get R's "dim" symbol.
    pub fn symbol_dim() -> &'static RObject<RSymbol> {
        Self::transmute_sexp_static(unsafe { R_DimSymbol })
    }

    /// Get R's "names" symbol.
    pub fn symbol_names() -> &'static RObject<RSymbol> {
        Self::transmute_sexp_static(unsafe { R_NamesSymbol })
    }

    /// Get R's "rownames" symbol.
    pub fn symbol_rownames() -> &'static RObject<RSymbol> {
        Self::transmute_sexp_static(unsafe { R_RowNamesSymbol })
    }

    /// Get R's "dimnames" symbol.
    pub fn symbol_dimnames() -> &'static RObject<RSymbol> {
        Self::transmute_sexp_static(unsafe { R_DimNamesSymbol })
    }

    /// Get R's "class" symbol.
    pub fn symbol_class() -> &'static RObject<RSymbol> {
        Self::transmute_sexp_static(unsafe { R_ClassSymbol })
    }

    /// Move Rust object to an R external pointer.
    ///
    /// This *method* moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RObject::decode_val`].
    ///
    #[allow(clippy::mut_from_ref)]
    pub fn encode<T, RType, RMode>(
        &self,
        x: T,
        tag: &RObject<RType, RMode>,
        managed_by_r: bool,
    ) -> &mut RObject<RExternalPtr> {
        unsafe {
            let ptr = Box::into_raw(Box::new(x));
            let sexp = self.protect(R_MakeExternalPtr(
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
            self.transmute_sexp_mut(sexp)
        }
    }

    /// Returns an R NULL value.
    pub fn null() -> &'static RObject {
        Self::transmute_sexp_static(unsafe { R_NilValue })
    }

    /// Returns an R NA value for storage mode "double".
    pub fn na_double() -> f64 {
        unsafe { R_NaReal }
    }

    /// Returns an R NA value for storage mode "integer".
    pub fn na_integer() -> i32 {
        unsafe { R_NaInt }
    }

    /// Returns an R NA value for storage mode "logical".
    pub fn na_logical() -> i32 {
        unsafe { R_NaInt }
    }

    /// Returns an R NaN value.
    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    /// Returns an R Inf value.
    pub fn infinity_positive() -> f64 {
        unsafe { R_PosInf }
    }

    /// Returns an R -Inf value.
    pub fn infinity_negative() -> f64 {
        unsafe { R_NegInf }
    }

    /// Checks if an f64 can be interpreted as an R NA value.
    pub fn is_na_double(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    /// Checks if an i32 can be interpreted as an R NA value.
    pub fn is_na_integer(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    /// Checks if a bool can be interpreted as an R NA value.
    pub fn is_na_logical(x: i32) -> bool {
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

#[repr(C)]
pub struct RObject<RType = RAnyType, RMode = RUnknown> {
    pub sexprec: SEXPREC,
    rtype: PhantomData<(RType, RMode)>,
}

impl<RType, RMode> RObject<RType, RMode> {
    pub fn sexp(&self) -> SEXP {
        self as *const RObject<RType, RMode> as SEXP
    }

    fn transmute_sexp<RTypeTo, RModeTo>(&self, sexp: SEXP) -> &RObject<RTypeTo, RModeTo> {
        unsafe { &*sexp.cast::<RObject<RTypeTo, RModeTo>>() }
    }

    fn transmute_sexp_mut<RTypeTo, RModeTo>(
        &mut self,
        sexp: SEXP,
    ) -> &mut RObject<RTypeTo, RModeTo> {
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
    pub fn unknown(&self) -> &RObject {
        self.transmute()
    }

    /// Returns the result of the is_null method, but as an Option value.
    pub fn option(&self) -> Option<&Self> {
        if self.is_null() {
            None
        } else {
            Some(self)
        }
    }

    pub fn scalar(&self) -> Result<&RObject<RScalar>, &'static str> {
        let s = self.vector()?;
        if s.is_scalar() {
            Ok(self.transmute())
        } else {
            Err("Not a scalar")
        }
    }

    pub fn scalar_mut(&mut self) -> Result<&mut RObject<RScalar>, &'static str> {
        let s = self.vector()?;
        if s.is_scalar() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a scalar")
        }
    }

    pub fn vector(&self) -> Result<&RObject<RVector>, &'static str> {
        if self.is_vector() {
            Ok(self.transmute())
        } else {
            Err("Not a vector")
        }
    }

    pub fn vector_mut(&mut self) -> Result<&mut RObject<RVector>, &'static str> {
        if self.is_vector() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an RObject<RMatrix>.
    /// Checks using R's `Rf_isMatrix` function.
    pub fn matrix(&self) -> Result<&RObject<RMatrix>, &'static str> {
        if self.is_matrix() {
            Ok(self.transmute())
        } else {
            Err("Not a matrix")
        }
    }

    /// Check if appropriate to characterize as an RObject<RMatrix>.
    /// Checks using R's `Rf_isMatrix` function.
    pub fn matrix_mut(&mut self) -> Result<&mut RObject<RMatrix>, &'static str> {
        if self.is_matrix() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a matrix")
        }
    }

    /// Check if appropriate to characterize as an RObject<RArray>.
    /// Checks using R's `Rf_isArray` function.
    pub fn array(&self) -> Result<&RObject<RArray>, &'static str> {
        if self.is_array() {
            Ok(self.transmute())
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an RObject<RArray>.
    /// Checks using R's `Rf_isArray` function.
    pub fn array_mut(&mut self) -> Result<&mut RObject<RArray>, &'static str> {
        if self.is_array() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an RObject<RVector, RList>.
    /// Checks using R's `Rf_isVectorList` function.
    pub fn list(&self) -> Result<&RObject<RVector, RList>, &'static str> {
        if self.is_list() {
            Ok(self.transmute())
        } else {
            Err("Not a list")
        }
    }

    /// Check if appropriate to characterize as an RObject<RVector, RList>.
    /// Checks using R's `Rf_isVectorList` function.
    pub fn list_mut(&mut self) -> Result<&mut RObject<RVector, RList>, &'static str> {
        if self.is_list() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a list")
        }
    }

    /// Check if appropriate to characterize as an RObject<RVector, RDataFrame>.
    /// Checks using R's `Rf_isFrame` function.
    pub fn data_frame(&self) -> Result<&RObject<RVector, RDataFrame>, &'static str> {
        if self.is_data_frame() {
            Ok(self.transmute())
        } else {
            Err("Not a data frame")
        }
    }

    /// Check if appropriate to characterize as an RObject<RVector, RDataFrame>.
    /// Checks using R's `Rf_isFrame` function.
    pub fn data_frame_mut(&mut self) -> Result<&mut RObject<RVector, RDataFrame>, &'static str> {
        if self.is_data_frame() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a data frame")
        }
    }

    /// Check if appropriate to characterize as an RObject<RFunction>.
    /// Checks using R's `Rf_isFunction` function.
    pub fn function(&self) -> Result<&RObject<RFunction>, &'static str> {
        if self.is_function() {
            Ok(self.transmute())
        } else {
            Err("Not a function")
        }
    }

    /// Check if appropriate to characterize as an RObject<RFunction>.
    /// Checks using R's `Rf_isFunction` function.
    pub fn function_mut(&mut self) -> Result<&mut RObject<RFunction>, &'static str> {
        if self.is_function() {
            Ok(self.transmute_mut())
        } else {
            Err("Not a function")
        }
    }

    /// Check if appropriate to characterize as an RObject<RExternalPtr>.
    /// Uses the SEXP type to determine if this is possible.
    pub fn external_ptr(&self) -> Result<&RObject<RExternalPtr>, &'static str> {
        if self.is_external_ptr() {
            Ok(self.transmute())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if appropriate to characterize as an RObject<RExternalPtr>.
    /// Uses the SEXP type to determine if this is possible.
    pub fn external_ptr_mut(&mut self) -> Result<&mut RObject<RExternalPtr>, &'static str> {
        if self.is_external_ptr() {
            Ok(self.transmute_mut())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if appropriate to characterize as an RObject<RExternalPtr>.
    /// Uses the SEXP type to determine if this is possible.
    pub fn symbol(&self) -> Result<&RObject<RSymbol>, &'static str> {
        if self.is_symbol() {
            Ok(self.transmute())
        } else {
            Err("Not an external pointer")
        }
    }

    /// Check if appropriate to characterize as an RObject<RExternalPtr>.
    /// Uses the SEXP type to determine if this is possible.
    pub fn symbol_mut(&mut self) -> Result<&mut RObject<RSymbol>, &'static str> {
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

    /// Check if RObject can be interpreted as an NA value in R.
    pub fn is_na(&self) -> bool {
        if self.is_vector() {
            let s: &RObject<RVector> = self.transmute();
            if s.is_scalar() {
                if s.is_double() {
                    unsafe { R_IsNA(Rf_asReal(s.sexp())) != 0 }
                } else if s.is_integer() {
                    unsafe { Rf_asInteger(s.sexp()) == Pc::na_integer() }
                } else if s.is_logical() {
                    unsafe { Rf_asLogical(s.sexp()) == Pc::na_logical() }
                } else if s.is_character() {
                    unsafe { Rf_asChar(s.sexp()) == R_NaString }
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Check if RObject can be interpreted as an NaN value in R.
    pub fn is_nan(&self) -> bool {
        if self.is_vector() {
            let s: &RObject<RVector> = self.transmute();
            if s.is_scalar() && s.is_double() {
                unsafe { R_IsNaN(Rf_asReal(s.sexp())) != 0 }
            } else {
                false
            }
        } else {
            false
        }
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
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), Pc::symbol_class().sexp()) })
    }

    /// Get an attribute.
    pub fn get_attribute(&self, which: &RObject<RSymbol>) -> &RObject {
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), which.sexp()) })
    }
}

impl<RType: HasLength, RMode> RObject<RType, RMode> {
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

impl<RType: HasLength, RMode: Atomic> RObject<RType, RMode> {
    fn slice_engine<U>(&self, data: *mut U) -> &[U] {
        unsafe { std::slice::from_raw_parts_mut(data, self.len()) }
    }

    fn slice_mut_engine<U>(&mut self, data: *mut U) -> &mut [U] {
        unsafe { std::slice::from_raw_parts_mut(data, self.len()) }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn double(&self) -> Result<&RObject<RType, f64>, &'static str> {
        if self.is_double() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'double'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn double_mut(&mut self) -> Result<&mut RObject<RType, f64>, &'static str> {
        if self.is_double() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'double'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_double(&self) -> bool {
        unsafe { Rf_isReal(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_double<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, f64> {
        if self.is_double() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), REALSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_double_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut RObject<RType, f64> {
        if self.is_double() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), REALSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn integer(&self) -> Result<&RObject<RType, i32>, &'static str> {
        if self.is_integer() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'integer'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn integer_mut(&mut self) -> Result<&mut RObject<RType, i32>, &'static str> {
        if self.is_integer() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'integer'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_integer(&self) -> bool {
        unsafe { Rf_isInteger(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_integer<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, i32> {
        if self.is_integer() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), INTSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_integer_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut RObject<RType, i32> {
        if self.is_integer() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), INTSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn raw(&self) -> Result<&RObject<RType, u8>, &'static str> {
        if self.is_raw() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'raw'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn raw_mut(&mut self) -> Result<&mut RObject<RType, u8>, &'static str> {
        if self.is_raw() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'raw'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_raw(&self) -> bool {
        unsafe { TYPEOF(self.sexp()) == RAWSXP as i32 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_raw<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, u8> {
        if self.is_raw() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), RAWSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_raw_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut RObject<RType, u8> {
        if self.is_raw() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), RAWSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn logical(&self) -> Result<&RObject<RType, bool>, &'static str> {
        if self.is_logical() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'logical'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn logical_mut(&mut self) -> Result<&mut RObject<RType, bool>, &'static str> {
        if self.is_logical() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'logical'")
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_logical(&self) -> bool {
        unsafe { Rf_isLogical(self.sexp()) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_logical<'a>(&'a self, pc: &'a Pc) -> &'a RObject<RType, bool> {
        if self.is_logical() {
            self.transmute()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), LGLSXP) });
            pc.transmute_sexp(sexp)
        }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_logical_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut RObject<RType, bool> {
        if self.is_logical() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), LGLSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn character(&self) -> Result<&RObject<RType, RCharacter>, &'static str> {
        if self.is_character() {
            Ok(self.transmute())
        } else {
            Err("Not of storage mode 'logical'")
        }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn character_mut(&mut self) -> Result<&mut RObject<RType, RCharacter>, &'static str> {
        if self.is_character() {
            Ok(self.transmute_mut())
        } else {
            Err("Not of storage mode 'logical'")
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
    pub fn to_character_mut<'a>(&'a mut self, pc: &'a Pc) -> &'a mut RObject<RType, RCharacter> {
        if self.is_character() {
            self.transmute_mut()
        } else {
            let sexp = pc.protect(unsafe { Rf_coerceVector(self.sexp(), STRSXP) });
            pc.transmute_sexp_mut(sexp)
        }
    }
}

impl<RType: HasLength> RObject<RType, f64> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[f64] {
        self.slice_engine(unsafe { REAL(self.sexp()) })
    }

    /// Returns a slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [f64] {
        self.slice_mut_engine(unsafe { REAL(self.sexp()) })
    }
}

impl<RType: HasLength> RObject<RType, i32> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[i32] {
        self.slice_engine(unsafe { INTEGER(self.sexp()) })
    }

    /// Returns a slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [i32] {
        self.slice_mut_engine(unsafe { INTEGER(self.sexp()) })
    }
}

impl<RType: HasLength> RObject<RType, u8> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[u8] {
        self.slice_engine(unsafe { RAW(self.sexp()) })
    }

    /// Returns a slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [u8] {
        self.slice_mut_engine(unsafe { RAW(self.sexp()) })
    }
}

impl<RType: HasLength> RObject<RType, bool> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[i32] {
        self.slice_engine(unsafe { LOGICAL(self.sexp()) })
    }

    /// Returns a slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [i32] {
        self.slice_mut_engine(unsafe { LOGICAL(self.sexp()) })
    }
}

impl<RMode> RObject<RMatrix, RMode> {
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
        let dim: &mut RObject<RVector, i32> = self
            .get_attribute(Pc::symbol_dim())
            .clone(pc)
            .transmute_mut();
        let slice = dim.slice_mut();
        slice.swap(0, 1);
        transposed.set_attribute(Pc::symbol_dim(), dim);
        unsafe { Rf_copyMatrix(transposed.sexp(), self.sexp(), Rboolean_TRUE) };
        transposed
    }

    /// Manipulates the matrix in place to be a vector by dropping the `dim` attribute.
    pub fn as_vector(&mut self) -> &mut RObject<RVector, RMode> {
        unsafe { Rf_setAttrib(self.sexp(), R_DimSymbol, R_NilValue) };
        self.transmute_mut()
    }
}

impl<RType> RObject<RArray, RType> {
    /// Returns the dimensions of the RArray.
    pub fn dim(&self) -> Vec<usize> {
        let d =
            self.transmute_sexp::<RVector, i32>(unsafe { Rf_getAttrib(self.sexp(), R_DimSymbol) });
        d.slice().iter().map(|&x| x.try_into().unwrap()).collect()
    }

    // Create a new vector from a matrix.
    /// Convert an RArray to a Vector.
    pub fn as_vector(&mut self) -> &mut RObject<RVector, RType> {
        unsafe { Rf_setAttrib(self.sexp(), R_DimSymbol, R_NilValue) };
        self.transmute_mut()
    }
}

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

impl<RMode: Atomic> RObject<RScalar, RMode> {
    /// Check if appropriate to characterize as an f64.
    pub fn f64(&self) -> f64 {
        unsafe { Rf_asReal(self.sexp()) }
    }

    /// Check if appropriate to characterize as an i32.
    pub fn i32(&self) -> Result<i32, &'static str> {
        if self.is_integer() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("i32 equals R's NA for integers")
            } else {
                Ok(x)
            }
        } else if self.is_double() {
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
        } else if self.is_raw() {
            Ok(unsafe { Rf_asInteger(self.sexp()) })
        } else if self.is_logical() {
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
        if self.is_integer() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers")
            } else if x < 0 {
                Err("Negative value not expected")
            } else {
                usize::try_from(x).map_err(|_| "Cannot convert to usize")
            }
        } else if self.is_double() {
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
        } else if self.is_raw() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            usize::try_from(x).map_err(|_| "Cannot convert to usize")
        } else if self.is_logical() {
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
        if self.is_integer() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            u8::try_from(x).map_err(|_| "Cannot convert to u8")
        } else if self.is_double() {
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
        } else if self.is_raw() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            u8::try_from(x).map_err(|_| "Cannot convert to u8")
        } else if self.is_logical() {
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
        if self.is_integer() {
            let x = unsafe { Rf_asInteger(self.sexp()) };
            if x == i32::MIN {
                Err("Equals R's NA for integers")
            } else {
                Ok(x != 0)
            }
        } else if self.is_double() {
            let y = unsafe { Rf_asReal(self.sexp()) };
            if Pc::is_na_double(y) {
                Err("Equals R's NA for doubles")
            } else if Pc::is_nan(y) {
                Err("Equals R's NaN")
            } else {
                Ok(y != 0.0)
            }
        } else if self.is_raw() {
            Ok(unsafe { Rf_asInteger(self.sexp()) } != 0)
        } else if self.is_logical() {
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
    pub fn to_str<'a>(&self, pc: &'a Pc) -> Result<&'a str, &'static str> {
        let s: &RObject<RVector, RCharacter> = self.to_character(pc).transmute();
        s.get(0)
    }

    /// Manipulates the matrix in place to be a vector by dropping the `dim` attribute.
    pub fn as_vector(&self) -> &RObject<RVector, RMode> {
        self.transmute()
    }
}

impl<RMode> RObject<RVector, RMode> {
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

impl RObject<RVector, f64> {
    /// Get the value at a certain index in an f64 RVector.
    pub fn get(&self, index: usize) -> Result<f64, &'static str> {
        self.get_engine(index, REAL_ELT)
    }

    /// Set the value at a certain index in an f64 RVector.
    pub fn set(&mut self, index: usize, value: f64) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_REAL_ELT)
    }
}

impl RObject<RVector, i32> {
    /// Get the value at a certain index in an i32 RVector.
    pub fn get(&self, index: usize) -> Result<i32, &'static str> {
        self.get_engine(index, INTEGER_ELT)
    }

    /// Set the value at a certain index in an i32 RVector.
    pub fn set(&mut self, index: usize, value: i32) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_INTEGER_ELT)
    }
}

impl RObject<RVector, u8> {
    /// Get the value at a certain index in a u8 RVector.
    pub fn get(&self, index: usize) -> Result<u8, &'static str> {
        self.get_engine(index, RAW_ELT)
    }

    /// Set the value at a certain index in a u8 RVector.
    pub fn set(&mut self, index: usize, value: u8) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_RAW_ELT)
    }
}

impl RObject<RVector, bool> {
    /// Get the value at a certain index in a logical RVector.
    pub fn get(&self, index: usize) -> Result<bool, &'static str> {
        self.get_engine(index, LOGICAL_ELT).map(|x| x != 0)
    }

    /// Get the value at a certain index in a logical RVector as an i32.
    pub fn get_i32(&self, index: usize) -> Result<i32, &'static str> {
        self.get_engine(index, LOGICAL_ELT)
    }

    /// Set the value at a certain index in a logical RVector.
    pub fn set(&mut self, index: usize, value: bool) -> Result<(), &'static str> {
        let value = if value {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }

    /// Set the value at certain index in a logical RVector with an i32.
    pub fn set_i32(&mut self, index: usize, value: i32) -> Result<(), &'static str> {
        let value = if value != 0 {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }
}

impl RObject<RVector, RCharacter> {
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
        unsafe {
            let value = Rf_mkCharLenCE(
                value.as_ptr() as *const c_char,
                value.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            );
            self.set_engine(index, value, SET_STRING_ELT)
        }
    }

    /// Set the value at a certain index in a character RVector to NA.
    pub fn set_na(&mut self, index: usize) {
        unsafe {
            SET_STRING_ELT(self.sexp(), index.try_into().unwrap(), R_NaString);
        }
    }
}

pub struct RListMap<'a> {
    unused_counter: usize,
    used: Vec<bool>,
    robj: &'a RObject<RVector, RList>,
    map: HashMap<&'a str, usize>,
}

impl RListMap<'_> {
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

impl RObject<RVector, RList> {
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
            robj: self,
            map,
        }
    }

    /// Set the value at a certain index in an RList.
    pub fn set<RType, RMode>(
        &mut self,
        index: usize,
        value: &RObject<RType, RMode>,
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
    ) -> Result<&'a mut RObject<RVector, RDataFrame>, &'static str> {
        if names.len() != self.len() {
            return Err("Length of names is not correct");
        }
        let mut nrow = -1;
        for i in 0..self.len() {
            let x = self.get(i).unwrap();
            if x.is_vector() {
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

impl RObject<RVector, RDataFrame> {
    /// Get the value at a certain index in an RDataFrame.
    pub fn get(&self, index: usize) -> Result<&RObject, &'static str> {
        self.transmute::<RVector, RList>().get(index)
    }

    /// Get the row names of a RDataFrame.
    pub fn get_rownames(&self) -> &RObject<RVector, RCharacter> {
        self.transmute_sexp(unsafe { Rf_getAttrib(self.sexp(), R_RowNamesSymbol) })
    }

    /// Set the value at a certain index in an RDataFrame.
    pub fn set<RType, RMode>(
        &mut self,
        index: usize,
        value: &RObject<RType, RMode>,
    ) -> Result<(), &'static str> {
        self.transmute_mut::<RVector, RList>().set(index, value)
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

impl<RMode> RObject<RMatrix, RMode> {
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
    pub fn set_dimnames(&mut self, dimnames: &RObject<RVector, RList>) -> Result<(), &'static str> {
        match dimnames.get(0) {
            Ok(rownames) => match rownames.vector() {
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
            Ok(colnames) => match colnames.vector() {
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

impl RObject<RMatrix, f64> {
    /// Get the value at a certain index in a double RMatrix.
    pub fn get(&self, index: (usize, usize)) -> Result<f64, &'static str> {
        self.transmute::<RVector, f64>().get(self.index(index))
    }

    /// Set the value at a certain index in a double RMatrix.
    pub fn set(&mut self, index: (usize, usize), value: f64) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, f64>().set(index, value)
    }
}

impl RObject<RMatrix, i32> {
    /// Get the value at a certain index in an integer RMatrix.
    pub fn get(&self, index: (usize, usize)) -> Result<i32, &'static str> {
        self.transmute::<RVector, i32>().get(self.index(index))
    }

    /// Set the value at a certain index in an integer RMatrix.
    pub fn set(&mut self, index: (usize, usize), value: i32) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, i32>().set(index, value)
    }
}

impl RObject<RMatrix, u8> {
    /// Get the value at a certain index in a raw RMatrix.
    pub fn get(&self, index: (usize, usize)) -> Result<u8, &'static str> {
        self.transmute::<RVector, u8>().get(self.index(index))
    }

    /// Set the value at a certain index in a raw RMatrix.
    pub fn set(&mut self, index: (usize, usize), value: u8) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, u8>().set(index, value)
    }
}

impl RObject<RMatrix, bool> {
    /// Get the value at a certain index in a logical RMatrix.
    pub fn get(&self, index: (usize, usize)) -> Result<bool, &'static str> {
        self.transmute::<RVector, bool>().get(self.index(index))
    }

    /// Get the value at a certain index in a logical RMatrix as an i32.
    pub fn get_i32(&self, index: (usize, usize)) -> Result<i32, &'static str> {
        self.transmute::<RVector, bool>().get_i32(self.index(index))
    }

    /// Set the value at a certain index in a logical RMatrix.
    pub fn set(&mut self, index: (usize, usize), value: bool) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, bool>().set(index, value)
    }

    /// Set the value at a certain index in a logical RMatrix an an i32.
    pub fn set_i32(&mut self, index: (usize, usize), value: i32) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, bool>().set_i32(index, value)
    }
}

impl RObject<RMatrix, RCharacter> {
    /// Get the value at a certain index in a character RMatrix.
    pub fn get(&self, index: (usize, usize)) -> Result<&str, &'static str> {
        self.transmute::<RVector, RCharacter>()
            .get(self.index(index))
    }

    /// Set the value at a certain index in a character RMatrix.
    pub fn set<RType, RMode>(
        &mut self,
        index: (usize, usize),
        value: &str,
    ) -> Result<(), &'static str> {
        let index = self.index(index);
        self.transmute_mut::<RVector, RCharacter>()
            .set(index, value)
    }
}

impl RObject<RExternalPtr> {
    /// Check if an external pointer is managed by R.
    pub fn is_managed_by_r(&self) -> bool {
        unsafe { Rf_getAttrib(self.sexp(), R_AtsignSymbol) == R_AtsignSymbol }
    }

    /// Move an R external pointer to a Rust object.
    ///
    /// This method moves an R external pointer created by [`Self::as_external_ptr`] to a Rust object and Rust will then manage its memory.
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
    /// This method obtains a reference to a Rust object from an R external pointer created by [`Self::as_external_ptr`].
    ///
    pub fn decode_ref<T>(&self) -> &T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a reference to a Rust object from an R external pointer created by [`Self::as_external_ptr`].
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
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`Self::as_external_ptr`].
    ///
    pub fn decode_mut<'a, T>(&mut self) -> &'a mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp()) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`Self::external_ptr`].
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
///
/// The traits [ToR2], [ToR3], and [ToR4] are all identical to this trait.
/// This was done to avoid conflicting trait implementations.
pub trait ToR1<'a, RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RType, RMode>;
}

/// Trait for converting objects to RObjects.
///
/// See [ToR1].
pub trait ToR2<RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut RObject<RType, RMode>;
}

/// Trait for converting objects to RObjects.
///
/// See [ToR1].
pub trait ToR3<RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut RObject<RType, RMode>;
}

/// Trait for converting objects to RObjects.
///
/// See [ToR1].
pub trait ToR4<RType, RMode> {
    #[allow(clippy::mut_from_ref)]
    fn to_r(self, pc: &Pc) -> &mut RObject<RType, RMode>;
}

// f64

impl<'a> ToR1<'a, RVector, f64> for f64 {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, f64> {
        pc.transmute_sexp_mut(pc.protect(unsafe { Rf_ScalarReal(*self) }))
    }
}

impl<'a, const N: usize> ToR1<'a, RVector, f64> for [f64; N] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, f64> {
        self.as_ref().to_r(pc)
    }
}

impl<'a> ToR1<'a, RVector, f64> for &[f64] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, f64> {
        let result = pc.new_vector_double(self.len());
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a> ToR1<'a, RVector, f64> for &mut [f64] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, f64> {
        let result = pc.new_vector_double(self.len());
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a f64> + ExactSizeIterator> ToR2<RVector, f64> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, f64> {
        let result = pc.new_vector_double(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut f64> + ExactSizeIterator> ToR3<RVector, f64> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, f64> {
        let result = pc.new_vector_double(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = f64> + ExactSizeIterator> ToR4<RVector, f64> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, f64> {
        let result = pc.new_vector_double(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// i32

impl<'a> ToR1<'a, RVector, i32> for i32 {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        pc.transmute_sexp_mut(pc.protect(unsafe { Rf_ScalarInteger(*self) }))
    }
}

impl<'a, const N: usize> ToR1<'a, RVector, i32> for [i32; N] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl<'a> ToR1<'a, RVector, i32> for &[i32] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a> ToR1<'a, RVector, i32> for &mut [i32] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a i32> + ExactSizeIterator> ToR2<RVector, i32> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut i32> + ExactSizeIterator> ToR3<RVector, i32> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = i32> + ExactSizeIterator> ToR4<RVector, i32> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// usize

impl<'a> ToR1<'a, RVector, i32> for usize {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        pc.transmute_sexp_mut(pc.protect(unsafe { Rf_ScalarInteger((*self).try_into().unwrap()) }))
    }
}

impl<'a, const N: usize> ToR1<'a, RVector, i32> for [usize; N] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl<'a> ToR1<'a, RVector, i32> for &[usize] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl<'a> ToR1<'a, RVector, i32> for &mut [usize] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, i32> {
        let result = pc.new_vector_integer(self.len());
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

// u8

impl<'a> ToR1<'a, RVector, u8> for u8 {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, u8> {
        pc.transmute_sexp_mut(pc.protect(unsafe { Rf_ScalarRaw(*self) }))
    }
}

impl<'a, const N: usize> ToR1<'a, RVector, u8> for [u8; N] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, u8> {
        self.as_ref().to_r(pc)
    }
}

impl<'a> ToR1<'a, RVector, u8> for &[u8] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, u8> {
        let result = pc.new_vector_raw(self.len());
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a> ToR1<'a, RVector, u8> for &mut [u8] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, u8> {
        let result = pc.new_vector_raw(self.len());
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a u8> + ExactSizeIterator> ToR2<RVector, u8> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, u8> {
        let result = pc.new_vector_raw(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut u8> + ExactSizeIterator> ToR3<RVector, u8> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, u8> {
        let result = pc.new_vector_raw(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = u8> + ExactSizeIterator> ToR4<RVector, u8> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, u8> {
        let result = pc.new_vector_raw(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// bool

impl<'a> ToR1<'a, RVector, bool> for bool {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, bool> {
        pc.transmute_sexp_mut(pc.protect(unsafe {
            Rf_ScalarLogical(if *self {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            })
        }))
    }
}

impl<'a, const N: usize> ToR1<'a, RVector, bool> for [bool; N] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, bool> {
        self.as_ref().to_r(pc)
    }
}

impl<'a> ToR1<'a, RVector, bool> for &[bool] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, bool> {
        let result = pc.new_vector_logical(self.len());
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).into();
        }
        result
    }
}

impl<'a> ToR1<'a, RVector, bool> for &mut [bool] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, bool> {
        let result = pc.new_vector_logical(self.len());
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).into();
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a bool> + ExactSizeIterator> ToR2<RVector, bool> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, bool> {
        let result = pc.new_vector_logical(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = if *from {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            };
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut bool> + ExactSizeIterator> ToR3<RVector, bool> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, bool> {
        let result = pc.new_vector_logical(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = if *from {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            };
        }
        result
    }
}

impl<T: IntoIterator<Item = bool> + ExactSizeIterator> ToR4<RVector, bool> for T {
    fn to_r(self, pc: &Pc) -> &mut RObject<RVector, bool> {
        let result = pc.new_vector_logical(self.len());
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = if from {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            };
        }
        result
    }
}

// &str

impl<'a> ToR1<'a, RVector, RCharacter> for &str {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, RCharacter> {
        let sexp = unsafe {
            Rf_ScalarString(Rf_mkCharLenCE(
                self.as_ptr() as *const c_char,
                self.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            ))
        };
        pc.transmute_sexp_mut(pc.protect(sexp))
    }
}

impl<'a, const N: usize> ToR1<'a, RVector, RCharacter> for [&str; N] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, RCharacter> {
        self.as_ref().to_r(pc)
    }
}

impl<'a> ToR1<'a, RVector, RCharacter> for &[&str] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, RCharacter> {
        let result = pc.new_vector_character(self.len());
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

impl<'a> ToR1<'a, RVector, RCharacter> for &mut [&str] {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RVector, RCharacter> {
        let result = pc.new_vector_character(self.len());
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

// &RObject and SEXP

impl<'a, RType, RMode> ToR1<'a, RAnyType, RUnknown> for RObject<RType, RMode> {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject {
        pc.transmute_sexp_mut(self.sexp())
    }
}

impl<'a> ToR1<'a, RAnyType, RUnknown> for SEXP {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RAnyType, RUnknown> {
        pc.transmute_sexp_mut(*self)
    }
}

impl<'a> ToR1<'a, RAnyType, RUnknown> for () {
    fn to_r(&self, pc: &'a Pc) -> &'a mut RObject<RAnyType, RUnknown> {
        pc.transmute_sexp_mut(unsafe { R_NilValue })
    }
}
