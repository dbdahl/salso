//! Extension Framework for R using Rust

//#![allow(dead_code)]

// Helpful resources:
//   https://cran.r-project.org/doc/manuals/r-release/R-ints.html
//   https://svn.r-project.org/R/trunk/src/include/Rinternals.h
//   https://github.com/hadley/r-internals
//   https://www.tidyverse.org/blog/2019/05/resource-cleanup-in-c-and-the-r-api
//   https://github.com/wch/r-source

use crate::pc::Pc;
use crate::rbindings::*;

use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr};
use std::marker::PhantomData;
use std::ops::Deref;

pub struct R {}

#[doc(hidden)]
pub struct AnyType(());

pub struct Vector(());

pub struct Matrix(());

pub struct Array(());

#[doc(hidden)]
pub struct Function(());

#[doc(hidden)]
pub struct ExternalPtr(());

#[doc(hidden)]
pub struct Symbol(());

#[doc(hidden)]
pub struct Unknown(());

#[doc(hidden)]
pub struct Character;

#[doc(hidden)]
pub struct List(());

#[doc(hidden)]
pub struct DataFrame(());

pub trait HasLength {}
impl HasLength for Vector {}
impl HasLength for Matrix {}
impl HasLength for Array {}

pub trait Atomic {}
impl Atomic for f64 {}
impl Atomic for i32 {}
impl Atomic for u8 {}
impl Atomic for bool {}
impl Atomic for Character {}
impl Atomic for Unknown {}

#[doc(hidden)]
pub trait Convertible {}
impl Convertible for Vector {}
impl Convertible for Matrix {}
impl Convertible for Array {}
impl Convertible for Function {}
impl Convertible for ExternalPtr {}

impl R {
    fn wrap<RType, RMode>(sexp: SEXP) -> RObject<RType, RMode> {
        RObject {
            sexp,
            rtype: PhantomData,
        }
    }

    /// Create a new object from a SEXP.
    pub fn new_object(sexp: SEXP) -> RObject {
        Self::wrap(sexp)
    }

    fn new_vector<RMode>(code: u32, length: usize, pc: &mut Pc) -> RObject<Vector, RMode> {
        Self::wrap(pc.protect(unsafe { Rf_allocVector(code, length.try_into().unwrap()) }))
    }

    /// Create a new vector of storage mode "double".
    pub fn new_vector_double(length: usize, pc: &mut Pc) -> RObject<Vector, f64> {
        Self::new_vector::<f64>(REALSXP, length, pc)
    }

    /// Create a new vector of type storage mode "integer".
    pub fn new_vector_integer(length: usize, pc: &mut Pc) -> RObject<Vector, i32> {
        Self::new_vector::<i32>(INTSXP, length, pc)
    }

    /// Create a new vector of storage mode "raw".
    pub fn new_vector_raw(length: usize, pc: &mut Pc) -> RObject<Vector, u8> {
        Self::new_vector::<u8>(RAWSXP, length, pc)
    }

    /// Create a new vector of storage mode "logical".
    pub fn new_vector_logical(length: usize, pc: &mut Pc) -> RObject<Vector, bool> {
        Self::new_vector::<bool>(LGLSXP, length, pc)
    }

    /// Create a new vector of storage mode "character".
    pub fn new_vector_character(length: usize, pc: &mut Pc) -> RObject<Vector, Character> {
        Self::new_vector(STRSXP, length, pc)
    }

    fn new_matrix<RMode>(
        code: u32,
        nrow: usize,
        ncol: usize,
        pc: &mut Pc,
    ) -> RObject<Matrix, RMode> {
        Self::wrap(pc.protect(unsafe {
            Rf_allocMatrix(code, nrow.try_into().unwrap(), ncol.try_into().unwrap())
        }))
    }

    /// Create a new matrix of storage mode "double".
    pub fn new_matrix_double(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, f64> {
        Self::new_matrix::<f64>(REALSXP, nrow, ncol, pc)
    }

    /// Create a new matrix of storage mode "integer".
    pub fn new_matrix_integer(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, i32> {
        Self::new_matrix::<i32>(INTSXP, nrow, ncol, pc)
    }

    /// Create a new matrix of storage mode "raw".
    pub fn new_matrix_raw(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, u8> {
        Self::new_matrix::<u8>(RAWSXP, nrow, ncol, pc)
    }

    /// Create a new matrix of storage mode "logical".
    pub fn new_matrix_logical(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, bool> {
        Self::new_matrix::<bool>(LGLSXP, nrow, ncol, pc)
    }

    /// Create a new matrix of storage mode "character".
    pub fn new_matrix_character(
        nrow: usize,
        ncol: usize,
        pc: &mut Pc,
    ) -> RObject<Matrix, Character> {
        Self::new_matrix::<Character>(STRSXP, nrow, ncol, pc)
    }

    fn new_array<RMode>(code: u32, dim: &[usize], pc: &mut Pc) -> RObject<Array, RMode> {
        let d = dim.iter().map(|x| i32::try_from(*x).unwrap()).to_r(pc);
        Self::wrap(pc.protect(unsafe { Rf_allocArray(code, d.sexp) }))
    }

    /// Create a new array of storage mode "double".
    pub fn new_array_double(dim: &[usize], pc: &mut Pc) -> RObject<Array, f64> {
        Self::new_array::<f64>(REALSXP, dim, pc)
    }

    /// Create a new array of storage mode "integer".
    pub fn new_array_integer(dim: &[usize], pc: &mut Pc) -> RObject<Array, i32> {
        Self::new_array::<i32>(INTSXP, dim, pc)
    }

    /// Create a new array of storage mode "raw".
    pub fn new_array_raw(dim: &[usize], pc: &mut Pc) -> RObject<Array, u8> {
        Self::new_array::<u8>(RAWSXP, dim, pc)
    }

    /// Create a new array of storage mode "logical".
    pub fn new_array_logical(dim: &[usize], pc: &mut Pc) -> RObject<Array, bool> {
        Self::new_array::<bool>(LGLSXP, dim, pc)
    }

    /// Create a new array of storage mode "character".
    pub fn new_array_character(dim: &[usize], pc: &mut Pc) -> RObject<Array, Character> {
        Self::new_array::<Character>(STRSXP, dim, pc)
    }

    /// Create a new list.
    pub fn new_list(length: usize, pc: &mut Pc) -> RObject<Vector, List> {
        Self::new_vector(VECSXP, length, pc)
    }

    /// Define a new error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use `stop!`.
    ///
    pub fn new_error(message: &str, pc: &mut Pc) -> RObject<Vector, List> {
        let mut list = Self::new_list(2, pc);
        let _ = list.set(0, &message.to_r(pc));
        let _ = list.set(1, &Self::null());
        let _ = list.set_names(&["message", "calls"].to_r(pc));
        list.set_class(&["error", "condition"].to_r(pc));
        list.convert()
    }

    /// Define a new symbol.
    pub fn new_symbol(x: &str, pc: &mut Pc) -> RObject<Symbol, ()> {
        let sexp = pc.protect(unsafe {
            Rf_mkCharLenCE(
                x.as_ptr() as *const c_char,
                x.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            )
        });
        Self::wrap(pc.protect(unsafe { Rf_installChar(sexp) }))
    }

    /// Get R's "dim" symbol.
    pub fn symbol_dim() -> RObject<Symbol, ()> {
        R::wrap(unsafe { R_DimSymbol })
    }

    /// Get R's "names" symbol.
    pub fn symbol_names() -> RObject<Symbol, ()> {
        R::wrap(unsafe { R_NamesSymbol })
    }

    /// Get R's "rownames" symbol.
    pub fn symbol_rownames() -> RObject<Symbol, ()> {
        R::wrap(unsafe { R_RowNamesSymbol })
    }

    /// Get R's "dimnames" symbol.
    pub fn symbol_dimnames() -> RObject<Symbol, ()> {
        R::wrap(unsafe { R_DimNamesSymbol })
    }

    /// Get R's "class" symbol.
    pub fn symbol_class() -> RObject<Symbol, ()> {
        R::wrap(unsafe { R_ClassSymbol })
    }

    /// Move Rust object to an R external pointer.
    ///
    /// This *method* moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RObject::decode_as_val`].
    ///
    pub fn encode<T, RType, RMode>(
        x: T,
        tag: &RObject<RType, RMode>,
        managed_by_r: bool,
        pc: &mut Pc,
    ) -> RObject<ExternalPtr, ()> {
        unsafe {
            let ptr = Box::into_raw(Box::new(x));
            let sexp = pc.protect(R_MakeExternalPtr(ptr as *mut c_void, tag.sexp, R_NilValue));
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
            Self::wrap(sexp)
        }
    }

    /// Returns an R NULL value.
    pub fn null() -> RObject {
        Self::wrap(unsafe { R_NilValue })
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
pub struct RObject<RType = AnyType, RMode = Unknown> {
    pub sexp: SEXP,
    rtype: PhantomData<(RType, RMode)>,
}

impl<RType, RMode> RObject<RType, RMode> {
    fn convert<RTypeTo, RModeTo>(&self) -> RObject<RTypeTo, RModeTo> {
        R::wrap(self.sexp)
    }

    /// Duplicate an object.
    ///
    /// Multiple symbols may be bound to the same object, so if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy first.
    /// E.g, call this method on arguments pass via `.Call` before modifying them.
    ///
    pub fn duplicate(&self, pc: &mut Pc) -> Self {
        R::wrap(pc.protect(unsafe { Rf_duplicate(self.sexp) }))
    }

    /// Duplicate an object.
    ///
    /// Multiple symbols may be bound to the same object, so if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy first.
    /// E.g, call this method on arguments pass via `.Call` before modifying them.
    ///
    pub fn clone(&self, pc: &mut Pc) -> Self {
        R::wrap(pc.protect(unsafe { Rf_duplicate(self.sexp) }))
    }

    /// Recharacterize an RObject<RType, RMode> as an RObject (i.e., an RObject<AnyType, Unknown>).
    pub fn unknown(self) -> RObject {
        self.convert()
    }

    /// Returns the result of the is_null method, but as an Option value.
    pub fn option(self) -> Option<Self> {
        if self.is_null() {
            None
        } else {
            Some(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<Vector, Unknown>.
    /// Checks using R's `Rf_isVectorAtomic` function.
    pub fn vector(self) -> Result<RObject<Vector, Unknown>, Self> {
        if unsafe { Rf_isVectorAtomic(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<Matrix, Unknown>.
    /// Checks using R's `Rf_isMatrix` function.
    pub fn matrix(self) -> Result<RObject<Matrix, Unknown>, Self> {
        if unsafe { Rf_isMatrix(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<Array, Unknown>.
    /// Checks using R's `Rf_isArray` function.
    pub fn array(self) -> Result<RObject<Array, Unknown>, Self> {
        if unsafe { Rf_isArray(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<Vector, List>.
    /// Checks using R's `Rf_isVectorList` function.
    pub fn list(self) -> Result<RObject<Vector, List>, Self> {
        if unsafe { Rf_isVectorList(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<Vector, DataFrame>.
    /// Checks using R's `Rf_isFrame` function.
    pub fn data_frame(self) -> Result<RObject<Vector, DataFrame>, Self> {
        if unsafe { Rf_isFrame(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<Function, ()>.
    /// Checks using R's `Rf_isFunction` function.
    pub fn function(self) -> Result<RObject<Function, ()>, Self> {
        if unsafe { Rf_isFunction(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an RObject<ExternalPtr, ()>.
    /// Uses the SEXP type to determine if this is possible.
    pub fn external_ptr(self) -> Result<RObject<ExternalPtr, ()>, Self> {
        if unsafe { TYPEOF(self.sexp) == EXTPTRSXP as i32 } {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize as an f64.
    pub fn f64(&self) -> Result<f64, &'static str> {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                Ok(unsafe { Rf_asReal(s.sexp) })
            } else {
                Err("Not a scalar")
            }
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as an i32.
    pub fn i32(&self) -> Result<i32, &'static str> {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                if s.is_integer() {
                    let x = unsafe { Rf_asInteger(s.sexp) };
                    if x == i32::MIN {
                        Err("Equals NA")
                    } else {
                        Ok(x)
                    }
                } else if s.is_double() {
                    let y = unsafe { Rf_asReal(s.sexp) };
                    if y > f64::from(i32::MAX) || y <= f64::from(i32::MIN) || y.is_nan() {
                        Err("Greater than maximum i32, equals NA, less than minimum i32, or equals NaN")
                    } else {
                        Ok(y.round() as i32)
                    }
                } else if s.is_raw() {
                    Ok(unsafe { Rf_asInteger(s.sexp) })
                } else if s.is_logical() {
                    let y = unsafe { Rf_asLogical(s.sexp) };
                    if y == i32::MIN {
                        Err("Equals NA")
                    } else {
                        Ok(y)
                    }
                } else {
                    Err("Unsupported type")
                }
            } else {
                Err("Not a scalar")
            }
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as a usize.
    pub fn usize(&self) -> Result<usize, &'static str> {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                if s.is_integer() {
                    let x = unsafe { Rf_asInteger(s.sexp) };
                    usize::try_from(x).map_err(|_| "Cannot map to usize")
                } else if s.is_double() {
                    let y = unsafe { Rf_asReal(s.sexp) };
                    let z = y as usize;
                    if z as f64 == y {
                        Ok(z)
                    } else {
                        Err("Conversion error for usize")
                    }
                } else if s.is_raw() {
                    let x = unsafe { Rf_asInteger(s.sexp) };
                    usize::try_from(x).map_err(|_| "Cannot map to usize")
                } else if s.is_logical() {
                    let x = unsafe { Rf_asLogical(s.sexp) };
                    if x == i32::MIN {
                        Err("Equals NA")
                    } else {
                        usize::try_from(x).map_err(|_| "Cannot map to usize")
                    }
                } else {
                    Err("Unsupported type")
                }
            } else {
                Err("Not a scalar")
            }
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as a u8.
    pub fn u8(&self) -> Result<u8, &'static str> {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                if s.is_integer() {
                    let x = unsafe { Rf_asInteger(s.sexp) };
                    u8::try_from(x).map_err(|_| "Cannot map to u8")
                } else if s.is_double() {
                    let y = unsafe { Rf_asReal(s.sexp) };
                    let z = y as u8;
                    if z as f64 == y {
                        Ok(z)
                    } else {
                        Err("Conversion error to u8")
                    }
                } else if s.is_raw() {
                    let x = unsafe { Rf_asInteger(s.sexp) };
                    u8::try_from(x).map_err(|_| "Cannot map to u8")
                } else if s.is_logical() {
                    let x = unsafe { Rf_asLogical(s.sexp) };
                    if x == i32::MIN {
                        Err("Equals R's NA for bool")
                    } else {
                        u8::try_from(x).map_err(|_| "Cannot map to usize")
                    }
                } else {
                    Err("Unsupported type")
                }
            } else {
                Err("Not a scalar")
            }
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as a bool.
    pub fn bool(&self) -> Result<bool, &'static str> {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                if s.is_integer() {
                    let x = unsafe { Rf_asInteger(s.sexp) };
                    if x == i32::MIN {
                        Err("Equals NA")
                    } else {
                        Ok(x != 0)
                    }
                } else if s.is_double() {
                    let y = unsafe { Rf_asReal(s.sexp) };
                    if R::is_na_double(y) || R::is_nan(y) {
                        Err("Equal NA or NaN")
                    } else {
                        Ok(y != 0.0)
                    }
                } else if s.is_raw() {
                    Ok(unsafe { Rf_asInteger(s.sexp) } != 0)
                } else if s.is_logical() {
                    let y = unsafe { Rf_asLogical(s.sexp) };
                    if y == i32::MIN {
                        Err("Equals NA")
                    } else {
                        Ok(y != 0)
                    }
                } else {
                    Err("Unsupported type")
                }
            } else {
                Err("Not a scalar")
            }
        } else {
            Err("Not a vector")
        }
    }

    /// Check if appropriate to characterize as a str reference.
    pub fn to_str(self, pc: &mut Pc) -> Result<&str, Self> {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                s.to_character(pc).get(0).map_err(|_| self)
            } else {
                Err(self)
            }
        } else {
            Err(self)
        }
    }

    /// Check if RObject can be interpreted as the NULL value in R.
    pub fn is_null(&self) -> bool {
        unsafe { Rf_isNull(self.sexp) != 0 }
    }

    /// Check if RObject can be interpreted as an NA value in R.
    pub fn is_na(&self) -> bool {
        if self.is_vector() {
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() {
                if s.is_double() {
                    unsafe { R_IsNA(Rf_asReal(s.sexp)) != 0 }
                } else if s.is_integer() {
                    unsafe { Rf_asInteger(s.sexp) == R::na_integer() }
                } else if s.is_logical() {
                    unsafe { Rf_asLogical(s.sexp) == R::na_logical() }
                } else if s.is_character() {
                    unsafe { Rf_asChar(s.sexp) == R_NaString }
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
            let s: RObject<Vector, Unknown> = self.convert();
            if s.is_scalar() && s.is_double() {
                unsafe { R_IsNaN(Rf_asReal(s.sexp)) != 0 }
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn is_vector(&self) -> bool {
        unsafe { Rf_isVectorAtomic(self.sexp) != 0 }
    }

    pub fn is_matrix(&self) -> bool {
        unsafe { Rf_isMatrix(self.sexp) != 0 }
    }

    pub fn is_array(&self) -> bool {
        unsafe { Rf_isArray(self.sexp) != 0 }
    }

    /// Get the class or classes of the data in an RObject.
    pub fn get_class(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R::symbol_class().sexp) })
    }

    /// Get an attribute.
    pub fn get_attribute(&self, which: RObject<Symbol, ()>) -> RObject<AnyType, Unknown> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, which.sexp) })
    }
}

impl<RType, RMode> RObject<RType, RMode> {
    /// Set the class or classes of the data for an RObject.
    pub fn set_class(&mut self, names: &RObject<Vector, Character>) {
        unsafe {
            Rf_classgets(self.sexp, names.sexp);
        }
    }

    /// Set an attribute.
    pub fn set_attribute<RTypeValue, RModeValue>(
        &mut self,
        which: &RObject<Symbol, ()>,
        value: &RObject<RTypeValue, RModeValue>,
    ) {
        unsafe {
            Rf_setAttrib(self.sexp, which.sexp, value.sexp);
        }
    }
}

impl<RType: HasLength, RMode> RObject<RType, RMode> {
    /// Returns the length of the RObject.
    pub fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.sexp) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    /// Checks to see if the RObject is empty.
    pub fn is_empty(&self) -> bool {
        unsafe { Rf_xlength(self.sexp) == 0 }
    }

    /// Checks to see if the RObject is a scalar (has a length of 1).
    pub fn is_scalar(&self) -> bool {
        unsafe { Rf_xlength(self.sexp) == 1 }
    }
}

impl<RType: HasLength, RMode: Atomic> RObject<RType, RMode> {
    fn slice_mut_engine<U>(&mut self, data: *mut U) -> &mut [U] {
        unsafe { std::slice::from_raw_parts_mut(data, self.len()) }
    }
}

impl<RType: HasLength, RMode: Atomic> RObject<RType, RMode> {
    fn slice_engine<V>(&self, data: *mut V) -> &[V] {
        unsafe { std::slice::from_raw_parts_mut(data, self.len()) }
    }

    /// Check if appropriate to characterize storage mode as "double".
    pub fn double(self) -> Result<RObject<RType, f64>, Self> {
        if self.is_double() {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize storage mode as "integer".
    pub fn integer(self) -> Result<RObject<RType, i32>, Self> {
        if self.is_integer() {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize storage mode as "raw".
    pub fn raw(self) -> Result<RObject<RType, u8>, Self> {
        if self.is_raw() {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize storage mode as "logical".
    pub fn logical(self) -> Result<RObject<RType, bool>, Self> {
        if self.is_logical() {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Check if appropriate to characterize storage mode as "character".
    pub fn character(self) -> Result<RObject<RType, Character>, Self> {
        if self.is_character() {
            Ok(self.convert())
        } else {
            Err(self)
        }
    }

    /// Checks to see if the data can be interpreted as R double.
    pub fn is_double(&self) -> bool {
        unsafe { Rf_isReal(self.sexp) != 0 }
    }

    /// Checks to see if the data can be interpreted as R integer.
    pub fn is_integer(&self) -> bool {
        unsafe { Rf_isInteger(self.sexp) != 0 }
    }

    /// Checks to see if the data can be interpreted as R raw.
    pub fn is_raw(&self) -> bool {
        unsafe { TYPEOF(self.sexp) == RAWSXP as i32 }
    }

    /// Checks to see if the data can be interpreted as R logical.
    pub fn is_logical(&self) -> bool {
        unsafe { Rf_isLogical(self.sexp) != 0 }
    }

    /// Checks to see if the data can be interpreted as R character.
    pub fn is_character(&self) -> bool {
        unsafe { Rf_isString(self.sexp) != 0 }
    }

    /// Attempts to coerce storage mode to "double".
    pub fn to_double(&self, pc: &mut Pc) -> RObject<RType, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    /// Attempts to coerce storage mode to "integer".
    pub fn to_integer(&self, pc: &mut Pc) -> RObject<RType, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    /// Attempts to coerce storage mode to "raw".
    pub fn to_raw(&self, pc: &mut Pc) -> RObject<RType, u8> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, RAWSXP) }))
    }

    /// Attempts to coerce storage mode to "logical".
    pub fn to_logical(&self, pc: &mut Pc) -> RObject<RType, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
    }

    /// Attempts to coerce storage mode to "character".
    pub fn to_character(&self, pc: &mut Pc) -> RObject<RType, Character> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, STRSXP) }))
    }
}

impl<RType: HasLength> RObject<RType, f64> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[f64] {
        self.slice_engine(unsafe { REAL(self.sexp) })
    }

    /// Returns a slice of the data structure with a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_static(&self) -> &'static [f64] {
        std::slice::from_raw_parts_mut(REAL(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, f64> {
    /// Returns a mutable slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [f64] {
        self.slice_mut_engine(unsafe { REAL(self.sexp) })
    }

    /// Returns a mutable slice of the data structure with a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_mut_static(&self) -> &'static mut [f64] {
        std::slice::from_raw_parts_mut(REAL(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, i32> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[i32] {
        self.slice_engine(unsafe { INTEGER(self.sexp) })
    }

    /// Returns a slice of the data structure, pretending a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_static(&self) -> &'static [i32] {
        std::slice::from_raw_parts_mut(INTEGER(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, i32> {
    /// Returns a mutable slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [i32] {
        self.slice_mut_engine(unsafe { INTEGER(self.sexp) })
    }

    /// Returns a mutable slice of the data structure, pretending a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_mut_static(self) -> &'static mut [i32] {
        std::slice::from_raw_parts_mut(INTEGER(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, u8> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[u8] {
        self.slice_engine(unsafe { RAW(self.sexp) })
    }

    /// Returns a slice of the data structure, pretending a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_static(&self) -> &'static [u8] {
        std::slice::from_raw_parts_mut(RAW(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, u8> {
    /// Returns a mutable slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [u8] {
        self.slice_mut_engine(unsafe { RAW(self.sexp) })
    }

    /// Returns a mutable slice of the data structure, pretending a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_mut_static(&self) -> &'static mut [u8] {
        std::slice::from_raw_parts_mut(RAW(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, bool> {
    /// Returns a slice of the data structure.
    pub fn slice(&self) -> &[i32] {
        self.slice_engine(unsafe { LOGICAL(self.sexp) })
    }

    /// Returns a slice of the data structure, pretending a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_static(&self) -> &'static [i32] {
        std::slice::from_raw_parts_mut(LOGICAL(self.sexp), self.len())
    }
}

impl<RType: HasLength> RObject<RType, bool> {
    /// Returns a mutable slice of the data structure.
    pub fn slice_mut(&mut self) -> &mut [i32] {
        self.slice_mut_engine(unsafe { LOGICAL(self.sexp) })
    }

    /// Returns a mutable slice of the data structure, pretending a static lifetime.
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn slice_mut_static(&self) -> &'static mut [i32] {
        std::slice::from_raw_parts_mut(LOGICAL(self.sexp), self.len())
    }
}

impl<RMode> RObject<Matrix, RMode> {
    /// Returns the number of rows in the Matrix.
    pub fn nrow(&self) -> usize {
        unsafe { Rf_nrows(self.sexp).try_into().unwrap() }
    }

    /// Returns the number of columns in the Matrix.
    pub fn ncol(&self) -> usize {
        unsafe { Rf_ncols(self.sexp).try_into().unwrap() }
    }

    /// Returns the dimensions of the Matrix.
    pub fn dim(&self) -> [usize; 2] {
        [self.nrow(), self.ncol()]
    }

    /// Transpose the matrix.
    pub fn transpose(&self, pc: &mut Pc) -> RObject<Matrix, RMode> {
        let mut transposed = self.duplicate(pc);
        let mut dim: RObject<Vector, i32> =
            self.get_attribute(R::symbol_dim()).duplicate(pc).convert();
        let slice = dim.slice_mut();
        slice.swap(0, 1);
        transposed.set_attribute(&R::symbol_dim(), &dim);
        unsafe { Rf_copyMatrix(transposed.sexp, self.sexp, Rboolean_TRUE) };
        transposed
    }

    /// Manipulates the matrix in place to be a vector by dropping the `dim` attribute.
    pub fn to_vector(&self) -> RObject<Vector, RMode> {
        unsafe { Rf_setAttrib(self.sexp, R_DimSymbol, R_NilValue) };
        self.convert()
    }
}

impl<RType> RObject<Array, RType> {
    /// Returns the dimensions of the Array.
    pub fn dim(&self) -> Vec<usize> {
        let d = R::wrap::<Vector, i32>(unsafe { Rf_getAttrib(self.sexp, R_DimSymbol) });
        d.slice().iter().map(|&x| x.try_into().unwrap()).collect()
    }

    // Create a new vector from a matrix.
    /// Convert an Array to a Vector.
    pub fn to_vector(&self) -> RObject<Vector, RType> {
        unsafe { Rf_setAttrib(self.sexp, R_DimSymbol, R_NilValue) };
        self.convert()
    }
}

impl RObject<Function, ()> {
    fn eval(expression: SEXP, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = pc.protect(expression);
        let mut p_out_error: i32 = 0;
        let sexp = pc.protect(unsafe {
            R_tryEval(expression, R_GetCurrentEnv(), &mut p_out_error as *mut i32)
        });
        match p_out_error {
            0 => {
                let robject = RObject {
                    sexp,
                    rtype: PhantomData,
                };
                Ok(robject)
            }
            e => Err(e),
        }
    }

    /// Evaluate a function with 0 parameters.
    pub fn call0(&self, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang1(self.sexp) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 1 parameter.
    pub fn call1<T1, M1>(&self, arg1: &RObject<T1, M1>, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang2(self.sexp, arg1.sexp) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 2 parameters.
    pub fn call2<T1, M1, T2, M2>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang3(self.sexp, arg1.sexp, arg2.sexp) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 3 parameters.
    pub fn call3<T1, M1, T2, M2, T3, M3>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T3, M3>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang4(self.sexp, arg1.sexp, arg2.sexp, arg3.sexp) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 4 parameters.
    pub fn call4<T1, M1, T2, M2, T3, M3, T4, M4>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T3, M3>,
        arg4: &RObject<T4, M4>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang5(self.sexp, arg1.sexp, arg2.sexp, arg3.sexp, arg4.sexp) };
        Self::eval(expression, pc)
    }

    /// Evaluate a function with 5 parameters.
    pub fn call5<T1, M1, T2, M2, T3, M3, T4, M4, T5, M5>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T3, M3>,
        arg4: &RObject<T4, M4>,
        arg5: &RObject<T5, M5>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe {
            Rf_lang6(
                self.sexp, arg1.sexp, arg2.sexp, arg3.sexp, arg4.sexp, arg5.sexp,
            )
        };
        Self::eval(expression, pc)
    }
}

impl<RMode> RObject<Vector, RMode> {
    fn get_engine<T>(
        &self,
        index: usize,
        f: unsafe extern "C" fn(SEXP, isize) -> T,
    ) -> Result<T, &'static str> {
        if index < self.len() {
            Ok(unsafe { f(self.sexp, index.try_into().unwrap()) })
        } else {
            Err("Index out of bounds")
        }
    }

    /// Get names of values in a Vector.
    pub fn get_names(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_NamesSymbol) })
    }
}

impl<RMode> RObject<Vector, RMode> {
    fn set_engine<T>(
        &mut self,
        index: usize,
        value: T,
        f: unsafe extern "C" fn(SEXP, isize, T),
    ) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { f(self.sexp, index.try_into().unwrap(), value) }
            Ok(())
        } else {
            Err("Index out of bounds")
        }
    }
}

impl<RMode> RObject<Vector, RMode> {
    /// Set names of values in a Vector.
    pub fn set_names(&mut self, names: &RObject<Vector, Character>) -> Result<(), &'static str> {
        if unsafe { Rf_length(names.sexp) != Rf_length(self.sexp) } {
            return Err("Length of names is not correct");
        }
        unsafe {
            Rf_namesgets(self.sexp, names.sexp);
        }
        Ok(())
    }
}

impl RObject<Vector, f64> {
    /// Get the value at a certain index in an f64 Vector.
    pub fn get(&self, index: usize) -> Result<f64, &'static str> {
        self.get_engine(index, REAL_ELT)
    }
}

impl RObject<Vector, f64> {
    /// Set the value at a certain index in an f64 Vector.
    pub fn set(&mut self, index: usize, value: f64) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_REAL_ELT)
    }
}

impl RObject<Vector, i32> {
    /// Get the value at a certain index in an i32 Vector.
    pub fn get(&self, index: usize) -> Result<i32, &'static str> {
        self.get_engine(index, INTEGER_ELT)
    }
}

impl RObject<Vector, i32> {
    /// Set the value at a certain index in an i32 Vector.
    pub fn set(&mut self, index: usize, value: i32) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_INTEGER_ELT)
    }
}

impl RObject<Vector, u8> {
    /// Get the value at a certain index in a u8 Vector.
    pub fn get(&self, index: usize) -> Result<u8, &'static str> {
        self.get_engine(index, RAW_ELT)
    }
}

impl RObject<Vector, u8> {
    /// Set the value at a certain index in a u8 Vector.
    pub fn set(&mut self, index: usize, value: u8) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_RAW_ELT)
    }
}

impl RObject<Vector, bool> {
    /// Get the value at a certain index in a logical Vector.
    pub fn get(&self, index: usize) -> Result<bool, &'static str> {
        self.get_engine(index, LOGICAL_ELT).map(|x| x != 0)
    }

    /// Get the value at a certain index in a logical Vector as an i32.
    pub fn get_i32(&self, index: usize) -> Result<i32, &'static str> {
        self.get_engine(index, LOGICAL_ELT)
    }
}

impl RObject<Vector, bool> {
    /// Set the value at a certain index in a logical Vector.
    pub fn set(&mut self, index: usize, value: bool) -> Result<(), &'static str> {
        let value = if value {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }

    /// Set the value at certain index in a logical Vector with an i32.
    pub fn set_i32(&mut self, index: usize, value: i32) -> Result<(), &'static str> {
        let value = if value != 0 {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }
}

impl RObject<Vector, Character> {
    /// Get the value at a certain index in a character Vector.
    pub fn get<'a>(&self, index: usize) -> Result<&'a str, &'static str> {
        match self.get_engine(index, STRING_ELT) {
            Ok(sexp) => {
                let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
                c_str.to_str().map_err(|_| "Not valid UTF8")
            }
            Err(e) => Err(e),
        }
    }
}

impl RObject<Vector, Character> {
    /// Set the value at a certain index in a character Vector.
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

    /// Set the value at a certain index in a character Vector to NA.
    pub fn set_na(&mut self, index: usize) {
        unsafe {
            SET_STRING_ELT(self.sexp, index.try_into().unwrap(), R_NaString);
        }
    }
}

pub struct RListMap<'a> {
    unused_counter: usize,
    used: Vec<bool>,
    robj: &'a RObject<Vector, List>,
    map: HashMap<&'a str, usize>,
}

impl RListMap<'_> {
    /// Find an RObject in the list based on its name.
    pub fn get(&mut self, name: &str) -> Result<RObject, String> {
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

impl RObject<Vector, List> {
    /// Get the value at a certain index in a List.
    pub fn get(&self, index: usize) -> Result<RObject, &'static str> {
        self.get_engine(index, VECTOR_ELT).map(R::wrap)
    }

    /// Get a value from the List based on its key.
    pub fn get_by_key(&self, key: impl AsRef<str>) -> Result<RObject, String> {
        let names = self.get_names();
        for i in 0..names.len() {
            if names.get(i).unwrap() == key.as_ref() {
                return Ok(self.get(i)?);
            }
        }
        Err(format!("Could not find '{}' in the list", key.as_ref()))
    }

    /// Convert the list into an [RListMap]
    ///
    /// This allows Rust HashMap methods to be used on the contents
    /// of the list, while still retaining the original List within
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
}

impl RObject<Vector, List> {
    /// Set the value at a certain index in a List.
    pub fn set<RType, RMode>(
        &mut self,
        index: usize,
        value: &RObject<RType, RMode>,
    ) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { SET_VECTOR_ELT(self.sexp, index.try_into().unwrap(), value.sexp) };
            Ok(())
        } else {
            Err("Index out of bounds")
        }
    }

    /// Convert a List to a DataFrame.
    pub fn to_data_frame(
        &mut self,
        names: RObject<Vector, Character>,
        rownames: RObject<Vector, Character>,
        pc: &mut Pc,
    ) -> Result<RObject<Vector, DataFrame>, &'static str> {
        if names.len() != self.len() {
            return Err("Length of names is not correct");
        }
        let mut nrow = -1;
        for i in 0..self.len() {
            let x = self.get(i).unwrap();
            if unsafe { Rf_isVectorAtomic(x.sexp) == 0 } {
                return Err("Expected an atomic vector... Have you set the list elements yet?");
            }
            let len = unsafe { Rf_xlength(x.sexp) };
            if i == 0 {
                nrow = len;
            } else if len != nrow {
                return Err("Inconsistent number of rows among list elements");
            }
        }
        if rownames.len() != nrow as usize {
            return Err("Length of row names is not correct");
        }
        self.set_names(&names)?;
        unsafe { Rf_setAttrib(self.sexp, R_RowNamesSymbol, rownames.sexp) };
        self.set_class(&["data.frame"].to_r(pc));
        Ok(self.convert())
    }
}

impl RObject<Vector, DataFrame> {
    /// Get the value at a certain index in a DataFrame.
    pub fn get(&self, index: usize) -> Result<RObject, &'static str> {
        self.convert::<Vector, List>().get(index)
    }

    /// Get the row names of a DataFrame.
    pub fn get_rownames(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_RowNamesSymbol) })
    }
}

impl RObject<Vector, DataFrame> {
    /// Set the value at a certain index in a DataFrame.
    pub fn set<RType, RMode>(
        &mut self,
        index: usize,
        value: &RObject<RType, RMode>,
    ) -> Result<(), &'static str> {
        self.convert::<Vector, List>().set(index, value)
    }

    /// Set the row names of a DataFrame.
    pub fn set_rownames(
        &mut self,
        rownames: &RObject<Vector, Character>,
    ) -> Result<(), &'static str> {
        if unsafe { Rf_length(rownames.sexp) != Rf_length(self.sexp) } {
            return Err("Length of row names is not correct");
        }
        unsafe { Rf_setAttrib(self.sexp, R_RowNamesSymbol, rownames.sexp) };
        Ok(())
    }
}

impl<RMode> RObject<Matrix, RMode> {
    /// Get the index of a value based on the row and column number.
    pub fn index(&self, (i, j): (usize, usize)) -> usize {
        let nrow = self.nrow();
        nrow * j + i
    }

    /// Get the dimnames of a matrix.
    pub fn get_dimnames(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_DimNamesSymbol) })
    }

    /// Set the dimnames of a matrix.
    pub fn set_dimnames(&mut self, dimnames: &RObject<Vector, List>) -> Result<(), &'static str> {
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
            Rf_dimnamesgets(self.sexp, dimnames.sexp);
        }
        Ok(())
    }
}

impl RObject<Matrix, f64> {
    /// Get the value at a certain index in a double Matrix.
    pub fn get(&self, index: (usize, usize)) -> Result<f64, &'static str> {
        self.convert::<Vector, f64>().get(self.index(index))
    }
}

impl RObject<Matrix, f64> {
    /// Set the value at a certain index in a double Matrix.
    pub fn set(&mut self, index: (usize, usize), value: f64) -> Result<(), &'static str> {
        self.convert::<Vector, f64>().set(self.index(index), value)
    }
}

impl RObject<Matrix, i32> {
    /// Get the value at a certain index in an integer Matrix.
    pub fn get(&self, index: (usize, usize)) -> Result<i32, &'static str> {
        self.convert::<Vector, i32>().get(self.index(index))
    }
}

impl RObject<Matrix, i32> {
    /// Set the value at a certain index in an integer Matrix.
    pub fn set(&mut self, index: (usize, usize), value: i32) -> Result<(), &'static str> {
        self.convert::<Vector, i32>().set(self.index(index), value)
    }
}

impl RObject<Matrix, u8> {
    /// Get the value at a certain index in a raw Matrix.
    pub fn get(&self, index: (usize, usize)) -> Result<u8, &'static str> {
        self.convert::<Vector, u8>().get(self.index(index))
    }
}

impl RObject<Matrix, u8> {
    /// Set the value at a certain index in a raw Matrix.
    pub fn set(&mut self, index: (usize, usize), value: u8) -> Result<(), &'static str> {
        self.convert::<Vector, u8>().set(self.index(index), value)
    }
}

impl RObject<Matrix, bool> {
    /// Get the value at a certain index in a logical Matrix.
    pub fn get(&self, index: (usize, usize)) -> Result<bool, &'static str> {
        self.convert::<Vector, bool>().get(self.index(index))
    }

    /// Get the value at a certain index in a logical Matrix as an i32.
    pub fn get_i32(&self, index: (usize, usize)) -> Result<i32, &'static str> {
        self.convert::<Vector, bool>().get_i32(self.index(index))
    }
}

impl RObject<Matrix, bool> {
    /// Set the value at a certain index in a logical Matrix.
    pub fn set(&mut self, index: (usize, usize), value: bool) -> Result<(), &'static str> {
        self.convert::<Vector, bool>().set(self.index(index), value)
    }

    /// Set the value at a certain index in a logical Matrix an an i32.
    pub fn set_i32(&mut self, index: (usize, usize), value: i32) -> Result<(), &'static str> {
        self.convert::<Vector, bool>()
            .set_i32(self.index(index), value)
    }
}

impl RObject<Matrix, Character> {
    /// Get the value at a certain index in a character Matrix.
    pub fn get(&self, index: (usize, usize)) -> Result<&str, &'static str> {
        self.convert::<Vector, Character>().get(self.index(index))
    }
}

impl RObject<Matrix, Character> {
    /// Set the value at a certain index in a character Matrix.
    pub fn set<RType, RMode>(
        &mut self,
        index: (usize, usize),
        value: &str,
    ) -> Result<(), &'static str> {
        self.convert::<Vector, Character>()
            .set(self.index(index), value)
    }
}

impl RObject<ExternalPtr, ()> {
    /// Check if an external pointer is managed by R.
    pub fn is_managed_by_r(&self) -> bool {
        unsafe { Rf_getAttrib(self.sexp, R_AtsignSymbol) == R_AtsignSymbol }
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
            let addr = R_ExternalPtrAddr(self.sexp);
            if addr.as_ref().is_none() {
                return Err("External pointer was already decoded by value");
            }
            R_ClearExternalPtr(self.sexp);
            Ok(*Box::from_raw(addr as *mut T))
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer.
    ///
    /// This method obtains a reference to a Rust object from an R external pointer created by [`Self::as_external_ptr`].
    ///
    pub fn decode_ref<T>(&self) -> &T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
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
        let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
        ptr.as_ref().unwrap()
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer.
    ///
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`Self::as_external_ptr`].
    ///
    pub fn decode_mut<T>(&mut self) -> &mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer, pretending a static lifetime.
    ///
    /// This method obtains a mutable reference to a Rust object from an R external pointer created by [`Self::as_external_ptr`].
    ///
    /// # Safety
    ///
    /// Despite the use of a static lifetime here, the reference is only valid as long as R's
    /// garbage collector has not reclaimed the underlying object's memory.
    pub unsafe fn decode_mut_static<T>(&mut self) -> &'static mut T {
        let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
        ptr.as_mut().unwrap()
    }

    /// Get the memory address of the external pointer.
    pub fn address(&self) -> *mut c_void {
        unsafe { R_ExternalPtrAddr(self.sexp) }
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
            R_RegisterCFinalizerEx(self.sexp, Some(func), 0);
            Ok(())
        }
    }

    /// Get tag for an R external pointer.
    ///
    /// This method gets the tag associated with an R external pointer, which was set by [`Self::as_external_ptr`].
    ///
    pub fn tag(&self) -> RObject {
        R::wrap(unsafe { R_ExternalPtrTag(self.sexp) })
    }
}

// Conversions

pub trait FromR<RType, RMode, U> {
    fn from_r(x: RObject<RType, RMode>, pc: &mut Pc) -> Result<Self, U>
    where
        Self: Sized;
}

/// Trait for converting objects to RObjects.
///
/// The traits [ToR2], [ToR3], and [ToR4] are all identical to this trait.
/// This was done to avoid conflicting trait implementations.
pub trait ToR1<RType, RMode> {
    fn to_r(&self, pc: &mut Pc) -> RObject<RType, RMode>;
}

/// Trait for converting objects to RObjects.
///
/// See [ToR1].
pub trait ToR2<RType, RMode> {
    fn to_r(self, pc: &mut Pc) -> RObject<RType, RMode>;
}

/// Trait for converting objects to RObjects.
///
/// See [ToR1].
pub trait ToR3<RType, RMode> {
    fn to_r(self, pc: &mut Pc) -> RObject<RType, RMode>;
}

/// Trait for converting objects to RObjects.
///
/// See [ToR1].
pub trait ToR4<RType, RMode> {
    fn to_r(self, pc: &mut Pc) -> RObject<RType, RMode>;
}

// f64

impl ToR1<Vector, f64> for f64 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_ScalarReal(*self) }))
    }
}

impl<const N: usize> ToR1<Vector, f64> for [f64; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, f64> for &[f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let mut result = R::new_vector_double(self.len(), pc);
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<Vector, f64> for &mut [f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let mut result = R::new_vector_double(self.len(), pc);
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a f64> + ExactSizeIterator> ToR2<Vector, f64> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let mut result = R::new_vector_double(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut f64> + ExactSizeIterator> ToR3<Vector, f64> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let mut result = R::new_vector_double(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = f64> + ExactSizeIterator> ToR4<Vector, f64> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let mut result = R::new_vector_double(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// i32

impl ToR1<Vector, i32> for i32 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_ScalarInteger(*self) }))
    }
}

impl<const N: usize> ToR1<Vector, i32> for [i32; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, i32> for &[i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<Vector, i32> for &mut [i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a i32> + ExactSizeIterator> ToR2<Vector, i32> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut i32> + ExactSizeIterator> ToR3<Vector, i32> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = i32> + ExactSizeIterator> ToR4<Vector, i32> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// usize

impl ToR1<Vector, i32> for usize {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_ScalarInteger((*self).try_into().unwrap()) }))
    }
}

impl<const N: usize> ToR1<Vector, i32> for [usize; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, i32> for &[usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl ToR1<Vector, i32> for &mut [usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let mut result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

// u8

impl ToR1<Vector, u8> for u8 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        R::wrap(pc.protect(unsafe { Rf_ScalarRaw(*self) }))
    }
}

impl<const N: usize> ToR1<Vector, u8> for [u8; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, u8> for &[u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let mut result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<Vector, u8> for &mut [u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let mut result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice_mut();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a u8> + ExactSizeIterator> ToR2<Vector, u8> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let mut result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut u8> + ExactSizeIterator> ToR3<Vector, u8> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let mut result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = u8> + ExactSizeIterator> ToR4<Vector, u8> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let mut result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice_mut();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// bool

impl ToR1<Vector, bool> for bool {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        R::wrap(pc.protect(unsafe {
            Rf_ScalarLogical(if *self {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            })
        }))
    }
}

impl<const N: usize> ToR1<Vector, bool> for [bool; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, bool> for &[bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let mut result = R::new_vector_logical(self.len(), pc);
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).into();
        }
        result
    }
}

impl ToR1<Vector, bool> for &mut [bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let mut result = R::new_vector_logical(self.len(), pc);
        let slice = result.slice_mut();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).into();
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a bool> + ExactSizeIterator> ToR2<Vector, bool> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let mut result = R::new_vector_logical(self.len(), pc);
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

impl<'a, T: IntoIterator<Item = &'a mut bool> + ExactSizeIterator> ToR3<Vector, bool> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let mut result = R::new_vector_logical(self.len(), pc);
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

impl<T: IntoIterator<Item = bool> + ExactSizeIterator> ToR4<Vector, bool> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let mut result = R::new_vector_logical(self.len(), pc);
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

impl ToR1<Vector, Character> for &str {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        let sexp = unsafe {
            Rf_ScalarString(Rf_mkCharLenCE(
                self.as_ptr() as *const c_char,
                self.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            ))
        };
        R::wrap(pc.protect(sexp))
    }
}

impl<const N: usize> ToR1<Vector, Character> for [&str; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, Character> for &[&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        let mut result = R::new_vector_character(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

impl ToR1<Vector, Character> for &mut [&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        let mut result = R::new_vector_character(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

// RObject and SEXP

impl<RType, RMode> ToR1<RType, RMode> for RObject<RType, RMode> {
    fn to_r(&self, _pc: &mut Pc) -> RObject<RType, RMode> {
        self.convert()
    }
}

impl ToR1<AnyType, Unknown> for SEXP {
    fn to_r(&self, _pc: &mut Pc) -> RObject<AnyType, Unknown> {
        R::wrap(*self)
    }
}

impl ToR1<AnyType, Unknown> for () {
    fn to_r(&self, _pc: &mut Pc) -> RObject<AnyType, Unknown> {
        R::null().convert()
    }
}

// Deref

impl Deref for RObject {
    type Target = SEXP;
    fn deref(&self) -> &Self::Target {
        &self.sexp
    }
}

// MapErrMsg

pub trait MapErrMsg<S> {
    fn map_err_msg(self) -> Result<S, &'static str>;
}

impl<U, V> MapErrMsg<RObject<Vector, Unknown>> for Result<RObject<Vector, Unknown>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<Vector, Unknown>, &'static str> {
        self.map_err(|_| "Not a vector")
    }
}

impl<U, V> MapErrMsg<RObject<Matrix, Unknown>> for Result<RObject<Matrix, Unknown>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<Matrix, Unknown>, &'static str> {
        self.map_err(|_| "Not a matrix")
    }
}

impl<U, V> MapErrMsg<RObject<Array, Unknown>> for Result<RObject<Array, Unknown>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<Array, Unknown>, &'static str> {
        self.map_err(|_| "Not an array")
    }
}

impl<U, V> MapErrMsg<RObject<Vector, List>> for Result<RObject<Vector, List>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<Vector, List>, &'static str> {
        self.map_err(|_| "Not a list")
    }
}

impl<U, V> MapErrMsg<RObject<Vector, DataFrame>>
    for Result<RObject<Vector, DataFrame>, RObject<U, V>>
{
    fn map_err_msg(self) -> Result<RObject<Vector, DataFrame>, &'static str> {
        self.map_err(|_| "Not a data frame")
    }
}

impl<U, V> MapErrMsg<RObject<Function, ()>> for Result<RObject<Function, ()>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<Function, ()>, &'static str> {
        self.map_err(|_| "Not a list")
    }
}

impl<U, V> MapErrMsg<RObject<ExternalPtr, ()>> for Result<RObject<ExternalPtr, ()>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<ExternalPtr, ()>, &'static str> {
        self.map_err(|_| "Not a data frame")
    }
}

impl<A: HasLength, U, V> MapErrMsg<RObject<A, f64>> for Result<RObject<A, f64>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<A, f64>, &'static str> {
        self.map_err(|_| "Not of storage mode 'double'")
    }
}

impl<A: HasLength, U, V> MapErrMsg<RObject<A, i32>> for Result<RObject<A, i32>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<A, i32>, &'static str> {
        self.map_err(|_| "Not of storage mode 'integer'")
    }
}

impl<A: HasLength, U, V> MapErrMsg<RObject<A, u8>> for Result<RObject<A, u8>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<A, u8>, &'static str> {
        self.map_err(|_| "Not of storage mode 'raw'")
    }
}

impl<A: HasLength, U, V> MapErrMsg<RObject<A, bool>> for Result<RObject<A, bool>, RObject<U, V>> {
    fn map_err_msg(self) -> Result<RObject<A, bool>, &'static str> {
        self.map_err(|_| "Not of storage mode 'logical'")
    }
}

impl<A: HasLength, U, V> MapErrMsg<RObject<A, Character>>
    for Result<RObject<A, Character>, RObject<U, V>>
{
    fn map_err_msg(self) -> Result<RObject<A, Character>, &'static str> {
        self.map_err(|_| "Not of storage mode 'character'")
    }
}

impl<U, V> MapErrMsg<f64> for Result<f64, RObject<U, V>> {
    fn map_err_msg(self) -> Result<f64, &'static str> {
        self.map_err(|_| "Cannot be interpreted as an f64")
    }
}

impl<U, V> MapErrMsg<i32> for Result<i32, RObject<U, V>> {
    fn map_err_msg(self) -> Result<i32, &'static str> {
        self.map_err(|_| "Cannot be interpreted as an i32")
    }
}

impl<U, V> MapErrMsg<usize> for Result<usize, RObject<U, V>> {
    fn map_err_msg(self) -> Result<usize, &'static str> {
        self.map_err(|_| "Cannot be interpreted as an usize")
    }
}
impl<U, V> MapErrMsg<u8> for Result<u8, RObject<U, V>> {
    fn map_err_msg(self) -> Result<u8, &'static str> {
        self.map_err(|_| "Cannot be interpreted as an u8")
    }
}
impl<U, V> MapErrMsg<bool> for Result<bool, RObject<U, V>> {
    fn map_err_msg(self) -> Result<bool, &'static str> {
        self.map_err(|_| "Cannot be interpreted as a bool")
    }
}
impl<'a, U, V> MapErrMsg<&'a str> for Result<&'a str, RObject<U, V>> {
    fn map_err_msg(self) -> Result<&'a str, &'static str> {
        self.map_err(|_| "Cannot be interpreted as an &str")
    }
}
