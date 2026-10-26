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
//! This crate provides the `roxido` procedural macro to support the
//! [roxido](https://crates.io/crates/roxido) crate

use proc_macro::TokenStream;
use quote::quote;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use syn::ext::IdentExt;
use syn::parse::Parser;
use syn::{Token, parse_quote};

/// Export a function to R, converting its result with `to_r`.
/// Mark a `&mut` argument `#[unsafe(in_place)]` to mutate R's object without copy-on-write.
#[proc_macro_attribute]
pub fn roxido(attr: TokenStream, item: TokenStream) -> TokenStream {
    let options: Vec<_> = syn::punctuated::Punctuated::<NestedMeta, Token![,]>::parse_terminated
        .parse(attr)
        .map(|punctuated| punctuated.into_iter().collect())
        .unwrap();
    match syn::parse_macro_input!(item as syn::Item) {
        syn::Item::Fn(item_fn) => roxido_fn(options, item_fn),
        _ => panic!("The 'roxido' attribute can only be applied to a function."),
    }
}

struct NestedMeta(syn::Meta);

impl syn::parse::Parse for NestedMeta {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident::peek_any) {
            input.parse().map(NestedMeta)
        } else {
            Err(input.error("Parse error."))
        }
    }
}

macro_rules! TYPE_MESSAGE {
    () => { "ach argument to a 'roxido' function must have one of the following types: &RArray, &RDataFrame, &RExternalPtr, &RFunction, &RList, &RMatrix, &RObject, &RScalar, &RSymbol, &RVector, SEXP, f64, i32, usize, u8, bool, &str, &[f64], &[i32], &[u8], or Option<T> for one of these types T." };
}

fn roxido_fn(options: Vec<NestedMeta>, item_fn: syn::ItemFn) -> TokenStream {
    let r_function_directory = match std::env::var("ROXIDO_R_FUNC_DIR") {
        Ok(x) if !x.is_empty() => {
            let path = Path::new(&x).to_owned();
            if path.exists() && path.is_dir() {
                Some(path)
            } else {
                None
            }
        }
        _ => None,
    };
    let mut invisible = false;
    for meta in options {
        let meta = meta.0;
        let meta_string = quote!(#meta).to_string();
        match meta {
            syn::Meta::NameValue(x) => {
                let name = x.path;
                let value = x.value;
                let name_string = quote!(#name).to_string();
                let value_string = quote!(#value).to_string();
                match name_string.as_str() {
                    "invisible" => match value_string.as_str() {
                        "true" => invisible = true,
                        "false" => invisible = false,
                        _ => {
                            panic!("Unsupported value '{value_string}' for {name_string}.")
                        }
                    },
                    // Read by build.rs, which names and registers the function.
                    "module" => {}
                    _ => panic!("Unsupported token '{name_string}'."),
                }
            }
            _ => panic!("Unsupported token '{meta_string}'."),
        }
    }
    let name = item_fn.sig.ident;
    let vis = item_fn.vis;
    let args = item_fn.sig.inputs;
    let body = item_fn.block;
    let output = item_fn.sig.output;
    // Check that visibility is okay.
    let vis_as_string = quote!(#vis).to_string();
    if !vis_as_string.is_empty() {
        panic!(
            "A function with the 'roxido' attribute may not have a visibility modifier, but found '{vis_as_string}'."
        );
    }
    // Generate code to convert each argument from its SEXP.
    let mut arg_names = Vec::with_capacity(args.len());
    let mut generated_statements: Vec<syn::Stmt> = Vec::new();
    // Arguments that become references, and whether each is modified in place.
    let mut borrowed_args: Vec<(Box<syn::Pat>, bool, bool)> = Vec::new();
    let mut new_args = args.clone();
    new_args.clear();
    for arg in &args {
        match arg {
            syn::FnArg::Typed(pat_type) => {
                let mut statements: Vec<syn::Stmt> = Vec::new();
                let name = &pat_type.pat;
                let name_as_string = quote!(#name).to_string();
                if let Some(name_as_string) = name_as_string.strip_prefix("mut ") {
                    panic!(
                        "'{name_as_string}' is marked as mutable, but arguments to 'roxido' functions cannot themselves be mutable."
                    );
                }
                let mut in_place = false;
                {
                    let mut y = pat_type.clone();
                    y.ty = Box::new(syn::parse_str::<syn::Type>("SEXP").unwrap());
                    y.attrs.retain(|attr| {
                        let attr: String = quote!(#attr).to_string().split_whitespace().collect();
                        match attr.as_str() {
                            "#[unsafe(in_place)]" => {
                                in_place = true;
                                false
                            }
                            "#[in_place]" => panic!(
                                "'{name_as_string}' must be marked '#[unsafe(in_place)]', not '#[in_place]'."
                            ),
                            _ => true,
                        }
                    });
                    new_args.push(syn::FnArg::Typed(y));
                }
                // An argument declared as 'Option<T>' is 'None' for NULL and is otherwise converted as 'T'.
                let option_inner = option_inner_type(&pat_type.ty);
                let optional = option_inner.is_some();
                let ty = option_inner.as_ref().unwrap_or(&pat_type.ty);
                if matches!(ty.as_ref(), syn::Type::Reference(_)) {
                    borrowed_args.push((name.clone(), in_place, optional));
                }
                if in_place
                    && !matches!(ty.as_ref(), syn::Type::Reference(r) if r.mutability.is_some())
                {
                    panic!(
                        "'{name_as_string}' is marked 'in_place', but only '&mut' arguments can be."
                    );
                }
                let error_msg = || {
                    panic!(
                        concat!("'{}' is of type '{}', but e", TYPE_MESSAGE!()),
                        name_as_string,
                        quote!(#ty)
                    )
                };
                let as_robject = |vec: &mut Vec<_>, mutable: bool| {
                    if in_place {
                        vec.push(parse_quote! { let #name = unsafe { RObject::from_sexp_mut(#name, pc) }; });
                    } else if mutable {
                        vec.push(parse_quote! { let #name = unsafe { RObject::__private_from_arg_mut(#name, stringify!(#name), pc) }; });
                    } else {
                        vec.push(
                            parse_quote! { let #name = unsafe { RObject::from_sexp(#name, pc) }; },
                        );
                    }
                };
                let as_rscalar = |vec: &mut Vec<_>, mutable: bool| {
                    as_robject(vec, mutable);
                    if mutable {
                        vec.push(parse_quote! { let #name = #name.as_scalar_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar.")); });
                    } else {
                        vec.push(parse_quote! { let #name = #name.as_scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar.")); });
                    }
                };
                let as_rvector = |vec: &mut Vec<_>, mutable: bool| {
                    as_robject(vec, mutable);
                    if mutable {
                        vec.push(parse_quote! { let #name = #name.as_vector_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a vector.")); });
                    } else {
                        vec.push(parse_quote! { let #name = #name.as_vector().stop_str(concat!("'", stringify!(#name),"' is expected to be a vector.")); });
                    }
                };
                let as_rmatrix = |vec: &mut Vec<_>, mutable: bool| {
                    as_robject(vec, mutable);
                    if mutable {
                        vec.push(parse_quote! { let #name = #name.as_matrix_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a matrix.")); });
                    } else {
                        vec.push(parse_quote! { let #name = #name.as_matrix().stop_str(concat!("'", stringify!(#name),"' is expected to be a matrix.")); });
                    }
                };
                let as_rarray = |vec: &mut Vec<_>, mutable: bool| {
                    as_robject(vec, mutable);
                    if mutable {
                        vec.push(parse_quote! { let #name = #name.as_array_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be an array.")); });
                    } else {
                        vec.push(parse_quote! { let #name = #name.as_array().stop_str(concat!("'", stringify!(#name),"' is expected to be an array.")); });
                    }
                };
                let as_type = |vec: &mut Vec<_>, tipe: &str, path: &str, mutable: bool| {
                    if let Some(snippet) = path.strip_prefix(tipe) {
                        if let Some(snippet) = snippet.strip_prefix(" < ") {
                            if let Some(snippet) = snippet.strip_suffix(" >") {
                                match snippet {
                                    "f64" => {
                                        if mutable {
                                            vec.push(parse_quote! { let #name = #name.as_f64_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode double.")); });
                                        } else {
                                            vec.push(parse_quote! { let #name = #name.as_f64().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode double.")); });
                                        }
                                    }
                                    "i32" => {
                                        if mutable {
                                            vec.push(parse_quote! { let #name = #name.as_i32_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode integer.")); });
                                        } else {
                                            vec.push(parse_quote! { let #name = #name.as_i32().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode integer.")); });
                                        }
                                    }
                                    "u8" => {
                                        if mutable {
                                            vec.push(parse_quote! { let #name = #name.as_u8_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode raw.")); });
                                        } else {
                                            vec.push(parse_quote! { let #name = #name.as_u8().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode raw.")); });
                                        }
                                    }
                                    "bool" => {
                                        if mutable {
                                            vec.push(parse_quote! { let #name = #name.as_bool_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode logical.")); });
                                        } else {
                                            vec.push(parse_quote! { let #name = #name.as_bool().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode logical.")); });
                                        }
                                    }
                                    "char" => {
                                        if mutable {
                                            vec.push(parse_quote! { let #name = #name.as_char_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode character.")); });
                                        } else {
                                            vec.push(parse_quote! { let #name = #name.as_char().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode character.")); });
                                        }
                                    }
                                    "()" => {
                                        // Any storage mode is accepted.
                                    }
                                    _ => {
                                        panic!(
                                            "'{name_as_string}' has type parameter '{snippet}', but one of the following was expected: f64, i32, u8, bool, char, ()."
                                        );
                                    }
                                }
                            }
                        } else if !snippet.is_empty() {
                            error_msg();
                        }
                    }
                };
                match ty.as_ref() {
                    syn::Type::Path(path) => {
                        let ty = &path.path;
                        let path = quote!(#ty).to_string();
                        match path.as_ref() {
                            "SEXP" => {}
                            "f64" => {
                                as_rscalar(&mut statements, false);
                                statements.push(parse_quote! { let #name = #name.f64(); });
                            }
                            "i32" => {
                                as_rscalar(&mut statements, false);
                                statements.push(parse_quote! { let #name = #name.i32().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be an integer: {}"), x)).stop(); });
                            }
                            "usize" => {
                                as_rscalar(&mut statements, false);
                                statements.push(parse_quote! { let #name = #name.usize().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a usize: {}"), x)).stop(); });
                            }
                            "u8" => {
                                as_rscalar(&mut statements, false);
                                statements.push(parse_quote! { let #name = #name.u8().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a raw: {}"), x)).stop(); });
                            }
                            "bool" => {
                                as_rscalar(&mut statements, false);
                                statements.push(parse_quote! { let #name = #name.bool().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a logical: {}"), x)).stop(); });
                            }
                            _ => {
                                error_msg();
                            }
                        }
                    }
                    syn::Type::Reference(reference) => {
                        let ty = &reference.elem;
                        let mutable = reference.mutability.is_some();
                        match ty.as_ref() {
                            syn::Type::Slice(slice) => {
                                as_rvector(&mut statements, mutable);
                                let ty = &slice.elem;
                                let path = quote!(#ty).to_string();
                                match path.as_ref() {
                                    "f64" => {
                                        as_type(
                                            &mut statements,
                                            "RVector",
                                            "RVector < f64 >",
                                            mutable,
                                        );
                                    }
                                    "i32" => {
                                        as_type(
                                            &mut statements,
                                            "RVector",
                                            "RVector < i32 >",
                                            mutable,
                                        );
                                    }
                                    "u8" => {
                                        as_type(
                                            &mut statements,
                                            "RVector",
                                            "RVector < u8 >",
                                            mutable,
                                        );
                                    }
                                    _ => {
                                        panic!(
                                            "'{name_as_string}' is slice of '{path}', but a 'roxido' function only supports slices of f64, i32, and u8."
                                        );
                                    }
                                }
                                if mutable {
                                    statements
                                        .push(parse_quote! { let #name = #name.slice_mut(); });
                                } else {
                                    statements.push(parse_quote! { let #name = #name.slice(); });
                                }
                            }
                            syn::Type::Path(ty) => {
                                if reference.lifetime.is_some() {
                                    panic!(
                                        "'{name_as_string}' has a lifetime, which is not supported for a 'roxido' function."
                                    );
                                }
                                let path = quote!(#ty).to_string();
                                if mutable
                                    && !in_place
                                    && matches!(path.as_str(), "RExternalPtr" | "RSymbol")
                                {
                                    panic!(
                                        "'{name_as_string}' is '&mut {path}', which R cannot copy, so it must be marked '#[unsafe(in_place)]'."
                                    );
                                }
                                match path.as_str() {
                                    "str" => {
                                        if mutable {
                                            panic!(
                                                "'{name_as_string}' is a &mut str, but only &str is supported for a 'roxido' function."
                                            );
                                        }
                                        as_rscalar(&mut statements, false);
                                        statements
                                            .push(parse_quote! { let #name = #name.str(pc).map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a string: {}"), x)).stop(); });
                                    }
                                    "RObject" => {
                                        as_robject(&mut statements, mutable);
                                    }
                                    "RList" => {
                                        as_robject(&mut statements, mutable);
                                        if mutable {
                                            statements.push(parse_quote! { let #name = #name.as_list_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a list.")); });
                                        } else {
                                            statements.push(parse_quote! { let #name = #name.as_list().stop_str(concat!("'", stringify!(#name),"' is expected to be a list.")); });
                                        }
                                    }
                                    "RDataFrame" => {
                                        as_robject(&mut statements, mutable);
                                        if mutable {
                                            statements.push(parse_quote! { let #name = #name.as_data_frame_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a data frame.")); });
                                        } else {
                                            statements.push(parse_quote! { let #name = #name.as_data_frame().stop_str(concat!("'", stringify!(#name),"' is expected to be a data frame.")); });
                                        }
                                    }
                                    "RFunction" => {
                                        as_robject(&mut statements, mutable);
                                        if mutable {
                                            statements.push(parse_quote! { let #name = #name.as_function_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a function.")); });
                                        } else {
                                            statements.push(parse_quote! { let #name = #name.as_function().stop_str(concat!("'", stringify!(#name),"' is expected to be a function.")); });
                                        }
                                    }
                                    "RExternalPtr" => {
                                        as_robject(&mut statements, mutable);
                                        if mutable {
                                            statements.push(parse_quote! { let #name = #name.as_external_ptr_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be an external pointer.")); });
                                        } else {
                                            statements.push(parse_quote! { let #name = #name.as_external_ptr().stop_str(concat!("'", stringify!(#name),"' is expected to be an external pointer.")); });
                                        }
                                    }
                                    "RSymbol" => {
                                        as_robject(&mut statements, mutable);
                                        if mutable {
                                            statements.push(parse_quote! { let #name = #name.as_symbol_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a symbol.")); });
                                        } else {
                                            statements.push(parse_quote! { let #name = #name.as_symbol().stop_str(concat!("'", stringify!(#name),"' is expected to be a symbol.")); });
                                        }
                                    }
                                    x if x.starts_with("RScalar") => {
                                        as_rscalar(&mut statements, mutable);
                                        as_type(&mut statements, "RScalar", x, mutable);
                                    }
                                    x if x.starts_with("RVector") => {
                                        as_rvector(&mut statements, mutable);
                                        as_type(&mut statements, "RVector", x, mutable);
                                    }
                                    x if x.starts_with("RMatrix") => {
                                        as_rmatrix(&mut statements, mutable);
                                        as_type(&mut statements, "RMatrix", x, mutable);
                                    }
                                    x if x.starts_with("RArray") => {
                                        as_rarray(&mut statements, mutable);
                                        as_type(&mut statements, "RArray", x, mutable);
                                    }
                                    _ => error_msg(),
                                }
                            }
                            _ => error_msg(),
                        }
                    }
                    _ => error_msg(),
                }
                if optional {
                    generated_statements.push(parse_quote! {
                        let #name = if unsafe { RObject::from_sexp(#name, pc) }.is_null() {
                            None
                        } else {
                            #( #statements )*
                            Some(#name)
                        };
                    });
                } else {
                    generated_statements.extend(statements);
                }
                arg_names.push(name_as_string);
            }
            _ => panic!(concat!("E", TYPE_MESSAGE!())),
        }
    }
    // Check in-place arguments before any argument is converted from its SEXP.
    let mut in_place_checks: Vec<syn::Stmt> = Vec::new();
    for (i, (name, in_place, optional)) in borrowed_args.iter().enumerate() {
        if *in_place {
            let others: Vec<_> = borrowed_args
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, (other, _, _))| other)
                .collect();
            in_place_checks.push(parse_quote! {
                unsafe { RObject::__private_check_in_place(#name, stringify!(#name), #optional, &[#( (#others, stringify!(#others)) ),*]) };
            });
        }
    }
    // Check that there is no explicit return type.
    match &output {
        syn::ReturnType::Default => {}
        _ => {
            panic!(
                "A 'roxido' function should not have an explicit return type, as its body is actually a closure which returns any value whose type provides a 'to_r' method."
            );
        }
    }
    let func_name = quote!(#name).to_string();
    if let Some(mut path) = r_function_directory {
        path.push(&func_name);
        let mut file = File::create(&path).unwrap_or_else(|err| {
            panic!(
                "Could not open file '{}' for writing: {err}",
                path.display()
            );
        });
        let args = arg_names.join(", ");
        let comma = if args.is_empty() { "" } else { ", " };
        let (invisible_opening, invisible_closing) = match invisible {
            true => ("invisible(", ")"),
            false => ("", ""),
        };
        let write_result = write!(
            file,
            "{func_name} <- function({args}) {invisible_opening}.Call(.{func_name}{comma}{args}){invisible_closing}"
        );
        if let Err(err) = write_result {
            panic!("Could not write to the file '{}': {err}", path.display());
        }
    }
    // Write the function itself, wrapping the body in 'catch_unwind' so that a panic runs Rust
    // destructors before Rf_error long-jumps into R.
    TokenStream::from(quote! {
        #[allow(clippy::useless_transmute)]
        /// # Safety
        /// Call from R's main thread after package initialization, with valid R arguments.
        /// In-place arguments must follow roxido's aliasing rule.
        #[deny(unsafe_op_in_unsafe_fn)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn #name(#new_args) -> SEXP {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let __roxido_pc = unsafe { Pc::__private_new() };
                let pc = &__roxido_pc;
                #( #in_place_checks )*
                #( #generated_statements )*
                let mut f = || { #body };
                f().to_r(pc).sexp()
            }));
            match result {
                Ok(obj) => obj,
                Err(ref payload) => {
                    use roxido::rbindings::*;
                    let msg = match payload.downcast_ref::<RStopHelper>() {
                        Some(x) => x.0.as_str(),
                        None => {
                            concat!("Panic in Rust function '", stringify!(#name),"' with 'roxido' attribute.")
                        }
                    };
                    // Rf_mkCharLenCE would reject a NUL with an R error, leaking the payload.
                    let msg = msg.split('\0').next().unwrap();
                    let len = msg.len();
                    let sexp = unsafe {
                        use std::convert::TryInto;
                        Rf_mkCharLenCE(
                            msg.as_ptr() as *const std::os::raw::c_char,
                            msg.len().try_into().unwrap(),
                            cetype_t_CE_UTF8,
                        )
                    };
                    drop(result);
                    unsafe {
                        Rf_error(c"%.*s".as_ptr(), len, R_CHAR(sexp));
                    }
                    R::null().sexp()  // We never get here.
                }
            }
        }
    })
}

// The type 'T' of an argument declared as 'Option<T>'.
fn option_inner_type(ty: &syn::Type) -> Option<Box<syn::Type>> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    if path.qself.is_some() {
        return None;
    }
    let segment = path.path.segments.last()?;
    if segment.ident != "Option" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };
    match arguments.args.iter().collect::<Vec<_>>().as_slice() {
        [syn::GenericArgument::Type(inner)] => Some(Box::new(inner.clone())),
        _ => None,
    }
}
