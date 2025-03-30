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
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use syn::ext::IdentExt;
use syn::parse::Parser;
use syn::{parse_quote, Token};

// See https://doc.rust-lang.org/nomicon/unwinding.html
//
// Which says, in part, "There is an API called catch_unwind that enables catching a panic without spawning a thread. Still, we would encourage you to only do this sparingly. In particular, Rust's current unwinding implementation is heavily optimized for the "doesn't unwind" case. If a program doesn't unwind, there should be no runtime cost for the program being ready to unwind. As a consequence, actually unwinding will be more expensive than in e.g. Java. Don't build your programs to unwind under normal circumstances. Ideally, you should only panic for programming errors or extreme problems."

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
    () => { "ach argument to a 'roxido' function must have one of the following types: &RArray, &RDataFrame, &RExternalPtr, &RFunction, &RList, &RMatrix, &RObject, &RScalar, &RSymbol, &RVector, SEXP, f64, i32, usize, u8, bool, &str, &[f64], &[i32], &[u8]." };
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
    let mut longjmp = true;
    let mut invisible = false;
    let mut module = String::new();
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
                    "longjmp" => match value_string.as_str() {
                        "true" => longjmp = true,
                        "false" => longjmp = false,
                        _ => {
                            panic!("Unsupported value '{value_string}' for {name_string}.")
                        }
                    },
                    "invisible" => match value_string.as_str() {
                        "true" => invisible = true,
                        "false" => invisible = false,
                        _ => {
                            panic!("Unsupported value '{value_string}' for {name_string}.")
                        }
                    },
                    "module" => {
                        module = value_string;
                    }
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
        panic!("A function with the 'roxido' attribute may not have a visibility modifier, but found '{}'.", vis_as_string);
    }
    // Check that all arguments are of type '&RObject'.
    let mut arg_names = Vec::with_capacity(args.len());
    let mut generated_statements: Vec<syn::Stmt> = Vec::new();
    let mut new_args = args.clone();
    new_args.clear();
    for arg in &args {
        match arg {
            syn::FnArg::Typed(pat_type) => {
                {
                    let mut y = pat_type.clone();
                    y.ty = Box::new(syn::parse_str::<syn::Type>("SEXP").unwrap());
                    new_args.push(syn::FnArg::Typed(y));
                }
                let name = &pat_type.pat;
                let name_as_string = quote!(#name).to_string();
                if let Some(name_as_string) = name_as_string.strip_prefix("mut ") {
                    panic!("'{}' is marked as mutable, but arguments to 'roxido' functions cannot themselves be mutable.", name_as_string);
                }
                let ty = &pat_type.ty;
                let error_msg = || {
                    panic!(
                        concat!("'{}' is of type '{}', but e", TYPE_MESSAGE!()),
                        name_as_string,
                        quote!(#ty)
                    )
                };
                let as_robject = |vec: &mut Vec<_>, mutable: bool| {
                    if mutable {
                        vec.push(parse_quote! { let #name = unsafe { RObject::from_sexp_mut(#name, pc) }; });
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
                                    _ => {
                                        panic!("'{}' has type parameter '{}', but one of the following was expected: f64, i32, u8, bool, char.", name_as_string, snippet);
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
                                as_rscalar(&mut generated_statements, false);
                                generated_statements
                                    .push(parse_quote! { let #name = #name.f64(); });
                            }
                            "i32" => {
                                as_rscalar(&mut generated_statements, false);
                                generated_statements.push(parse_quote! { let #name = #name.i32().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be an integer: {}"), x)).stop(); });
                            }
                            "usize" => {
                                as_rscalar(&mut generated_statements, false);
                                generated_statements.push(parse_quote! { let #name = #name.usize().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a usize: {}"), x)).stop(); });
                            }
                            "u8" => {
                                as_rscalar(&mut generated_statements, false);
                                generated_statements.push(parse_quote! { let #name = #name.u8().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a raw: {}"), x)).stop(); });
                            }
                            "bool" => {
                                as_rscalar(&mut generated_statements, false);
                                generated_statements.push(parse_quote! { let #name = #name.bool().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a logical: {}"), x)).stop(); });
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
                                as_rvector(&mut generated_statements, mutable);
                                let ty = &slice.elem;
                                let path = quote!(#ty).to_string();
                                match path.as_ref() {
                                    "f64" => {
                                        as_type(
                                            &mut generated_statements,
                                            "RVector",
                                            "RVector < f64 >",
                                            mutable,
                                        );
                                    }
                                    "i32" => {
                                        as_type(
                                            &mut generated_statements,
                                            "RVector",
                                            "RVector < i32 >",
                                            mutable,
                                        );
                                    }
                                    "u8" => {
                                        as_type(
                                            &mut generated_statements,
                                            "RVector",
                                            "RVector < u8 >",
                                            mutable,
                                        );
                                    }
                                    _ => {
                                        panic!("'{}' is slice of '{}', but a 'roxido' function only supports slices of f64, i32, and u8.", name_as_string, path);
                                    }
                                }
                                if mutable {
                                    generated_statements
                                        .push(parse_quote! { let #name = #name.slice_mut(); });
                                } else {
                                    generated_statements
                                        .push(parse_quote! { let #name = #name.slice(); });
                                }
                            }
                            syn::Type::Path(ty) => {
                                if reference.lifetime.is_some() {
                                    panic!("'{}' has a lifetime, which is not supported for a 'roxido' function.", name_as_string);
                                }
                                let path = quote!(#ty).to_string();
                                match path.as_str() {
                                    "str" => {
                                        if mutable {
                                            panic!("'{}' is a &mut str, but only &str is supported for a 'roxido' function.", name_as_string);
                                        }
                                        as_rscalar(&mut generated_statements, false);
                                        generated_statements
                                            .push(parse_quote! { let #name = #name.str(pc); });
                                    }
                                    "RObject" => {
                                        as_robject(&mut generated_statements, mutable);
                                    }
                                    "RList" => {
                                        as_robject(&mut generated_statements, mutable);
                                        if mutable {
                                            generated_statements.push(parse_quote! { let #name = #name.as_list_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a list.")); });
                                        } else {
                                            generated_statements.push(parse_quote! { let #name = #name.as_list().stop_str(concat!("'", stringify!(#name),"' is expected to be a list.")); });
                                        }
                                    }
                                    "RDataFrame" => {
                                        as_robject(&mut generated_statements, mutable);
                                        if mutable {
                                            generated_statements.push(parse_quote! { let #name = #name.as_data_frame_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a data frame.")); });
                                        } else {
                                            generated_statements.push(parse_quote! { let #name = #name.as_data_frame().stop_str(concat!("'", stringify!(#name),"' is expected to be a data frame.")); });
                                        }
                                    }
                                    "RFunction" => {
                                        as_robject(&mut generated_statements, mutable);
                                        if mutable {
                                            generated_statements.push(parse_quote! { let #name = #name.as_function_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a function.")); });
                                        } else {
                                            generated_statements.push(parse_quote! { let #name = #name.as_function().stop_str(concat!("'", stringify!(#name),"' is expected to be a function.")); });
                                        }
                                    }
                                    "RExternalPtr" => {
                                        as_robject(&mut generated_statements, mutable);
                                        if mutable {
                                            generated_statements.push(parse_quote! { let #name = #name.as_external_ptr_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a external pointer.")); });
                                        } else {
                                            generated_statements.push(parse_quote! { let #name = #name.as_external_ptr().stop_str(concat!("'", stringify!(#name),"' is expected to be a external pointer.")); });
                                        }
                                    }
                                    "RSymbol" => {
                                        as_robject(&mut generated_statements, mutable);
                                        if mutable {
                                            generated_statements.push(parse_quote! { let #name = #name.as_symbol_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a symbol.")); });
                                        } else {
                                            generated_statements.push(parse_quote! { let #name = #name.as_symbol().stop_str(concat!("'", stringify!(#name),"' is expected to be a symbol.")); });
                                        }
                                    }
                                    x if x.starts_with("RScalar") => {
                                        as_rscalar(&mut generated_statements, mutable);
                                        as_type(&mut generated_statements, "RScalar", x, mutable);
                                    }
                                    x if x.starts_with("RVector") => {
                                        as_rvector(&mut generated_statements, mutable);
                                        as_type(&mut generated_statements, "RVector", x, mutable);
                                    }
                                    x if x.starts_with("RMatrix") => {
                                        as_rmatrix(&mut generated_statements, mutable);
                                        as_type(&mut generated_statements, "RMatrix", x, mutable);
                                    }
                                    x if x.starts_with("RArray") => {
                                        as_rarray(&mut generated_statements, mutable);
                                        as_type(&mut generated_statements, "RArray", x, mutable);
                                    }
                                    _ => error_msg(),
                                }
                            }
                            _ => error_msg(),
                        }
                    }
                    _ => error_msg(),
                }
                arg_names.push(name_as_string);
            }
            _ => panic!(concat!("E", TYPE_MESSAGE!())),
        }
    }
    // Check that return is of type '&RObject'.
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
        let mut file = File::create(&path)
            .unwrap_or_else(|_| panic!("Could not open file '{:?}' for writing.", &path));
        let args = arg_names.join(", ");
        let comma = if args.is_empty() { "" } else { ", " };
        let (invisible_opening, invisible_closing) = match invisible {
            true => ("invisible(", ")"),
            false => ("", ""),
        };
        let write_result = if longjmp {
            write!(
                file,
                "{} <- function({}) {}.Call(.{}{}{}){}",
                func_name, args, invisible_opening, func_name, comma, args, invisible_closing
            )
        } else {
            write!(
                file,
                "{} <- function({}) {{\n  x <- .Call(.{}{}{})\n  if ( inherits(x,'error') ) stop(x) else {}x{}\n}}",
                func_name, args, func_name, comma, args, invisible_opening, invisible_closing
            )
        };
        if write_result.is_err() {
            panic!("Could not write to the file '{:?}'.", path);
        }
    } else {
        match std::env::var("R_CARGO_RUN_COUNTER") {
            Ok(x) if x == "1" => {
                let filename = "roxido.txt";
                if let Ok(mut file) = OpenOptions::new().append(true).create(true).open(filename) {
                    let mut line = String::new();
                    if !module.is_empty() {
                        line.push_str(module.as_str());
                        line.push_str("::");
                    }
                    line.push_str(&func_name);
                    for arg in arg_names.iter() {
                        line.push_str(", ");
                        line.push_str(arg)
                    }
                    line.push('\n');
                    if file.write_all(line.as_bytes()).is_err() {
                        eprintln!("Couldn't append to file: {filename}.");
                    }
                } else {
                    eprintln!("Couldn't open the file: {filename}.");
                }
            }
            _ => {}
        }
    }
    // Write the function itself, wrapping the body in 'catch_unwind' to prevent unwinding into C.
    if longjmp {
        // This will long jump, but that's seems to be okay because this is the last Rust stack
        // frame and we've cleaned up all of our heap memory.  Light testing indicated no memory
        // leaks.  See https://docs.rs/crate/setjmp for background information.
        TokenStream::from(quote! {
            #[allow(clippy::useless_transmute)]
            #[allow(clippy::not_unsafe_ptr_arg_deref)]
            #[no_mangle]
            pub extern "C" fn #name(#new_args) -> SEXP {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let pc = &mut Pc::__private_new();
                    #( #generated_statements )*
                    let mut f = || { #body };
                    f().to_r(pc).sexp()
                }));
                match result {
                    Ok(obj) => obj,
                    Err(ref payload) => {
                        use crate::rbindings::*;
                        let msg = match payload.downcast_ref::<RStopHelper>() {
                            Some(x) => x.0.as_str(),
                            None => {
                                concat!("Panic in Rust function '", stringify!(#name),"' with 'roxido' attribute.")
                            }
                        };
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
                            Rf_error(b"%.*s\0".as_ptr() as *const std::os::raw::c_char, len, R_CHAR(sexp));
                        }
                        R::null().sexp()  // We never get here.
                    }
                }
            }
        })
    } else {
        TokenStream::from(quote! {
            #[allow(clippy::useless_transmute)]
            #[allow(clippy::not_unsafe_ptr_arg_deref)]
            #[no_mangle]
            pub extern "C" fn #name(#new_args) -> SEXP {
                let result: Result<SEXP, _> = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let pc = &mut Pc::__private_new();
                    #( #generated_statements )*
                    let mut f = || { #body };
                    f().to_r(pc).sexp()
                }));
                match result {
                    Ok(obj) => obj,
                    Err(_) => {
                        let pc = &mut Pc::__private_new();
                        RError::new(concat!("Panic in Rust function '",stringify!(#name),"' with 'roxido' attribute."), pc).sexp()
                    }
                }
            }
        })
    }
}
