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
        _ => panic!("The 'roxido' attribute can only be added to a function."),
    }
}

struct NestedMeta(syn::Meta);

impl syn::parse::Parse for NestedMeta {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident::peek_any) {
            input.parse().map(NestedMeta)
        } else {
            Err(input.error("Parse error"))
        }
    }
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
                    _ => panic!("Unsupported option '{name_string}'."),
                }
            }
            _ => panic!("Unsupported option '{meta_string}'."),
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
        panic!("A function with the 'roxido' attribute may not have a visibility modifier, but found '{}'", vis_as_string);
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
                    y.ty = Box::new(syn::parse_str::<syn::Type>("crate::rbindings::SEXP").unwrap());
                    new_args.push(syn::FnArg::Typed(y));
                }
                let name = &pat_type.pat;
                let name_as_string = quote!(#name).to_string();
                if let Some(name_as_string) = name_as_string.strip_prefix("mut ") {
                    panic!("'{}' is marked as mutable, but arguments to 'roxido' functions cannot be mutable", name_as_string);
                }
                let ty = &pat_type.ty;
                let error_msg = || {
                    panic!("'{}' is of type '{}', but arguments to 'roxido' functions must be of type &RObject, &RObject<A,B>, SEXP, f64, i32, usize, u8, bool, or &str", name_as_string, quote!(#ty))
                };
                match ty.as_ref() {
                    syn::Type::Path(path) => {
                        let ty = &path.path;
                        let path = quote!(#ty).to_string();
                        match path.as_ref() {
                            "SEXP" => {}
                            "f64" => {
                                generated_statements.push(parse_quote! { let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name).scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar")).f64(); });
                            }
                            "i32" => {
                                generated_statements.push(parse_quote! { let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name).scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar")).i32().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be an integer: {}"), x)).stop(); });
                            }
                            "usize" => {
                                generated_statements.push(parse_quote! { let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name).scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar")).usize().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a usize: {}"), x)).stop(); });
                            }
                            "u8" => {
                                generated_statements.push(parse_quote! { let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name).scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar")).u8().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a raw: {}"), x)).stop(); });
                            }
                            "bool" => {
                                generated_statements.push(parse_quote! { let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name).scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar")).bool().map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a logical: {}"), x)).stop(); });
                            }
                            _ => {
                                error_msg();
                            }
                        }
                    }
                    syn::Type::Reference(reference) => {
                        let ty = &reference.elem;
                        match ty.as_ref() {
                            syn::Type::Path(ty) => {
                                let path = quote!(#ty).to_string();
                                if !path.starts_with("RObject") {
                                    if path == "str" {
                                        generated_statements.push(parse_quote! { let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name).scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar")).to_str(pc).map_err(|x| format!(concat!("'", stringify!(#name), "' cannot be a string: {}"), x)).stop(); });
                                    } else {
                                        error_msg();
                                    }
                                } else {
                                    if reference.lifetime.is_some() {
                                        panic!("'{}' has a lifetime, which is not supported for a 'roxido' function.", name_as_string);
                                    }
                                    let mutable = reference.mutability.is_some();
                                    if mutable {
                                        generated_statements.push(parse_quote! {
                                            let #name = pc.transmute_sexp_mut::<RAnyType, RUnknown>(#name);
                                        });
                                    } else {
                                        generated_statements.push(parse_quote! {
                                            let #name = pc.transmute_sexp::<RAnyType, RUnknown>(#name);
                                        });
                                    }
                                    if path.ends_with('>') {
                                        let Some(snippet) = path.strip_prefix("RObject < ") else {
                                            error_msg()
                                        };
                                        let Some(snippet) = snippet.strip_suffix(" >") else {
                                            error_msg()
                                        };
                                        let tasks: Vec<_> = snippet.split(", ").collect();
                                        if !(1..=2).contains(&tasks.len()) {
                                            panic!(
                                                "'{}' should have at most 2 type parameters",
                                                name_as_string
                                            );
                                        }
                                        if let Some(&rtype) = tasks.first() {
                                            match rtype {
                                                "RScalar" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.scalar_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.scalar().stop_str(concat!("'", stringify!(#name),"' is expected to be a scalar"));
                                                        });
                                                    }
                                                }
                                                "RVector" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.vector_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a vector"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.vector().stop_str(concat!("'", stringify!(#name),"' is expected to be a vector"));
                                                        });
                                                    }
                                                }
                                                "RMatrix" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.matrix_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a matrix"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.matrix().stop_str(concat!("'", stringify!(#name),"' is expected to be a matrix"));
                                                        });
                                                    }
                                                }
                                                "RArray" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.array_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be an array"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.array().stop_str(concat!("'", stringify!(#name),"' is expected to be an array"));
                                                        });
                                                    }
                                                }
                                                "RList" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.list_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a list"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.list().stop_str(concat!("'", stringify!(#name),"' is expected to be a list"));
                                                        });
                                                    }
                                                }
                                                "RFunction" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.function_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a function"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.function().stop_str(concat!("'", stringify!(#name),"' is expected to be a function"));
                                                        });
                                                    }
                                                }
                                                "RExternalPtr" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.external_ptr_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be an external pointer"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.external_ptr().stop_str(concat!("'", stringify!(#name),"' is expected to be an external pointer"));
                                                        });
                                                    }
                                                }
                                                "RSymbol" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.symbol_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a symbol"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.symbol().stop_str(concat!("'", stringify!(#name),"' is expected to be a symbol"));
                                                        });
                                                    }
                                                }
                                                e => {
                                                    panic!("'{}' has '{}' as the first type parameter, but one of the following was expected: RScalar, RVector, RMatrix, RArray, RList, RFunction, RExternalPtr, RSymbol", name_as_string, e);
                                                }
                                            }
                                        }
                                        if let Some(&rtype) = tasks.get(1) {
                                            match rtype {
                                                "f64" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.double_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode double"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.double().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode double"));
                                                        });
                                                    }
                                                }
                                                "i32" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.integer_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode integer"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.integer().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode integer"));
                                                        });
                                                    }
                                                }
                                                "u8" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.raw_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode raw"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.raw().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode raw"));
                                                        });
                                                    }
                                                }
                                                "bool" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.logical_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode logical"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.logical().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode logical"));
                                                        });
                                                    }
                                                }
                                                "RCharacter" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.character_mut().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode character"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.character().stop_str(concat!("'", stringify!(#name),"' is expected to have storage mode character"));
                                                        });
                                                    }
                                                }
                                                "RDataFrame" => {
                                                    if mutable {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.data_frame_mut().stop_str(concat!("'", stringify!(#name),"' is expected to be a data frame"));
                                                        });
                                                    } else {
                                                        generated_statements.push(parse_quote! {
                                                            let #name = #name.data_frame().stop_str(concat!("'", stringify!(#name),"' is expected to be a data frame"));
                                                        });
                                                    }
                                                }
                                                e => {
                                                    panic!("'{}' has '{}' as the second type parameter, but one of the following was expected: f64, i32, u8, bool, RCharacter, RDataFrame", name_as_string, e);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            _ => error_msg(),
                        }
                    }
                    _ => error_msg(),
                }
                arg_names.push(name_as_string);
            }
            _ => panic!("Each argument to a 'roxido' function must be of type &RObject, &RObject<A,B>, SEXP, f64, i32, usize, u8, bool, or &str")
        }
    }
    // Check that return is of type '&RObject'.
    match &output {
        syn::ReturnType::Default => {}
        syn::ReturnType::Type(_, tipe) => {
            let tipe_as_string = quote!(#tipe).to_string();
            if tipe_as_string != "& RObject" && tipe_as_string != "SEXP" {
                panic!(
                    "A function with the 'roxido' attribute always implicitly returns an '&RObject' or 'SEXP', but found '{}'",
                    tipe_as_string
                );
            }
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
        if longjmp {
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
        }
        .expect("Could not write to the file.");
    } else {
        match std::env::var("R_CARGO_RUN_COUNTER") {
            Ok(x) if x == "1" => {
                let filename = "roxido.txt";
                if let Ok(mut file) = OpenOptions::new().append(true).create(true).open(filename) {
                    let mut line = String::new();
                    line.push_str(&func_name);
                    for arg in arg_names.iter() {
                        line.push_str(", ");
                        line.push_str(arg)
                    }
                    line.push('\n');
                    if file.write_all(line.as_bytes()).is_err() {
                        eprintln!("Couldn't append to file: {filename}");
                    }
                } else {
                    eprintln!("Couldn't open the file: {filename}");
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
            #[no_mangle]
            extern "C" fn #name(#new_args) -> crate::rbindings::SEXP {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let pc = &mut Pc::new();
                    #( #generated_statements )*
                    let mut f = || { #body };
                    f().to_r(pc).sexp()
                }));
                match result {
                    Ok(obj) => obj,
                    Err(ref payload) => {
                        let msg = match payload.downcast_ref::<crate::stop::RStopHelper>() {
                            Some(x) => x.0.as_str(),
                            None => {
                                concat!("Panic in Rust function '", stringify!(#name),"' with 'roxido' attribute")
                            }
                        };
                        let len = msg.len();
                        let sexp = unsafe {
                            use std::convert::TryInto;
                            crate::rbindings::Rf_mkCharLen(
                                msg.as_ptr() as *const std::os::raw::c_char,
                                msg.len().try_into().unwrap(),
                            )
                        };
                        drop(result);
                        unsafe {
                            crate::rbindings::Rf_error(b"%.*s\0".as_ptr() as *const std::os::raw::c_char, len, crate::rbindings::R_CHAR(sexp));
                        }
                        crate::Pc::null().sexp()  // We never get here.
                    }
                }
            }
        })
    } else {
        TokenStream::from(quote! {
            #[allow(clippy::useless_transmute)]
            #[no_mangle]
            extern "C" fn #name(#new_args) -> crate::rbindings::SEXP {
                let result: Result<crate::rbindings::SEXP, _> = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let pc = &mut Pc::new();
                    #( #generated_statements )*
                    let mut f = || { #body };
                    f().to_r(pc).sexp()
                }));
                match result {
                    Ok(obj) => obj,
                    Err(_) => {
                        let pc = &mut crate::Pc::new();
                        pc.new_error(concat!("Panic in Rust function '",stringify!(#name),"' with 'roxido' attribute")).sexp()
                    }
                }
            }
        })
    }
}
