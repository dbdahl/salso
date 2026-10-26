use quote::ToTokens;
use std::env;
use std::fs;
use std::path::Path;
use syn::Token;
use syn::punctuated::Punctuated;

fn main() {
    println!(
        "Running build.rs with working directory {:?}",
        std::env::current_dir()
    );
    // Cargo reruns this script, and then recompiles the crate, only when these change.
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=../../DESCRIPTION");
    println!("cargo:rerun-if-env-changed=R_PACKAGE_NAME");
    let package_name = env::var("R_PACKAGE_NAME")
        .ok()
        .or_else(package_name_from_description);
    let snippet = match package_name {
        Some(package_name) => {
            let mut functions_info = Vec::new();
            find_roxido_functions(
                Path::new("src/lib.rs"),
                Path::new("src"),
                &mut functions_info,
            );
            // Only write R code when R is installing the package, not when an editor runs Cargo.
            let write_r_code = env::var("R_PACKAGE_NAME").is_ok();
            make_registration_code(&package_name, functions_info, write_r_code)
        }
        None => String::new(),
    };
    let out_dir = env::var_os("OUT_DIR").expect(
        "Environment variable 'OUT_DIR' is not set.  Are you running as part of 'cargo build'?",
    );
    let dest_path = Path::new(&out_dir).join("registration.rs");
    fs::write(&dest_path, snippet).unwrap_or_else(|err| {
        panic!("Could not write file '{}': {err}", dest_path.display());
    });
}

fn package_name_from_description() -> Option<String> {
    let description = fs::read_to_string("../../DESCRIPTION").ok()?;
    description
        .lines()
        .find_map(|line| line.strip_prefix("Package:"))
        .map(|name| name.trim().to_owned())
}

// Record, as "module::name, arg1, arg2", each function with the 'roxido' attribute in `file`
// and the modules it declares. `dir` is where the files of its child modules are.
fn find_roxido_functions(file: &Path, dir: &Path, functions_info: &mut Vec<String>) {
    // The compiler reports a missing module file or a syntax error better than this script could.
    let Ok(source) = fs::read_to_string(file) else {
        return;
    };
    let Ok(parsed) = syn::parse_file(&source) else {
        return;
    };
    let file_dir = file.parent().unwrap_or(Path::new("."));
    find_in_items(&parsed.items, dir, file_dir, functions_info);
}

fn find_in_items(
    items: &[syn::Item],
    dir: &Path,
    file_dir: &Path,
    functions_info: &mut Vec<String>,
) {
    for item in items {
        match item {
            syn::Item::Fn(item_fn) if is_active(&item_fn.attrs) => {
                let roxido_attr = item_fn.attrs.iter().find(|attr| {
                    attr.path()
                        .segments
                        .last()
                        .is_some_and(|s| s.ident == "roxido")
                });
                if let Some(attr) = roxido_attr {
                    functions_info.push(function_info(attr, item_fn));
                }
            }
            syn::Item::Mod(item_mod) if is_active(&item_mod.attrs) => {
                let name = item_mod.ident.to_string();
                let path_attr = item_mod.attrs.iter().find_map(|attr| match &attr.meta {
                    syn::Meta::NameValue(nv) if nv.path.is_ident("path") => match &nv.value {
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(s),
                            ..
                        }) => Some(s.value()),
                        _ => None,
                    },
                    _ => None,
                });
                match (&item_mod.content, path_attr) {
                    (Some((_, items)), _) => {
                        let child_dir = dir.join(&name);
                        find_in_items(items, &child_dir, &child_dir, functions_info);
                    }
                    (None, Some(path)) => {
                        let file = file_dir.join(path);
                        let child_dir = file.parent().unwrap_or(Path::new(".")).to_owned();
                        find_roxido_functions(&file, &child_dir, functions_info);
                    }
                    (None, None) => {
                        let flat = dir.join(format!("{name}.rs"));
                        let file = if flat.exists() {
                            flat
                        } else {
                            dir.join(&name).join("mod.rs")
                        };
                        find_roxido_functions(&file, &dir.join(&name), functions_info);
                    }
                }
            }
            _ => {}
        }
    }
}

fn function_info(attr: &syn::Attribute, item_fn: &syn::ItemFn) -> String {
    let mut line = String::new();
    if let syn::Meta::List(list) = &attr.meta {
        let options = list
            .parse_args_with(Punctuated::<syn::MetaNameValue, Token![,]>::parse_terminated)
            .unwrap_or_default();
        if let Some(module) = options.iter().find(|option| option.path.is_ident("module")) {
            line.push_str(&module.value.to_token_stream().to_string());
            line.push_str("::");
        }
    }
    line.push_str(&item_fn.sig.ident.to_string());
    for arg in &item_fn.sig.inputs {
        if let syn::FnArg::Typed(pat_type) = arg {
            line.push_str(", ");
            line.push_str(&pat_type.pat.to_token_stream().to_string());
        }
    }
    line
}

// Whether every '#[cfg(...)]' among `attrs` holds for this build. Cargo gives a build script the
// target's configuration in 'CARGO_CFG_*' and its features in 'CARGO_FEATURE_*'.
fn is_active(attrs: &[syn::Attribute]) -> bool {
    attrs
        .iter()
        .filter(|attr| attr.path().is_ident("cfg"))
        .all(|attr| match attr.parse_args::<syn::Meta>() {
            Ok(meta) => cfg_holds(&meta),
            // Assume an unrecognized predicate holds, so a mistake fails to compile rather than
            // leaving a function unregistered.
            Err(_) => true,
        })
}

fn cfg_holds(meta: &syn::Meta) -> bool {
    let env_name = |path: &syn::Path| {
        path.get_ident()
            .map(|ident| ident.to_string().to_uppercase().replace('-', "_"))
    };
    match meta {
        syn::Meta::Path(path) => match env_name(path) {
            Some(name) => env::var_os(format!("CARGO_CFG_{name}")).is_some(),
            None => true,
        },
        syn::Meta::NameValue(nv) => {
            let (
                Some(name),
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(value),
                    ..
                }),
            ) = (env_name(&nv.path), &nv.value)
            else {
                return true;
            };
            if name == "FEATURE" {
                let feature = value.value().to_uppercase().replace('-', "_");
                env::var_os(format!("CARGO_FEATURE_{feature}")).is_some()
            } else {
                env::var(format!("CARGO_CFG_{name}"))
                    .is_ok_and(|values| values.split(',').any(|v| v == value.value()))
            }
        }
        syn::Meta::List(list) => {
            let Ok(nested) =
                list.parse_args_with(Punctuated::<syn::Meta, Token![,]>::parse_terminated)
            else {
                return true;
            };
            if list.path.is_ident("all") {
                nested.iter().all(cfg_holds)
            } else if list.path.is_ident("any") {
                nested.iter().any(cfg_holds)
            } else if list.path.is_ident("not") {
                !nested.iter().all(cfg_holds)
            } else {
                true
            }
        }
    }
}

fn make_registration_code(
    package_name: &str,
    functions_info: Vec<String>,
    write_r_code: bool,
) -> String {
    let mut functions_info = functions_info;
    functions_info.sort_unstable();
    functions_info.dedup();
    let mut buffer = String::new();
    buffer.push_str("# Automatically regenerated. Do not edit.\n\n");
    for line in functions_info.iter() {
        let line_for_r = line.replace(':', "_");
        buffer.push_str("# .Call(.");
        buffer.push_str(&line_for_r);
        buffer.push_str(")\n");
    }
    buffer.push_str(
        r#"
#' @keywords internal
#' @usage NULL
#' @useDynLib "#,
    );
    buffer.push_str(package_name);
    buffer.push_str(
        r#", .registration = TRUE
"_PACKAGE""#,
    );
    if write_r_code && fs::read_to_string("../../R/roxido.R").ok().as_deref() != Some(&buffer) {
        let _ = fs::write("../../R/roxido.R", &buffer);
    }
    let mut snippet = String::new();
    let n_functions = functions_info.len();
    snippet.push_str(&format!(
        r#"use roxido::*;

#[unsafe(no_mangle)]
unsafe extern "C" fn R_init_{package_name}_rust(info: *mut rbindings::DllInfo) {{
    let mut call_routines = Vec::with_capacity({n_functions});
    let mut _names: Vec<std::ffi::CString> = Vec::with_capacity({n_functions});"#
    ));
    for line in functions_info {
        let tidbits: Vec<_> = line.split_whitespace().collect();
        let func_name_with_comma = tidbits[0];
        let (func_name, should_be_comma) =
            func_name_with_comma.split_at(func_name_with_comma.len() - 1);
        let func_name = if should_be_comma.is_empty() || should_be_comma != "," {
            func_name_with_comma
        } else {
            func_name
        };
        let func_name_for_r = func_name.replace(':', "_");
        let n_args = tidbits.len() - 1;
        snippet.push_str(&format!(
            r#"
    _names.push(std::ffi::CString::new(".{func_name_for_r}").unwrap());
    call_routines.push(rbindings::R_CallMethodDef {{
        name: _names.last().unwrap().as_ptr(),
        fun: unsafe {{ std::mem::transmute::<*const u8, rbindings::DL_FUNC>(crate::{func_name} as *const u8) }},
        numArgs: {n_args},
    }});"#
        ));
    }
    snippet.push_str(
        r#"
    call_routines.push(rbindings::R_CallMethodDef {
        name: std::ptr::null(),
        fun: None,
        numArgs: 0,
    });
    unsafe {
        rbindings::R_registerRoutines(
            info,
            std::ptr::null(),
            call_routines.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
        );
        rbindings::R_useDynamicSymbols(info, 0);
        rbindings::R_forceSymbols(info, 1);
        roxido::__private_init();
    }
}"#,
    );
    snippet
}
