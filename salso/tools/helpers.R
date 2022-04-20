# Assumes working directory is package root.
make_Rd_from_template <- function(shlib) {
    rdfiles <- list.files("man")
    for ( rdfile in rdfiles ) {
        pattern <- "^#\\s*R_CARGO\\s*(.*)"
        lines <- readLines(paste0("man/",rdfile))
        lines <- if ( shlib ) {
            lines[!grepl(pattern, lines)]
        } else {
            sub(pattern,"\\1",lines)
        }
        writeLines(lines, paste0("man/",rdfile))
    }
}

# Assumes working directory is package root.
package_rust_source_in_installation_package <- function(pkgname) {
    dir.create("inst", showWarnings=FALSE)
    file.rename("src/rust", "inst/rust")
    unlink("src", recursive=TRUE)
    unlink("inst/rust/vendor", recursive=TRUE)
    unlink("inst/rust/.cargo", recursive=TRUE)
    lines <- c('','#[no_mangle]',
               sprintf('extern "C" fn R_init_%s(info: *mut rbindings::DllInfo) {', pkgname),
               sprintf('    R_init_%s_rust(info);', pkgname),
               '}')
    cat(paste0(lines,collapse="\n"), "\n", sep="", file="inst/rust/src/registration.rs", append=TRUE)
    cargo_toml_filename <- "inst/rust/Cargo.toml"
    lines <- readLines(cargo_toml_filename)
    lines <- sub('^(\\s*crate-type\\s*=\\s*\\[\\s*)".*"','\\1"cdylib"',lines)
    writeLines(lines, cargo_toml_filename)
}

# Assumes working directory is package root.
deregister <- function() {
    lines <- readLines("NAMESPACE")
    lines <- lines[!grepl("useDynLib", lines, fixed=TRUE)]
    writeLines(lines, "NAMESPACE")
    file.rename("tools/registration.R", "R/registration.R")
}
