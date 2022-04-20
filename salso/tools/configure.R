source("tools/helpers.R")
setwd("src/rust")
if ( ! dir.exists("vendor") ) untar("vendor.tar.gz", tar="internal")

if ( cargo::run('build', '--offline', '--release', '--jobs', '2', minimum_version='../..') == 0 ) {
    file.copy("target/release/librust.a", "..", overwrite=TRUE)
    if ( ! file.exists("#SRC#") ) make_Rd_from_template(TRUE)
    cat("Built Rust static library.\n")
} else {
    setwd("../..")
    if ( file.exists("#SRC#") ) {
        cat("Cargo must be available for development. Hint: cargo::install().\n")
        q(status=1)
    }
    pkgname <- read.dcf("DESCRIPTION")[,"Package"]
    package_rust_source_in_installation_package(pkgname)
    deregister()
    make_Rd_from_template(FALSE)
    cat("Could not find a suitable Cargo installation.\n")
    cat("The package's Rust code can be compiled by the end user when Cargo is installed. Hint: cargo::install().\n")
}
