target_args <- if ( .Platform$OS.type == "windows" ) c("--target", cargo::target()) else NULL

if ( compareVersion(as.character(packageVersion("cargo")),"0.1.28") <= 0 ) {
  cat("Older version of cargo package found.  Setting RUSTC environment variable myself.\n")
  find_cmd <- function(what) {
    if ( .Platform$OS.type=="windows" ) what <- paste0(what,".exe")
    if ( Sys.getenv("R_CARGO_HOME","<unset>") != "<unset>" ) {
      candidate <- file.path(Sys.getenv("R_CARGO_HOME"),"bin",what)
      if ( file.exists(candidate) ) return(candidate)
    }
    if ( Sys.getenv("CARGO_HOME","<unset>") != "<unset>" ) {
      candidate <- file.path(Sys.getenv("CARGO_HOME"),"bin",what)
      if ( file.exists(candidate) ) return(candidate)
    }
    candidate <- Sys.which(what)
    if ( candidate != "" && file.exists(candidate) ) return(candidate)
    candidate <- file.path("~",".cargo","bin",what)
    if ( file.exists(candidate) ) return(candidate)
    candidate <- file.path(Sys.getenv(ifelse(.Platform$OS.type=="windows","USERPROFILE","HOME")),".cargo","bin",what)
    if ( file.exists(candidate) ) return(candidate)
    NULL
  }
  rustc_cmd <- find_cmd("rustc")
  if ( is.null(rustc_cmd) ) {
    cat("The Rust compiler (rustc) is not found. Please see the package's INSTALL instructions.\n")
  } else {
    n <- function(x) normalizePath(x, mustWork=FALSE)
    rustc_cmd <- n(rustc_cmd)
    cat(sprintf("rustc executable: %s\n",rustc_cmd))
    Sys.setenv(RUSTC=rustc_cmd)
  }
}

if ( cargo::run("build", target_args, "--release", "--manifest-path", "rustlib/Cargo.toml", minimum_version="1.42") ) {

  if ( ! is.null(target_args) ) {
    args <- commandArgs(TRUE)
    lib_dir_template <- args[1]
    statlib          <- args[2]
    dir.create(dirname(statlib), showWarnings=FALSE, recursive=TRUE)
    file.copy(file.path(gsub("___",target_args[2],lib_dir_template),basename(statlib)), statlib)
  }

} else {

  cargo:::download_staticlib(
    "https://r.ddahl.org/staticlib/${name}_${version}/${target}.tar.gz"
    ,
    "https://dahl.byu.edu/rrepository/staticlib/${name}_${version}/${target}.tar.gz"
  )

}
