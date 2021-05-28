source("../tools/cargo.R")

target_args <- if ( .Platform$OS.type == "windows" ) c("--target", target()) else NULL

if ( run("build", target_args, "--release", "--manifest-path", "rustlib/Cargo.toml") ) {

  if ( ! is.null(target_args) ) {
    args <- commandArgs(TRUE)
    lib_dir_template <- args[1]
    statlib          <- args[2]
    dir.create(dirname(statlib), showWarnings=FALSE, recursive=TRUE)
    file.copy(file.path(gsub("___",target_args[2],lib_dir_template),basename(statlib)), statlib)
  }

} else {

  download_staticlib(
    "https://r.ddahl.org/staticlib/${name}_${version}/${target}.tar.gz"
    ,
    "https://dahl.byu.edu/rrepository/staticlib/${name}_${version}/${target}.tar.gz"
  )

}
