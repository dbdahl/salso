target <- cargo::target()

if ( ! cargo::run("build", "--target", target, "--release", "--manifest-path", "rustlib/Cargo.toml", minimum_version="1.42") ) {

  cargo:::download_staticlib(target,
    "https://r.ddahl.org/staticlib/${name}_${version}/${target}.tar.gz"
    ,
    "https://dahl.byu.edu/rrepository/staticlib/${name}_${version}/${target}.tar.gz"
  )

}

args <- commandArgs(TRUE)
lib_dir_template <- args[1]
statlib          <- args[2]
dir.create(dirname(statlib), showWarnings=FALSE, recursive=TRUE)
file.copy(file.path(gsub("___",target,lib_dir_template),basename(statlib)), statlib)
