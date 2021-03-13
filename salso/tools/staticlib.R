lib_dir_template <- commandArgs(TRUE)[1]
statlib          <- commandArgs(TRUE)[2]
target <- cargo::target()

cat("Target is: ", target, "\n", sep="")

if ( ! cargo::build("1.42", "--target", target, "--release", "--manifest-path", "rustlib/Cargo.toml") ) {

  cargo:::download_staticlib(target,
    "https://r.ddahl.org/staticlib/${name}_${version}/${target}.tar.gz"
    ,
    "https://dahl.byu.edu/rrepository/staticlib/${name}_${version}/${target}.tar.gz"
  )

}

dir.create(dirname(statlib), showWarnings=FALSE, recursive=TRUE)
file.copy(file.path(gsub("___",target,lib_dir_template),basename(statlib)), statlib)
