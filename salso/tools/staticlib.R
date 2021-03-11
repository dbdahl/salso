lib_dir_template <- commandArgs(TRUE)[1]
statlib          <- commandArgs(TRUE)[2]
target <- cargo::target()

cat("Target is: ", target, "\n", sep="")

if ( cargo::is_available("1.42") ) {

  nCores <- Sys.getenv("R_CARGO_NCORES","1")
  if ( nCores == "0" ) nCores <- parallel::detectCores()
  cargo::run(c("build", "--jobs", nCores, "--target", target, "--release", "--manifest-path", "rustlib/Cargo.toml"))

} else {

  cargo:::download_static_library(target,
    mkURL1=function(pkgName,pkgVersion,osName,target) {
      sprintf("https://r.ddahl.org/staticlib/%s_%s/%s/%s.tar.gz",pkgName,pkgVersion,osName,target)
    },
    mkURL2=function(pkgName,pkgVersion,osName,target) {
      sprintf("https://dahl.byu.edu/rrepository/staticlib/%s_%s/%s/%s.tar.gz",pkgName,pkgVersion,osName,target)
    }
  )

}

dir.create(dirname(statlib), showWarnings=FALSE, recursive=TRUE)
file.copy(file.path(gsub("___",target,lib_dir_template),basename(statlib)), statlib)
