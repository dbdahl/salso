statlib_template <- commandArgs(TRUE)[1]
statlib          <- commandArgs(TRUE)[2]
target <- cargo::target()

cat("Target is: ", target, "\n", sep="")

if ( cargo::is_available("1.40") ) {

  cargo::run(c("build", paste0("--target=",target), "--release", "--manifest-path=rustlib/Cargo.toml"))

} else {

  cargo:::download_static_library(target,
    mkURL1=function(pkgName,pkgVersion,osName,target) {
      sprintf("https://daviddahl.org/rrepository/staticlib/%s_%s/%s/%s.tar.gz",pkgName,pkgVersion,osName,target)
    },
    mkURL2=function(pkgName,pkgVersion,osName,target) {
      sprintf("https://dahl.byu.edu/rrepository/staticlib/%s_%s/%s/%s.tar.gz",pkgName,pkgVersion,osName,target)
    }
  )

}

file.copy(file.path(gsub("___",target,statlib_template),basename(statlib)), statlib)
