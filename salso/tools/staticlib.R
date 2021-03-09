target <- commandArgs(TRUE)[1]

if ( ! cargo::is_available("1.40") ) {
  cargo:::download_static_library(target,
    mkURL2=function(pkgName,pkgVersion,osName,target) {
      sprintf("https://daviddahl.org/rrepository/staticlib/%s_%s/%s/%s.tar.gz",pkgName,pkgVersion,osName,target)
    },
    mkURL1=function(pkgName,pkgVersion,osName,target) {
      sprintf("https://dahl.byu.edu/rrepository/staticlib/%s_%s/%s/%s.tar.gz",pkgName,pkgVersion,osName,target)
    }
    )
  quit(status=0)
}

status <- cargo::run(c("build", paste0("--target=",target), "--release", "--manifest-path=rustlib/Cargo.toml"))
quit(status=status)
