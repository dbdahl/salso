args <- commandArgs(TRUE)
target <- args[1]

if ( ! cargo::is_available("1.40") ) {
  cargo:::download_static_library(target, function(osName,pkgName,pkgVersion) {
    sprintf("https://dbdahl.github.io/rpackages/lib/%s/%s/%s.tar.gz",osName,pkgName,pkgVersion)
  })
  quit(status=0)
}

targetArg <- if ( is.na(target) ) NULL else paste0("--target=",args[1])
status <- cargo::run(c("build", targetArg, "--release", "--manifest-path=rustlib/Cargo.toml"))
quit(status=status)
