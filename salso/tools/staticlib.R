FORCE_DOWNLOAD <- FALSE

if ( ( ! FORCE_DOWNLOAD ) && ( Sys.which("cargo") != "" ) ) {
  cat("\nCompiling static library.\n\n")
  args <- commandArgs(TRUE)
  target <- if ( length(args) > 0 ) paste0("--target",args[1]) else NULL
  status <- system2("cargo",c("build",target,"--release","--manifest-path=rustlib/Cargo.toml"))
  quit(status=status)
}

osType <- function() {
  if ( .Platform$OS.type == "windows" ) "windows"
  else {
    sysname <- Sys.info()["sysname"]
    if ( sysname == "Darwin" ) "macosx"
    else if ( sysname == "Linux" ) "linux"
    else sysname
  }
}
osType <- osType()

if ( ! ( osType %in% c("windows","macosx","linux") ) ) {
  cat(sprintf("\nThe package is not supported on this platform (%s).\n\n",osType))
  quit(status=1)
}

cat("\nDownloading static library.\n\n")

desc <- read.dcf("../DESCRIPTION")
pkgName    <- as.character(desc[,"Package"])
pkgVersion <- as.character(desc[,"Version"])

download.file(sprintf("https://dbdahl.github.io/rpackages/lib/%s/%s/%s.tar.gz",osType,pkgName,pkgVersion), "staticlib.tar.gz", quiet=TRUE)
untar(sprintf("staticlib.tar.gz",pkgVersion), exdir="..")
unlink("staticlib.tar.gz")

