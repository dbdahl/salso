if ( FALSE && ( Sys.which("cargo") != "" ) ) {
#if ( Sys.which("cargo") != "" ) {
  system2("cargo",c("build","--release","--manifest-path=rustlib/Cargo.toml"))
  quit(status=0)
}

cat("Downloading static library.\n")

osType <- function() {
  if ( .Platform$OS.type == "windows" ) "windows"
  else {
    sysname <- Sys.info()["sysname"]
    if ( sysname == "Darwin" ) "mac"
    else if ( sysname == "Linux" ) "linux"
    else ""
  }
}
osType <- osType()

desc <- read.dcf("../DESCRIPTION")
pkgName    <- as.character(desc[,"Package"])
pkgVersion <- as.character(desc[,"Version"])

download.file(sprintf("https://dbdahl.github.io/rpackages/lib/%s/%s/%s.tar.gz",osType,pkgName,pkgVersion), "staticlib.tar.gz", quiet=TRUE)
untar(sprintf("staticlib.tar.gz",pkgVersion), exdir="..")
unlink("staticlib.tar.gz")

