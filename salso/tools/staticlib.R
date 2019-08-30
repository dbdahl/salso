#if ( FALSE && ( Sys.which("cargo") != "" ) ) {
if ( Sys.which("cargo") != "" ) {
  system2("cargo",c("build","--release","--manifest-path=rustlib/Cargo.toml"))
  quit(status=0)
}

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

print(sprintf("%s %s %s\n",pkgName,pkgVersion,osType))

cat("I need to download.\n")


# Build against depending static libs
if ( FALSE ) {
#if ( ! file.exists("../windows/gifski-0.8.6/include/gifski.h") ) {
    download.file("https://github.com/rwinlib/gifski/archive/v0.8.6.zip", "lib.zip", quiet = TRUE)
    dir.create("../windows", showWarnings = FALSE)
    unzip("lib.zip", exdir = "../windows")
    unlink("lib.zip")
}

