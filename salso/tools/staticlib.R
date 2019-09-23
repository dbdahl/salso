FORCE_DOWNLOAD <- as.logical(Sys.getenv("RUSTLIB_FORCE_DOWNLOAD",unset="FALSE"))

args <- commandArgs(TRUE)
target <- if ( length(args) > 0 ) args[1] else NULL
cat("RUSTLIB_FORCE_DOWNLOAD=",FORCE_DOWNLOAD,"\n",sep="")

requiredCargoVersion <- "1.31.0"
if ( ( ! FORCE_DOWNLOAD ) && ( Sys.which("cargo") != "" ) ) {
  installedCargoVersion <- gsub("cargo ([^ ]+) .*", "\\1", system2("cargo","--version",stdout=TRUE))
  cat(sprintf("\nCargo %s is found", installedCargoVersion))
  if ( compareVersion(installedCargoVersion, requiredCargoVersion) < 0 ) {
    cat(sprintf(" but Cargo (>= %s) is required for compilation.\n\n", requiredCargoVersion))
  } else {
    cat("; compiling static library.\n\n")
    targetArg <- if ( is.null(target) ) NULL else paste0("--target=",target)
    status <- system2("cargo",c("build",targetArg,"--release","--manifest-path=rustlib/Cargo.toml"))
    cat("\n")
    quit(status=status)
  }
}

osType <- function() {
  info <- Sys.info()
  sysname <- info["sysname"]
  if ( ( ! grepl("^x86", info["machine"]) ) || ( ! ( sysname %in% c("Windows","Darwin","Linux") ) ) ) sprintf("%s-%s",info["sysname"],info["machine"])
  else if ( sysname == "Windows" ) "windows"
  else if ( sysname == "Darwin" ) "macosx"
  else if ( sysname == "Linux" ) "linux"
  else sysname
}
osType <- osType()

if ( ! ( osType %in% c("windows","macosx","linux") ) ) {
  cat(sprintf("\nCargo (>= %s) is not installed.\n\n",requiredCargoVersion))
  cat(paste(readLines("../INSTALL"),collapse="\n"))
  cat("\n")
  quit(status=1)
}

cat(sprintf("\nDownloading static library for %s.\n\n",osType))

desc <- read.dcf("../DESCRIPTION")
pkgName    <- as.character(desc[,"Package"])
pkgVersion <- as.character(desc[,"Version"])

download.file(sprintf("https://dbdahl.github.io/rpackages/lib/%s/%s/%s.tar.gz",osType,pkgName,pkgVersion), "staticlib.tar.gz", quiet=TRUE)
untar(sprintf("staticlib.tar.gz",pkgVersion), exdir="..")
unlink("staticlib.tar.gz")

if ( osType == "windows" ) {
  destDir <- sprintf("rustlib/target/%s/release", target)
  headDir <- if ( substr(target,1,3) == "x86" ) "x64" else "i386"
  dir.create(destDir, recursive=TRUE, showWarnings=FALSE)
  invisible(file.rename(sprintf("../src-%s/%s/rustlib.lib", headDir, destDir),
                        sprintf(          "%s/rustlib.lib",          destDir)))
}

