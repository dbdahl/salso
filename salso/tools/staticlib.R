args <- commandArgs(TRUE)
target <- args[1]
targetArg <- if ( is.na(target) ) NULL else paste0("--target=",args[1])

status <- cargo::run(c("build", targetArg, "--release", "--manifest-path=rustlib/Cargo.toml"),
                     minimum_version="1.31", verbose=TRUE)

if ( ! is.null(status) && ( status == 0 ) ) quit(status=0)

cargo:::download_static_library(target,
        function(osName,pkgName,pkgVersion) sprintf("https://dbdahl.github.io/rpackages/lib/%s/%s/%s.tar.gz",osName,pkgName,pkgVersion))
