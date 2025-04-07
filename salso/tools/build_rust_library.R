sysname <- Sys.info()[["sysname"]]
arch <- R.version$arch

dcf <- read.dcf("DESCRIPTION")

system_requirements <- unname(dcf[, "SystemRequirements"])
message(sprintf("SystemRequirements: %s", system_requirements))

msrv <- gsub(".*rustc\\s*\\(\\s*>=\\s*(*[^)]*)\\).*", "\\1",
             system_requirements)

if (msrv == system_requirements) {
  message("Could not determine the Minimum Supported Rust Version (MSRV).")
  message("The DESCRIPTION file should have an explicit rustc version such as:")
  message("    Cargo (Rust's package manager), rustc (>= 1.84).")
  stop("Exiting.")
} else {
  message(sprintf("Minimum Supported Rust Version (MSRV): %s", msrv))
}

get_version <- function(what = c("cargo", "rustc")[1]) {
  version_string_full <- tryCatch({
    message(sprintf("Trying to run: %s --version", what))
    system2(what, "--version", stdout = TRUE)
  },
  error = function(e) NULL,
  warning = function(e) NULL)
  if (is.null(version_string_full)) return(NULL)
  message(sprintf("'%s --version' reports: %s", what, version_string_full))

  version_string <- gsub("^(cargo|rustc)\\s*([^ ]+).*$", "\\2",
                         version_string_full)
  if (version_string == version_string_full) {
    message(sprintf("Could not determine %s version based on this output:",
                    what))
    message(version_string_full)
    NULL
  } else {
    dir.create("inst", showWarnings = FALSE, recursive = FALSE)
    writeLines(version_string, con = file.path("inst", "cargo.log"))
    version_string
  }
}

check_msrv <- function() {
  cargo_version_string <- get_version("cargo")
  if (is.null(cargo_version_string)) return(FALSE)
  message(sprintf("Found cargo version: %s", cargo_version_string))
  rustc_version_string <- get_version("rustc")
  if (is.null(rustc_version_string)) return(FALSE)
  message(sprintf("Found rustc version: %s", rustc_version_string))
  if (compareVersion(rustc_version_string, msrv) == -1L) {
    message("MSRV is *not* met.")
    FALSE
  } else {
    message("MSRV is met.")
    TRUE
  }
}

original_path <- Sys.getenv("PATH")
cargo_bin_dir <- if (sysname == "Windows") {
  normalizePath(file.path(Sys.getenv("USERPROFILE"), ".cargo", "bin"))
} else {
  path.expand("~/.cargo/bin")
}
Sys.setenv(PATH = paste0(cargo_bin_dir, .Platform$path.sep, original_path))

if (!check_msrv()) {
  message("Trying again with the original PATH variable.")
  Sys.setenv(PATH = original_path)
  if (!check_msrv()) {
    message("Could not find a suitable installation of cargo and rustc.")
    message(paste0(readLines("INSTALL"), collapse = "\n"))
    stop("Exiting.")
  }
}

message("Found a suitable installation of cargo and rustc.")

setwd("src/rust")
vendor_tarball <- "vendor.tar.gz"
cran_build <- file.exists(vendor_tarball)
if (cran_build) {
  untar(vendor_tarball, tar = "internal")
  offline_option <- "--offline"
  jobs_option <- c("--jobs", "2")
  cargo_home_env <- new.env()
  assign("cargo_home", file.path(getwd(), ".cargo"), envir = cargo_home_env)
  Sys.setenv(CARGO_HOME = get("cargo_home", envir = cargo_home_env))
  reg.finalizer(cargo_home_env, function(x) {
    message("Deleting temporary CARGO_HOME directory.")
    unlink(get("cargo_home", envir = cargo_home_env),
           recursive = TRUE, force = TRUE)
  }, onexit = TRUE)
} else {
  offline_option <- NULL
  jobs_option <- NULL
}

for (run_counter in 1:2) {
  Sys.setenv(R_CARGO_RUN_COUNTER = run_counter)
  status <- system2("cargo",
                    c("build", offline_option, "--release", jobs_option))
  if (status != 0) {
    message("Error running Cargo.\n")
    message(paste0(readLines("../../INSTALL"), collapse = "\n"))
    stop("Exiting.")
  }
}
unlink("roxido.txt")

file.copy("target/release/librust.a", "..", overwrite = TRUE)
if (cran_build) {
  unlink("target", recursive = TRUE, force = TRUE, expand = FALSE)
  unlink("vendor", recursive = TRUE, force = TRUE, expand = FALSE)
}
message("Built Rust static library.")
