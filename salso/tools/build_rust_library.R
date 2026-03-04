sysname <- Sys.info()[["sysname"]]

original_path <- Sys.getenv("PATH")
cargo_bin_dir <- if (sysname == "Windows") {
  normalizePath(file.path(Sys.getenv("USERPROFILE"), ".cargo", "bin"))
} else {
  path.expand("~/.cargo/bin")
}
Sys.setenv(PATH = paste0(cargo_bin_dir, .Platform$path.sep, original_path))

setwd("src/rust")
vendor_tarball <- "vendor.tar.gz"
cran_build <- file.exists(vendor_tarball)
if (cran_build) {
  untar(vendor_tarball, tar = "internal")
  offline_option <- "--offline"
  jobs_option <- c("--jobs", "2")
  Sys.setenv(CARGO_HOME = file.path(getwd(), ".cargo"))
} else {
  offline_option <- NULL
  jobs_option <- NULL
}

if (sysname == "Windows") {
  target_option <- c("--target", "x86_64-pc-windows-gnu")
} else {
  target_option <- NULL
}

for (run_counter in 1:2) {
  Sys.setenv(R_CARGO_RUN_COUNTER = run_counter)
  status <- system2("cargo",
                    c("build", offline_option, "--release", jobs_option,
                      target_option))
  if (status != 0) {
    message("Error running Cargo.\n")
    message(paste0(readLines("../../INSTALL"), collapse = "\n"))
    stop("Exiting.")
  }
}
unlink("roxido.txt")

liba <- list.files("target", "librust.a", full.names = TRUE, recursive = TRUE)
liba <- liba[grepl("(^|/|\\\\)release(/|\\\\)", liba)]
liba <- if (length(liba) > 0) {
  file_info <- file.info(liba)
  file_info <- rownames(file_info)[which(file_info$mtime == max(file_info$mtime))]
  file_info[which.min(nchar(file_info))]
} else {
  message("No matching files found.")
  stop("Exiting.")
}

file.copy(liba, "..", overwrite = TRUE)
if (cran_build) {
  message("Deleting temporary CARGO_HOME directory.")
  unlink(".cargo", recursive = TRUE, force = TRUE)
  unlink("target", recursive = TRUE, force = TRUE, expand = FALSE)
  unlink("vendor", recursive = TRUE, force = TRUE, expand = FALSE)
}
message("Built Rust static library.")
