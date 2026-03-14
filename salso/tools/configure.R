#!/usr/bin/env Rscript

fail <- function(...) {
  cat(..., "\n", sep = "")
  quit(save = "no", status = 1L, runLast = FALSE)
}

note <- function(...) {
  cat(..., "\n", sep = "")
}

cargo_bin_dir <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Windows")) {
    userprofile <- Sys.getenv("USERPROFILE", unset = "")
    if (!nzchar(userprofile)) {
      return("")
    }
    return(file.path(userprofile, ".cargo", "bin"))
  }
  path.expand("~/.cargo/bin")
}

tool_executable_name <- function(tool) {
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    paste0(tool, ".exe")
  } else {
    tool
  }
}

tool_path_in_dir <- function(dir, tool) {
  if (!nzchar(dir) || !dir.exists(dir)) {
    return("")
  }
  with_ext <- file.path(dir, tool_executable_name(tool))
  if (file.exists(with_ext)) {
    return(with_ext)
  }
  without_ext <- file.path(dir, tool)
  if (file.exists(without_ext)) {
    return(without_ext)
  }
  ""
}

normalize_tool_path <- function(path) {
  expanded <- path.expand(path)
  if (!file.exists(expanded)) {
    stop(sprintf("Path does not exist: %s", path))
  }
  gsub("\\\\", "/", expanded)
}

extract_semver <- function(version_lines, tool) {
  pattern <- sprintf("^\\s*%s\\s+([0-9]+\\.[0-9]+(\\.[0-9]+)?)\\b", tool)
  for (line in version_lines) {
    m <- regexec(pattern, line)
    parts <- regmatches(line, m)[[1]]
    if (length(parts) >= 2L) {
      return(list(version = parts[[2]], line = line))
    }
  }
  NULL
}

tool_info <- function(tool, tool_path) {
  if (!nzchar(tool_path)) {
    stop(sprintf("Could not find '%s'.", tool))
  }
  tool_path <- normalize_tool_path(tool_path)
  output <- tryCatch(
    system2(tool_path, "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) e
  )
  if (inherits(output, "error")) {
    stop(sprintf(
      "Could not run '%s --version': %s",
      tool_path,
      conditionMessage(output)
    ))
  }
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(sprintf(
      "'%s --version' returned non-zero exit status: %s",
      tool_path,
      status
    ))
  }
  if (length(output) < 1L || !any(nzchar(output))) {
    stop(sprintf("'%s --version' did not report a version string.", tool_path))
  }
  parsed <- extract_semver(output, tool)
  if (is.null(parsed)) {
    stop(
      sprintf(
        "Could not parse semantic version from '%s --version' output: %s",
        tool_path,
        paste(output, collapse = " | ")
      )
    )
  }
  list(path = tool_path, line = parsed$line, version = parsed$version)
}

read_msrv <- function(cargo_toml_path) {
  lines <- readLines(cargo_toml_path, warn = FALSE)
  for (line in lines) {
    if (grepl("^\\s*#", line)) {
      next
    }
    m <- regexec("^\\s*rust-version\\s*=\\s*\"([^\"]+)\"", line)
    parts <- regmatches(line, m)[[1]]
    if (length(parts) >= 2L) {
      return(parts[[2]])
    }
  }
  fail(sprintf("Could not find 'rust-version' in %s.", cargo_toml_path))
}

extract_vendor_if_present <- function(rust_dir) {
  tarball <- file.path(rust_dir, "vendor.tar.xz")
  if (!file.exists(tarball)) {
    note("No src/rust/vendor.tar.xz found; using default cargo behavior.")
    return(invisible(FALSE))
  }

  note(sprintf("Found %s; extracting vendored crates.", tarball))
  unlink(file.path(rust_dir, "vendor"), recursive = TRUE, force = TRUE)
  unlink(file.path(rust_dir, ".cargo"), recursive = TRUE, force = TRUE)
  unlink(file.path(rust_dir, ".cargo-home"), recursive = TRUE, force = TRUE)

  tar_result <- tryCatch(
    utils::untar(tarball, exdir = rust_dir, tar = "internal"),
    error = function(e) e
  )
  if (inherits(tar_result, "error")) {
    fail(sprintf(
      "Failed to extract %s: %s",
      tarball,
      conditionMessage(tar_result)
    ))
  }
  if (is.numeric(tar_result) && length(tar_result) == 1L && tar_result != 0L) {
    fail(sprintf("Extracting %s failed with status %s.", tarball, tar_result))
  }
  if (!dir.exists(file.path(rust_dir, "vendor"))) {
    fail(sprintf("Expected %s/vendor after extraction.", rust_dir))
  }
  config_path <- file.path(rust_dir, ".cargo", "config.toml")
  if (!file.exists(config_path)) {
    fail(sprintf("Expected %s after extraction.", config_path))
  }
  note("Vendored crates and cargo config are ready.")
  invisible(TRUE)
}

make_toolchain_candidates <- function() {
  path_cargo <- Sys.which("cargo")
  path_rustc <- Sys.which("rustc")
  personal_dir <- cargo_bin_dir()
  personal_cargo <- tool_path_in_dir(personal_dir, "cargo")
  personal_rustc <- tool_path_in_dir(personal_dir, "rustc")

  candidates <- list(
    list(
      name = "system PATH",
      cargo_path = path_cargo,
      rustc_path = path_rustc
    ),
    list(
      name = sprintf("personal cargo bin (%s)", personal_dir),
      cargo_path = personal_cargo,
      rustc_path = personal_rustc
    )
  )

  keep <- rep(TRUE, length(candidates))
  if (!nzchar(personal_dir)) {
    keep[[2]] <- FALSE
  }
  candidates <- candidates[keep]

  deduped <- list()
  seen <- character()
  for (candidate in candidates) {
    key <- paste(candidate$cargo_path, candidate$rustc_path, sep = "|")
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    deduped[[length(deduped) + 1L]] <- candidate
  }
  deduped
}

select_toolchain <- function(msrv) {
  candidates <- make_toolchain_candidates()
  diagnostics <- character()

  for (candidate in candidates) {
    missing <- character()
    if (!nzchar(candidate$cargo_path)) {
      missing <- c(missing, "cargo")
    }
    if (!nzchar(candidate$rustc_path)) {
      missing <- c(missing, "rustc")
    }
    if (length(missing) > 0L) {
      diagnostics <- c(
        diagnostics,
        sprintf(
          "* %s: missing %s",
          candidate$name,
          paste(missing, collapse = " and ")
        )
      )
      next
    }

    cargo <- tryCatch(
      tool_info("cargo", candidate$cargo_path),
      error = function(e) e
    )
    if (inherits(cargo, "error")) {
      diagnostics <- c(
        diagnostics,
        sprintf("* %s: %s", candidate$name, conditionMessage(cargo))
      )
      next
    }

    rustc <- tryCatch(
      tool_info("rustc", candidate$rustc_path),
      error = function(e) e
    )
    if (inherits(rustc, "error")) {
      diagnostics <- c(
        diagnostics,
        sprintf("* %s: %s", candidate$name, conditionMessage(rustc))
      )
      next
    }

    if (utils::compareVersion(rustc$version, msrv) < 0L) {
      diagnostics <- c(
        diagnostics,
        sprintf(
          "* %s: rustc %s does not satisfy MSRV %s",
          candidate$name,
          rustc$version,
          msrv
        )
      )
      next
    }

    return(list(candidate = candidate$name, cargo = cargo, rustc = rustc))
  }

  fail(
    paste(
      c(
        "Could not find a usable Rust toolchain.",
        "Checked system PATH and personal Cargo bin locations.",
        "Details:",
        diagnostics
      ),
      collapse = "\n"
    )
  )
}

discover_rust_dependency_files <- function(rust_dir) {
  rust_dir <- normalizePath(rust_dir, winslash = "/", mustWork = TRUE)
  files <- list.files(
    rust_dir,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  files <- files[file.exists(files) & !dir.exists(files)]
  if (length(files) < 1L) {
    fail(sprintf("No Rust files were found under %s.", rust_dir))
  }
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  inside <- startsWith(files, paste0(rust_dir, "/"))
  files <- files[inside]
  rel <- substring(files, nchar(rust_dir) + 2L)

  excluded_dirs <- c("target", "vendor", "\\.cargo", "\\.cargo-home")
  excluded_pattern <- paste0(
    "(^|/)(",
    paste(excluded_dirs, collapse = "|"),
    ")/"
  )
  keep <- !grepl(excluded_pattern, rel)
  rel <- rel[keep]
  rel <- rel[!(rel %in% c("librust.a", "roxido.txt"))]
  if (length(rel) < 1L) {
    fail(sprintf(
      "No Rust dependency files remained after filtering under %s.",
      rust_dir
    ))
  }

  deps <- file.path("rust", rel)
  sort(unique(deps))
}

render_rust_deps <- function(rust_dir, template_name) {
  deps <- c(template_name, discover_rust_dependency_files(rust_dir))
  lines <- sprintf("  %s", deps)
  if (length(lines) > 1L) {
    lines[seq_len(length(lines) - 1L)] <- paste0(
      lines[seq_len(length(lines) - 1L)],
      " \\"
    )
  }
  paste(c("RUST_DEPS = \\", lines), collapse = "\n")
}

render_makevars <- function(template_path, output_path, substitutions) {
  if (!file.exists(template_path)) {
    fail(sprintf("Could not find template: %s", template_path))
  }
  lines <- readLines(template_path, warn = FALSE)
  for (token in names(substitutions)) {
    lines <- gsub(token, substitutions[[token]], lines, fixed = TRUE)
  }
  all_text <- paste(lines, collapse = "\n")
  remaining <- unique(regmatches(all_text, gregexpr("@[A-Z_]+@", all_text))[[
    1
  ]])
  if (length(remaining) > 0L) {
    fail(sprintf(
      "Template %s has unreplaced tokens: %s",
      template_path,
      paste(remaining, collapse = ", ")
    ))
  }
  writeLines(lines, con = output_path, useBytes = TRUE)
}

render_platform_makevars <- function(
  cargo_path,
  rust_dir = file.path("src", "rust"),
  cran_vendor_mode = FALSE,
  host_triple = ""
) {
  sysname <- Sys.info()[["sysname"]]
  makevars <- file.path("src", "Makevars")
  makevars_win <- file.path("src", "Makevars.win")
  cargo_env <- ""
  if (isTRUE(cran_vendor_mode)) {
    cargo_env <- "CARGO_HOME=\"$(CURDIR)/$(RUST_DIR)/.cargo-home\""
  }
  escaped_cargo <- gsub("\"", "\\\\\"", cargo_path, fixed = TRUE)
  cargo_sub <- paste0("\"", escaped_cargo, "\"")
  unlink(c(makevars, makevars_win), force = TRUE)

  if (identical(sysname, "Windows")) {
    gnu_host <- identical(host_triple, "x86_64-pc-windows-gnu")
    if (gnu_host) {
      note(
        "GNU Windows host detected; building without --target (host = target)."
      )
      target_decl <- ""
      rust_target_lib <- "$(RUST_DIR)/target/release/librust.a"
      target_flag <- ""
      cargo_pre <- 'unset CARGO_BUILD_TARGET && export RUSTFLAGS="-C link-self-contained=yes" && '
    } else {
      note(
        "Non-GNU Windows host detected; cross-compiling with --target=x86_64-pc-windows-gnu."
      )
      target_decl <- "TARGET = x86_64-pc-windows-gnu"
      rust_target_lib <- "$(RUST_DIR)/target/$(TARGET)/release/librust.a"
      target_flag <- " --target=$(TARGET)"
      cargo_pre <- ""
    }
    render_makevars(
      file.path("src", "Makevars.win.in"),
      makevars_win,
      list(
        "@CARGO@" = cargo_sub,
        "@RUST_DEPS@" = render_rust_deps(rust_dir, "Makevars.win.in"),
        "@CARGO_ENV@" = cargo_env,
        "@TARGET_DECL@" = target_decl,
        "@RUST_TARGET_LIB@" = rust_target_lib,
        "@TARGET_FLAG@" = target_flag,
        "@CARGO_PRE@" = cargo_pre
      )
    )
    note("Rendered src/Makevars.win from template.")
  } else {
    render_makevars(
      file.path("src", "Makevars.in"),
      makevars,
      list(
        "@CARGO@" = cargo_sub,
        "@RUST_DEPS@" = render_rust_deps(rust_dir, "Makevars.in"),
        "@CARGO_ENV@" = cargo_env
      )
    )
    note("Rendered src/Makevars from template.")
  }
}

note("*** starting configure.R ***")
note(sprintf("Timestamp: %s", Sys.time()))

msrv <- read_msrv(file.path("src", "rust", "Cargo.toml"))
note(sprintf("MSRV (src/rust/Cargo.toml rust-version): %s", msrv))

toolchain <- select_toolchain(msrv)
cargo <- toolchain$cargo
rustc <- toolchain$rustc
note(sprintf("Selected Rust toolchain source: %s", toolchain$candidate))
note(sprintf("cargo path: %s", cargo$path))
note(sprintf("cargo --version: %s", cargo$line))
note(sprintf("rustc path: %s", rustc$path))
note(sprintf("rustc --version: %s", rustc$line))
note("MSRV check passed.")
rustc_vv <- tryCatch(
  system2(rustc$path, "-vV", stdout = TRUE, stderr = TRUE),
  error = function(e) "(could not run rustc -vV)"
)
note(paste(c("rustc -vV:", rustc_vv), collapse = "\n  "))

host_triple <- ""
for (line in rustc_vv) {
  m <- regexec("^\\s*host:\\s*(\\S+)", line)
  parts <- regmatches(line, m)[[1]]
  if (length(parts) >= 2L) {
    host_triple <- parts[[2]]
    break
  }
}
note(sprintf("Host triple: %s", host_triple))

cran_vendor_mode <- file.exists(file.path("src", "rust", "vendor.tar.xz"))
if (cran_vendor_mode) {
  note(
    "Detected src/rust/vendor.tar.xz; setting CARGO_HOME to a local directory."
  )
} else {
  note("No src/rust/vendor.tar.xz found; not setting CARGO_HOME.")
}

render_platform_makevars(
  cargo$path,
  cran_vendor_mode = cran_vendor_mode,
  host_triple = host_triple
)

dir.create("inst", showWarnings = FALSE, recursive = TRUE)
writeLines(cargo$version, con = file.path("inst", "cargo.log"), useBytes = TRUE)
note("Wrote inst/cargo.log.")

extract_vendor_if_present(file.path("src", "rust"))
note("*** finishing configure.R ***")
