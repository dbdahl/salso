args <- commandArgs(TRUE)
target <- if ( length(args) > 0 ) paste0("--target=",args[1]) else NULL
cargo::ensure("1.31")
status <- cargo::run(c("build",target,"--release","--manifest-path=rustlib/Cargo.toml"))
quit(status=status)
