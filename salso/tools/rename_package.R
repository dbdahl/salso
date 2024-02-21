rename_package <- function(current_name, new_name, package_root) {
  sed <- function(pattern, replacement, filename) {
    lines <- readLines(filename)
    lines <- gsub(pattern, replacement, lines)
    writeLines(lines, filename)
  }
  sed(current_name, new_name, file.path(package_root, "DESCRIPTION"))
  sed(current_name, new_name, file.path(package_root, "NAMESPACE"))
  new_package_Rd <- file.path(package_root, paste0("man/", new_name, "-package.Rd"))
  current_package_Rd <- file.path(package_root, paste0("man/", current_name, "-package.Rd"))
  sed(current_name, new_name, current_package_Rd)
  file.rename(current_package_Rd, new_package_Rd)
}
