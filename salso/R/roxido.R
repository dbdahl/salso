#' @docType package
#' @usage NULL
#' @useDynLib salso, .registration = TRUE
NULL

.Kall <- function(...) {
  x <- .Call(...)
  if (inherits(x, "error")) stop(x) else x
}
