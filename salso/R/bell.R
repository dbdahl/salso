#' Compute the Bell Number
#'
#' These functions compute the Bell number (the number of partitions of a given
#' number of items) or its natural logarithm.
#'
#' @param nItems The size of the set \eqn{{1, 2, ..., n}}.
#'
#' @return A numeric vector of length one giving the Bell number or its natural
#'   logarithm.
#'
#' @export
#' @rdname bell
#' @examples
#' # R_CARGO \dontrun{
#' # R_CARGO # Example disabled since Cargo was not found when installing from source package.
#' # R_CARGO # You can still run the example if you install Cargo. Hint: cargo::install().
#' bell(12)
#' lbell(300)
#' all.equal( bell(5), exp(lbell(5)) )
#' # R_CARGO }
#'
bell <- function(nItems) {
  .Call(.bell, nItems)
}

#' @export
#' @rdname bell
#'
lbell <- function(nItems) {
  .Call(.lbell, nItems)
}
