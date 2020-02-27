#' Compute the Bell Number
#'
#' These functions compute the Bell number (the number of partitions of a set)
#' or its natural logarithm.
#'
#' @param nItems The size of the set \eqn{{1, 2, ..., n}}.
#'
#' @return A numeric vector of length one giving the Bell number or its natural
#'   logarithm.
#'
#' @export
#' @useDynLib salso .bell
#' @rdname bell
#' @examples
#' bell(12)
#' lbell(300)
#' all.equal( bell(5), exp(lbell(5)) )
#'
bell <- function(nItems) {
  .Call(.bell, nItems)
}

#' @export
#' @useDynLib salso .lbell
#' @rdname bell
#'
lbell <- function(nItems) {
  .Call(.lbell, nItems)
}
