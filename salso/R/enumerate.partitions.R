#' Enumerate Partitions of a Set
#'
#' This function produces a matrix whose rows provide all possible partitions of
#' the set \eqn{{1, 2, ..., n}}. These partitions are provided as cluster
#' labels, where two items are in the same subset (i.e., cluster) if their
#' labels are equal.
#'
#' @param nItems The size of the set \eqn{{1, 2, ..., n}}, i.e., \eqn{n}.
#'
#' @return A matrix of integers, where each row is a partition encoded as a
#'   vector of cluster labels.
#'
#' @export
#' @examples
#' # R_CARGO \dontrun{
#' # R_CARGO # Example disabled since Cargo was not found when installing from source package.
#' # R_CARGO # You can still run the example if you install Cargo. Hint: cargo::install().
#' enumerate.partitions(5)
#' # R_CARGO }
#'
enumerate.partitions <- function(nItems) {
  if ( nItems > 13 ) stop("Long vectors are not supported; 'nItems' may be at most 13.")
  .Call(.enumerate_partitions, nItems)
}
