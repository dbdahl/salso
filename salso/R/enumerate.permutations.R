#' Enumerate Permutations of Items
#'
#' This function produces a matrix whose rows provide all possible permutations of
#' the set of integers \code{\{0, 1, ..., n-1\}}.
#'
#' @param nItems The size of the set \code{\{0, 1, ..., n-1\}}, i.e., \code{n}.
#'
#' @return A matrix of integers, where each row is a permutation.
#'
#' @export
#' @useDynLib salso .enumerate_partitions
#' @examples
#' enumerate.permutations(5)
#'
enumerate.permutations <- function(nItems) {
  if ( nItems == 0 ) return(matrix(integer(), nrow=0, ncol=0))
  engine <- function(x) {
    if ( length(x) == 1 ) x
    else {
      result <- matrix(nrow=0, ncol=length(x))
      for ( i in seq_along(x) ) {
        result <- rbind(result, cbind(x[i], Recall(x[-i])))
      }
      result
    }
  }
  engine(as.integer(0:(nItems-1L)))
}
