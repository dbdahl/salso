#' Enumerate Permutations of Items
#'
#' This function produces a matrix whose rows provide all possible permutations
#' of the set \eqn{{1, 2, ..., n}}.
#'
#' @param nItems The size of the set \eqn{{1, 2, ..., n}}, i.e., \eqn{n}.
#'
#' @return A matrix of integers, where each row is a permutation.
#'
#' @export
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
  engine(as.integer(1:nItems))
}

