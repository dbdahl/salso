#' Compute the Pairwise Similarity Matrix
#'
#' Based on the provided samples, this function computes the \eqn{n}-by-\eqn{n}
#' matrix whose \eqn{(i,j)} element gives the (estimated) probability that items
#' \eqn{i} and \eqn{j} are in the same subset (i.e., cluster).  This is the mean
#' of the adjacency matrices of the provided samples.
#'
#' @param x A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B} rows
#'   represents a clustering of \eqn{n} items using cluster labels. For
#'   clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.
#' @param parallel Should the computation use all CPU cores?
#'
#' @return A \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element
#'   gives the estimated expected number of times that items \eqn{i} and
#'   \eqn{j} are in the same subset (i.e., cluster or feature) based on the
#'   frequencies from the supplied clusterings or feature allocations.
#'
#' @seealso \code{\link{adjacency.matrix}}
#'
#' @export
#' @useDynLib salso .psm
#' @examples
#' dim(iris.clusterings)
#' # Use 'parallel=FALSE' per CRAN rules for examples but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' dim(probs)
#' probs[1:6, 1:6]
#'
psm <- function(x, parallel=TRUE) {
  y <- .Call(.psm, x, parallel)
  dim(y) <- rep(ncol(x), 2)
  dimnames(y) <- list(colnames(x), colnames(x))
  y
}
