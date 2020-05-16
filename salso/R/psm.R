#' Compute an Adjacency or Pairwise Similarity Matrix
#'
#' If only one sample is provided, this function computes an adjacency matrix,
#' i.e., a binary matrix whose \eqn{(i,j)} element is one if and only if
#' elements \eqn{i} and \eqn{j} in the partition have the same cluster label. If
#' multiple samples are provided (as rows of the \code{x} matrix), this function
#' computes the \eqn{n}-by-\eqn{n} matrix whose \eqn{(i,j)} element gives the
#' relative frequency (i.e., estimated probability) that items \eqn{i} and
#' \eqn{j} are in the same subset (i.e., cluster).  This is the mean of the
#' adjacency matrices of the provided samples.
#'
#' @param x A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B} rows
#'   represents a clustering of \eqn{n} items using cluster labels. For
#'   clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.
#' @param parallel Should the computation use all CPU cores?
#'
#' @return A \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element gives
#'   the relative frequency that that items \eqn{i} and \eqn{j} are in the same
#'   subset (i.e., cluster).
#'
#' @export
#' @useDynLib salso .psm
#' @examples
#' partition <- iris.clusterings[1,]
#' psm(partition)
#'
#' dim(iris.clusterings)
#' # For examples, use 'parallel=FALSE' per CRAN rules but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' dim(probs)
#' probs[1:6, 1:6]
#'
psm <- function(x, parallel=TRUE) {
  if ( ! is.matrix(x) ) x <- t(x)
  y <- .Call(.psm, x, parallel)
  dim(y) <- rep(ncol(x), 2)
  dimnames(y) <- list(colnames(x), colnames(x))
  y
}
