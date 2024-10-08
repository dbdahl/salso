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
#' @inheritParams salso
#'
#' @return A \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element gives
#'   the relative frequency that that items \eqn{i} and \eqn{j} are in the same
#'   subset (i.e., cluster).
#'
#' @export
#' @importFrom utils head
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' data(iris.clusterings)
#' partition <- iris.clusterings[1,]
#' psmatrix <- psm(partition, nCores=1)
#' psmatrix[1:6, 1:6]
#'
#' dim(iris.clusterings)
#' probs <- psm(iris.clusterings, nCores=1)
#' dim(probs)
#' probs[1:6, 1:6]
#'
psm <- function(x, nCores=0) {
  if ( nCores < 0.0 ) stop("'nCores' may not be negative.")
  if ( nCores > .Machine$integer.max ) nCores <- .Machine$integer.max
  if ( ! is.matrix(x) ) x <- t(x)
  y <- .Call(.psm, x, nCores)
  dim(y) <- rep(ncol(x), 2)
  dimnames(y) <- list(colnames(x), colnames(x))
  y
}
