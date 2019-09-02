#' Compute the Pairwise Similarity Matrix
#'
#' This function computes the \code{n}-by-\code{n} matrix whose \code{(i,j)}
#' element gives the (estimated) probability that items \code{i} and \code{j}
#' are in the same subset (i.e, cluster).
#'
#' @param x A \code{B}-by-\code{n} matrix, where each of the \code{B} rows
#'   represents a clustering of \code{n} items using cluster labels.  For
#'   clustering \code{b}, items \code{i} and \code{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.
#' @param parallel Should the computation use all CPU cores?
#'
#' @return A \code{n}-by-\code{n} symmetric matrix whose \code{(i,j)} elements
#'   gives the estimated expected number of times that items \code{i} and
#'   \code{j} are in the same subset (i.e, cluster or feature) based on the
#'   frequencies from the supplied clusterings or feature allocations.
#'
#' @export
#' @useDynLib salso .psm
#' @examples
#' dim(iris.clusterings)
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' dim(probs)
#'
psm <- function(x, parallel=TRUE) {
  y <- .Call(.psm, x, parallel)
  dim(y) <- rep(ncol(x), 2)
  dimnames(y) <- list(colnames(x), colnames(x))
  y
}