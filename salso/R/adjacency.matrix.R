#' Compute an Adjacency Matrix
#'
#' Given a partition, this function computes an adjacency matrix, i.e., binary
#' matrix whose \eqn{(i,j)} element is one if and only if elements \eqn{i} and
#' \eqn{j} in the partition have the same cluster label.
#'
#' @param partition An integer vector of cluster labels for \eqn{n} items. Two
#'   items are in the same subset (i.e., cluster) if their labels are equal.
#'
#' @return An adjacency matrix, i.e., binary matrix whose \eqn{(i,j)} element is
#'   one if and only if \eqn{i} and \eqn{j} have the same cluster label in the
#'   supplied partition.
#'
#' @seealso \code{\link{psm}}
#'
#' @export
#' @examples
#' partition <- iris.clusterings[1,]
#'
#' adjacency.matrix(partition)
#'
adjacency.matrix <- function(partition) {
  if ( ! is.vector(partition) ) stop("'partition' should be a vector.")
  n <- length(partition)
  matrix( 1L*( rep(partition,times=n) == rep(partition,each=n) ), nrow=n)
}
