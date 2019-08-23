#' Compute a Partition Loss Function
#'
#' These functions compute the Binder loss and the lower bound of the variation of information loss.
#'
#' @param partitions An integer matrix of cluster labels, where two items are in the same subset (i.e., cluster) if
#' their labels are equal.
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} elements gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#'
#' @export
#' @examples
#' probs <- psm(iris.clusterings)
#' binder(iris.clusterings[1:5,], probs)
#' VI.lb(iris.clusterings[1:5,], probs)
#'
binder <- function(partitions, psm) {
  expectedLoss(partitions, psm, FALSE)
}

#' @export
#' @rdname binder
#' @examples
#' VI.lb(c(1,1,1), psm(matrix(c(1,2,2,1,2,1), nrow=2, byrow=TRUE)))
#'
VI.lb <- function(partitions, psm) {
  expectedLoss(partitions, psm, TRUE)
}

#' @useDynLib salso .expected_loss
#'
expectedLoss <- function(partitions, psm, use.vilb) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( ! is.matrix(partitions) ) {
    dim(partitions) <- c(1,length(partitions))
  }
  if ( ncol(partitions) != nrow(psm) ) {
    stop("The length of 'partitions' is not equal to the number of rows of 'psm'.")
  }
  .Call(.expected_loss, partitions, psm, use.vilb)
}
