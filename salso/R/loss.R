#' Compute a Partition Loss Function
#'
#' Based on the supplied pairwise similarity matrix, these functions compute,
#' for the given partitions, 1. the expectation of the Binder loss
#' (\code{binder}), 2. the loss for the adjusted Rand index with the posterior
#' expected clustering (\code{lpear}), and 3. the lower bound of the expectation
#' of the variation of information loss (\code{VI.lb}).
#'
#' @param partitions An integer matrix of cluster labels, where each row is a
#'   partition given as cluster labels. Two items are in the same subset (i.e.,
#'   cluster) if their labels are equal.
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#'
#' @return A numeric vector of length equal to the number of rows of
#'   \code{partitions}, where each element gives the value of the loss function.
#'
#' @export
#' @examples
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' binder(iris.clusterings[1:5,], probs)
#' lpear(iris.clusterings[1:5,], probs)
#' VI.lb(iris.clusterings[1:5,], probs)
#'
binder <- function(partitions, psm) {
  expectedLoss(partitions, psm, 0L)
}

#' @export
#' @rdname binder
#'
lpear <- function(partitions, psm) {
  expectedLoss(partitions, psm, 1L)
}

#' @export
#' @rdname binder
#'
VI.lb <- function(partitions, psm) {
  expectedLoss(partitions, psm, 2L)
}

#' @useDynLib salso .expected_loss
#'
expectedLoss <- function(partitions, psm, lossCode) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( ! is.matrix(partitions) ) {
    dim(partitions) <- c(1,length(partitions))
  }
  if ( ncol(partitions) != nrow(psm) ) {
    stop("The length of 'partitions' is not equal to the number of rows of 'psm'.")
  }
  .Call(.expected_loss, partitions, psm, lossCode)
}
