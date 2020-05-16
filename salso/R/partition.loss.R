#' Compute a Partition Loss
#'
#' Given partitions \eqn{\pi*} and \eqn{\pi}, \code{\link{partition.loss}}
#' computes the loss when using \eqn{\pi*} to estimate \eqn{\pi}, based on one
#' of three partition losses. Smaller loss values indicate higher concordance
#' between partitions. The implementation currently supports the computation of
#' the following partition loss functions: "binder", "pear", and "VI.lb". For
#' details on these criteria, see \code{\link{partition.expected.loss}}.
#'
#' The functions \code{\link{randi}}, \code{\link{arandi}}, and
#' \code{\link{vi.dist}} are convenience functions. Note that:
#' \itemize{
#' \item \code{partition.loss(p1, p2, "binder") = choose(length(p1), 2) * ( 1 - randi(p1, p2) )}
#' \item \code{partition.loss(p1, p2, "pear") = 1 - arandi(p1, p2)}
#' \item \code{partition.loss(p1, p2, "VI.lb") = vi.dist(p1, p2)}
#' }
#'
#' @param partition1 An integer vector of cluster labels for \eqn{n} items. Two
#'   items are in the same subset (i.e., cluster) if their labels are equal.
#' @param partition2 An integer vector of cluster labels having the same length
#'   as \code{partition1}.
#' @param loss One of \code{"binder"}, \code{"pear"}, or \code{"VI.lb"}.  See
#'   \code{\link{partition.expected.loss}} for details on these loss functions.
#'
#' @return A numeric scalar giving the loss.
#'
#' @seealso \code{\link{partition.expected.loss}}
#'
#' @export
#' @examples
#' p1 <- iris.clusterings[1,]
#' p2 <- iris.clusterings[2,]
#'
#' partition.loss(p1, p2, loss="binder")
#' partition.loss(p1, p2, loss="pear")
#' partition.loss(p1, p2, loss="VI.lb")
#'
#' all.equal(partition.loss(p1, p2, "binder"), choose(length(p1), 2) * ( 1 - randi(p1, p2) ))
#' all.equal(partition.loss(p1, p2, "pear"), 1 - arandi(p1, p2))
#' all.equal(partition.loss(p1, p2, "VI.lb"), vi.dist(p1, p2))
#'
partition.loss <- function(partition1, partition2, loss=c("binder", "pear", "VI")[3]) {
  expected.loss(partition1, adjacency.matrix(partition2), lossCode(loss))
}

#' @export
#' @rdname partition.loss
randi <- function(partition1, partition2) {
  1 - ( partition.loss(partition1, partition2, "binder") / choose(length(partition1),2) )
}

#' @export
#' @rdname partition.loss
arandi <- function(partition1, partition2) {
  1 - partition.loss(partition1, partition2, "pear")
}

#' @export
#' @rdname partition.loss
vi.dist <- function(partition1, partition2) {
  VI(partition1, partition2)
}
