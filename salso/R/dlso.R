#' Draws-Based Latent Structure Optimization
#'
#' This function provides a partition to summarize a partition distribution
#' using the draws latent structure optimization (DLSO) method, which is also
#' known as the least-squares clustering method (Dahl 2006). The method seeks to
#' minimize an estimation criterion by picking the minimizer among the
#' partitions supplied by the \code{draws} argument. The implementation
#' currently supports the minimization of several partition estimation criteria.
#' For details on these criteria, see \code{\link{partition.loss}}.
#'
#' @param candidates A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B} rows
#'   represents a clustering of \eqn{n} items using cluster labels. For the
#'   \eqn{b}th clustering, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.  One of the rows will be used as the partition
#'   estimate.
#' @param loss See the documentation for this argument in
#'   \code{\link{partition.loss}}.
#' @param x See the documentation for this argument in
#'   \code{\link{partition.loss}}.
#'
#' @return An integer vector giving the estimated partition, encoded using
#'   cluster labels.
#'
#' @seealso \code{\link{partition.loss}}, \code{\link{psm}},
#'   \code{\link{summary.salso.estimate}}, \code{\link{salso}}
#'
#' @export
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' dlso(iris.clusterings, loss=VI())
#' probs <- psm(iris.clusterings, nCores=1)
#' dlso(iris.clusterings, loss=binder(), x=probs)
#'
#' # Compute loss with all draws, but pick the best among the first 10.
#' dlso(iris.clusterings[1:10,], loss=VI(), x=iris.clusterings)
#'
dlso <- function(candidates, loss=VI(), x=NULL) {
  if ( is.null(x) ) x <- candidates
  expectedLoss <- partition.loss(candidates, x, loss)
  index <- which.min(expectedLoss)
  estimate <-  candidates[index,]
  attr(estimate,"info") <- {
    if ( inherits(loss, "salso.loss") ) {
      if ( loss$loss == "binder" ) a <- loss$a
      loss <- loss$loss
    }
    attr <- list(loss=loss, expectedLoss=expectedLoss[index])
    as.data.frame(attr, row.names="")
  }
  attr(estimate,"draws") <- x
  attr(estimate,"psm") <- NULL
  class(estimate) <- "salso.estimate"
  estimate
}
