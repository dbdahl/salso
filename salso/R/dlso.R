#' Latent Structure Optimization Based on Draws
#'
#' This function provides a partition to summarize a partition distribution
#' using the draws-based latent structure optimization (DLSO) method, which is
#' also known as the least-squares clustering method (Dahl 2006). The method
#' seeks to minimize an estimation criterion by picking the minimizer among the
#' partitions supplied. The implementation currently supports the minimization
#' of several partition estimation criteria. For details on these criteria, see
#' \code{\link{partition.loss}}.
#'
#' @inheritParams partition.loss
#'
#' @return An integer vector giving the estimated partition, encoded using
#'   cluster labels.
#'
#' @seealso \code{\link{partition.loss}}, \code{\link{psm}},
#'   \code{\link{summary.salso.estimate}}, \code{\link{salso}}
#'
#' @references
#'
#' D. B. Dahl (2006), Model-Based Clustering for Expression Data via a Dirichlet
#' Process Mixture Model, in \emph{Bayesian Inference for Gene Expression and
#' Proteomics}, Kim-Anh Do, Peter Müller, Marina Vannucci (Eds.), Cambridge
#' University Press.
#
#' @export
#' @examples
#' data(iris.clusterings)
#' dlso(iris.clusterings, loss=VI())
#' dlso(iris.clusterings, loss=binder())
#'
#' # Compute expected loss using all draws, but pick the best among the first 10.
#' dlso(iris.clusterings[1:10,], loss=VI(), x=iris.clusterings)
#'
dlso <- function(partitions, loss=VI(), x=NULL) {
  if ( is.null(x) ) x <- partitions
  expectedLoss <- partition.loss(partitions, x, loss)
  index <- which.min(expectedLoss)
  estimate <-  partitions[index,]
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
