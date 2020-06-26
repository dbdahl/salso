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
#'   represents a clustering of \eqn{n} items using cluster labels. For
#'   clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.  One of the rows will be used as the paritition
#'   estimate.
#' @param loss One of \code{"binder"}, \code{"omARI"}, \code{"omARI.approx"},
#'   \code{"VI"}, or \code{"VI.lb"}.  See \code{\link{partition.loss}} for
#'   details on these loss functions.
#' @param x Either: 1. A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B}
#'   rows represents a clustering of \eqn{n} items using cluster labels. (For
#'   clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.), or 2. A pairwise similarity matrix, i.e.,
#'   \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element gives the
#'   (estimated) probability that items \eqn{i} and \eqn{j} are in the same
#'   subset (i.e., cluster) of a partition (i.e., clustering).  If \code{NULL},
#'   this argument is set equal to \code{candidates}.
#'
#' @return An integer vector giving the estimated partition, encoded using
#'   cluster labels.
#'
#' @seealso \code{\link{partition.loss}}, \code{\link{psm}},
#'   \code{\link{summary.salso.estimate}}, \code{\link{salso}}
#'
#' @export
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules but, in practice, omit this.
#' probs <- psm(iris.clusterings, nCores=1)
#' dlso(iris.clusterings, loss="binder", x=probs)
#' dlso(iris.clusterings, loss="omARI")
#' dlso(iris.clusterings, loss="omARI.approx")  # The psm will be computed if not provided.
#' dlso(iris.clusterings, loss="VI")
#' dlso(iris.clusterings, loss="VI.lb", x=probs)
#' dlso(iris.clusterings[1:10,], loss="VI", x=iris.clusterings)  # Candidates can be constrained.
#'
dlso <- function(candidates, loss="VI", x=NULL) {
  if ( is.null(x) ) x <- candidates
  expectedLoss <- partition.loss(candidates, x, loss)
  index <- which.min(expectedLoss)
  estimate <-  candidates[index,]
  attr(estimate,"info") <- {
    attr <- list(loss=loss, expectedLoss=expectedLoss[index])
    as.data.frame(attr, row.names="")
  }
  attr(estimate,"draws") <- x
  attr(estimate,"psm") <- NULL
  class(estimate) <- "salso.estimate"
  estimate
}
