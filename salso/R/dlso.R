#' Draws-Based Latent Structure Optimization
#'
#' This function provides a partition to summarize a partition distribution
#' based a pairwise similarity matrix using the draws latent structure
#' optimization (DLSO) method, which is also known as the least-squares
#' clustering method (Dahl 2006). The method seeks to estimation criteria by
#' picking the minimizer among the partitions supplied by the \code{draws}
#' argument. The implementation currently supports the minimization of three
#' partition estimation criteria: "binder", "pear", and "VI.lb". For details on
#' these criteria, see \code{\link{partitionExpectedLoss}}.
#'
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).  If not provided, this argument is computed
#'   from the \code{draws} argument.
#' @param loss One of \code{"binder"}, \code{"pear"}, or \code{"VI.lb"}.  See
#'   \code{\link{partitionExpectedLoss}} for details on these loss functions.
#' @param draws A \code{B}-by-\code{n} matrix, where each of the \code{B} rows
#'   represents a clustering of \code{n} items using cluster labels. For
#'   clustering \code{b}, items \code{i} and \code{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.
#' @param parallel Should the search use all CPU cores?  (Currently ignored
#'   since parallelization is not implemented.)
#'
#' @return A list of the following elements: \describe{ \item{estimate}{An
#'   integer vector giving a partition encoded using cluster labels.}
#'   \item{loss}{A character vector equal to the \code{loss} argument.}
#'   \item{expectedLoss}{A numeric vector of length one giving the expected
#'   loss.} }
#'
#' @seealso \code{\link{partitionExpectedLoss}}, \code{\link{psm}},
#'   \code{\link{confidence}}, \code{\link{salso}}
#'
#' @export
#' @examples
#' dlso(draws=iris.clusterings, loss="binder")
#' dlso(draws=iris.clusterings, loss="pear")
#' dlso(draws=iris.clusterings, loss="VI.lb")
#'
dlso <- function(psm, loss=c("binder", "pears", "VI.lb")[3], draws, parallel=FALSE) {
  if ( missing(psm) ) psm <- salso::psm(draws)
  expectedLoss <- partitionExpectedLoss(draws, psm, loss)
  index <- which.min(expectedLoss)
  estimate <-  draws[index,]
  subsetSizes <- table(estimate)
  list(estimate=estimate, loss=loss, expectedLoss=expectedLoss[index], subsetSizes=subsetSizes)
}
