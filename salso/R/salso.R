#' Sequentially-Allocated Latent Structure Optimization
#'
#' This function provides a partition to summarize a partition distribution
#' using the sequentially-allocated latent structure optimization (SALSO)
#' method. The implementation currently supports the minimization of several
#' partition estimation criteria. For details on these criteria, see
#' \code{\link{partition.loss}}.
#'
#' The SALSO method was first presented at the workshop "Bayesian Nonparametric
#' Inference: Dependence Structures and their Applications" in Oaxaca, Mexico on
#' December 6, 2017. See
#' <https://www.birs.ca/events/2017/5-day-workshops/17w5060/schedule>.
#'
#' @param x Either: 1. A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B}
#'   rows represents a clustering of \eqn{n} items using cluster labels. (For
#'   clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.), or 2. A pairwise similarity matrix, i.e.,
#'   \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element gives the
#'   (estimated) probability that items \eqn{i} and \eqn{j} are in the same
#'   subset (i.e., cluster) of a partition (i.e., clustering).
#' @param loss One of \code{"binder"}, \code{"omARI"}, \code{"omARI.approx"},
#'   \code{"VI"}, or \code{"VI.lb"}.  See \code{\link{partition.loss}} for
#'   details on these loss functions.
#' @param maxSize The maximum number of subsets (i.e., clusters).  The
#'   optimization is constrained to produce solutions whose number of subsets is
#'   no more than the supplied value. If zero, the following default is used. If
#'   \code{x} is a pairwise similarity matrix, \code{20} is used for
#'   \code{maxSize}.  Otherwise, \code{maxSize} is two standard deviations above
#'   the mean number of subsets in \code{x}, rounding up to the nearest integer.
#' @param nRuns The number of runs to try, although the actual number by differ
#'   for the following reasons: 1. The actual number is a multiple of the number
#'   of cores when \code{parallel=TRUE}, and 2. The search is curtailed when the
#'   \code{seconds} threshold is exceeded.
#' @param seconds A time threshold in seconds after which the function will be
#'   curtailed (with a warning) instead of performing all the requested number
#'   of permutations. Note that the function could take longer because the
#'   threshold is only checked after each permutation is completed.
#' @param maxScans The maximum number of reallocation scans after the initial
#'   sequential allocation. The actual number of scans may be less than
#'   \code{maxScans} since the method stops if the result does not change
#'   between scans.
#' @param probSequentialAllocation TODO During initial allocation, the probability of
#'   unilaterally allocating an item to an empty subset (if the \code{maxSize}
#'   is not yet reached).
#' @param probSingletonsInitialization TODO During initial allocation, the probability of
#'   unilaterally allocating an item to an empty subset (if the \code{maxSize}
#'   is not yet reached).
#' @param parallel Should the search use all CPU cores?
#'
#' @return An integer vector giving the estimated partition, encoded using
#'   cluster labels.
#'
#' @seealso \code{\link{partition.loss}}, \code{\link{psm}},
#'   \code{\link{confidence}}, \code{\link{dlso}}
#'
#' @export
#' @useDynLib salso .minimize_by_salso
#'
#' @examples
#' # For examples, use 'parallel=FALSE' per CRAN rules but, in practice, omit this.
#'
#' draws <- iris.clusterings
#' salso(draws, loss="VI", nRuns=1, parallel=FALSE)
#'
#' probs <- psm(draws, parallel=FALSE)
#' salso(probs, loss="VI.lb", parallel=FALSE)
#' salso(draws, loss="VI.lb", parallel=FALSE)
#'
salso <- function(x, loss="VI", maxSize=0, nRuns=100, seconds=Inf, maxScans=50, probSequentialAllocation=2/3, probSingletonsInitialization=1/3, parallel=TRUE) {
  z <- x2drawspsm(x, loss, parallel)
  if ( maxSize < 0 ) stop("'maxSize' may not be negative.")
  if ( maxSize == Inf ) maxSize <- 0L
  if ( maxScans < 0 ) stop("'maxScans' may not be negative.")
  if ( probSequentialAllocation < 0.0 || probSequentialAllocation > 1.0 ) stop("'probSequentialAllocation' should be in [0,1].")
  if ( probSingletonsInitialization < 0.0 || probSingletonsInitialization > 1.0 ) stop("'probSingletonsInitialization' should be in [0,1].")
  if ( nRuns <= 0 ) stop("'nRuns' may be strictly positive.")
  seed <- sapply(1:32, function(i) sample.int(256L,1L)-1L)
  if ( ( maxSize == 0 ) && ( ! is.null(z$psm) ) ) {
    maxSize <- if ( is.null(z$draws) ) 20
    else {
      nClusters <- apply(z$draws,1,max)
      maxSize <- ceiling(mean(nClusters) + 2*sd(nClusters))
    }
  }
  y <- .Call(.minimize_by_salso, z$draws, z$psm, lossCode(loss), maxSize, maxScans, nRuns, probSequentialAllocation, probSingletonsInitialization, seconds, parallel, seed)
  estimate <- y[[1]]
  attr(estimate,"info") <- {
    attr <- y[[2]]
    attr[[1]] <- loss
    names(attr) <- c("loss","expectedLoss","nScans","nRuns","maxSize")
    as.data.frame(attr)
  }
  if ( attr(estimate,"info")$nRuns < nRuns ) {
    warning(sprintf("Only %s of the requested %s permutations %s performed. Increase 'seconds' or lower 'nRuns'.",y$nRuns,nRuns,ifelse(y$nRuns==1L,"was","were")))
  }
  estimate
}
