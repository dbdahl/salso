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
#'   \code{"VI"}, or \code{"VI.lb"}.  Note that, if \code{loss="binder.psm"}, an
#'   algorithm based on the pairwise similarity matrix is used, whereas
#'   \code{loss="binder.draws"} results in an algorithm based on the samples.
#'   See \code{\link{partition.loss}} for details on these loss functions.
#' @param maxSize If \code{x} is a matrix of clusterings, the number of clusters
#'   in the optimization is less than the smaller of two values: 1. the maximum
#'   number clusters among the clusterings in \code{x} and 2. the supplied value
#'   (unless the supplied value is zero, in which case, only the first
#'   constraint holds). If \code{x} is a pairwise similarity matrix, the
#'   optimization is constrained to produce solutions whose number of clusters
#'   is no more than the supplied value (unless the supplied value is zero, in
#'   which case, there is no size constraint).
#' @param nRuns The number of runs to try, although the actual number by differ
#'   for the following reasons: 1. The actual number is a multiple of the number
#'   of cores when \code{parallel=TRUE}, and 2. The search is curtailed when the
#'   \code{seconds} threshold is exceeded.
#' @param seconds A time threshold in seconds after which the function will be
#'   curtailed (with a warning) instead of performing all the requested number
#'   of permutations. Note that the function could take longer because the
#'   threshold is only checked after each permutation is completed.
#' @param maxScans The maximum number of reallocation scans after the initial
#'   allocation. The actual number of scans may be less than
#'   \code{maxScans} since the method stops if the result does not change
#'   between scans.
#' @param probSequentialAllocation Probability of sequential allocation instead
#'   of using \code{sample(1:maxSize, ncol(x), TRUE)} for the initial
#'   allocation.
#' @param probSingletonsInitialization When doing a sequential allocation to
#'   obtain the initial allocation, the probability of placing the first
#'   \code{maxSize} randomly-selected items in singletons subsets.
#' @param mergeSplit Should merge and split updates be considered?  While they
#'   may be helpful in optimization, they also become costly as the number of
#'   clusters increases.
#' @param parallel Should the search use all CPU cores?
#'
#' @return An integer vector giving the estimated partition, encoded using
#'   cluster labels.
#'
#' @seealso \code{\link{partition.loss}}, \code{\link{psm}},
#'   \code{\link{summary.salso.estimate}}, \code{\link{dlso}}
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
salso <- function(x, loss="VI", maxSize=0, nRuns=32, seconds=Inf, maxScans=Inf, probSequentialAllocation=0.5, probSingletonsInitialization=0, mergeSplit=TRUE, parallel=TRUE) {
  z <- x2drawspsm(x, loss, parallel)
  if ( maxSize < 0.0 ) stop("'maxSize' may not be negative.")
  if ( maxSize == Inf ) maxSize <- 0L
  if ( nRuns < 1.0 ) stop("'nRuns' may be at least one.")
  if ( maxScans < 0.0 ) stop("'maxScans' may not be negative.")
  if ( maxScans > .Machine$integer.max ) maxScans <- .Machine$integer.max
  if ( probSequentialAllocation < 0.0 || probSequentialAllocation > 1.0 ) stop("'probSequentialAllocation' should be in [0,1].")
  if ( probSingletonsInitialization < 0.0 || probSingletonsInitialization > 1.0 ) stop("'probSingletonsInitialization' should be in [0,1].")
  if ( ! is.logical(mergeSplit) ) stop("'mergeSplitStrategy' must be a logical.")
  seed <- sapply(1:32, function(i) sample.int(256L,1L)-1L)
  if ( ( maxSize == 0 ) && ( ! is.null(z$psm) ) && ( ! is.null(z$draws) ) ) {
    maxSize <- max(apply(z$draws, 1, function(x) length(unique(x))))
  }
  y <- .Call(.minimize_by_salso, z$draws, z$psm, z$lossCode, maxSize, maxScans, nRuns, probSequentialAllocation, probSingletonsInitialization, mergeSplit, seconds, parallel, seed)
  estimate <- y[[1]]
  attr(estimate,"info") <- {
    attr <- y[[2]]
    names(attr) <- c("loss","expectedLoss","initMethod","nScans","nMerges","nSplits","nRuns","maxSize")
    attr$loss <- loss
    attr$initMethod <- names(which(initMethodMapping==attr$initMethod))
    as.data.frame(attr, row.names="")
  }
  attr(estimate,"draws") <- z$draws
  attr(estimate,"psm") <- z$psm
  if ( attr(estimate,"info")$nRuns < nRuns ) {
    warning(sprintf("Only %s of the requested %s permutations %s performed. Increase 'seconds' or lower 'nRuns'.",y$nRuns,nRuns,ifelse(y$nRuns==1L,"was","were")))
  }
  class(estimate) <- "salso.estimate"
  estimate
}

#' @export
#'
print.salso.estimate <- function(x, ...) {
  class(x) <- NULL
  attr(x,"draws") <- NULL
  attr(x,"psm") <- NULL
  print(x)
}
