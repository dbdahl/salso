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
#'   no more than the supplied value. If zero, the size is constrained to not
#'   more than two standard deviations above the mean number of subsets in
#'   \code{x}.
#' @param nPermutations The number of permutations to consider, although the
#'   actual number by differ for the following reasons: 1. The actual number is
#'   a multiple of the number of cores when \code{parallel=TRUE}, and 2. The
#'   search is curtailed when the \code{seconds} threshold is exceeded.
#' @param seconds A time threshold in seconds after which the function will be
#'   curtailed (with a warning) instead of performing all the requested number
#'   of permutations. Note that the function could take longer because the
#'   threshold is only checked after each permutation is completed.
#' @param maxScans The maximum number of reallocation scans after the initial
#'   sequential allocation. The actual number of scans may be less than
#'   \code{maxScans} since the method stops if the result does not change
#'   between scans.
#' @param probExplorationProbAtZero The probability of the point mass at zero
#'   for the spike-and-slab distribution of the probability of exploration,
#'   i.e., the probability of picking the second best micro-optimization
#'   (instead of the best).  This probability is randomly sampled for (and
#'   constant within) each permutation.
#' @param probExplorationShape The shape of the gamma distribution for the slab
#'   in the spike-and-slab distribution of the probability of exploration.
#' @param probExplorationRate The rate of the gamma distribution for the slab in
#'   the spike-and-slab distribution of the probability of exploration.
#' @param parallel Should the search use all CPU cores?
#'
#' @return A list of the following elements: \describe{ \item{estimate}{An
#'   integer vector giving a partition encoded using cluster labels.}
#'   \item{loss}{A character vector equal to the \code{loss} argument.}
#'   \item{expectedLoss}{A numeric vector of length one giving the expected
#'   loss.} \item{nScans}{An integer vector giving the number of scans used to
#'   arrive at the supplied estimate.} \item{probExploration}{The probability of
#'   picking the second best micro-optimization (instead of the best) for the
#'   permutation yielding the supplied estimate.} \item{nPermutations}{An
#'   integer giving the number of permutations actually performed.}}
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
#' salso(draws, loss="VI", nPermutations=1, parallel=FALSE)
#'
#' probs <- psm(draws, parallel=FALSE)
#' salso(probs, loss="VI.lb", parallel=FALSE)
#' salso(draws, loss="VI.lb", parallel=FALSE)
#'
salso <- function(x, loss="VI.lb", maxSize=0, nPermutations=100, seconds=Inf, maxScans=10, probExplorationProbAtZero=0.5, probExplorationShape=0.5, probExplorationRate=50, parallel=TRUE) {
  z <- x2drawspsm(x, loss, parallel)
  if ( maxSize < 0 ) stop("'maxSize' may not be negative.")
  if ( maxSize == Inf ) maxSize <- 0L
  if ( maxScans < 0 ) stop("'maxScans' may not be negative.")
  if ( nPermutations <= 0 ) stop("'nPermutations' may be strictly positive.")
  seed <- sapply(1:32, function(i) sample.int(256L,1L)-1L)
  y <- .Call(.minimize_by_salso, z$draws, z$psm, lossCode(loss), maxSize, maxScans, nPermutations, probExplorationProbAtZero, probExplorationShape, probExplorationRate, seconds, parallel, seed)
  names(y) <- c("estimate","loss","expectedLoss","nScans","probExploration","nPermutations")
  names(y$estimate) <- colnames(psm)
  y$loss <- loss
  if ( y$nPermutations < nPermutations ) {
    warning(sprintf("Only %s of the requested %s permutations %s performed. Increase 'seconds' or lower 'nPermutations'.",y$nPermutations,nPermutations,ifelse(y$nPermutations==1L,"was","were")))
  }
  y
}
