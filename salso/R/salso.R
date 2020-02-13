#' Sequentially-Allocated Latent Structure Optimization
#'
#' This function provides a partition to summarize a partition distribution
#' based a pairwise similarity matrix using the sequentially-allocated latent
#' structure optimization (SALSO) method. The implementation currently supports
#' the minimization one of three partition estimation criteria: "binder",
#' "pear", and "VI.lb". For details on these criteria, see
#' \code{\link{partitionExpectedLoss}}.
#'
#' The SALSO method was first presented at the workshop "Bayesian Nonparametric
#' Inference: Dependence Structures and their Applications" in Oaxaca, Mexico on
#' December 6, 2017. See
#' <https://www.birs.ca/events/2017/5-day-workshops/17w5060/schedule>.
#'
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#' @param loss One of \code{"binder"}, \code{"pear"}, or \code{"VI.lb"}.  See
#'   \code{\link{partitionExpectedLoss}} for details on these loss functions.
#' @param maxSize The maximum number of subsets (i.e, clusters).  The
#'   optimization is constrained to produce solutions whose number of subsets is
#'   no more than the supplied value. If zero, the size is not constrained.
#' @param batchSize The number of permutations to consider per batch (although
#'   the actual number of permutations per batch is a multiple of the number of
#'   cores when \code{parallel=TRUE}). Batches are sequentially performed until
#'   the most recent batch does not lead to a better result. Therefore, at least
#'   two batches are performed (unless the \code{seconds} threshold is
#'   exceeded.)
#' @param seconds A time threshold in seconds after which the function will be
#'   curtailed (with a warning) instead of performing another batch of
#'   permutations. Note that the function could take considerably longer because
#'   the threshold is only checked after each batch is completed.
#' @param maxScans The maximum number of reallocation scans after the initial
#'   allocation. The actual number of scans may be less than \code{maxScans}
#'   since the method stops if the result does not change between scans.
#' @param probExplorationProbAtZero The probability of the point mass at zero
#'   for the spike-and-slab distribution of the probability of exploration, i.e.
#'   the probability of picking the second best micro-optimization (instead of
#'   the best).  This probability is randomly sampled for (and constant within)
#'   each permutation.
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
#'   integer giving the number of permutations actually performed.}
#'   \item{batchSize}{An integer giving the number of permutations per batch.}
#'   \item{curtailed}{A logical indicating whether the search was cut short
#'   because the time exceeded the threshold.}}
#'
#' @seealso \code{\link{partitionExpectedLoss}}, \code{\link{psm}},
#'   \code{\link{confidence}}, \code{\link{dlso}}
#'
#' @export
#' @useDynLib salso .minimize_by_salso
#'
#' @examples
#' # Use 'parallel=FALSE' per CRAN rules for examples but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' salso(probs, parallel=FALSE)
#'
salso <- function(psm, loss=c("binder", "pear", "VI.lb")[3], maxSize=0, batchSize=100, seconds=Inf, maxScans=10, probExplorationProbAtZero=0.5, probExplorationShape=0.5, probExplorationRate=50, parallel=TRUE) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  lossCode <- lossCode(loss)
  if ( maxSize < 0 ) stop("'maxSize' may not be negative.")
  if ( maxSize == Inf ) maxSize <- 0L
  if ( maxScans < 0 ) stop("'maxScans' may not be negative.")
  if ( batchSize < 0 ) stop("'batchSize' may not be negative.")
  seed <- sapply(1:32, function(i) sample.int(256L,1L)-1L)
  y <- .Call(.minimize_by_salso, nrow(psm), psm, lossCode, maxSize, maxScans, batchSize, probExplorationProbAtZero, probExplorationShape, probExplorationRate, seconds, parallel, seed)
  names(y) <- c("estimate","loss","expectedLoss","nScans","probExploration","nPermutations","batchSize","curtailed","subsetSizes")
  names(y$estimate) <- colnames(psm)
  y$loss <- loss
  y$batchSize <- batchSize
  y$subsetSizes <- table(y$estimate)
  proportionSingletons <- sum(y$subsetSizes==1)/length(y$subsetSizes)
  if ( proportionSingletons >= 0.5 ) {
    warning(sprintf("%2.0f%% of the subsets are singletons.  For the sake of interpretability, consider using the 'maxSize' argument.",100*proportionSingletons))
  }
  if ( y$curtailed ) {
    warning("The search was curtailed since the time threshold was reached.  Consider increasing 'seconds' or lowering 'batchSize'.")
  }
  y
}
