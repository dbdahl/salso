#' Sequentially-Allocated Latent Structure Optimization
#'
#' This function provides a point estimate for a partition distribution using
#' the sequentially-allocated latent structure optimization (SALSO) method. The
#' method seeks to minimize the Binder loss or the lower bound of the variation
#' of information loss. The SALSO method was presented at the workshop "Bayesian
#' Nonparametric Inference: Dependence Structures and their Applications" in
#' Oaxaca, Mexico on December 6, 2017. See
#' <https://www.birs.ca/events/2017/5-day-workshops/17w5060/schedule>.
#'
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} elements gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#' @param loss Either \code{"VI.lb"} or \code{"binder"}, to indicate the desired
#'   loss function.
#' @param maxSize Either zero or a positive integer.  If a positive integer, the
#'   optimization is constrained to produce solutions whose number of subsets
#'   (i.e., clusters) is no more than the supplied value.  If zero, the size is
#'   not constrained.
#' @param maxScans The maximum number of reallocation scans after the intial
#'   allocation.  The actual number of scans may be less than \code{maxScans}
#'   since the method stops if the result does not change between scans.
#' @param nPermutations The desired number of permutations to consider when
#'   searching for the minimizer.
#' @param seconds A time threshold in seconds after which the function will
#'   return early (with a warning) instead of finishing all the desired
#'   permutations.  Note that the function could take considerably longer,
#'   however, because this threshold is only checked after each permutation is
#'   completed.
#' @param parallel Should the search use all CPU cores?
#'
#' @return A list of the following elements: \describe{ \item{estimate}{An
#'   integer vector giving a partition encoded using cluster labels.}
#'   \item{loss}{A character vector equal to the \code{loss} argument.}
#'   \item{expectedLoss}{A numeric vector of length one giving the expected
#'   loss.} \item{nScans}{An integer vector giving the number of scans used to
#'   arrive at the supplied estimate.} \item{nPermutations}{An integer vector
#'   giving the number of permutations actually performed.} }
#'
#' @export
#' @useDynLib salso .minimize_by_salso
#' @examples
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' salso(probs, nPermutations=50, parallel=FALSE)
#'
salso <- function(psm, loss=c("VI.lb","binder")[1], maxSize=0, maxScans=5, nPermutations=5000, seconds=10, parallel=TRUE) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( ( length(loss) != 1 ) || ! ( loss %in% c("VI.lb","binder") ) ) stop("'loss' is not recognized.")
  if ( maxSize < 0 ) stop("'maxSize' may not be negative.")
  if ( maxScans < 0 ) stop("'maxScans' may not be negative.")
  if ( nPermutations < 0 ) stop("'nPermutations' may not be negative.")
  useVIlb <- loss == "VI.lb"
  y <- .Call(.minimize_by_salso, nrow(psm), psm, useVIlb, maxSize, maxScans, nPermutations, seconds, parallel)
  names(y) <- c("estimate","loss","expectedLoss","nScans","nPermutations")
  names(y[[1]]) <- colnames(psm)
  y[[2]] <- loss
  if ( y$nPermutations <  nPermutations ) {
    warning(sprintf("Only %s permutations of %s were tried.  Adjust the 'seconds' and/or 'nPermutations' parameters.",y$nPermutations,nPermutations))
  }
  y
}
