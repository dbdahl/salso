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
#' @param x One of the following: 1. A \eqn{B}-by-\eqn{n} matrix, where each of
#'   the \eqn{B} rows represents a clustering of \eqn{n} items using cluster
#'   labels. (For clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same
#'   cluster if \code{x[b,i] == x[b,j]}.), 2. A vector of length \eqn{n} which
#'   is converted to an \eqn{1}-by-\eqn{n} matrix as described above, or 3. A
#'   pairwise similarity matrix, i.e., \eqn{n}-by-\eqn{n} symmetric matrix whose
#'   \eqn{(i,j)} element gives the (estimated) probability that items \eqn{i}
#'   and \eqn{j} are in the same subset (i.e., cluster) of a partition (i.e.,
#'   clustering).
#' @param loss One of \code{"binder"}, \code{"omARI"}, \code{"omARI.approx"},
#'   \code{"VI"}, \code{"VI.lb"}, \code{"NIV"}, \code{"ID"}, \code{"NID"}, or
#'   the result of a calling a function of these names. Note that, if
#'   \code{loss="binder.psm"}, an algorithm based on the pairwise similarity
#'   matrix is used, whereas \code{loss="binder.draws"} results in an algorithm
#'   based on the samples. When \code{loss="binder"}, the algorithm choice will
#'   be based on the \code{x} argument.
#' @param maxSize This argument controls \code{Q}, maximum number of clusters
#'   that can be considered by the optimization algorithm.  \code{Q} has
#'   important implications for the interpretability of the resulting clustering
#'   and directly influences the RAM needed for the optimization algorithm. If
#'   \code{maxSize} is negative, \code{Q} is the absolute value of
#'   \code{maxSize}. Otherwise, if \code{x} is a matrix of clusterings, \code{Q}
#'   is less than the smaller of two values: 1. the maximum number of clusters
#'   among the clusterings in \code{x} and 2. the supplied value of
#'   \code{maxSize} (unless the supplied value is zero, in which case, only the
#'   first constraint holds). If \code{x} is a pairwise similarity matrix,
#'   \code{Q} is the supplied value of \code{maxSize} (unless the supplied value
#'   is zero, in which case, there is no constraint).
#' @param nRuns The number of runs to try, although the actual number by differ
#'   for the following reasons: 1. The actual number is a multiple of the number
#'   of cores specified by the \code{nCores} argument, and 2. The search is
#'   curtailed when the \code{seconds} threshold is exceeded.
#' @param seconds Instead of performing all the requested number of runs,
#'   curtail the search after the specified expected number of seconds. Note
#'   that the function will finish earlier if all the requested runs are
#'   completed. The specified seconds does not account for the overhead involved
#'   in starting the search and returning results.
#' @param maxScans The maximum number of full reallocation scans. The actual
#'   number of scans may be less than \code{maxScans} since the method stops if
#'   the result does not change between scans.
#' @param maxZealousAttempts The maximum number of attempts for zealous updates,
#'   in which entire clusters are destroyed and items are sequentially
#'   reallocated.  While zealous updates may be helpful in optimization, they
#'   also take more CPU time which might be better used trying additional runs.
#' @param probSequentialAllocation For the initial allocation, the probability
#'   of sequential allocation instead of using \code{sample(1:Q, ncol(x),
#'   TRUE)}, where \code{K} is set according to the \code{maxSize} argument.
#' @param probSingletonsInitialization When doing a sequential allocation to
#'   obtain the initial allocation, the probability of placing the first
#'   \code{maxSize} randomly-selected items in singletons subsets.
#' @param nCores The number of CPU cores to use, i.e., the number of
#'   simultaneous runs at any given time. A value of zero indicates to use all
#'   cores on the system.
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
#' # For examples, use 'nCores=1' per CRAN rules but, in practice, omit this.
#'
#' draws <- iris.clusterings
#' salso(draws, loss="VI", nRuns=1, nCores=1)
#'
#' probs <- psm(draws, nCores=1)
#' salso(probs, loss="VI.lb", nCores=1, maxZealousAttempts=0)
#' salso(draws, loss="VI.lb", nCores=1, maxZealousAttempts=0)
#'
salso <- function(x, loss="VI", maxSize=0, nRuns=8, seconds=Inf, maxScans=Inf, maxZealousAttempts=10, probSequentialAllocation=0.5, probSingletonsInitialization=0, nCores=0) {
  if ( nCores < 0.0 ) stop("'nCores' may not be negative.")
  if ( nCores > .Machine$integer.max ) nCores <- .Machine$integer.max
  z <- x2drawspsm(x, loss, nCores)
  if ( ! is.finite(maxSize) ) maxSize <- 0L
  if ( nRuns < 1.0 ) stop("'nRuns' must be at least one.")
  if ( is.infinite(seconds) && is.infinite(nRuns) ) stop("At least one of 'nRuns' and 'seconds' must be finite.")
  nRunsX <- if ( nRuns > .Machine$integer.max ) .Machine$integer.max else nRuns
  if ( maxScans < 0.0 ) stop("'maxScans' may not be negative.")
  if ( maxScans > .Machine$integer.max ) maxScans <- .Machine$integer.max
  if ( maxZealousAttempts < 0.0 ) stop("'maxZealousAttempts' may not be negative.")
  if ( maxZealousAttempts > .Machine$integer.max ) maxZealousAttempts <- .Machine$integer.max
  if ( ( z$loss %in% c("binder.psm", "omARI.approx", "VI.lb") ) && maxZealousAttempts != 0  ) {
    stop(sprintf("Zealous attempts are not implemented for '%s' loss.", z$loss))
  }
  if ( probSequentialAllocation < 0.0 || probSequentialAllocation > 1.0 ) stop("'probSequentialAllocation' should be in [0,1].")
  if ( probSingletonsInitialization < 0.0 || probSingletonsInitialization > 1.0 ) stop("'probSingletonsInitialization' should be in [0,1].")
  seed <- sapply(1:32, function(i) sample.int(256L,1L)-1L)
  if ( ( maxSize == 0 ) && ( ! is.null(z$psm) ) && ( ! is.null(z$draws) ) ) {
    maxSize <- max(apply(z$draws, 1, function(x) length(unique(x))))
  }
  y <- .Call(.minimize_by_salso, z$draws, z$psm, z$lossCode, z$a, maxSize, nRunsX, seconds, maxScans, maxZealousAttempts, probSequentialAllocation, probSingletonsInitialization, nCores, seed)
  estimate <- y[[1]]
  attr(estimate,"info") <- {
    attr <- y[[2]]
    names(attr) <- c("loss","a","maxSize","expectedLoss","initMethod","nScans","nZAcc","nZAtt","nRuns","seconds")
    attr$loss <- z$loss
    if ( z$loss != "binder" ) z$a <- NULL
    attr$initMethod <- names(which(initMethodMapping==attr$initMethod))
    as.data.frame(attr, row.names="")
  }
  attr(estimate,"draws") <- z$draws
  attr(estimate,"psm") <- z$psm
  actualNRuns <- attr(estimate,"info")$nRuns
  if ( is.finite(nRuns) && ( actualNRuns < nRunsX ) ) {
    warning(sprintf("Only %s of the requested %s run%s performed. Consider increasing 'seconds' or lowering 'nRuns'.",actualNRuns,nRuns,ifelse(actualNRuns==1L," was","s were")))
  }
  if ( maxZealousAttempts > 0 && attr(estimate,"info")$nZAtt > maxZealousAttempts ) {
    warning("The number of possible zealous attempts exceeded the maximum. Do you really want that many clusters? Consider lowering 'maxSize' or increasing 'maxZealousAttempts'.")
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
