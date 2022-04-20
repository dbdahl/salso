#' SALSO Greedy Search
#'
#' This function provides a partition to summarize a partition distribution
#' using the SALSO greedy search method (Dahl, Johnson, and Müller, 2021). The
#' implementation currently supports the minimization of several partition
#' estimation criteria. For details on these criteria, see
#' \code{\link{partition.loss}}.
#'
#' The initial version of the SALSO method was presented at the workshop
#' "Bayesian Nonparametric Inference: Dependence Structures and their
#' Applications" in Oaxaca, Mexico on December 6, 2017. See
#' <https://www.birs.ca/events/2017/5-day-workshops/17w5060/schedule>.
#'
#' @param x A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B} rows
#'   represents a clustering of \eqn{n} items using cluster labels. For the
#'   \eqn{b}th clustering, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.
#' @param loss The loss function to use, as indicated by \code{"binder"},
#'   \code{"omARI"}, \code{"VI"}, \code{"NVI"}, \code{"ID"}, \code{"NID"}, or
#'   the result of calling a function with these names. Also supported are
#'   \code{"binder.psm"}, \code{"VI.lb"}, \code{"omARI.approx"}, or the result
#'   of calling a function with these names, in which case \code{x} above can
#'   optionally be a pairwise similarity matrix, i.e., \eqn{n}-by-\eqn{n}
#'   symmetric matrix whose \eqn{(i,j)} element gives the (estimated)
#'   probability that items \eqn{i} and \eqn{j} are in the same subset (i.e.,
#'   cluster) of a partition (i.e., clustering). The loss functions
#'   \code{"binder.psm"}, \code{"VI.lb"}, and \code{"omARI.approx"} are
#'   generally not recommended and the current implementation requires that
#'   \code{maxZealousAttempts = 0} and \code{probSequentialAllocation = 1.0}.
#' @param maxNClusters The maximum number of clusters that can be considered by
#'   the optimization algorithm, which has important implications for the
#'   interpretability of the resulting clustering and can greatly influence the
#'   RAM needed for the optimization algorithm. If the supplied value is zero
#'   and \code{x} is a matrix of clusterings, the optimization is constrained by
#'   the maximum number of clusters among the clusterings in \code{x}. If the
#'   supplied value is zero and \code{x} is a pairwise similarity matrix, there
#'   is no constraint.
#' @param nRuns The number of runs to try, although the actual number may differ
#'   for the following reasons: 1. The actual number is a multiple of the number
#'   of cores specified by the \code{nCores} argument, and 2. The search is
#'   curtailed when the \code{seconds} threshold is exceeded.
#' @param maxZealousAttempts The maximum number of attempts for zealous updates,
#'   in which entire clusters are destroyed and items are sequentially
#'   reallocated. While zealous updates may be helpful in optimization, they
#'   also take more CPU time which might be better used trying additional runs.
#' @param probSequentialAllocation For the initial allocation, the probability
#'   of sequential allocation instead of using \code{sample(1:K, ncol(x),
#'   TRUE)}, where \code{K} is set according to the \code{maxNClusters}
#'   argument.
#' @param nCores The number of CPU cores to use, i.e., the number of
#'   simultaneous runs at any given time. A value of zero indicates to use all
#'   cores on the system.
#' @param ... Extra arguments not intended for the end user, including: 1.
#'   \code{seconds}: Instead of performing all the requested number of runs,
#'   curtail the search after the specified expected number of seconds. Note
#'   that the function will finish earlier if all the requested runs are
#'   completed. The specified seconds does not account for the overhead involved
#'   in starting the search and returning results. 2. \code{maxScans} The
#'   maximum number of full reallocation scans. The actual number of scans may
#'   be less than \code{maxScans} since the method stops if the result does not
#'   change between scans, and 3. \code{probSingletonsInitialization}: When
#'   doing a sequential allocation to obtain the initial allocation, the
#'   probability of placing the first \code{maxNClusters} randomly-selected
#'   items in singletons subsets.
#'
#' @return An integer vector giving the estimated partition, encoded using
#'   cluster labels.
#'
#' @seealso \code{\link{partition.loss}}, \code{\link{psm}},
#'   \code{\link{summary.salso.estimate}}, \code{\link{dlso}}
#'
#' @references
#'
#' D. B. Dahl, D. J. Johnson, and P. Müller (2021), Search Algorithms and Loss
#' Functions for Bayesian Clustering, <arXiv:2105.04451>.
#'
#' @importFrom stats uniroot optimize
#' @export
#'
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' data(iris.clusterings)
#' draws <- iris.clusterings
#' # R_CARGO \dontrun{
#' # R_CARGO # Example disabled since Cargo was not found when installing from source package.
#' # R_CARGO # You can still run the example if you install Cargo. Hint: cargo::install().
#' salso(draws, loss=VI(), nRuns=1, nCores=1)
#' salso(draws, loss=VI(a=0.7), nRuns=1, nCores=1)
#' salso(draws, loss=binder(), nRuns=1, nCores=1)
#' salso(iris.clusterings, binder(a=NULL), nRuns=4, nCores=1)
#' salso(iris.clusterings, binder(a=list(nClusters=3)), nRuns=4, nCores=1)
#' # R_CARGO }
#'
salso <- function(x, loss=VI(), maxNClusters=0, nRuns=16, maxZealousAttempts=10, probSequentialAllocation=0.5, nCores=0, ...) {
  z <- x2drawspsm(x, loss, nCores)
  if ( ( z$lossStr %in% c("binder.draws","VI") ) && ( ! is.numeric(z$a) ) ) {
    argg <- c(as.list(environment()), list(...))
    argg$z <- NULL
    salsoFnc <- if ( z$lossStr == "binder.draws" ) {
      function(a) {
        argg$loss <- binder(a=a)
        suppressWarnings(do.call(salso, argg))
      }
    } else if ( z$lossStr == "VI" ) {
      function(a) {
        argg$loss <- VI(a=a)
        suppressWarnings(do.call(salso, argg))
      }
    } else stop("Unexpected loss function.")
    if ( is.null(z$a) ) {
      f <- function(a) attr(salsoFnc(a),"info")$expectedLoss
      search <- optimize(f, c(0.0,2.0), maximum=TRUE)
      return(salsoFnc(search$maximum))
    } else {
      f <- function(a) length(unique(salsoFnc(a))) - round(z$a$nClusters)
      search <- uniroot(f, c(0.0,2.0), extendInt="no")
      return(salsoFnc(search$root))
    }
  }
  if ( nCores < 0.0 ) stop("'nCores' may not be negative.")
  if ( nCores > .Machine$integer.max ) nCores <- .Machine$integer.max
  if ( ! is.finite(maxNClusters) ) maxNClusters <- 0L
  if ( nRuns < 1.0 ) stop("'nRuns' must be at least one.")
  nRunsX <- if ( nRuns > .Machine$integer.max ) .Machine$integer.max else nRuns
  if ( maxZealousAttempts < 0.0 ) stop("'maxZealousAttempts' may not be negative.")
  if ( maxZealousAttempts > .Machine$integer.max ) maxZealousAttempts <- .Machine$integer.max
  if ( ( z$lossStr %in% c("binder.psm", "omARI.approx", "VI.lb") ) && maxZealousAttempts != 0 ) {
    warning(sprintf("'maxZealousAttempts' set to 0 since other values are not implemented for '%s' loss.", z$loss))
    maxZealousAttempts <- 0
  }
  if ( ( z$lossStr %in% c("binder.psm", "omARI.approx", "VI.lb") ) && probSequentialAllocation != 1 ) {
    warning(sprintf("'probSequentialAllocation' set to 1 since other values are not implemented for '%s' loss.", z$loss))
    probSequentialAllocation <- 1
  }
  if ( z$a != 1 && z$lossStr == "binder.psm" ) {
    stop("The current implementation requires that samples be provided when 'a' is not 1.0 for Binder loss.")
  }
  if ( probSequentialAllocation < 0.0 || probSequentialAllocation > 1.0 ) stop("'probSequentialAllocation' should be in [0,1].")
  dots <- list(...)
  unrecognizedArguments <- setdiff(names(dots), c("seconds","maxScans","probSingletonsInitialization"))
  if ( length(unrecognizedArguments) > 0 ) {
    stop(paste0("unused argument: ", paste0(unrecognizedArguments,collapse=", ")))
  }
  if ( ! "seconds" %in% names(dots) ) {
    seconds <- Inf
  } else {
    seconds <- dots[["seconds"]]
  }
  if ( is.infinite(seconds) && is.infinite(nRuns) ) stop("At least one of 'nRuns' and 'seconds' must be finite.")
  if ( ! "maxScans" %in% names(dots) ) {
    maxScans <- .Machine$integer.max
  } else {
    maxScans <- dots[["maxScans"]]
    if ( maxScans < 0.0 ) stop("'maxScans' may not be negative.")
    if ( maxScans > .Machine$integer.max ) maxScans <- .Machine$integer.max
  }
  if ( ! "probSingletonsInitialization" %in% names(dots) ) {
    probSingletonsInitialization <- 0.0
  } else {
    probSingletonsInitialization <- dots[["probSingletonsInitialization"]]
    if ( probSingletonsInitialization < 0.0 || probSingletonsInitialization > 1.0 ) stop("'probSingletonsInitialization' should be in [0,1].")
  }
  if ( ( maxNClusters == 0 ) && ( ! is.null(z$psm) ) && ( ! is.null(z$draws) ) ) {
    maxNClusters <- max(apply(z$draws, 1, function(x) length(unique(x))))
  }
  estimate <- .Call(.minimize_by_salso, z$draws, z$psm, z$lossCode, z$a, maxNClusters, nRunsX, seconds, maxScans, maxZealousAttempts, probSequentialAllocation, probSingletonsInitialization, nCores)
  info <- attr(estimate, "info")
  info$loss <- z$loss
  if ( ! z$loss %in% c("binder","VI") ) info$a <- NULL
  info$initMethod <- names(which(initMethodMapping==info$initMethod))
  attr(estimate, "info") <- as.data.frame(info, row.names="")
  attr(estimate, "draws") <- z$draws
  attr(estimate, "psm") <- z$psm
  actualNRuns <- info$nRuns
  if ( is.finite(nRuns) && ( actualNRuns < nRunsX ) ) {
    warning(sprintf("Only %s of the requested %s run%s performed. Consider increasing 'seconds' or lowering 'nRuns'.",actualNRuns,nRuns,ifelse(actualNRuns==1L," was","s were")))
  }
  if ( ( maxNClusters == 0 ) && ( length(unique(estimate)) == info$maxNClusters ) ) {
    warning("The number of clusters equals the default maximum possible number of clusters.")
  }
  if ( ( maxZealousAttempts > 0 ) && ( info$nZAtt > maxZealousAttempts ) ) {
    warning("The number of possible zealous attempts exceeded the maximum. Do you really want that many clusters? Consider lowering 'maxNClusters' or increasing 'maxZealousAttempts'.")
  }
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
