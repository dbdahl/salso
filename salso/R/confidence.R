#' Compute Clustering Confidence
#'
#' This function has been deprecated.  Instead of this function, see the
#' \code{score} in the list returned by the
#' \code{\link[=summary.salso.estimate]{summary}} function on the result of the
#' \code{\link{salso}} function.
#'
#' This function computes the confidence values for \code{n} observations based
#' on a clustering estimate and the expected pairwise allocation matrix.
#'
#' @param estimate A vector of length \code{n}, where \code{i} and \code{j} are
#'   in the same cluster if and only if \code{estimate[i] == estimate[j]}.
#' @param psm A \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element
#'   gives the relative frequency that that items \eqn{i} and \eqn{j} are in the
#'   same subset (i.e., cluster).
#'
#' @keywords internal
#' @examples
#' suppressWarnings({  # For testing purposes, suppress deprecation warning.
#'
#' clustering <- salso(iris.clusterings)
#' psm <- psm(iris.clusterings)
#' salso:::confidence(clustering, psm)
#'
#' })
#'
confidence <- function(estimate, psm) {
  .Deprecated("confidence", msg="This function is deprecated. Use the 'summary' function on the result of the 'salso' function.")
  estimate <- as.vector(estimate)
  attr(estimate,"info") <- list(loss="binder")
  attr(estimate,"psm") <- psm
  class(estimate) <- "salso.estimate"
  summary(estimate)$score
}
