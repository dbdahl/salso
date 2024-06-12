#' CHiPS Partition Greedy Search
#'
#' This function provides a partition to a subset of items which has high
#' marginal probability based on samples from a partition distribution
#' using the CHiPS greedy search method (Dahl, Page, Barrientos, 2024).
#'
#' @param x A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B} rows
#'   represents a clustering of \eqn{n} items using cluster labels. For the
#'   \eqn{b}th clustering, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b, i] == x[b, j]}.
#' @param threshold The minimum marginal probability for the partial partition.
#'   Values closer to 1.0 will yield a partition of fewer items and values
#'   closer to 0.0 will yield a partition of more items.
#' @param intermediateResults Should intermediate subset partitions be returned?
#' @param nRuns The number of runs to try, where the best result is returned.
#' @param nCores The number of CPU cores to use, i.e., the number of
#'   simultaneous runs at any given time. A value of zero indicates to use all
#'   cores on the system.
#'
#' @return If `intermediateResults` is `FALSE`, an integer vector giving the
#'   estimated subset partition, encoded using cluster labels with `-1`
#'   indicating not allocated.  If `TRUE`, a matrix with intermediate subset
#'   partitions in the rows.
#'
#' @export
#' @examples
#' # For examples, use 'nCores = 1' per CRAN rules, but in practice omit this.
#' data(iris.clusterings)
#' draws <- iris.clusterings
#' chips(draws, threshold = 0, nRuns = 1)
#' chips(draws, nCores = 1)
chips <- function(x, threshold = 0.50, nRuns = 16, intermediateResults = (nRuns == 1), nCores = 0) {
  .Call(.chips, x, threshold, nRuns, intermediateResults, nCores)
}

