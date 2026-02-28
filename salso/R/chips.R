#' CHIPS Partition Greedy Search
#'
#' This function provides a partition to a subset of items which has high
#' marginal probability based on samples from a partition distribution
#' using the conditional high inclusion probability subset (CHIPS) partition
#' greedy search method (Barrientos, Page, Dahl, Dunson, 2024).
#'
#' A complete, end-to-end demonstration is provided in the package demo.
#' To run it and to access the accompanying synthetic dataset:
#' - Run the demo: `demo("chips", package = "salso")`
#' - Load the synthetic data used in the demo: `data("synthetic", package = "salso")`
#'
#' @param partitions A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B} rows
#'   represents a clustering of \eqn{n} items using cluster labels. For the
#'   \eqn{b}th clustering, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b, i] == x[b, j]}.
#' @param threshold The minimum marginal probability for the subpartition, i.e., the gamma parameter.
#'   Values closer to 1.0 will yield a partition of fewer items and values
#'   closer to 0.0 will yield a partition of more items.
#' @param nRuns The number of runs to try. When multiple runs produce candidate
#'   subpartitions, the one allocating the most items is selected; among ties,
#'   the candidate with the highest posterior probability is chosen.
#' @param intermediateResults Should intermediate subset partitions be returned?
#' @param allCandidates Should all the final subset partitions from multiple runs
#'   be returned?
#' @param andSALSO Should the resulting incomplete partition be completed using
#'   SALSO?
#' @param loss When \code{andSALSO = TRUE}, the loss function to use, as
#'   indicated by \code{"binder"}, \code{"VI"}, or the result of calling a
#'   function with these names (which permits unequal costs).
#' @param maxNClusters The maximum number of clusters that can be considered by
#'   SALSO, which has important implications for the interpretability of the
#'   resulting clustering and can greatly influence the RAM needed for the
#'   optimization algorithm. If the supplied value is zero, the optimization is
#'   constrained by the maximum number of clusters among the clusterings in
#'   \code{x}.
#' @param initialPartition An vector of length \eqn{n} containing cluster labels
#'   for items that are initially clustered or \eqn{-1} for items that are not
#'   initially clustered.  As a special case, vector of length 0 is equivalent
#'   to a vector of length \eqn{n} with \eqn{-1} for all values.
#' @param nCores The number of CPU cores to use, i.e., the number of
#'   simultaneous runs at any given time. A value of zero indicates to use all
#'   cores on the system.
#'
#' @return A list containing:
#'   + `chips_partition`: If `intermediateResults` is `FALSE`, an integer vector giving the
#'     estimated subset partition, encoded using cluster labels with `-1`
#'     indicating not allocated.  If `TRUE`, an integer matrix with intermediate subset
#'     partitions in the rows.
#'   + `n_items`: Number of items in the estimated subset partition.
#'   + `probability`: Monte Carlo estimate of the probability of the subset partition.
#'   + `AUChips`: If `intermediateResults` is `TRUE`, this element is provided and gives
#'     the area under the probability curve as a function of the number of clusters
#'     after scaling to be between 0 and 1.
#'   + `chips_and_salso_partition`: If `andSALSO` is `TRUE`, this element is provided and
#'     gives an integer vector giving the
#'     estimated partition of all items based on CHiPS until the `threshold` is met
#'     and using SALSO to allocate the rest.
#'
#' @export
#' @examples
#' data(iris.clusterings)
#' draws <- iris.clusterings
#'
#' # For examples, use 'nRuns = 1' and 'nCores = 1' for CRAN, but in practice omit this.
#' all <- chips(draws, nRuns = 1, nCores = 1)
#' plot(all$n_items, all$probability, type = "l")
#' subpartition <- threshold(all, 0.80)
#' str(subpartition)
#'
#' # See the full CHIPS demo run: demo("chips", package = "salso")
#'
chips <- function(
  partitions,
  threshold = 0.0,
  nRuns = 64,
  intermediateResults = identical(threshold, 0.0),
  allCandidates = FALSE,
  andSALSO = !intermediateResults && !allCandidates,
  loss = VI(a = 1),
  maxNClusters = 0,
  initialPartition = integer(0),
  nCores = 0
) {
  loss_code <- 0
  a <- 1.0
  if (!isFALSE(andSALSO)) {
    loss_str <- if (inherits(loss, "salso.loss")) {
      if (loss$loss %in% c("binder", "VI")) {
        a <- loss$a
      }
      loss$loss
    } else {
      loss
    }
    if (loss_str == "binder") {
      loss_str <- "binder.draws"
    }
    if ((length(loss_str) != 1) || !(loss_str %in% names(lossMapping))) {
      stop(sprintf(
        'loss="%s" is not recognized.  Please use one of the following: %s',
        loss,
        paste0('"', names(lossMapping), '"', collapse = ", ")
      ))
    }
    loss_code <- unname(lossMapping[loss_str])
  }
  y <- .Call(
    .chips,
    partitions,
    threshold,
    nRuns,
    intermediateResults,
    allCandidates,
    andSALSO,
    loss_code,
    a,
    maxNClusters,
    initialPartition,
    nCores
  )
  if (!intermediateResults && !allCandidates) {
    y$probability_of_chips_clusters <- sapply(
      sort(setdiff(unique(y$chips_partition), -1)),
      \(label) {
        mean(
          apply(
            partitions[, which(y$chips_partition == label), drop = FALSE],
            1,
            \(z) length(unique(z))
          ) ==
            1
        )
      }
    )
  }
  attr(y, "partitions") <- partitions
  class(y) <- "salso_chips"
  y
}

#' Threshold CHIPS Output
#'
#' This function applies a threshold to the output of the \code{chips} function to find a CHIPS subpartition which achieves the specified \code{threshold} and then completes the partition using the SALSO algorithm.
#'
#' @param chipsOutput Output from the \code{chips} function.
#' @param threshold The minimum marginal probability for the subpartition.
#'   Values closer to 1.0 will yield a partition of fewer items and values
#'   closer to 0.0 will yield a partition of more items.
#' @param ... Other arguments passed to the \code{chips} function.
#'
#' @return A list containing the same values as the \code{chips} function.
#'
#' @export
#' @examples
#' # For examples, use 'nCores = 1' per CRAN rules, but in practice omit this.
#' data(iris.clusterings)
#' draws <- iris.clusterings
#'
#' all <- chips(draws, nRuns = 1, nCores = 1)
#' plot(all$n_items, all$probability)
#'
#' threshold(all, threshold = 0.5, nCores = 1)
#'
threshold <- function(chipsOutput, threshold, ...) {
  index <- min(which(chipsOutput$probability >= threshold))
  chips(
    attr(chipsOutput, "partitions"),
    threshold = chipsOutput$probability[index],
    initialPartition = chipsOutput$chips_partition[index, ],
    ...
  )
}

#' @export
print.salso_chips <- function(x, ...) {
  attrs <- attributes(x)
  attributes(x) <- attrs[!(names(attrs) %in% "partitions")]
  print.default(x, ...)
}
