#' Synthetic Dataset for CHIPS Demo
#'
#' A small synthetic dataset used in the CHIPS demo.
#'
#' Load with `data("synthetic", package = "salso")` to get a list with
#' the following components:
#' - `partitions`: integer matrix of size `B x n` giving `B` sampled
#'   clusterings (rows) of `n` items (columns), encoded with cluster labels.
#' - `data`: numeric matrix of size `n x 2` giving the two-dimensional
#'   observations used to generate the partitions.
#' - `means`: numeric array of size `B x K x 2` giving cluster-specific means
#'   across MCMC iterations (`B`), for each cluster (`K`) and dimension (2).
#'
#' See also the CHIPS demo: `demo("chips-demo", package = "salso")`.
#'
#' @format A list with elements `partitions`, `data`, and `means` as described.
#' @seealso chips
#' @keywords datasets
#' @name synthetic
#' @aliases synthetic
#' @title Synthetic Dataset for CHIPS Demo
#' @usage data(synthetic)
#' @docType data
#' @examples
#' data(synthetic)
#' str(synthetic)
NULL
