#' Canonicalize Cluster Labels
#'
#' @param x A vector of cluster labels.
#'
#' @return A numeric vector containing cluster labels in cannonical form.
#'
#' @export
canonicalize_cluster_labels <- function(x) {
  .Call(.canonicalize_cluster_labels, x)
}
