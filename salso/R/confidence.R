#' Compute Clustering Confidence
#'
#' This function computes the confidence values for \code{n} observations based
#' on a clustering estimate and the expected pairwise allocation matrix.
#'
#' @param estimate A vector of length \code{n}, where \code{i} and \code{j} are
#'   in the same cluster if and only if \code{clustering[i] == clustering[j]}.
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' est <- salso(probs, nPermutations=50, parallel=FALSE)$estimate
#' conf <- confidence(est,probs)
#' conf
#'
#' @seealso \code{\link{salso}}, \code{\link{dlso}}, \code{\link{psm}}
#'
#' @export
#'
confidence <- function(estimate, psm) {
  #if ( ! isCanonical(estimate) ) stop("'estimate' must be in canonical form.")
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( length(estimate) != nrow(psm) ) stop("Dimension of 'psm' does not match the length of 'estimate'.")
  confidence <- sapply(1:length(estimate), function(i) mean(psm[i,estimate==estimate[i]]))
  labels <- unique(estimate)
  nSubsets <- length(labels)
  confidenceMatrix <- matrix(0, nrow=nSubsets, ncol=nSubsets)
  for ( k1 in labels ) {
    for ( k2 in labels ) {
      confidenceMatrix[k1+1,k2+1] <- mean(psm[estimate==k1, estimate==k2])
    }
  }
  dimnames(confidenceMatrix) <- list(labels,labels)
  o <- order(table(estimate),decreasing=TRUE)
  confidenceMatrix <- confidenceMatrix[o,o]
  w <- rev(1:length(o))
  order <- order(match(estimate+1,o),confidence)
  exemplar <- rep(0L, nSubsets)
  for ( k in labels ) {
    x <- seq_along(estimate)[estimate==k & confidence==max(confidence[estimate==k])]
    exemplar[k+1] <- if ( length(x) == 1 ) x else sample(x,1)
  }
  result <- list(estimate=estimate, psm=psm, confidence=confidence, confidenceMatrix=confidenceMatrix,
                 exemplar=exemplar, order=order)
  class(result) <- "salso.confidence"
  result
}

