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
#' @return A list of the following elements: \describe{ \item{estimate}{The
#'   value of the \code{estimate} argument.}
#'   \item{psm}{The value of the \code{psm} argument.}
#'   \item{confidence}{asdf}
#'   \item{confidenceMatrix}{asdf}
#'   \item{exemplar}{asdf}
#'   \item{order}{asdf}
#'   }
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' est <- salso(probs, nPermutations=50, parallel=FALSE)$estimate
#' conf <- confidence(est, probs)
#' conf
#'
#' @seealso \code{\link{salso}}, \code{\link{dlso}}, \code{\link{psm}}
#'
#' @export
#'
confidence <- function(estimate, psm, method="average") {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( length(estimate) != nrow(psm) ) stop("Dimension of 'psm' does not match the length of 'estimate'.")
  confidence <- sapply(1:length(estimate), function(i) mean(psm[i,estimate==estimate[i]]))
  tab <- table(estimate)
  o <- order(tab, decreasing=TRUE)
  labels <- as.numeric(names(tab[o]))
  confidenceMatrix <- matrix(0, nrow=length(labels), ncol=length(labels))
  dimnames(confidenceMatrix) <- list(labels,labels)
  o2 <- rep(0,length(estimate))
  for ( i1 in seq_along(labels) ) {
    k1 <- labels[i1]
    for ( i2 in seq_along(labels) ) {
      k2 <- labels[i2]
      minipsm <- psm[estimate==k1, estimate==k2]
      if ( ( k1 == k2 ) && ( sum(estimate==k1) > 1 ) ) {
        o3 <- hclust(as.dist(1-minipsm), method=method)$order
        o2[estimate==k1] <- o3
      }
      confidenceMatrix[i1,i2] <- mean(minipsm)
    }
  }
  exemplar <- sapply(labels, function(l) which(estimate==l & confidence==max(confidence[estimate==l]))[1])
  names(exemplar) <- labels
  order <- order(match(estimate,labels),order(o2))
  result <- list(estimate=estimate, psm=psm, confidence=confidence, confidenceMatrix=confidenceMatrix,
                 exemplar=exemplar, order=order)
  class(result) <- "salso.confidence"
  result
}

