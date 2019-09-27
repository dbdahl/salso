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
#'   \item{psm}{The value of the \code{psm} argument (the given pairwise similarity matrix).}
#'   \item{confidence}{A numeric vector containing the confidence or probability that each item is paired with all of the other items in its particular cluster.}
#'   \item{confidenceMatrix}{A matrix containing the average confidence of all items in a cluster (on the diagonal). The off-diagonal shows the confidence or probability that items in a cluster should be clustered with the items in a different cluster. High probabilities on the diagonal and low probabilities everywhere else indicates a high confidence that the clusterings chosen are correct.}
#'   \item{exemplar}{A numeric vector containing the exemplar for each cluster. The "exemplar" is the best representative of a particular cluster, meaning that it has the highest confidence or probability of being clustered with all of the other items in its cluster.}
#'   \item{order}{A numeric vector containg the ordering of the items in the clusters, labeled by the original index or item number. Note that the ordering is somewhat arbitrary, as the important thing is that items are clustered with the correct items, not the order of these items within a particular cluster.}
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
confidence <- function(estimate, psm) {
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
      confidenceMatrix[i1,i2] <- mean(minipsm)
    }
  }
  exemplar <- sapply(labels, function(l) which(estimate==l & confidence==max(confidence[estimate==l]))[1])
  names(exemplar) <- labels
  secondary <- confidence
  if ( length(secondary) %% 2 == 1 ) secondary <- c(secondary,0)
  secondary <- c(-1,1) * secondary
  secondary <- secondary[seq_along(estimate)]
  order <- order(match(estimate,labels), secondary)
  result <- list(estimate=estimate, psm=psm, confidence=confidence, confidenceMatrix=confidenceMatrix,
                 exemplar=exemplar, order=order)
  class(result) <- "salso.confidence"
  result
}

