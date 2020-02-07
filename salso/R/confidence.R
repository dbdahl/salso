#' Compute Clustering Confidence
#'
#' This function computes the confidence values for \code{n} observations based
#' on a clustering estimate and the pairwise similarity matrix.
#'
#' @param estimate A vector of length \code{n}, where \code{i} and \code{j} are
#'   in the same subset (i.e., cluster) if and only if \code{estimate[i] ==
#'   estimate[j]}.
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#'
#' @return A list of the following elements: \describe{
#'   \item{estimate}{The value of the \code{estimate} argument.}
#'   \item{psm}{The value of the \code{psm} argument.}
#'   \item{confidence}{A numeric vector with the same length as \code{estimate} that contains the mean probability that each item is paired with all of the other items in its subset (i.e., cluster).}
#'   \item{confidenceMatrix}{A matrix containing the mean confidences of items in each subset on the diagonal. In the off-diagonal elements, the mean confidence among all pairs from the two subsets. High probabilities on the diagonal and low probabilities everywhere else indicate good separability among the subsets.}
#'   \item{exemplar}{A numeric vector containing the exemplar for each subset (i.e, cluster). The "exemplar" of a subset has the highest probability of being clustered with all of the other items in its subset.}
#'   \item{order}{A vector giving the permutation of the original observations used in plotting.}
#'   }
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' # Use 'parallel=FALSE' per CRAN rules for examples but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' est <- salso(probs, parallel=FALSE)$estimate
#' conf <- confidence(est, probs)
#' conf
#'
#' @seealso \code{\link{plot.salso.confidence}}, \code{\link{salso}}, \code{\link{dlso}}, \code{\link{psm}}
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

