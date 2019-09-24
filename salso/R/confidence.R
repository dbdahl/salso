#psm <- psm(iris.clusterings)
#estimate <- salso(psm,loss="binder")$estimate

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
    x <- (1:nItems)[estimate==k && conf==max(conf[estimate==k])]
    examplar[k+1] <- if ( length(x) == 1 ) x else sample(x,1)
  }
  result <- list(estimate=estimate, psm=psm, confidence=confidence, confidenceMatrix=confidenceMatrix,
                 exemplar=exemplar, order=order)
  class(result) <- "salso.confidence"
  result
}

