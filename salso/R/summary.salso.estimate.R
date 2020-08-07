#' Summary of Partitions Estimated Using Posterior Expected Loss
#'
#' Assessing the quality of clusters in a partition estimate is added by this
#' function. The result can then be plotted with
#' \code{\link{plot.salso.summary}}.  The current implementation of the
#' calculation of these summaries is not terribly efficient and may be improved
#' in the future.
#'
#' @param object An object returned by the \code{\link{salso}} function.
#' @param ... Currently ignored
#'
#' @return A list containing the pairwise similarity matrix, the mean pairwise
#'   similarity matrix, the score and mean pairwise similarity for each
#'   observation, exemplar observation for each cluster, a dendrogram object, a
#'   vector for ordering observations in the heatmap plot, the size of each
#'   cluster, and the number of clusters.
#'
#' @export
#'
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' draws <- iris.clusterings
#' est <- salso(draws, nCores=1)
#' summ <- summary(est)
#' plot(summ, type="heatmap")
#' plot(summ, type="mds")
#' plot(summ, type="pairs", data=iris)
#' plot(summ, type="dendrogram")
#'
summary.salso.estimate <- function(object, ...) {
  loss <- attr(object,"info")$loss
  x <- if ( ! is.null(attr(object,"psm")) ) attr(object,"psm") else attr(object,"draws")
  isPSM <- isPSM(x)
  nItems <- length(object)
  one2n <- seq_len(nItems)
  # pairwise similarity matrix
  psm <- if ( isPSM ) x else psm(x)
  # score
  score <- sapply(one2n, function(i) {
    subset <- ( object == object[i] ) & ( one2n != i )
    xi <- if ( isPSM ) x[subset, subset, drop=FALSE] else x[, subset, drop=FALSE]
    partition.loss(rep(1,ncol(xi)), xi, loss)
  })
  score[is.na(score)] <- 0
  # meanPS
  meanPS <- sapply(seq_along(object), function(i) {
    mean(psm[object==object[i], i])
  })
  # exemplar
  all <- data.frame(id=seq_len(nItems), label=as.vector(object), score=score)
  exemplar <- sapply(split(all, all$label), function(d) {
    if ( nrow(d) > 1 ) d$id[which.max(d$score)] else d$id
  })
  # dendrogram
  dendrogram <- partition.loss.dendrogram(as.vector(object), x, loss)
  # order
  walk <- function(dendrogram, order=numeric()) {
    if ( is.null(attr(dendrogram,"leaf")) ) {
      order <- c(order,walk(dendrogram[[1]]))
      order <- c(order,walk(dendrogram[[2]]))
    } else {
      order <- c(order,as.numeric(attr(dendrogram,"label")))
    }
    order
  }
  order <- order(order(walk(dendrogram))[object], -1*score)
  # nClusters
  nClusters <- length(exemplar)
  # pairwise similarity matrix averaged by clustering estimate
  m <- matrix(0.0, nrow=nClusters, ncol=nClusters)
  for ( i in seq_len(nClusters) ) {
    for ( j in seq_len(i) ) {
      m[i,j] <- mean(psm[object==i, object==j])
    }
  }
  ut <- upper.tri(m)
  m[ut] <- t(m)[ut]
  # sizes
  sizes <- table(object)
  names <- names(sizes)
  sizes <- as.vector(sizes)
  names(sizes) <- names
  # Finalize
  result <- list(estimate=object, nClusters=nClusters, score=score, order=order, dendrogram=dendrogram, psm=psm, meanPS=meanPS, meanPSM=m, exemplar=exemplar, sizes=sizes)
  class(result) <- "salso.summary"
  result
}
