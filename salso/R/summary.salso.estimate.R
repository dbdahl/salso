#' Summary of Partitions Estimated Using Posterior Expected Loss
#'
#' Assessing the quality of clusters in a partition estimate is added by this
#' function. The result can then be plotted with
#' \code{\link{plot.salso.summary}}. The current implementation of the
#' calculation of these summaries is not terribly efficient and may be improved
#' in the future.
#'
#' @param object An object returned by the \code{\link{salso}} function.
#' @param alternative An optional argument specifying an alternative clustering
#'   to use instead of that provided by \code{object}. Use this feature to
#'   obtain numerical and graphical summaries of a clustering estimate from
#'   other procedures. This clustering must be provided in canonical form:
#'   cluster labels as integers starting at 1 for the first observation and
#'   incrementing by one for each new label.
#' @param orderingMethod An integer giving method to use to order the
#'   observations for a heatmap plot.  Currently values \code{1} or \code{2} are
#'   supported.
#' @param ... Currently ignored.
#'
#' @return A list containing the estimate, the pairwise similarity matrix, the
#'   mean pairwise similarity matrix, the score and mean pairwise similarity for
#'   each observation, exemplar observation for each cluster, a dendrogram
#'   object, a vector for ordering observations in the heatmap plot, the size of
#'   each cluster, and the number of clusters.
#'
#' @importFrom utils combn
#' @importFrom stats as.dist hclust
#' @export
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' data(iris.clusterings)
#' draws <- iris.clusterings
#' est <- salso(draws, nCores=1)
#' summ <- summary(est)
#' plot(summ, type="heatmap")
#' plot(summ, type="mds")
#' plot(summ, type="pairs", data=iris)
#' plot(summ, type="dendrogram")
#'
summary.salso.estimate <- function(object, alternative, orderingMethod=1, ...) {
  estimate <- as.vector(if ( ! missing(alternative) ) alternative else object)
  if ( missing(alternative) ) estimate <- object
  else {
    estimate <- as.integer(alternative)
    uniq <- unique(estimate)
    if ( ! isTRUE(identical(uniq,seq_len(length(uniq)))) ) stop("The alternative clustering is not in canonical form.")
  }
  loss <- attr(object,"info")$loss
  x <- if ( ! is.null(attr(object,"psm")) ) attr(object,"psm") else attr(object,"draws")
  isPSM <- isPSM(x)
  nItems <- length(estimate)
  one2n <- seq_len(nItems)
  # pairwise similarity matrix
  psm <- if ( isPSM ) x else psm(x)
  # score
  score <- sapply(one2n, function(i) {
    subset <- ( estimate == estimate[i] ) & ( one2n != i )
    xi <- if ( isPSM ) x[subset, subset, drop=FALSE] else x[, subset, drop=FALSE]
    partition.loss(xi, rep(1,ncol(xi)), loss)
  })
  score[is.na(score)] <- 0
  # meanPS
  meanPS <- sapply(seq_along(estimate), function(i) {
    mean(psm[estimate==estimate[i], i])
  })
  # exemplar
  all <- data.frame(id=seq_len(nItems), label=as.vector(estimate), score=score)
  exemplar <- sapply(split(all, all$label), function(d) {
    if ( nrow(d) > 1 ) d$id[which.max(d$score)] else d$id
  })
  # dendrogram
  dendrogram <- partition.loss.dendrogram(as.vector(estimate), x, loss)
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
  order <- if ( orderingMethod == 1 ) {
    order(order(walk(dendrogram))[estimate], -1*score)
  } else if ( orderingMethod == 2 ) {
    order(estimate, order(hclust(as.dist(1-psm))$order))
  } else stop("Unsupported value for the 'orderingMethod' argument.")
  # nClusters
  nClusters <- length(exemplar)
  # pairwise similarity matrix averaged by clustering estimate
  m <- matrix(0.0, nrow=nClusters, ncol=nClusters)
  for ( i in seq_len(nClusters) ) {
    for ( j in seq_len(i) ) {
      m[i,j] <- if ( i == j ) local({
        y <- estimate == i
        x <- psm[y, y, drop=FALSE]
        diag(x) <- 0
        sum(x) / ( length(x) - sum(y) )
      }) else {
        mean(psm[estimate==i, estimate==j])
      }
    }
  }
  ut <- upper.tri(m)
  m[ut] <- t(m)[ut]
  # sizes
  sizes <- table(estimate)
  names <- names(sizes)
  sizes <- as.vector(sizes)
  names(sizes) <- names
  # finalize
  result <- list(estimate=estimate, nClusters=nClusters, score=score, order=order, dendrogram=dendrogram, psm=psm, meanPS=meanPS, meanPSM=m, exemplar=exemplar, sizes=sizes)
  class(result) <- "salso.summary"
  result
}
