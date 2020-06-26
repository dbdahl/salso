#' Summary of Partitions Estimated Using Posterior Expected Loss
#'
#' @param object An object returned by the \code{\link{salso}} function.
#' @param ... Currently ignored
#'
#' @return A list containing the score for each observation, exemplar
#'   observation for each cluster, a dendrogram object, and a vector for
#'   ordering observations in the heatmap plot.
#' @export
#'
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules but, in practice, omit this.
#' draws <- iris.clusterings
#' est <- salso(draws, nCores=1)
#' summ <- summary(est)
#' plot(summ, type="heatmap")
#' plot(summ, type="dendrogram")
#' plot(summ, type="exemplar", data=iris)
#'
summary.salso.estimate <- function(object, ...) {
  loss <- attr(object,"info")$loss
  x <- if ( ! is.null(attr(object,"psm")) ) attr(object,"psm") else attr(object,"draws")
  isPSM <- isPSM(x)
  nItems <- length(object)
  one2n <- seq_len(nItems)
  # score
  score <- sapply(one2n, function(i) {
    subset <- ( object==object[i] ) & ( one2n!= i )
    xi <- if ( isPSM ) x[subset, subset, drop=FALSE] else x[, subset, drop=FALSE]
    partition.loss(rep(1,ncol(xi)), xi, loss)
  })
  score[is.na(score)] <- 0
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
  # matrix
  matrix <- NULL
  # Finalize
  result <- list(estimate=object, score=score, dendrogram=dendrogram, order=order, matrix=matrix, exemplar=exemplar)
  class(result) <- "salso.summary"
  result
}
