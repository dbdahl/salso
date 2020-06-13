#' @export
#'
summary.salso.estimate <- function(object, ...) {
  loss <- attr(object,"info")$loss
  x <- if ( ! is.null(attr(object,"psm")) ) attr(object,"psm") else attr(object,"draws")
  isPSM <- salso:::isPSM(x)
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

# exemplar <- function(estimate) {
#   x <- if ( ! is.null(attr(estimate,"psm")) ) attr(estimate,"psm") else attr(estimate,"draws")
#   isPSM <- salso:::isPSM(x)
#   loss <- attr(estimate,"info")$loss
#   nItems <- length(estimate)
#   one2n <- seq_len(nItems)
#   score <- sapply(one2n, function(i) {
#     subset <- ( estimate==estimate[i] ) & ( one2n!= i )
#     xi <- if ( isPSM ) x[subset, subset, drop=FALSE] else x[, subset, drop=FALSE]
#     partition.loss(rep(1,ncol(xi)), xi, loss)
#   })
#   score[is.na(score)] <- 0
#   print(score)
#   all <- data.frame(id=seq_len(nItems), label=as.vector(estimate), score=score)
#   sapply(split(all, all$label), function(d) {
#     if ( nrow(d) > 1 ) d$id[which.max(d$score)] else d$id
#   })
# }
