#' @useDynLib salso .minimize_by_enumeration
#'
minimize.by.enumeration <- function(x, loss=c("binder","pear","VI.lb","VI")[3]) {
  FORCE_INEFFICIENT <- if ( grepl("^__", loss) ) {
    loss <- gsub("^__","",loss)
    TRUE
  } else FALSE
  z <- x2drawspsm(x, loss)
  if ( ( loss %in% c("binder","pear","VI.lb") ) && ! FORCE_INEFFICIENT ) {
    y <- .Call(.minimize_by_enumeration, z$psm, lossCode(loss))
    names(y) <- colnames(z$psm)
  } else {
    warning("The current implementation is not parallelized nor memory efficient.")
    all <- enumerate.partitions(ncol(x))
    y <- all[which.min(partition.loss(all, x, loss)),]
    names(y) <- if ( !is.null(z$draws) ) colnames(z$draws) else colnames(z$psm)
  }
  y
}
