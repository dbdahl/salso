#' @useDynLib salso .minimize_by_enumeration
#'
minimize.by.enumeration <- function(x, loss="VI.lb") {
  FORCE_INEFFICIENT <- if ( grepl("^__", loss) ) {
    loss <- gsub("^__","",loss)
    TRUE
  } else FALSE
  z <- x2drawspsm(x, loss)
  if ( ( loss %in% c("binder","omARI.approx","VI.lb") ) && ! FORCE_INEFFICIENT ) {
    y <- .Call(.minimize_by_enumeration, z$psm, lossCode(loss))
    names(y) <- colnames(z$psm)
  } else {
    cat("The current implementation is not parallelized nor memory efficient.\n")
    all <- enumerate.partitions(ncol(x))
    y <- all[which.min(partition.loss(all, x, loss)),]
    names(y) <- if ( !is.null(z$draws) ) colnames(z$draws) else colnames(z$psm)
  }
  y
}
