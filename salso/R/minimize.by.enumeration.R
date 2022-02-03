minimize.by.enumeration <- function(x, loss="VI") {
  FORCE_INEFFICIENT <- if ( is.character(loss) && grepl("^__", loss) ) {
    loss <- gsub("^__","",loss)
    TRUE
  } else FALSE
  z <- x2drawspsm(x, loss)
  if ( ( z$lossStr %in% c("binder.psm","omARI.approx","VI.lb") ) && ! FORCE_INEFFICIENT ) {
    y <- .Call(.minimize_by_enumeration, z$psm, z$lossCode, z$a)
    names(y) <- colnames(z$psm)
  } else {
    cat("The current implementation is not parallelized nor memory efficient.\n")
    all <- enumerate.partitions(ncol(x))
    y <- all[which.min(partition.loss(x, all, loss)),]
    names(y) <- if ( !is.null(z$draws) ) colnames(z$draws) else colnames(z$psm)
  }
  y
}
