#' @useDynLib salso .minimize_by_enumeration
#'
minimize.by.enumeration <- function(psm, loss=c("VI.lb","binder")[1]) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( ( length(loss) != 1 ) || ! ( loss %in% c("VI.lb","binder") ) ) stop("'loss' is not recognized.")
  useVIlb <- loss == "VI.lb"
  y <- .Call(.minimize_by_enumeration, nrow(psm), psm, useVIlb)
  names(y) <- colnames(psm)
  y
}
