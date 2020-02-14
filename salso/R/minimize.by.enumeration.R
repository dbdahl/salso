#' @useDynLib salso .minimize_by_enumeration
#'
minimize.by.enumeration <- function(psm, loss=c("binder","pear","VI.lb")[3]) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  y <- .Call(.minimize_by_enumeration, nrow(psm), psm, lossCode(loss))
  names(y) <- colnames(psm)
  y
}
