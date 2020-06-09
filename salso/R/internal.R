lossMapping <- c("binder" = 0L, "binder2" = 1L, "omARI" = 2L, "omARI.approx" = 3L, "VI" = 4L, "VI.lb" = 5L)

lossCode <- function(loss) {
  if ( ( length(loss) != 1 ) || ! ( loss %in% names(lossMapping) ) ) {
    stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  }
  unname(lossMapping[loss])
}

x2drawspsm <- function(x, loss, parallel=TRUE) {
  draws <- NULL
  psm <- NULL
  if ( isSymmetric(x) && all(0 <= x) && all(x <= 1) && all(diag(x)==1) ) {
    psm <- x
  } else {
    draws <- x
  }
  if ( loss %in% c("binder","omARI.approx","VI.lb") ) {
    if ( is.null(psm) ) psm <- salso::psm(draws, parallel)
  } else if ( loss %in% c("binder2","omARI","VI") ) {
    if ( is.null(draws) ) stop(sprintf("For the '%s' criterion, 'x' must be samples from a partition distribution.",loss))
  } else stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  list(draws=draws, psm=psm)
}
