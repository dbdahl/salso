lossMapping <- c("binder" = 0L, "pear" = 1L, "VI.lb" = 2L, "VI" = 3L)

lossCode <- function(loss) {
  if ( ( length(loss) != 1 ) || ! ( loss %in% names(lossMapping) ) ) {
    stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  }
  unname(lossMapping[loss])
}

x2drawspsm <- function(x, loss) {
  draws <- NULL
  psm <- NULL
  if ( isSymmetric(x) && all(0 <= x) && all(x <= 1) && all(diag(x)==1) ) {
    psm <- x
  } else {
    draws <- x
  }
  if ( loss %in% c("VI.lb","pear","binder") ) {
    if ( is.null(psm) ) psm <- salso::psm(draws)
  } else if ( loss %in% c("VI") ) {
    if ( is.null(draws) ) stop(sprintf("For the '%s' criterion, 'x' must be samples from a partition distribution.",loss))
  } else stop("Unrecognized loss.")
  list(draws=draws, psm=psm)
}
