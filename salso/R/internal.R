initMethodMapping <- c("empty" = 0L, "singletons" = 1L, "sample" = 2L)

lossMapping <- c("binder.draws" = 0L, "binder.psm" = 1L, "omARI" = 2L, "omARI.approx" = 3L, "VI" = 4L, "VI.lb" = 5L)

isPSM <- function(x) {
  ( isSymmetric(x) && all(0 <= x) && all(x <= 1) && all(diag(x)==1) )
}

x2drawspsm <- function(x, loss, nCores=0) {
  draws <- NULL
  psm <- NULL
  if ( isPSM(x) ) {
      psm <- x
      if ( loss == "binder" ) loss <- "binder.psm"
  } else {
      draws <- x
      if ( loss == "binder" ) loss <- "binder.draws"
  }
  if ( ( length(loss) != 1 ) || ! ( loss %in% names(lossMapping) ) ) {
    stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  }
  lossCode <- unname(lossMapping[loss])
  if ( loss %in% c("binder.psm","omARI.approx","VI.lb") ) {
    if ( is.null(psm) ) psm <- salso::psm(draws, nCores)
  } else if ( loss %in% c("binder.draws","omARI","VI") ) {
    if ( is.null(draws) ) stop(sprintf("For the '%s' criterion, 'x' must be samples from a partition distribution.",loss))
  } else stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  list(loss=loss, lossCode=lossCode, draws=draws, psm=psm)
}
