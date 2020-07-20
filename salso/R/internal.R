initMethodMapping <- c("empty" = 0L, "singletons" = 1L, "sample" = 2L)

lossMapping <- c("binder.draws" = 0L, "binder.psm" = 1L, "omARI" = 2L,
                 "omARI.approx" = 3L, "VI" = 4L, "VI.lb" = 5L,
                 "NVI" = 6L, "ID" = 7L, "NID" = 8L)

isPSM <- function(x) {
  ( isSymmetric(x) && all(0 <= x) && all(x <= 1) && all(diag(x)==1) )
}

x2drawspsm <- function(x, loss, nCores=0) {
  draws <- NULL
  psm <- NULL
  a <- 1
  if ( inherits(loss, "salso.loss") ) {
    if ( loss$loss == "binder" ) a <- loss$a
    loss <- loss$loss
  }
  lossStr <- loss
  if ( isPSM(x) ) {
      psm <- x
      if ( loss == "binder" ) lossStr <- "binder.psm"
  } else {
      draws <- x
      if ( loss == "binder" ) lossStr <- "binder.draws"
  }
  if ( ( length(lossStr) != 1 ) || ! ( lossStr %in% names(lossMapping) ) ) {
    stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  }
  lossCode <- unname(lossMapping[lossStr])
  if ( lossStr %in% c("binder.psm","omARI.approx","VI.lb") ) {
    if ( is.null(psm) ) psm <- salso::psm(draws, nCores)
  } else {
    if ( is.null(draws) ) stop(sprintf("For the '%s' criterion, 'x' must be samples from a partition distribution.",loss))
  }
  list(loss=loss, lossStr=lossStr, lossCode=lossCode, a=a, draws=draws, psm=psm)
}
