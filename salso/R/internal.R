lossMapping <- c("binder" = 0L, "pear" = 1L, "VI.lb" = 2L, "VI" = 3L)

lossCode <- function(loss) {
  if ( ( length(loss) != 1 ) || ! ( loss %in% names(lossMapping) ) ) {
    stop(sprintf('loss="%s" is not recognized.  Please use one of the following: %s', loss, paste0('"',names(lossMapping),'"',collapse=", ")))
  }
  unname(lossMapping[loss])
}
