#' Confidence and Exemplar Plotting
#'
#' This function produces confidence plots (e.g., heatmaps of pairwise
#' allocation probabilities) and exemplar plots. The "exemplar" refers to the
#' best representative of a particular cluster. See \code{\link{confidence}} for
#' further explanation.
#'
#' @param x An object returned by the \code{\link{confidence}} function.
#' @param estimate A vector of length \eqn{n}, where \eqn{i} and \eqn{j} are in
#'   the same subset (i.e., cluster) if and only if \code{estimate[i] ==
#'   estimate[j]}.'  If \code{NULL}, the \code{x$estimate} in used.
#' @param data The data from which the distances were computed.
#' @param showLabels Should the names of items be shown in the plot?
#' @param ... Currently ignored.
#'
#' @return \code{NULL}, invisibly.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' # For examples, use 'parallel=FALSE' per CRAN rules but, in practice, omit this.
#' draws <- iris.clusterings
#' est <- salso(draws, parallel=FALSE)$estimate
#' probs <- psm(draws, parallel=FALSE)
#' conf <- confidence(est, probs)
#' plot(conf)
#' plot(conf, data=iris)
#'
#' @seealso \code{\link{confidence}}, \code{\link{psm}}, \code{\link{dlso}},
#'   \code{\link{salso}}
#'
#' @importFrom grDevices heat.colors rainbow topo.colors
#' @importFrom graphics abline axis box image pairs par points polygon segments
#'   text
#' @export
#'
plot.salso.confidence <- function(x, estimate=NULL, data=NULL, showLabels=length(x$estimate)<=50, ...) {
  if ( ! is.null(data) ) {
    if ( ! is.null(estimate) ) stop("'estimate' must be 'NULL' for pairs plot.")
    m <- match(x$estimate, as.numeric(names(x$exemplar)))
    i <- x$exemplar[m]
    c <- rainbow(length(x$exemplar))[m]
    panelFnc <- function(x0,y0,...) {
      points(x0,y0,col=c,pch=19,...)
      segments(x0,y0,x0[i],y0[i],col=c,...)
      points(x0[x$exemplar],y0[x$exemplar],pch=22,bg="white",cex=2,...)
    }
    pairs(data,panel=panelFnc)
    return(invisible())
  }
  if ( is.null(estimate) ) {
    estimate <- x$estimate
    o <- x$order
  } else {
    o <- order(estimate)
  }
  pm <- x$psm[o,rev(o)]
  n <- nrow(pm)
  sizes <- rle(estimate[o])$lengths
  cuts <- cumsum(sizes)
  centers <- ( c(0,cuts[-length(cuts)]) + cuts ) / 2
  cuts <- cuts[-length(cuts)]
  labels <- rle(estimate[o])$values
  if ( showLabels ) {
    mymai <- c(1.5,0.5,0.5,1.5)
    cexscale <- 0.85 * 50 / length(estimate)
  } else {
    mymai <- c(0,0,0,0)
    cexscale <- 1 * 50 / length(estimate)
  }
  opar <- par(pty="s",mai=mymai)
  colors <- topo.colors(200)
  colors <- rev(heat.colors(200))
  image(x=1:n,y=1:n,z=pm,axes=FALSE,xlab="",ylab="",col=colors)
  box()
  abline(v=cuts+0.5,lwd=3)
  abline(h=n-cuts+0.5,lwd=3)
  text(centers+0.5,n-centers+0.5,labels,cex=0.8*cexscale*sizes)
  if ( showLabels ) {
    axisLabels <- if ( is.null(names(estimate)) ) o
    else names(estimate[o])
    axis(4,1:length(estimate),rev(axisLabels),las=2,cex.axis=0.8*cexscale)
    axis(1,1:length(estimate),axisLabels,las=2,cex.axis=0.8*cexscale)
    nn <- length(colors)
    bx <- par("usr")
    bx.cx <- c(bx[1] - 1.6 * (bx[2] - bx[1]) / 50, bx[1] - 0.3 * (bx[2] - bx[1]) / 50)
    bx.cy <- c(bx[3], bx[3])
    bx.sy <- (bx[4] - bx[3]) / nn
    xx <- rep(bx.cx, each=2)
    for ( i in 1:nn ) {
      yy <- c(bx.cy[1] + (bx.sy * (i - 1)),
              bx.cy[1] + (bx.sy * (i)),
              bx.cy[1] + (bx.sy * (i)),
              bx.cy[1] + (bx.sy * (i - 1)))
      polygon(xx,yy,col=colors[i],border=colors[i],xpd=TRUE)
    }
  }
  par(opar)
  invisible()
}

