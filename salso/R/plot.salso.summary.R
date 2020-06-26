#' Heatmap, Dendrogram, and Exemplar Plotting for Partition Estimation
#'
#' This function produces one of three plots:  1. A heatmap shows the pairwise
#' allocation probabilities that items are clustered.
#' 2. A dendrogram based on expected partition loss shows the relationships among
#' clusters when merging pairs of clusters such that the increase in the
#' expectation of the posterior loss is minimized.  3. An exemplar plot shows
#' pairs plots of all the variables with the exemplar (i.e., the most representative observation) of each cluster emphasized.
#'
#' @param x An object returned by \code{summary(x)}, where \code{x} itself is returned by the \code{\link{salso}} function.
#' @param type A string equal to \code{"heatmap"}, \code{"dendrogram"}, or \code{"exemplar"}.
#' @param data The data from which the partition estimation was obtained.  This is required when \code{type='exemplar'} and ignored otherwise.
#' @param showLabels Should the names of items be shown in the plot when \code{type="heatmap"}?
#' @param ... Currently ignored.
#'
#' @return \code{NULL}, invisibly.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules but, in practice, omit this.
#' draws <- iris.clusterings
#' est <- salso(draws, nCores=1)
#' summ <- summary(est)
#' plot(summ, type="heatmap")
#' plot(summ, type="dendrogram")
#' plot(summ, type="exemplar", data=iris)
#'
#' @seealso \code{\link{salso}}, \code{\link{summary.salso.estimate}}
#'
#' @importFrom grDevices heat.colors rainbow topo.colors
#' @importFrom graphics abline axis box image pairs par points polygon segments
#'   text
#' @export
#'
plot.salso.summary <- function(x, type=c("heatmap","dendrogram","exemplar")[1], data=NULL, showLabels=length(x$estimate)<=50, ...) {
  if ( type == "dendrogram" ) {
    plot(x$dendrogram)
    return(invisible())
  }
  if ( type == "exemplar" ) {
    if ( is.null(data) ) stop("'data' must be supplied when type='exemplar'")
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
  if ( type != "heatmap" ) stop(sprintf("Unknown plot type '%s'.",type))
  estimate <- x$estimate
  o <- x$order
  pm <- if ( ! is.null(attr(x$estimate,"psm")) ) attr(x$estimate,"psm") else psm(attr(x$estimate,"draws"))
  pm <- pm[o,rev(o)]
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

