#' Heatmap, Multidimensional Scaling, Pairs, and Dendrogram Plotting for
#' Partition Estimation
#'
#' This function produces one of four plots:  1. \code{"heatmap"}: A heatmap
#' showing the pairwise allocation probabilities that items are clustered. 2.
#' \code{"mds"}: A scatter plot using classical multidimensional scaling (also
#' known as principal coordinates analysis) with the exemplar (i.e., the most
#' representative observation) of each cluster emphasized. 3. \code{"pairs"}:
#' Pairs plots of all the variables with the exemplar (i.e., the most
#' representative observation) of each cluster emphasized. 4.
#' \code{"dendrogram"}: A dendrogram based on expected partition loss showing
#' the relationships among clusters when merging pairs of clusters such that the
#' increase in the expectation of the posterior loss is minimized.
#'
#' @param x An object returned by \code{summary(y)}, where \code{y} itself is
#'   returned by the \code{\link{salso}} function.
#' @param type A string equal to \code{"heatmap"}, \code{"mds"}, \code{"pairs"},
#'   or \code{"dendrogram"}.
#' @param data The data from which the partition estimation was obtained. This
#'   is required when \code{type='pairs'} and ignored otherwise.
#' @param showLabels Should the names of items be shown in the plot when
#'   \code{type="heatmap"}?
#' @param ... Arguments to be passed to methods, such as graphical parameters
#'   (see \code{\link{par}}).
#'
#' @return \code{NULL}, invisibly.
#'
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' draws <- iris.clusterings
#' est <- salso(draws, nCores=1)
#' summ <- summary(est)
#' plot(summ, type="heatmap")
#' plot(summ, type="mds")
#' plot(summ, type="pairs", data=iris)
#' plot(summ, type="dendrogram")
#'
#' @seealso \code{\link{salso}}, \code{\link{summary.salso.estimate}},
#'   \code{\link[stats]{cmdscale}}.
#'
#' @importFrom grDevices heat.colors rainbow topo.colors
#' @importFrom graphics abline axis box image pairs par points polygon segments
#'   text
#' @export
#'
plot.salso.summary <- function(x, type=c("heatmap","mds","pairs","dendrogram")[1], data=NULL, showLabels=length(x$estimate)<=50, ...) {
  opar <- NULL
  if ( type == "dendrogram" ) {
    plot(x$dendrogram)
  } else if ( type == "heatmap" ) {
    estimate <- x$estimate
    o <- x$order
    pm <- x$psm
    pm <- pm[o,rev(o)]
    n <- nrow(pm)
    sizes <- rle(estimate[o])$lengths
    cuts <- cumsum(sizes)
    centers <- ( c(0,cuts[-length(cuts)]) + cuts ) / 2
    cuts <- cuts[-length(cuts)]
    labels <- rle(estimate[o])$values
    if ( showLabels ) {
      mymar <- c(1.5,0.5,0.5,1.5)
      cexscale <- 0.85 * 50 / length(estimate)
    } else {
      mymar <- c(0,0,0,0)
      cexscale <- 1 * 50 / length(estimate)
    }
    opar <- par(pty="s",mar=mymar)
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
  } else {
    colors <- rainbow(x$nClusters)
    colorsExpanded <- colors[x$estimate]
    i <- x$exemplar[x$estimate]
    if ( type == "pairs" ) {
      if ( is.null(data) ) stop("'data' must be supplied when type='pairs'")
      permutation <- sample(seq_along(x$estimate))
      panelFnc <- function(x0,y0,...) {
        points(x0[permutation],y0[permutation],col=colorsExpanded[permutation],pch=19,...)
        segments(x0[permutation],y0[permutation],x0[i[permutation]],y0[i[permutation]],col=colorsExpanded[permutation],...)
        points(x0[x$exemplar],y0[x$exemplar],pch=22,bg="white",cex=2,...)
      }
      opar <- par(no.readonly=TRUE)
      pairs(data, panel=panelFnc, oma=c(3,10,3,3))
      legend("topleft", legend=as.character(seq_len(x$nClusters)), pch=20, col=colors, xpd=TRUE, ...)
    } else if ( type == "mds" ) {
      distances <- 1 - x$psm
      points <- cmdscale(distances, list.=TRUE)$points
      # points <- MASS::isoMDS(distances)$points
      opar <- par(pty="s", mar=c(0,0,0,0))
      plot(points[,1], points[,2], pch=20, col=colorsExpanded, type=ifelse(showLabels, "n", "p"), xlab="", ylab="", axes=FALSE, ...)
      box()
      permutation <- sample(seq_along(x$estimate))
      segments(points[permutation,1],points[permutation,2],points[i[permutation],1],points[i[permutation],2],col=colorsExpanded[permutation],...)
      if ( showLabels ) {
        text(points[permutation,1], points[permutation,2], as.character(seq_along(x$estimate))[permutation], col=colorsExpanded[permutation],...)
      }
      points(points[x$exemplar,1],points[x$exemplar,2],pch=22,bg="white",cex=2,...)
      legend("topleft", legend=as.character(seq_len(x$nClusters)), pch=20, col=colors, ...)
    } else stop(sprintf("Unknown plot type '%s'.",type))
  }
  if ( ! is.null(opar) ) par(opar)
  invisible()
}

