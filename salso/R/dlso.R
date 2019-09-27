#' Draws-Based Latent Structure Optimization
#'
#' This function provides a point estimate for a partition distribution using
#' the draws latent structure optimization (DLSO) method, which is also known as
#' the least-squares clustering method (Dahl 2006). The method seeks to minimize
#' the expectation of the Binder loss or the lower bound of the variation of
#' information loss by picking the minimizer among the partitions supplied by
#' the `draws` argument.
#'
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#' @param loss Either \code{"VI.lb"} or \code{"binder"}, to indicate the desired
#'   loss function.
#' @param draws A \code{B}-by-\code{n} matrix, where each of the \code{B} rows
#'   represents a clustering of \code{n} items using cluster labels.  For
#'   clustering \code{b}, items \code{i} and \code{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.
#' @param parallel Should the search use all CPU cores?
#'
#' @return A list of the following elements: \describe{ \item{estimate}{An
#'   integer vector giving a partition encoded using cluster labels.}
#'   \item{loss}{A character vector equal to the \code{loss} argument.}
#'   \item{expectedLoss}{A numeric vector of length one giving the expected
#'   loss.} }
#'
#' @export
#' @references
#' D. A. Binder (1978), Bayesian cluster analysis, \emph{Biometrika} \bold{65},
#' 31-38.
#'
#' D. B. Dahl (2006), Model-Based Clustering for Expression Data via a Dirichlet
#' Process Mixture Model, in \emph{Bayesian Inference for Gene Expression and
#' Proteomics}, Kim-Anh Do, Peter Müller, Marina Vannucci (Eds.), Cambridge
#' University Press.
#'
#' J. W. Lau and P. J. Green (2007), Bayesian model based clustering procedures,
#' \emph{Journal of Computational and Graphical Statistics} \bold{16}, 526-558.
#
#' D. B. Dahl, M. A. Newton (2007), Multiple Hypothesis Testing by Clustering
#' Treatment Effects, \emph{Journal of the American Statistical Association},
#' \bold{102}, 517-526.
#'
#' A. Fritsch and K. Ickstadt (2009), An improved criterion for clustering
#' based on the posterior similarity matrix, \emph{Bayesian Analysis},
#' \bold{4}, 367-391.
#'
#' S. Wade and Z. Ghahramani (2018), Bayesian cluster analysis: Point
#' estimation and credible balls. \emph{Bayesian Analysis}, \bold{13:2},
#' 559-626.
#'
#' @examples
#' dlso(draws=iris.clusterings, parallel=FALSE)
#'
dlso <- function(psm, loss=c("VI.lb","binder")[1], draws, parallel=TRUE) {
  if ( missing(psm) ) psm <- salso::psm(draws)
  f <- if ( loss == "VI.lb" ) VI.lb
  else if ( loss == "binder" ) binder
  else stop("'loss' is not recognized.")
  expectedLoss <- f(draws, psm)
  index <- which.min(expectedLoss)
  list(estimate=draws[index,], loss=loss, expectedLoss=expectedLoss[index])
}
