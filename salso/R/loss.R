#' Compute a Partition Loss Function
#'
#' Based on the supplied pairwise similarity matrix, these functions compute,
#' for the given partitions: \describe{ \item{\code{binder}}{The
#' expectation of the Binder loss. See D. A. Binder (1978), D. B. Dahl (2006),
#' J. W. Lau and P. J. Green (2007), D. B. Dahl and M. A. Newton (2007)}
#' \item{\code{adjRand}}{The first-order approximation of the
#' expectation of the loss associated with the adjusted Rand index.  Whereas
#' high values of the adjusted Rand index between \eqn{\pi*} and \eqn{\pi}
#' correspond to high concordance between these partitions, the loss associated
#' with the adjusted Rand index for a partition \eqn{\pi*} in estimating
#' \eqn{\pi} is one minus the adjusted Rand index between these partitions.  The
#' \code{adjRand} function returns an approximation of the expectation of the
#' adjusted Rand index loss, specifically, one minus the first-order
#' approximation of the expectation of the adjusted Rand index.  The
#' approximation is based on \eqn{E(X/Y) \approx E(X)/E(Y)}.  See A. Fritsch and
#' K. Ickstadt (2009).} \item{\code{VI.lb}}{The lower bound of the
#' expectation of the variation of information loss, where the lower bound is
#' obtained from Jensen's inequality.  See S. Wade and Z. Ghahramani (2018).} }
#'
#' @param partitions An integer matrix of cluster labels, where each row is a
#'   partition given as cluster labels. Two items are in the same subset (i.e.,
#'   cluster) if their labels are equal.
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#'
#' @return A numeric vector of length equal to the number of rows of
#'   \code{partitions}, where each element gives the value of the loss function.
#'
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
#'
#' D. B. Dahl and M. A. Newton (2007), Multiple Hypothesis Testing by Clustering
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
#' @export
#' @examples
#' # Use 'parallel=FALSE' per CRAN rules for examples but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#'
#' binder(iris.clusterings[1:5,], probs)
#' adjRand(iris.clusterings[1:5,], probs)
#' VI.lb(iris.clusterings[1:5,], probs)
#'
binder <- function(partitions, psm) {
  expectedLoss(partitions, psm, 0L)
}

#' @export
#' @rdname binder
#'
adjRand <- function(partitions, psm) {
  expectedLoss(partitions, psm, 1L)
}

#' @export
#' @rdname binder
#'
VI.lb <- function(partitions, psm) {
  expectedLoss(partitions, psm, 2L)
}

#' @useDynLib salso .expected_loss
#'
expectedLoss <- function(partitions, psm, lossCode) {
  if ( ! ( isSymmetric(psm) && all(0 <= psm) && all(psm <= 1) && all(diag(psm)==1) ) ) {
    stop("'psm' should be symmetric with diagonal elements equal to 1 and off-diagonal elements in [0, 1].")
  }
  if ( ! is.matrix(partitions) ) {
    dim(partitions) <- c(1,length(partitions))
  }
  if ( ncol(partitions) != nrow(psm) ) {
    stop("The length of 'partitions' is not equal to the number of rows of 'psm'.")
  }
  .Call(.expected_loss, partitions, psm, lossCode)
}
