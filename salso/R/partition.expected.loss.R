#' Compute the Expectation of a Partition Loss
#'
#' Based on the supplied pairwise similarity matrix,
#' \code{\link{partition.expected.loss}} computes the (approximation of the)
#' expectation of one of three partition losses for the supplied partitions.
#' Smaller values of the expected loss indicate a better partition estimate. The
#' functions \code{\link{binder}}, \code{\link{pear}}, and \code{\link{VI.lb}}
#' are convenience functions.  Note that:
#' \itemize{
#' \item \code{partition.expected.loss(partitions, psm, "binder") = binder(partitions, psm)}
#' \item \code{partition.expected.loss(partitions, psm, "pear") = 1 - pear(partitions, psm)}
#' \item \code{partition.expected.loss(partitions, psm, "VI.lb") = VI.lb(partitions, psm)}
#' }
#'
#' Three partition estimation criteria can be specified using the \code{loss}
#' argument: \describe{
#'
#' \item{\code{"binder"}}{Computes the expectation of the Binder loss. Whereas
#' high values of the Rand index \eqn{R} between \eqn{\pi*} and \eqn{\pi}
#' correspond to high concordance between these partitions, the Binder loss
#' \eqn{L} for a partition \eqn{\pi*} in estimating \eqn{\pi} is \eqn{L =
#' n*(n-1)*(1-R)/2}. See also Dahl (2006), Lau and Green (2007), Dahl and Newton
#' (2007)}
#'
#' \item{\code{"pear"}}{Computes the first-order approximation of the
#' expectation of the loss associated with the adjusted Rand index (Hubert and
#' Arabie, 1985).  Whereas high values of the adjusted Rand index between
#' \eqn{\pi*} and \eqn{\pi} correspond to high concordance between these
#' partitions, the loss associated with the adjusted Rand index for a partition
#' \eqn{\pi*} in estimating \eqn{\pi} is one minus the adjusted Rand index
#' between these partitions.  The adjusted Rand index involves a ratio and the
#' first-order approximation is based on \eqn{E(X/Y) \approx E(X)/E(Y)}.  See
#' Fritsch and Ickstadt (2009).}
#'
#' \item{\code{"VI.lb"}}{Computes the lower bound of the expectation of the
#' variation of information loss, where the lower bound is obtained from
#' Jensen's inequality.  See Meilă (2007) and Wade and Ghahramani (2018).}
#'
#' }
#'
#' @param partitions An integer matrix of cluster labels with \code{n} columns,
#'   where each row is a partition of \code{n} items given as cluster labels.
#'   Two items are in the same subset (i.e., cluster) if their labels are equal.
#' @param psm A pairwise similarity matrix, i.e., \code{n}-by-\code{n} symmetric
#'   matrix whose \code{(i,j)} element gives the (estimated) probability that
#'   items \code{i} and \code{j} are in the same subset (i.e., cluster) of a
#'   partition (i.e., clustering).
#' @param loss One of \code{"binder"}, \code{"pear"}, or \code{"VI.lb"}.  See
#'   the details below.
#'
#' @return A numeric vector of length equal to the number of rows of
#'   \code{partitions}, where each element gives the value of the expected loss.
#'
#' @seealso \code{\link{psm}}, \code{\link{salso}}, \code{\link{dlso}}
#
#' @references
#'
#' W. M. Rand (1971), Objective Criteria for the Evaluation of Clustering
#' Methods. \emph{Journal of the American Statistical Association}, \bold{66}:
#' 846–850.
#'
#' D. A. Binder (1978), Bayesian cluster analysis, \emph{Biometrika} \bold{65},
#' 31-38.
#'
#' L. Hubert and P. Arabie (1985), Comparing Partitions. \emph{Journal of
#' Classification}, \bold{2}, 193–218.
#'
#' D. B. Dahl (2006), Model-Based Clustering for Expression Data via a Dirichlet
#' Process Mixture Model, in \emph{Bayesian Inference for Gene Expression and
#' Proteomics}, Kim-Anh Do, Peter Müller, Marina Vannucci (Eds.), Cambridge
#' University Press.
#'
#' J. W. Lau and P. J. Green (2007), Bayesian model based clustering procedures,
#' \emph{Journal of Computational and Graphical Statistics} \bold{16}, 526-558.
#'
#' M. Meilă (2007), Comparing Clusterings - an Information Based Distance.
#' \emph{Journal of Multivariate Analysis}, \bold{98}: 873–895.
#'
#' D. B. Dahl and M. A. Newton (2007), Multiple Hypothesis Testing by Clustering
#' Treatment Effects, \emph{Journal of the American Statistical Association},
#' \bold{102}, 517-526.
#'
#' A. Fritsch and K. Ickstadt (2009), An improved criterion for clustering based
#' on the posterior similarity matrix, \emph{Bayesian Analysis}, \bold{4},
#' 367-391.
#'
#' S. Wade and Z. Ghahramani (2018), Bayesian cluster analysis: Point estimation
#' and credible balls. \emph{Bayesian Analysis}, \bold{13:2}, 559-626.
#'
#' @export
#' @examples
#' # Use 'parallel=FALSE' per CRAN rules for examples but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' partitions <- iris.clusterings[1:5,]
#'
#' partition.expected.loss(partitions, probs, loss="binder")
#' partition.expected.loss(partitions, probs, loss="pear")
#' partition.expected.loss(partitions, probs, loss="VI.lb")
#'
#' all.equal(partition.expected.loss(partitions, probs, "binder"), binder(partitions, probs))
#' all.equal(partition.expected.loss(partitions, probs, "pear"), 1 - pear(partitions, probs))
#' all.equal(partition.expected.loss(partitions, probs, "VI.lb"),   VI.lb(partitions, probs))
#'
partition.expected.loss <- function(partitions, psm, loss=c("binder", "pear", "VI.lb")[3]) {
  expected.loss(partitions, psm, lossCode(loss))
}

#' @export
#' @rdname partition.expected.loss
binder <- function(partitions, psm) {
  expected.loss(partitions, psm, lossCode("binder"))
}

#' @export
#' @rdname partition.expected.loss
pear <- function(partitions, psm) {
  1-expected.loss(partitions, psm, lossCode("pear"))
}

#' @export
#' @rdname partition.expected.loss
VI.lb <- function(partitions, psm) {
  expected.loss(partitions, psm, lossCode("VI.lb"))
}

#' @useDynLib salso .expected_loss
#'
expected.loss <- function(partitions, psm, lossCode) {
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
