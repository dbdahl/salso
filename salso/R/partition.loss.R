#' Compute Partition Loss or the Expectation of Partition Loss
#'
#' Given two partitions \eqn{\pi*} and \eqn{\pi}, these functions compute the
#' specified loss when using \eqn{\pi*} to estimate \eqn{\pi}.  Smaller loss
#' values indicate higher concordance between partitions.  These functions also
#' compute a Monte Carlo estimate of the expectation for the specified loss
#' based on samples or a pairwise similarity matrix.  This function also
#' supports computing approximations to the expectation of several losses.
#' Supported criteria are described below. Some criteria only require the
#' pairwise similarity matrix (as computed, for example, by \code{\link{psm}})
#' whereas others require samples from a partition distribution.  For those
#' criteria that only need the pairwise similarity matrix, posterior samples can
#' still be provided in the \code{x} argument and the pairwise similarity matrix
#' will automatically be computed as needed.
#'
#' The partition estimation criterion can be specified using the \code{loss}
#' argument: \describe{
#'
#' \item{\code{"binder"}}{Binder loss. Whereas high values of the Rand index
#' \eqn{R} between \eqn{\pi*} and \eqn{\pi} correspond to high concordance
#' between the partitions, the N-invariant Binder loss \eqn{L} for a partition
#' \eqn{\pi*} in estimating \eqn{\pi} is \eqn{L = (1-R)*(n-1)/n}.  This package
#' reports the N-invariant Binder loss and the original Binder loss equals the
#' N-invariant Binder loss multiplied by \eqn{n^2 / 2}. Only the pairwise
#' similarity matrix is required for "binder".  See also Dahl (2006), Lau and
#' Green (2007), Dahl and Newton (2007), Fritsch and Ickstadt (2009), and Wade
#' and Ghahramani (2018).}
#'
#' \item{\code{"omARI"}}{One Minus Adjusted Rand Index. Computes the expectation
#' of the one minus the adjusted Rand index (Hubert and Arabie, 1985).  Whereas
#' high values of the adjusted Rand index between \eqn{\pi*} and \eqn{\pi}
#' correspond to high concordance between the partitions, the loss associated
#' with the adjusted Rand index for a partition \eqn{\pi*} in estimating
#' \eqn{\pi} is one minus the adjusted Rand index between the partitions.
#' Samples from a partition distribution are required for "omARI".  See Fritsch
#' and Ickstadt (2009).}
#'
#' \item{\code{"omARI.approx"}}{Approximation of One Minus Adjusted Rand Index.
#' Computes the first-order approximation of the expectation of the one
#' minus the adjusted Rand index. The adjusted Rand index involves a ratio and
#' the first-order approximation of the expectation is based on \eqn{E(X/Y)
#' \approx E(X)/E(Y)}. Only the pairwise similarity matrix is required for
#' "omARI.approx". See Fritsch and Ickstadt (2009).}
#'
#' \item{\code{"VI"}}{Variation of Information. Computes the expectations of
#' variation of information loss.  Samples from a partition distribution are
#' required for "VI". See Meilă (2007), Wade and Ghahramani (2018), and Rastelli
#' and Friel (2018).}
#'
#' \item{\code{"VI.lb"}}{Lower Bound of the Variation of Information.  Computes
#' the lower bound of the expectation of the variation of information loss,
#' where the lower bound is obtained by Jensen's inequality.  Only the pairwise
#' similarity matrix is required for "VI.lb".  See Wade and Ghahramani (2018).}
#'
#' }
#'
#' The functions \code{\link{RI}} and \code{\link{ARI}} are convenience
#' functions. Note that:
#' \itemize{
#' \item \code{binder(p1, p2) = ( 1 - RI(p1, p2) )*(n-1)/n}
#' \item \code{omARI(p1, p2) = 1 - ARI(p1, p2)}
#' }
#'
#' @param partition1 An integer vector of cluster labels for \eqn{n} items. Two
#'   items are in the same subset (i.e., cluster) if their labels are equal.
#' @param partition2 An integer vector of cluster labels having the same length
#'   as \code{partition1}.
#' @param partitions An integer matrix of cluster labels with \eqn{n} columns,
#'   where each row is a partition of \eqn{n} items given as cluster labels. Two
#'   items are in the same subset (i.e., cluster) if their labels are equal.
#' @param x Either: 1. A \eqn{B}-by-\eqn{n} matrix, where each of the \eqn{B}
#'   rows represents a clustering of \eqn{n} items using cluster labels. (For
#'   clustering \eqn{b}, items \eqn{i} and \eqn{j} are in the same cluster if
#'   \code{x[b,i] == x[b,j]}.), or 2. A pairwise similarity matrix, i.e.,
#'   \eqn{n}-by-\eqn{n} symmetric matrix whose \eqn{(i,j)} element gives the
#'   (estimated) probability that items \eqn{i} and \eqn{j} are in the same
#'   subset (i.e., cluster) of a partition (i.e., clustering).
#' @param loss One of \code{"binder"}, \code{"omARI"}, \code{"omARI.approx"},
#'   \code{"VI"}, or \code{"VI.lb"}.
#'
#' @return A numeric vector.
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
#' R. Rastelli and N. Friel (2018), Optimal Bayesian estimators for latent
#' variable cluster models. \emph{Statistics and Computing}, \bold{28},
#' 1169-1186.
#'
#' @export
#' @examples
#' # For examples, use 'parallel=FALSE' per CRAN rules but, in practice, omit this.
#' probs <- psm(iris.clusterings, parallel=FALSE)
#' partitions <- iris.clusterings[1:5,]
#'
#' all.equal(partition.loss(partitions, probs, loss="binder"), binder(partitions, probs))
#'
#' all.equal(partition.loss(partitions, partitions, loss="omARI"),   omARI(partitions, partitions))
#' all.equal(partition.loss(partitions, probs, loss="omARI.approx"), omARI.approx(partitions, probs))
#'
#' all.equal(partition.loss(partitions, partitions, loss="VI"), VI(partitions, partitions))
#' all.equal(partition.loss(partitions, probs, loss="VI.lb"),   VI.lb(partitions, probs))
#'
#' p1 <- iris.clusterings[1,]
#' p2 <- iris.clusterings[2,]
#'
#' VI(p1, p2)
#' all.equal(binder(p1, p2), ( 1 - RI(p1, p2) ) * (length(p1)-1) / length(p1))
#' all.equal(omARI(p1, p2), 1 - ARI(p1, p2))
#'
partition.loss <- function(partitions, x, loss="VI.lb") {
  expected.loss(partitions, x, loss)
}

#' @export
#' @rdname partition.loss
binder <- function(partitions, x) {
  expected.loss(partitions, x, "binder")
}

#' @export
#' @rdname partition.loss
omARI <- function(partitions, x) {
  expected.loss(partitions, x, "omARI")
}

#' @export
#' @rdname partition.loss
omARI.approx <- function(partitions, x) {
  expected.loss(partitions, x, "omARI.approx")
}

#' @export
#' @rdname partition.loss
VI <- function(partitions, x) {
  expected.loss(partitions, x, "VI")
}

#' @export
#' @rdname partition.loss
VI.lb <- function(partitions, x) {
  expected.loss(partitions, x, "VI.lb")
}

#' @export
#' @rdname partition.loss
RI <- function(partition1, partition2) {
  1 - binder(partition1, psm(partition2)) * length(partition1) / (length(partition1)-1)
}

#' @export
#' @rdname partition.loss
ARI <- function(partition1, partition2) {
  1 - omARI(partition1, partition2)
}

#' @useDynLib salso .expected_loss
#'
expected.loss <- function(partitions, x, loss) {
  if ( ! is.matrix(partitions) ) dim(partitions) <- c(1,length(partitions))
  if ( ! is.matrix(x) ) dim(x) <- c(1,length(x))
  if ( ncol(x) != ncol(partitions) ) {
    stop("The number of items (i.e., number of columns) in 'partitions' and 'x' are not the same.")
  }
  y <- x2drawspsm(x, loss)
  .Call(.expected_loss, partitions, y$draws, y$psm, lossCode(loss))
}