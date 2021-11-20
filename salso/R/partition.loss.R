#' Compute Partition Loss or the Expectation of Partition Loss
#'
#' Given two partitions \eqn{\pi*} and \eqn{\pi}, these functions compute the
#' specified loss when using \eqn{\pi*} to estimate \eqn{\pi}. Smaller loss
#' values indicate higher concordance between partitions. These functions also
#' compute a Monte Carlo estimate of the expectation for the specified loss
#' based on samples or a pairwise similarity matrix. This function also supports
#' computing approximations to the expectation of several losses. Supported
#' criteria are described below. Some criteria only require the pairwise
#' similarity matrix (as computed, for example, by \code{\link{psm}}) whereas
#' others require samples from a partition distribution. For those criteria that
#' only need the pairwise similarity matrix, posterior samples can still be
#' provided in the \code{x} argument and the pairwise similarity matrix will
#' automatically be computed as needed.
#'
#' The partition estimation criterion can be specified using the \code{loss}
#' argument, which is either a string or a result of calling the associated
#' functions. These losses are described below: \describe{
#'
#' \item{\code{"binder"}}{Binder. Whereas high values of the Rand index \eqn{R}
#' between \eqn{\pi*} and \eqn{\pi} correspond to high concordance between the
#' partitions, the N-invariant Binder loss \eqn{L} for a partition \eqn{\pi*} in
#' estimating \eqn{\pi} is \eqn{L = (1-R)*(n-1)/n}, meaning that low values
#' correspond to high concordance between the partitions. This package reports
#' the N-invariant Binder loss. The original Binder loss equals the N-invariant
#' Binder loss multiplied by \eqn{n^2 / 2}. Only the pairwise similarity matrix
#' is required for this loss, but samples can be provided. As originally
#' discussed by Binder (1978), two mistakes are possible: 1. Placing two items
#' in separate clusters when in truth they belong to the same cluster, and 2.
#' Placing two items in the same cluster when in truth they belong to separate
#' clusters. The default cost of the first mistake is one, but can be specified
#' with the argument \code{a} in [0,2]. Without loss of generality, the cost of
#' the second mistake is fixed at \code{2-a}. For a discussion of general
#' weights, see Dahl, Johnson, and Müller (2021). For a discussion of the equal
#' weights case, see also Dahl (2006), Lau and Green (2007), Dahl and Newton
#' (2007), Fritsch and Ickstadt (2009), and Wade and Ghahramani (2018).}
#'
#' \item{\code{"omARI"}}{One Minus Adjusted Rand Index. Computes the expectation
#' of one minus the adjusted Rand index (Hubert and Arabie, 1985). Whereas high
#' values of the adjusted Rand index between \eqn{\pi*} and \eqn{\pi} correspond
#' to high concordance between the partitions, the loss associated with the
#' adjusted Rand index for a partition \eqn{\pi*} in estimating \eqn{\pi} is one
#' minus the adjusted Rand index between the partitions, meaning that low values
#' correspond to high concordance between the partitions. Samples from a
#' partition distribution are required for this loss. See Fritsch and Ickstadt
#' (2009).}
#'
#' \item{\code{"omARI.approx"}}{Approximation of One Minus Adjusted Rand Index.
#' Computes the first-order approximation of the expectation of one minus the
#' adjusted Rand index. The adjusted Rand index involves a ratio and the
#' first-order approximation of the expectation is based on \eqn{E(X/Y) \approx
#' E(X)/E(Y)}. Only the pairwise similarity matrix is required for this
#' criterion, but samples can be provided. See Fritsch and Ickstadt (2009).}
#'
#' \item{\code{"VI"}}{Variation of Information. Computes the expectations of the
#' (generalized) variation of information loss. Samples from a partition
#' distribution are required for this loss. See Meilă (2007), Wade and
#' Ghahramani (2018), and Rastelli and Friel (2018). The original variation of
#' information of Meilă (2007) has been extended to the generalized variation of
#' information of Dahl, Johnson, and  Müller (2021) to allow for unequal
#' weighting of two possible mistakes: 1. Placing two items in separate clusters
#' when in truth they belong to the same cluster, and 2. Placing two items in
#' the same cluster when in truth they belong to separate clusters.  The value
#' \code{a} controls the cost of the first mistake and defaults to one, but can
#' be specified with the argument \code{a} in [0,2]. Without loss of generality,
#' the cost of the second mistake is controlled by \code{2-a}. See Dahl,
#' Johnson, Müller (2021).}
#'
#' \item{\code{"VI.lb"}}{Lower Bound of the Variation of Information. Computes
#' the lower bound of the expectation of the variation of information loss,
#' where the lower bound is obtained by Jensen's inequality. Only the pairwise
#' similarity matrix is required for this criterion, but samples can be
#' provided. See Wade and Ghahramani (2018).}
#'
#' \item{\code{"NVI"}}{Normalized Variation of Information. Computes the
#' expectation of the normalized variation of information loss. Samples from a
#' partition distribution are required for this loss. See Vinh, Epps, and Bailey
#' (2010) and Rastelli and Friel (2018).}
#'
#' \item{\code{"ID"}}{Information Distance. Computes the expectation of the
#' information distance (\eqn{D_{max}}) loss. Samples from a partition
#' distribution are required for this loss. See Vinh, Epps, and Bailey (2010).}
#'
#' \item{\code{"NID"}}{Normalized Information Distance. Computes the expectation
#' of the normalized information distance loss. Samples from a partition
#' distribution are required for this loss. See Vinh, Epps, and Bailey (2010)
#' and Rastelli and Friel (2018).}
#'
#' }
#'
#' The functions \code{\link{RI}} and \code{\link{ARI}} are convenience
#' functions. Note that:
#' \itemize{
#' \item \code{binder(p1, p2, a=1) = ( 1 - RI(p1, p2) )*(n-1)/n}
#' \item \code{omARI(p1, p2) = 1 - ARI(p1, p2)}
#' }
#'
#' @param truth An integer vector of cluster labels for \eqn{n} items
#'   representing the true clustering. Two items are in the same cluster if
#'   their labels are equal. Or, a matrix of \eqn{n} columns where each row is a
#'   clustering.
#' @param estimate An integer vector of cluster labels having the same length as
#'   \code{truth} representing the estimated clustering. Or, a matrix of
#'   \eqn{n} columns where each row is a clustering.
#' @param loss The loss function to use, as indicated by \code{"binder"},
#'   \code{"omARI"}, \code{"VI"}, \code{"NVI"}, \code{"ID"}, \code{"NID"}, or
#'   the result of calling a function with these names. Also supported are
#'   \code{"binder.psm"}, \code{"VI.lb"}, \code{"omARI.approx"}, or the result
#'   of calling a function with these names, in which case \code{x} above can
#'   optionally be a pairwise similarity matrix, i.e., \eqn{n}-by-\eqn{n}
#'   symmetric matrix whose \eqn{(i,j)} element gives the (estimated)
#'   probability that items \eqn{i} and \eqn{j} are in the same subset (i.e.,
#'   cluster) of a partition (i.e., clustering).
#' @param a (Only used for Binder and VI loss) The argument \code{a} is either:
#'   i. a nonnegative scalar in [0,2] giving (for Binder loss) the cost of
#'   placing two items in separate clusters when in truth they belong to the
#'   same cluster, ii. \code{NULL}, in which case \code{a} that maximizes the
#'   expected loss is found, and iii. a list containing the desired number of
#'   clusters (\code{"nClusters"}) when searching for \code{a} that yields this
#'   number of clusters. In all but the first case, one may want to modifying
#'   \code{maxSize} in the \code{\link{salso}} function. To increase the
#'   probability of hitting exactly the desired number of clusters, the
#'   \code{nRuns} in the \code{\link{salso}} function may need to be increased.
#'   Without loss of generality, the cost (under Binder loss) of placing two
#'   items in the same cluster when in truth they belong to separate clusters is
#'   fixed \code{2-a}. For VI, \code{a} has a similar interpretation, although is
#'   not a unit cost. See Dahl, Johnson, Müller (2021).
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
#' N. X. Vinh, J. Epps, and J. Bailey (2010), Information Theoretic Measures for
#' Clusterings Comparison: Variants, Properties, Normalization and Correction
#' for Chance, \emph{Journal of Machine Learning Research}, \bold{11},
#' 2837-2854.
#'
#' S. Wade and Z. Ghahramani (2018), Bayesian cluster analysis: Point estimation
#' and credible balls. \emph{Bayesian Analysis}, \bold{13:2}, 559-626.
#'
#' R. Rastelli and N. Friel (2018), Optimal Bayesian estimators for latent
#' variable cluster models. \emph{Statistics and Computing}, \bold{28},
#' 1169-1186.
#'
#' D. B. Dahl, D. J. Johnson, and P. Müller (2021), Search Algorithms and Loss
#' Functions for Bayesian Clustering, <arXiv:2105.04451>.
#'
#' @export
#' @examples
#' # For examples, use 'nCores=1' per CRAN rules, but in practice omit this.
#' data(iris.clusterings)
#' partitions <- iris.clusterings[1:5,]
#'
#' all.equal(partition.loss(partitions, partitions, loss=binder(a=1.4)),
#'           binder(partitions, partitions, a=1.4))
#' all.equal(partition.loss(partitions, partitions, loss=omARI()),
#'           omARI(partitions, partitions))
#' all.equal(partition.loss(partitions, partitions, loss=VI(a=0.8)),
#'           VI(partitions, partitions, a=0.8))
#'
#' truth <- iris.clusterings[1,]
#' estimate <- iris.clusterings[2,]
#'
#' VI(truth, estimate, a=1.0)
#' n <- length(truth)
#' all.equal(binder(truth, estimate), ( 1 - RI(truth, estimate) ) * (n-1) / n)
#' all.equal(omARI(truth, estimate), 1 - ARI(truth, estimate))
#'
partition.loss <- function(truth, estimate, loss=VI()) {
  expected.loss(truth, estimate, loss)
}

checkAokay <- function(a) {
  if ( is.vector(a) && length(a) == 1 && is.numeric(a) && 0.0 <= a && a <= 2.0 ) {
      return()
  } else if ( is.null(a) ) {
      return()
  } else if ( is.list(a) && all(c("nClusters") %in% names(a)) &&
              is.vector(a$nClusters) && length(a$nClusters) == 1 && is.numeric(a$nClusters) && a$nClusters >= 1 ) {
      return()
  } else {
     stop("'a' should be in a scalar in [0,2].")
  }
}

#' @export
#' @rdname partition.loss
binder <- function(truth, estimate, a=1) {
  if ( missing(truth) && missing(estimate) ) {
    checkAokay(a)
    structure(list(loss="binder", a=a), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall(a=a))
  }
}

#' @export
#' @rdname partition.loss
RI <- function(truth, estimate) {
  1 - binder(truth, estimate) * length(truth) / (length(truth)-1)
}

#' @export
#' @rdname partition.loss
omARI <- function(truth, estimate) {
  if ( missing(truth) && missing(estimate) ) {
    structure(list(loss="omARI"), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall())
  }
}

#' @export
#' @rdname partition.loss
omARI.approx <- function(truth, estimate) {
  if ( missing(truth) && missing(estimate) ) {
    structure(list(loss="omARI.approx"), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall())
  }
}

#' @export
#' @rdname partition.loss
ARI <- function(truth, estimate) {
  1 - omARI(truth, estimate)
}

#' @export
#' @rdname partition.loss
VI <- function(truth, estimate, a=1) {
  if ( missing(truth) && missing(estimate) ) {
    checkAokay(a)
    structure(list(loss="VI", a=a), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall(a=a))
  }
}

#' @export
#' @rdname partition.loss
VI.lb <- function(truth, estimate) {
  if ( missing(truth) && missing(estimate) ) {
    structure(list(loss="VI.lb"), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall())
  }
}

#' @export
#' @rdname partition.loss
NVI <- function(truth, estimate) {
  if ( missing(truth) && missing(estimate) ) {
    structure(list(loss="NVI"), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall())
  }
}

#' @export
#' @rdname partition.loss
ID <- function(truth, estimate) {
  if ( missing(truth) && missing(estimate) ) {
    structure(list(loss="ID"), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall())
  }
}

#' @export
#' @rdname partition.loss
NID <- function(truth, estimate) {
  if ( missing(truth) && missing(estimate) ) {
    structure(list(loss="NID"), class="salso.loss")
  } else {
    expected.loss(truth, estimate, Recall())
  }
}

expected.loss <- function(truth, estimate, loss) {
  truth <- unclass(truth)
  estimate <- unclass(estimate)
  if ( ! is.matrix(truth) ) dim(truth) <- c(1,length(truth))
  if ( ! is.matrix(estimate) ) dim(estimate) <- c(1,length(estimate))
  if ( nrow(estimate) == 0 ) return(numeric())
  if ( ncol(truth) != ncol(estimate) ) {
    stop("The number of items (i.e., number of columns) in 'truth' and 'estimate' are not the same.")
  }
  if ( ncol(truth) == 0 ) return(rep(NA, nrow(estimate)))
  z <- x2drawspsm(truth, loss)
  if ( is.list(z$a) ) stop("'a' must be explicitly provided when computing the expected loss.")
  .Call(.expected_loss, estimate, z$draws, z$psm, z$lossCode, z$a)
}
