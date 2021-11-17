context("VI")

test_that("Computation of variation of information loss using existing packages", {
  s <- salso::VI(p1, p2, a=1)
  o1 <- mcclust::vi.dist(p1, p2)
  x <- aricode::entropy(p1,p2)
  o2 <- log2(exp(2*x$UV - x$U - x$V))
  expect_true(isTRUE(all.equal(s,o1)))
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Computations with unequal weights using by-hand calculations", {
  byhand <- function(p1,p2,a,type=c("binder","binder2","VI")) {
    tab <- table(p1,p2)
    n <- length(p1)
    if ( type == "VI" ) {
      plogp <- function(p) ifelse(p==0,rep(0,length(p)),p*log2(p))
      a * sum(plogp(apply(tab,1,sum)/n)) + sum(plogp(apply(tab,2,sum)/n)) - (a+1)*sum(plogp(tab/n))
    } else if ( type == "binder" ) {
      a * sum((apply(tab,1,sum)/n)^2) + sum((apply(tab,2,sum)/n)^2) - (a+1)*sum((tab/n)^2)
    } else if ( type == "binder2" ) {
      psm1 <- salso::psm(p1)
      u1 <- psm1[upper.tri(psm1)]
      psm2 <- salso::psm(p2)
      u2 <- psm2[upper.tri(psm2)]
      2 * ( a * sum(u2[u1==1] != 1) + sum(u1[u2==1] != 1) ) / n^2
    } else stop("Unrecognized loss.")
  }
  # VI
  s <- salso::VI(p1, p2, a=0.5)
  o1 <- byhand(p1, p2, a=0.5, "VI")
  expect_true(isTRUE(all.equal(s,o1)))
  s <- salso::VI(p1, p2, a=4)
  o1 <- byhand(p1, p2, a=4, "VI")
  expect_true(isTRUE(all.equal(s,o1)))
  # binder
  s <- salso::binder(p1, p2, a=0.5)
  o1 <- byhand(p1, p2, a=0.5, "binder")
  expect_true(isTRUE(all.equal(s,o1)))
  s <- salso::binder(p1, p2, a=4)
  o1 <- byhand(p1, p2, a=4, "binder")
  expect_true(isTRUE(all.equal(s,o1)))
  # binder
  s <- salso::binder(p1, p2, a=0.5)
  o1 <- byhand(p1, p2, a=0.5, "binder2")
  expect_true(isTRUE(all.equal(s,o1)))
  s <- salso::binder(p1, p2, a=4)
  o1 <- byhand(p1, p2, a=4, "binder2")
  expect_true(isTRUE(all.equal(s,o1)))
})

test_that("Computation of expectation of variation of information loss", {
  s1 <- salso::VI(subset, draws, a=1)
  s2 <- salso::partition.loss(subset, draws, loss=salso::VI(a=1))
  o <- mcclust.ext::VI(subset, draws)
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

