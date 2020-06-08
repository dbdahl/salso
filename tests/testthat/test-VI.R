context("VI")

test_that("Computation of variation of information loss", {
  s <- salso::VI(p1, p2)
  o1 <- mcclust::vi.dist(p1, p2)
  x <- aricode::entropy(p1,p2)
  o2 <- log2(exp(2*x$UV - x$U - x$V))
  expect_true(isTRUE(all.equal(s,o1)))
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Computation of expectation of variation of information loss", {
  s1 <- salso::VI(subset, draws)
  s2 <- salso::partition.loss(subset, draws, loss="VI")
  o <- mcclust.ext::VI(subset, draws)
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

