context("omARI.approx")

test_that("Computation of approximation of the expectation of one minus adjusted Rand index loss", {
  s1 <- salso::omARI.approx(subset, psm)
  s2 <- salso::partition.loss(subset, draws, loss="omARI.approx")
  o <- 1 - mcclust::pear(subset, psm)
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

