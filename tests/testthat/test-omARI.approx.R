context("omARI.approx")

test_that("Computation of approximation of the expectation of one minus adjusted Rand index loss", {
  s <- salso::omARI.approx(subset, psm)
  o <- 1 - mcclust::pear(subset, psm)
  expect_true(isTRUE(all.equal(s,o)))
})

