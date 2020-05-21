context("VI.lb")

test_that("Computation of approximation of the expectation of one minus adjusted Rand index loss", {
  s <- salso::VI.lb(subset, psm)
  o <- mcclust.ext::VI.lb(subset, psm)
  expect_true(isTRUE(all.equal(s,o)))
})

