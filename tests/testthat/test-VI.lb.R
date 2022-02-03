context("VI.lb")

test_that("Computation of approximation of the expectation of one minus adjusted Rand index loss", {
  s1 <- salso::VI.lb(psm, subset)
  s2 <- salso::partition.loss(psm, subset, loss="VI.lb")
  o <- mcclust.ext::VI.lb(subset, psm)
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

