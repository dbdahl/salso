context("VI.lb")

test_that("Computation of approximation of the expectation of one minus adjusted Rand index loss", {
  s1 <- salso::VI.lb(subset, psm)
  s2 <- salso::partition.loss(subset, psm, loss="VI.lb")
  o <- mcclust.ext::VI.lb(subset, psm)
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

