context("psm")

test_that("Computation of adjacency matrix", {
  s <- salso::psm(p1)
  dimnames(s) <- NULL
  o <- mcclust::cltoSim(p1)
  expect_true(isTRUE(all.equal(s,o)))
})

test_that("Computation of pairwise similarity matrix", {
  s <- salso::psm(draws)
  dimnames(s) <- NULL
  o <- mcclust::comp.psm(draws)
  expect_true(isTRUE(all.equal(s,o)))
})

