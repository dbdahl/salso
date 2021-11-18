context("NVI")

test_that("Computation of normalized variation of information loss", {
  s <- salso::NVI(p1, p2)
  o2 <- aricode::NVI(p1, p2)
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Computation of expectation of normalized variation of information loss", {
  s1 <- salso::NVI(draws, subset)
  s2 <- salso::partition.loss(draws, subset, loss="NVI")
  o <- apply(subset, 1, function(x) mean(apply(draws, 1, function(y) aricode::NVI(x, y))))
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

