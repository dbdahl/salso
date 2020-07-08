context("NID")

test_that("Computation of normalized variation of information loss", {
  s <- salso::NID(p1, p2)
  o2 <- aricode::NID(p1, p2)
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Computation of expectation of normalized variation of information loss", {
  s1 <- salso::NID(subset, draws)
  s2 <- salso::partition.loss(subset, draws, loss="NID")
  o <- apply(subset, 1, function(x) mean(apply(draws, 1, function(y) aricode::NID(x, y))))
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

