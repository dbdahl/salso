context("omARI")

test_that("Computation of one minus adjusted Rand index loss", {
  s <- salso::omARI(p1, p2)
  o <- 1 - mcclust::arandi(p1, p2)
  expect_true(isTRUE(all.equal(s,o)))
})

test_that("Computation of adjusted Rand index", {
  s <- salso::ARI(p1, p2)
  o1 <- mcclust::arandi(p1, p2)
  o2 <- aricode::ARI(p1, p2)
  expect_true(isTRUE(all.equal(s,o1)))
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Computation of expectation of one minus adjusted Rand index loss", {
  s1 <- salso::omARI(subset, draws)
  s2 <- salso::partition.loss(subset, draws, loss="omARI")
  o <- 1 - apply(subset, 1, function(x) mean(apply(draws, 1, function(y) mcclust::arandi(x, y))))
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

