context("ID")

aricode__ID <- function(c1, c2) {
  H <- aricode::entropy(c1, c2)
  MI <- -H$UV + H$U + H$V
  log2(exp(max(H$U, H$V) - MI))
}

test_that("Computation of information distance loss", {
  s <- salso::ID(p1, p2)
  o2 <- aricode__ID(p1, p2)
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Computation of expectation of information distance loss", {
  s1 <- salso::ID(draws, subset)
  s2 <- salso::partition.loss(draws, subset, loss="ID")
  o <- apply(subset, 1, function(x) mean(apply(draws, 1, function(y) aricode__ID(x, y))))
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
  expect_true(isTRUE(all.equal(s2,s1)))
})

