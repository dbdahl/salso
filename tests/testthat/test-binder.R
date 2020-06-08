context("binder")

test_that("Computation of Binder loss", {
  s <- salso::binder(p1, p2)
  o <- mcclust::binder(p1, salso::psm(p2)) / ( n^2 / 2 )
  expect_true(isTRUE(all.equal(s,o)))
})

test_that("Computation of Rand index", {
  s <- salso::RI(p1, p2)
  o1 <- mcclust::arandi(p1, p2, adjust=FALSE)
  o2 <- aricode::RI(p1, p2)
  expect_true(isTRUE(all.equal(s,o1)))
  expect_true(isTRUE(all.equal(s,o2)))
})

test_that("Relationship between Binder loss and Rand index", {
  p1 <- draws[1,]
  p2 <- draws[2,]
  a <- salso::binder(p1, p2)
  b <- ( 1 - salso::RI(p1, p2) ) * (n-1)/n
  expect_true(isTRUE(all.equal(a,b)))
})

test_that("Computation of expectation of Binder loss", {
  s1 <- salso::binder(subset, psm)
  s2 <- salso::partition.loss(subset, psm, loss="binder")
  s3 <- salso::partition.loss(subset, draws, loss="binder2")
  o <- mcclust::binder(subset, psm) / ( n^2 / 2 )
  expect_true(isTRUE(all.equal(s1,o)))
  expect_true(isTRUE(all.equal(s2,s1)))
  expect_true(isTRUE(all.equal(s3,s2)))
})


