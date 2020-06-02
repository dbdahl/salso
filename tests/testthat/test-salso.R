context("salso")

subset <- c(4, 23, 33, 76, 89, 90, 110, 130, 144)
draws.small <- draws[,subset]
psm.small <- psm[subset,subset]

test_that("SALSO minimizes binder", {
  s <- salso::salso(psm.small, loss="binder", seconds=0, batchSize=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes binder2", {
  s <- salso::salso(draws.small, loss="binder2", seconds=0, batchSize=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes omARI", {
  s <- salso::salso(draws.small, loss="omARI", seconds=0, batchSize=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="omARI"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes omARI.approx", {
  s <- salso::salso(psm.small, loss="omARI.approx", seconds=0, batchSize=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="omARI.approx"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes VI", {
  s <- salso::salso(draws.small, loss="VI", seconds=0, batchSize=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="VI"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes VI.lb", {
  s <- salso::salso(psm.small, loss="VI.lb", seconds=0, batchSize=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="VI.lb"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes binder (nonparallel)", {
  s <- salso::salso(psm.small, loss="binder", seconds=0, batchSize=100, parallel=FALSE)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(salso::VI(s$estimate, o1),0),0)))
})

test_that("SALSO minimizes binder2 (nonparallel)", {
  s <- salso::salso(draws.small, loss="binder2", seconds=0, batchSize=100, parallel=FALSE)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes omARI (nonparallel)", {
  s <- salso::salso(draws.small, loss="omARI", seconds=0, batchSize=100, parallel=FALSE)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="omARI"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes omARI.approx (nonparallel)", {
  s <- salso::salso(psm.small, loss="omARI.approx", seconds=0, batchSize=100, parallel=FALSE)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="omARI.approx"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes VI (nonparallel)", {
  s <- salso::salso(draws.small, loss="VI", seconds=0, batchSize=100, parallel=FALSE)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="VI"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

test_that("SALSO minimizes VI.lb (nonparallel)", {
  s <- salso::salso(psm.small, loss="VI.lb", seconds=0, batchSize=100, parallel=FALSE)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="VI.lb"))
  expect_true(isTRUE(all.equal(salso::VI(s$estimate, o1),0)))
})

