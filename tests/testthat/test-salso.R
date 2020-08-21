context("salso")

subset <- c(21,31,44,45,78,84,94,102,126)
draws.small <- draws[,subset]
psm.small <- psm[subset,subset]

test_that("SALSO minimizes binder.psm", {
  s <- salso::salso(psm.small, loss="binder", nRuns=100, maxZealousAttempts=0, probSequentialAllocation=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes binder.draws", {
  s <- salso::salso(draws.small, loss="binder", nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes binder.draws with unequal costs", {
  s <- salso::salso(draws.small, loss=salso::binder(a=1.5), nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss=salso::binder(a=1.5)))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes omARI", {
  s <- salso::salso(draws.small, loss="omARI", nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="omARI"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes omARI.approx", {
  s <- salso::salso(psm.small, loss="omARI.approx", nRuns=100, maxZealousAttempts=0, probSequentialAllocation=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="omARI.approx"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes VI", {
  s <- salso::salso(draws.small, loss="VI", nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="VI"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes VI with unequal costs", {
  s <- salso::salso(draws.small, loss=salso::VI(a=0.75), nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss=salso::VI(a=0.75)))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes VI.lb", {
  s <- salso::salso(psm.small, loss="VI.lb", nRuns=100, maxZealousAttempts=0, probSequentialAllocation=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="VI.lb"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes NVI", {
  s <- salso::salso(draws.small, loss="NVI", nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="NVI"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes ID", {
  s <- salso::salso(draws.small, loss="ID", nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="ID"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes NID", {
  s <- salso::salso(draws.small, loss="NID", nRuns=100)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="NID"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes binder.psm (nonparallel)", {
  s <- salso::salso(psm.small, loss="binder", nRuns=100, nCores=1, maxZealousAttempts=0, probSequentialAllocation=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(salso::VI(s, o1),0),0)))
})

test_that("SALSO minimizes binder.draws (nonparallel)", {
  s <- salso::salso(draws.small, loss="binder", nRuns=100, nCores=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="binder"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes omARI (nonparallel)", {
  s <- salso::salso(draws.small, loss="omARI", nRuns=100, nCores=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="omARI"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes omARI.approx (nonparallel)", {
  s <- salso::salso(psm.small, loss="omARI.approx", nRuns=100, nCores=1, maxZealousAttempts=0, probSequentialAllocation=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="omARI.approx"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes VI (nonparallel)", {
  s <- salso::salso(draws.small, loss="VI", nRuns=100, nCores=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="VI"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes VI.lb (nonparallel)", {
  s <- salso::salso(psm.small, loss="VI.lb", nRuns=100, nCores=1, maxZealousAttempts=0, probSequentialAllocation=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(psm.small, loss="VI.lb"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes NVI (nonparallel)", {
  s <- salso::salso(draws.small, loss="NVI", nRuns=100, nCores=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="NVI"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes ID (nonparallel)", {
  s <- salso::salso(draws.small, loss="ID", nRuns=100, nCores=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="ID"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

test_that("SALSO minimizes NID (nonparallel)", {
  s <- salso::salso(draws.small, loss="NID", nRuns=100, nCores=1)
  o1 <- as.vector(salso:::minimize.by.enumeration(draws.small, loss="NID"))
  expect_true(isTRUE(all.equal(salso::VI(s, o1),0)))
})

