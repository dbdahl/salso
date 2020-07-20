context("reports")

test_that("binder.psm", {
  s <- salso::salso(psm, loss="binder", nRuns=10, maxZealousAttempts=0)
  o <- salso::partition.loss(s, psm, loss="binder")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("binder.draws", {
  s <- salso::salso(draws, loss="binder", nRuns=10)
  o <- salso::partition.loss(s, draws, loss="binder")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI", {
  s <- salso::salso(draws, loss="omARI", nRuns=10)
  o <- salso::partition.loss(s, draws, loss="omARI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI.approx", {
  s <- salso::salso(psm, loss="omARI.approx", nRuns=10, maxZealousAttempts=0)
  o <- salso::partition.loss(s, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI", {
  s <- salso::salso(draws, loss="VI", nRuns=10)
  o <- salso::partition.loss(s, draws, loss="VI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI.lb", {
  s <- salso::salso(psm, loss="VI.lb", nRuns=10, maxZealousAttempts=0)
  o <- salso::partition.loss(s, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("NVI", {
  s <- salso::salso(draws, loss="NVI", nRuns=10, maxZealousAttempts=30)
  o <- salso::partition.loss(s, draws, loss="NVI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("ID", {
  s <- salso::salso(draws, loss="ID", nRuns=10, maxZealousAttempts=30)
  o <- salso::partition.loss(s, draws, loss="ID")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("NID", {
  s <- salso::salso(draws, loss="NID", nRuns=10, maxZealousAttempts=30)
  o <- salso::partition.loss(s, draws, loss="NID")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("binder.psm (nonparallel)", {
  s <- salso::salso(psm, loss="binder", nRuns=10, nCores=1, maxZealousAttempts=0)
  o <- salso::partition.loss(s, psm, loss="binder")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("binder.draws (nonparallel)", {
  s <- salso::salso(draws, loss="binder", nRuns=10, nCores=1)
  o <- salso::partition.loss(s, draws, loss="binder")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI (nonparallel)", {
  s <- salso::salso(draws, loss="omARI", nRuns=10, nCores=1)
  o <- salso::partition.loss(s, draws, loss="omARI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI.approx (nonparallel)", {
  s <- salso::salso(psm, loss="omARI.approx", nRuns=10, nCores=1, maxZealousAttempts=0)
  o <- salso::partition.loss(s, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI (nonparallel)", {
  s <- salso::salso(draws, loss="VI", nRuns=10, nCores=1)
  o <- salso::partition.loss(s, draws, loss="VI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI.lb (nonparallel)", {
  s <- salso::salso(psm, loss="VI.lb", nRuns=10, nCores=1, maxZealousAttempts=0)
  o <- salso::partition.loss(s, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("NVI (nonparallel)", {
  s <- salso::salso(draws, loss="NVI", nRuns=10, maxZealousAttempts=30, nCores=1)
  o <- salso::partition.loss(s, draws, loss="NVI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("ID (nonparallel)", {
  s <- salso::salso(draws, loss="ID", nRuns=10, maxZealousAttempts=30, nCores=1)
  o <- salso::partition.loss(s, draws, loss="ID")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("NID (nonparallel)", {
  s <- salso::salso(draws, loss="NID", nRuns=10, maxZealousAttempts=30, nCores=1)
  o <- salso::partition.loss(s, draws, loss="NID")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

