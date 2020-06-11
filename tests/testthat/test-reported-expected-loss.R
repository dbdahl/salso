context("reports")

test_that("binder", {
  s <- salso::salso(draws, loss="binder2", nRuns=10)
  o <- salso::partition.loss(s, psm, loss="binder")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("binder2", {
  s <- salso::salso(draws, loss="binder2", nRuns=10)
  o <- salso::partition.loss(s, draws, loss="binder2")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI", {
  s <- salso::salso(draws, loss="omARI", nRuns=10)
  o <- salso::partition.loss(s, draws, loss="omARI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI.approx", {
  s <- salso::salso(psm, loss="omARI.approx", nRuns=10)
  o <- salso::partition.loss(s, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI", {
  s <- salso::salso(draws, loss="VI", nRuns=10)
  o <- salso::partition.loss(s, draws, loss="VI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI.lb", {
  s <- salso::salso(psm, loss="VI.lb", nRuns=10)
  o <- salso::partition.loss(s, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("binder (nonparallel)", {
  s <- salso::salso(psm, loss="binder", nRuns=10, parallel=FALSE)
  o <- salso::partition.loss(s, psm, loss="binder")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("binder2 (nonparallel)", {
  s <- salso::salso(draws, loss="binder2", nRuns=10, parallel=FALSE)
  o <- salso::partition.loss(s, draws, loss="binder2")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI (nonparallel)", {
  s <- salso::salso(draws, loss="omARI", nRuns=10, parallel=FALSE)
  o <- salso::partition.loss(s, draws, loss="omARI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("omARI.approx (nonparallel)", {
  s <- salso::salso(psm, loss="omARI.approx", nRuns=10, parallel=FALSE)
  o <- salso::partition.loss(s, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI (nonparallel)", {
  s <- salso::salso(draws, loss="VI", nRuns=10, parallel=FALSE)
  o <- salso::partition.loss(s, draws, loss="VI")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})

test_that("VI.lb (nonparallel)", {
  s <- salso::salso(psm, loss="VI.lb", nRuns=10, parallel=FALSE)
  o <- salso::partition.loss(s, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(attr(s,"info")$expectedLoss,o)))
})




