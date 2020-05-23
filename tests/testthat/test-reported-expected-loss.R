context("reports")

test_that("binder", {
  s <- salso::salso(psm, loss="binder", batchSize=10)
  o <- salso::partition.loss(s$estimate, psm, loss="binder")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("omARI", {
  s <- salso::salso(draws, loss="omARI", batchSize=10)
  o <- salso::partition.loss(s$estimate, draws, loss="omARI")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("omARI.approx", {
  s <- salso::salso(psm, loss="omARI.approx", batchSize=10)
  o <- salso::partition.loss(s$estimate, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("VI", {
  s <- salso::salso(draws, loss="VI", batchSize=10)
  o <- salso::partition.loss(s$estimate, draws, loss="VI")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("VI.lb", {
  s <- salso::salso(psm, loss="VI.lb", batchSize=10)
  o <- salso::partition.loss(s$estimate, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})




