context("reports")

test_that("binder", {
  s <- salso::salso(psm, loss="binder", nPermutations=10)
  o <- salso::partition.loss(s$estimate, psm, loss="binder")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("binder2", {
  s <- salso::salso(draws, loss="binder2", nPermutations=10)
  o <- salso::partition.loss(s$estimate, psm, loss="binder")  # This should be changed to 'binder2'
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("omARI", {
  s <- salso::salso(draws, loss="omARI", nPermutations=10)
  o <- salso::partition.loss(s$estimate, draws, loss="omARI")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("omARI.approx", {
  s <- salso::salso(psm, loss="omARI.approx", nPermutations=10)
  o <- salso::partition.loss(s$estimate, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("VI", {
  s <- salso::salso(draws, loss="VI", nPermutations=10)
  o <- salso::partition.loss(s$estimate, draws, loss="VI")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("VI.lb", {
  s <- salso::salso(psm, loss="VI.lb", nPermutations=10)
  o <- salso::partition.loss(s$estimate, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("binder (nonparallel)", {
  s <- salso::salso(psm, loss="binder", nPermutations=10, parallel=FALSE)
  o <- salso::partition.loss(s$estimate, psm, loss="binder")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("binder2 (nonparallel)", {
  s <- salso::salso(draws, loss="binder2", nPermutations=10, parallel=FALSE)
  o <- salso::partition.loss(s$estimate, draws, loss="binder2")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("omARI (nonparallel)", {
  s <- salso::salso(draws, loss="omARI", nPermutations=10, parallel=FALSE)
  o <- salso::partition.loss(s$estimate, draws, loss="omARI")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("omARI.approx (nonparallel)", {
  s <- salso::salso(psm, loss="omARI.approx", nPermutations=10, parallel=FALSE)
  o <- salso::partition.loss(s$estimate, psm, loss="omARI.approx")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("VI (nonparallel)", {
  s <- salso::salso(draws, loss="VI", nPermutations=10, parallel=FALSE)
  o <- salso::partition.loss(s$estimate, draws, loss="VI")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})

test_that("VI.lb (nonparallel)", {
  s <- salso::salso(psm, loss="VI.lb", nPermutations=10, parallel=FALSE)
  o <- salso::partition.loss(s$estimate, psm, loss="VI.lb")
  expect_true(isTRUE(all.equal(s$expectedLoss,o)))
})




