context("regressions")

test_that("normalized information losses handle zero-entropy partitions", {
  for (n_items in c(1L, 4L, 31L)) {
    one <- rep(1L, n_items)
    # Labels above one avoid the R interface treating a 1-by-1 matrix as a PSM.
    singleton <- seq_len(n_items) + 10L
    samples <- matrix(one, nrow = 3L, ncol = n_items, byrow = TRUE)
    for (loss in c("NVI", "NID")) {
      expect_equal(salso::partition.loss(samples, one, loss = loss), 0)
      expect_equal(salso::partition.loss(singleton, singleton, loss = loss), 0)
      if (n_items > 1L) {
        expect_equal(salso::partition.loss(one, singleton, loss = loss), 1)
        expect_equal(salso::partition.loss(singleton, one, loss = loss), 1)
      }
      for (sequential in c(0, 1)) {
        for (n_cores in c(1L, 2L)) {
          set.seed(7)
          estimate <- salso::salso(
            samples, loss = loss, nRuns = 4, nCores = n_cores,
            maxNClusters = 1, probSequentialAllocation = sequential,
            maxZealousAttempts = 0
          )
          expect_equal(as.integer(estimate), one)
          expect_equal(attr(estimate, "info")$expectedLoss, 0)
        }
      }
    }
  }
})

test_that("PSM losses accept absent draws and agree with draws-based input", {
  samples <- rbind(c(1L, 1L, 2L, 2L), c(1L, 2L, 1L, 2L), c(1L, 1L, 1L, 2L))
  similarity <- salso::psm(samples, nCores = 1)
  estimates <- rbind(c(1L, 1L, 2L, 2L), c(1L, 2L, 3L, 4L))
  for (loss in c("binder", "omARI.approx", "VI.lb")) {
    actual <- salso::partition.loss(similarity, estimates, loss = loss)
    expected <- salso::partition.loss(samples, estimates, loss = loss)
    expect_length(actual, nrow(estimates))
    expect_true(all(is.finite(actual)))
    expect_equal(actual, expected)
  }
})

test_that("native loss wrapper still rejects missing required draws", {
  native <- get(".expected_loss", envir = asNamespace("salso"))
  expect_error(
    .Call(native, matrix(c(1L, 1L, 2L, 2L), nrow = 1L), NULL, NULL, 4L, 1),
    "'draws' is expected to be a matrix.", fixed = TRUE
  )
})

test_that("native optimizer rejects an incomplete result", {
  native <- get(".minimize_by_salso", envir = asNamespace("salso"))
  samples <- matrix(c(1L, 1L, 2L, 2L), nrow = 1L)
  # The public R function rejects zero runs; exercise the native result guard.
  expect_error(
    .Call(native, samples, NULL, 4L, 1, 2L, 0L, Inf, 10L, 0L, 1, 0, 1L),
    "SALSO did not produce a complete clustering with a finite expected loss.",
    fixed = TRUE
  )
})
