test_that("mb_its recovers level and slope changes on noiseless data", {
  time <- 1:48
  base <- 10 + 0.05 * time
  step <- ifelse(time >= 25, 2 + 0.1 * (time - 25), 0)
  y    <- base + step
  res  <- mb_its(y, time, intervention_time = 25)
  expect_s3_class(res, "mb_its")
  expect_equal(res$level_change, 2, tolerance = 1e-8)
  expect_equal(res$slope_change, 0.1, tolerance = 1e-8)
})

test_that("mb_its returns coefficients with names", {
  time <- 1:24
  y <- rnorm(24)
  res <- mb_its(y, time, intervention_time = 12)
  expect_named(res$coefficients,
               c("(Intercept)", "time", "post", "post_time"))
  expect_named(res$se,
               c("(Intercept)", "time", "post", "post_time"))
})

test_that("mb_its respects lag parameter", {
  time <- 1:48
  y <- rnorm(48)
  res_no_lag <- mb_its(y, time, intervention_time = 25)
  res_lag    <- mb_its(y, time, intervention_time = 25, lag = 3L)
  expect_equal(res_no_lag$n - res_lag$n, 3L)
})

test_that("mb_its errors on degenerate design", {
  time <- 1:10
  expect_error(
    mb_its(rep(1, 10), time, intervention_time = 100),
    "singular"
  )
})

test_that("mb_its counts pre and post observations", {
  time <- 1:30
  y <- rnorm(30)
  res <- mb_its(y, time, intervention_time = 15)
  expect_equal(res$n_pre + res$n_post, 30L)
  expect_equal(res$n_post, 16L)
})

test_that("mb_its validates lag is non-negative integer", {
  time <- 1:24
  y <- rnorm(24)
  expect_error(mb_its(y, time, intervention_time = 12, lag = -1), "non-negative")
  expect_error(mb_its(y, time, intervention_time = 12, lag = 1.5), "non-negative integer")
})
