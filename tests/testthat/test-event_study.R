test_that("mb_event_study returns expected event-time grid", {
  set.seed(3)
  n_units <- 50; n_periods <- 10; tt <- 6
  panel <- expand.grid(unit = 1:n_units, time = 1:n_periods)
  panel$treated <- as.integer(panel$unit <= 25)
  panel$y <- 0.1 * panel$time +
             0.5 * (panel$treated * (panel$time >= tt)) + rnorm(nrow(panel))
  res <- mb_event_study(panel$y, panel$unit, panel$time,
                        treatment_time = tt, treated = panel$treated,
                        leads = 3, lags = 3)
  expect_s3_class(res, "mb_event_study")
  expect_equal(sort(res$event_time), c(-3, -2, 0, 1, 2, 3))
  expect_equal(length(res$estimate), length(res$event_time))
  expect_equal(res$n_units, 50L)
  expect_equal(res$n_periods, 10L)
})

test_that("mb_event_study post-treatment leads are positive in noiseless DGP", {
  n_units <- 50; n_periods <- 10; tt <- 6
  panel <- expand.grid(unit = 1:n_units, time = 1:n_periods)
  panel$treated <- as.integer(panel$unit <= 25)
  panel$y <- 0.5 * (panel$treated * (panel$time >= tt))
  res <- mb_event_study(panel$y, panel$unit, panel$time,
                        treatment_time = tt, treated = panel$treated,
                        leads = 3, lags = 3)
  post_idx <- which(res$event_time >= 0)
  expect_true(all(res$estimate[post_idx] > 0))
})

test_that("mb_event_study errors when no controls are present", {
  panel <- expand.grid(unit = 1:5, time = 1:6)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- 1L
  expect_error(
    mb_event_study(panel$y, panel$unit, panel$time, treatment_time = 4,
                   treated = panel$treated),
    "never-treated"
  )
})

test_that("mb_event_study validates leads/lags as non-negative integers", {
  panel <- expand.grid(unit = 1:5, time = 1:6)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- as.integer(panel$unit <= 3)
  expect_error(
    mb_event_study(panel$y, panel$unit, panel$time, treatment_time = 4,
                   treated = panel$treated, leads = -1),
    "non-negative"
  )
  expect_error(
    mb_event_study(panel$y, panel$unit, panel$time, treatment_time = 4,
                   treated = panel$treated, leads = 1.5),
    "non-negative integer"
  )
})

test_that("mb_event_study errors on length mismatch", {
  expect_error(
    mb_event_study(rnorm(10), 1:5, 1:5, treatment_time = 3, treated = c(1, 0)),
    "same length"
  )
})
