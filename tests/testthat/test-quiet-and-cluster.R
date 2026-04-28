test_that("quiet argument propagates onto mb_did_2x2 / mb_its / mb_event_study", {
  set.seed(1)
  n <- 200
  treated <- rep(c(0, 1), each = n / 2)
  post    <- rep(c(0, 1), times = n / 2)
  y       <- rnorm(n)
  did_q  <- mb_did_2x2(y, treated, post, quiet = TRUE)
  did_nq <- mb_did_2x2(y, treated, post)
  expect_true(did_q$quiet)
  expect_false(did_nq$quiet)

  its_q <- mb_its(rnorm(24), 1:24, intervention_time = 12, quiet = TRUE)
  expect_true(its_q$quiet)

  panel <- expand.grid(unit = 1:20, time = 1:8)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- as.integer(panel$unit <= 10)
  ev_q <- mb_event_study(panel$y, panel$unit, panel$time,
                         treatment_time = 5, treated = panel$treated,
                         leads = 2, lags = 2, quiet = TRUE)
  expect_true(ev_q$quiet)
})

test_that("mb_event_study cluster argument toggles cluster_robust flag", {
  set.seed(2)
  panel <- expand.grid(unit = 1:30, time = 1:8)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- as.integer(panel$unit <= 15)
  ev_ols <- mb_event_study(panel$y, panel$unit, panel$time,
                           treatment_time = 5, treated = panel$treated,
                           leads = 2, lags = 2)
  ev_cr  <- mb_event_study(panel$y, panel$unit, panel$time,
                           treatment_time = 5, treated = panel$treated,
                           leads = 2, lags = 2,
                           cluster = panel$unit)
  expect_false(ev_ols$cluster_robust)
  expect_true(ev_cr$cluster_robust)
  # SEs should generally differ (not by exactly zero) once clustering is added
  expect_false(isTRUE(all.equal(ev_ols$se, ev_cr$se)))
})

test_that("mb_event_study cluster errors on length mismatch", {
  set.seed(3)
  panel <- expand.grid(unit = 1:30, time = 1:8)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- as.integer(panel$unit <= 15)
  expect_error(
    mb_event_study(panel$y, panel$unit, panel$time,
                   treatment_time = 5, treated = panel$treated,
                   leads = 2, lags = 2,
                   cluster = c(1, 2, 3)),
    "must have length"
  )
})

test_that("mb_event_study cluster errors with single cluster", {
  set.seed(4)
  panel <- expand.grid(unit = 1:30, time = 1:8)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- as.integer(panel$unit <= 15)
  expect_error(
    mb_event_study(panel$y, panel$unit, panel$time,
                   treatment_time = 5, treated = panel$treated,
                   leads = 2, lags = 2,
                   cluster = rep(1, nrow(panel))),
    "at least 2 clusters"
  )
})
