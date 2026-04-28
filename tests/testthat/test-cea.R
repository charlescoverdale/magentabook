test_that("mb_cea computes cost per unit", {
  res <- mb_cea(cost = 1e6, effect = 250)
  expect_s3_class(res, "mb_cea")
  expect_equal(res$cost_per_unit, 4000)
})

test_that("mb_cea sums vector inputs", {
  res <- mb_cea(cost = c(500, 500), effect = c(100, 150))
  expect_equal(res$total_cost, 1000)
  expect_equal(res$total_effect, 250)
  expect_equal(res$cost_per_unit, 4)
})

test_that("mb_cea errors on zero effect", {
  expect_error(mb_cea(cost = 1000, effect = 0), "undefined")
})

test_that("mb_icer computes ICER and tags dominance", {
  # B more costly, more effective
  r1 <- mb_icer(1e6, 200, 1.5e6, 300)
  expect_equal(r1$icer, (1.5e6 - 1e6) / (300 - 200))
  expect_equal(r1$dominance, "b_more_costly_more_effective")

  # B dominates: cheaper and more effective
  r2 <- mb_icer(1e6, 200, 0.5e6, 300)
  expect_equal(r2$dominance, "b_dominates")

  # A dominates: B more costly and less effective
  r3 <- mb_icer(1e6, 300, 1.5e6, 200)
  expect_equal(r3$dominance, "a_dominates")

  # B less costly, less effective
  r4 <- mb_icer(1e6, 300, 0.5e6, 200)
  expect_equal(r4$dominance, "b_less_costly_less_effective")
})

test_that("mb_icer handles zero effect-difference edge case", {
  # No difference in effect, positive cost diff
  r <- mb_icer(1e6, 100, 1.5e6, 100)
  expect_true(is.infinite(r$icer))
  # No diff in either: NaN
  r0 <- mb_icer(1e6, 100, 1e6, 100)
  expect_true(is.nan(r0$icer))
})

test_that("mb_ceac probabilities are non-decreasing in WTP for positive effect", {
  set.seed(4)
  delta_cost   <- rnorm(2000, mean = 50000, sd = 10000)
  delta_effect <- rnorm(2000, mean = 2,     sd = 0.5)
  res <- mb_ceac(delta_cost, delta_effect, wtp_grid = seq(0, 100000, by = 10000))
  expect_s3_class(res, "mb_ceac")
  expect_true(all(diff(res$prob_cost_effective) >= -0.05))
})

test_that("mb_ceac probabilities are in [0,1]", {
  delta_cost <- runif(100, 0, 1000)
  delta_effect <- runif(100, 0.1, 1)
  res <- mb_ceac(delta_cost, delta_effect, wtp_grid = c(0, 500, 1000, 5000))
  expect_true(all(res$prob_cost_effective >= 0))
  expect_true(all(res$prob_cost_effective <= 1))
})

test_that("mb_ceac validates equal lengths", {
  expect_error(
    mb_ceac(c(1, 2), c(1, 2, 3), wtp_grid = c(1, 2)),
    "same length"
  )
})

test_that("mb_inb returns positive when ICER below WTP", {
  inb <- mb_inb(delta_cost = 50000, delta_effect = 2, wtp = 30000)
  # ICER = 25000 < WTP = 30000 → INB > 0
  expect_gt(inb, 0)
})

test_that("mb_inb returns negative when ICER above WTP", {
  inb <- mb_inb(delta_cost = 80000, delta_effect = 2, wtp = 30000)
  # ICER = 40000 > WTP = 30000 → INB < 0
  expect_lt(inb, 0)
})

test_that("mb_qaly without discounting returns persons * utility * years", {
  expect_equal(mb_qaly(0.8, persons = 100, years = 5), 400)
})

test_that("mb_qaly with utility vector sums utilities", {
  expect_equal(mb_qaly(c(0.5, 0.7, 0.9), persons = 50), 50 * (0.5 + 0.7 + 0.9))
})

test_that("mb_qaly with discounting reduces total", {
  undisc <- mb_qaly(0.8, persons = 100, years = 5)
  disc   <- mb_qaly(0.8, persons = 100, years = 5, discount_rate = 0.035)
  expect_lt(disc, undisc)
})

test_that("mb_qaly enforces utility in [0, 1]", {
  expect_error(mb_qaly(1.2), "between 0 and 1")
  expect_error(mb_qaly(-0.1), "between 0 and 1")
})

test_that("mb_qaly enforces discount_rate < 1", {
  expect_error(
    mb_qaly(0.8, years = 2, discount_rate = 1),
    "less than 1"
  )
})

test_that("mb_daly sums YLD and YLL", {
  expect_equal(mb_daly(yld = 2.5, yll = 8.0, persons = 100), 1050)
})

test_that("mb_daly handles vector inputs", {
  expect_equal(mb_daly(yld = c(1, 2), yll = c(3, 4), persons = 1), 10)
})

test_that("mb_daly errors on negative inputs", {
  expect_error(mb_daly(-1, 1), "non-negative")
  expect_error(mb_daly(1, -1), "non-negative")
})
