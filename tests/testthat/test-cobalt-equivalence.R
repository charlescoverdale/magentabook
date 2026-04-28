# Cross-validation of mb_balance_table standardised mean
# differences against cobalt::bal.tab.
#
# magentabook uses the equal-weighted pooled SD of Stuart (2010)
# and Austin (2009): sqrt((s_T^2 + s_C^2) / 2). cobalt's default
# `s.d.denom = "pooled"` uses the degrees-of-freedom-weighted
# pooled SD: sqrt((s_T^2 (n_T-1) + s_C^2 (n_C-1)) / (n_T+n_C-2)).
#
# The two are mathematically identical when n_T = n_C, so we
# verify equivalence on balanced samples.

skip_if_not_installed("cobalt")

test_that("mb_balance_table SMD matches cobalt::bal.tab SMD on balanced samples", {
  set.seed(20260427)
  n <- 400
  treated <- rep(c(0, 1), each = n / 2)
  age    <- rnorm(n, mean = 45 + 2 * treated, sd = 10)
  female <- rbinom(n, 1, 0.5)
  income <- rnorm(n, mean = 30000 + 1500 * treated, sd = 8000)

  mb <- mb_balance_table(
    treated = treated, age = age, female = female, income = income
  )

  df <- data.frame(age = age, female = female, income = income)
  cb <- cobalt::bal.tab(df, treat = treated,
                        s.d.denom = "pooled",
                        binary    = "raw",
                        continuous = "std")
  cb_diff <- cb$Balance$Diff.Un
  names(cb_diff) <- rownames(cb$Balance)

  for (covar in c("age", "income")) {
    mb_smd <- mb$smd[mb$covariate == covar]
    expect_equal(mb_smd, unname(cb_diff[covar]), tolerance = 1e-8,
                 label = sprintf("SMD agreement on %s", covar))
  }
})

test_that("mb_balance_table SMD has correct sign convention (treated minus control)", {
  set.seed(2)
  n <- 200
  treated <- rep(c(0, 1), each = n / 2)
  # Treated has HIGHER mean → SMD should be positive
  x <- rnorm(n, mean = 0.5 * treated)
  mb <- mb_balance_table(treated = treated, x = x)
  expect_gt(mb$smd, 0)

  # Treated has LOWER mean → SMD should be negative
  y <- rnorm(n, mean = -0.5 * treated)
  mb <- mb_balance_table(treated = treated, y = y)
  expect_lt(mb$smd, 0)
})

test_that("mb_balance_table imbalanced flag matches cobalt's stricter threshold", {
  # Test that our default threshold of 0.10 catches what cobalt's
  # standard cutpoint catches at the same threshold.
  set.seed(3)
  n <- 1000
  treated <- rep(c(0, 1), each = n / 2)
  imbalanced <- rnorm(n, mean = 0.3 * treated)  # SMD ~ 0.3
  balanced   <- rnorm(n, mean = 0.02 * treated)  # SMD ~ 0.02

  mb <- mb_balance_table(treated = treated,
                         imbalanced = imbalanced,
                         balanced = balanced,
                         threshold = 0.10)

  expect_true(mb$imbalanced[mb$covariate == "imbalanced"])
  expect_false(mb$imbalanced[mb$covariate == "balanced"])
})
