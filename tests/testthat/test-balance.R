test_that("mb_balance_table flags balanced and imbalanced numeric covariates", {
  set.seed(20260427)
  n <- 500
  treated <- rep(c(0, 1), each = n / 2)
  balanced     <- rnorm(n, mean = 50)
  imbalanced   <- rnorm(n, mean = 50 + 5 * treated)
  bal <- mb_balance_table(treated = treated,
                          balanced = balanced,
                          imbalanced = imbalanced,
                          threshold = 0.10)
  expect_s3_class(bal, "mb_balance_table")
  expect_equal(nrow(bal), 2L)
  expect_false(bal$imbalanced[bal$covariate == "balanced"])
  expect_true(bal$imbalanced[bal$covariate == "imbalanced"])
})

test_that("mb_balance_table SMD has expected sign and magnitude", {
  set.seed(1)
  n <- 1000
  treated <- rep(c(0, 1), each = n / 2)
  x <- rnorm(n, mean = 5 * treated, sd = 1)
  bal <- mb_balance_table(treated = treated, x = x)
  expect_gt(bal$smd, 4.5)
  expect_lt(bal$smd, 5.5)
})

test_that("mb_balance_table handles binary covariates", {
  set.seed(2)
  n <- 400
  treated <- rep(c(0, 1), each = n / 2)
  female  <- rbinom(n, 1, 0.5)
  bal <- mb_balance_table(treated = treated, female = female)
  expect_lt(abs(bal$smd), 0.30)  # likely balanced under independence
})

test_that("mb_balance_table decomposes factor covariates by level", {
  set.seed(3)
  n <- 300
  treated <- rep(c(0, 1), each = n / 2)
  region <- factor(sample(c("North", "South", "East"), n, replace = TRUE))
  bal <- mb_balance_table(treated = treated, region = region)
  # 3 levels, omit ref → 2 rows
  expect_equal(nrow(bal), 2L)
  expect_true(all(grepl("region:", bal$covariate)))
})

test_that("mb_balance_table validates inputs", {
  expect_error(mb_balance_table(treated = c(0, 1)), "Provide at least one")
  expect_error(mb_balance_table(treated = rep(0, 10), x = rnorm(10)),
               "both 0 and 1")
  expect_error(
    mb_balance_table(treated = c(0, 1, 0), x = rnorm(5)),
    "must have length"
  )
})

test_that("mb_balance_table p_value is in [0, 1]", {
  set.seed(4)
  n <- 200
  treated <- rep(c(0, 1), each = n / 2)
  x <- rnorm(n)
  y <- rnorm(n, mean = 1 * treated)
  bal <- mb_balance_table(treated = treated, x = x, y = y)
  expect_true(all(bal$p_value >= 0 & bal$p_value <= 1, na.rm = TRUE))
})

test_that("mb_balance_table threshold argument controls imbalance flag", {
  set.seed(5)
  n <- 1000
  treated <- rep(c(0, 1), each = n / 2)
  x <- rnorm(n, mean = 0.05 * treated)  # SMD ~0.05 (small)
  bal_strict <- mb_balance_table(treated = treated, x = x, threshold = 0.04)
  bal_loose  <- mb_balance_table(treated = treated, x = x, threshold = 0.10)
  expect_true(any(bal_strict$imbalanced))
  expect_false(any(bal_loose$imbalanced))
})

test_that("print method on mb_balance_table runs", {
  set.seed(6)
  n <- 200
  bal <- mb_balance_table(
    treated = rep(c(0, 1), each = n / 2),
    x = rnorm(n)
  )
  expect_invisible(print(bal))
})
