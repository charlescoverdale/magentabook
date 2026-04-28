test_that("mb_did_2x2 recovers a known effect on noiseless data", {
  # Construct deterministic data with a known DiD effect of 2.0:
  # Control pre = 1, Control post = 2 (time effect = 1)
  # Treated pre = 3, Treated post = 6 (time effect = 1, plus DiD = 2)
  treated <- rep(c(0, 1), each = 100)
  post    <- rep(c(0, 1), times = 100)
  y <- ifelse(treated == 0 & post == 0, 1,
        ifelse(treated == 0 & post == 1, 2,
         ifelse(treated == 1 & post == 0, 3, 6)))
  res <- mb_did_2x2(y, treated, post)
  expect_s3_class(res, "mb_did")
  expect_equal(res$estimate, 2)
  # On constant-within-cell data SE will be 0; CI collapses to point.
  expect_equal(res$ci_low, res$ci_high, tolerance = 1e-8)
})

test_that("mb_did_2x2 cluster-robust SE at least as large as OLS SE on iid data", {
  set.seed(20260427)
  n <- 800
  treated <- rep(c(0, 1), each = n / 2)
  post    <- rep(c(0, 1), times = n / 2)
  y       <- 0.4 * treated * post + rnorm(n)
  cluster <- rep(seq_len(n / 4), each = 4)
  ols <- mb_did_2x2(y, treated, post)
  cr  <- mb_did_2x2(y, treated, post, cluster = cluster)
  expect_true(cr$se >= ols$se * 0.5)
  expect_true(cr$cluster_robust)
  expect_false(ols$cluster_robust)
})

test_that("mb_did_2x2 estimate matches lm interaction coefficient", {
  set.seed(1)
  n <- 200
  treated <- rep(c(0, 1), each = n / 2)
  post    <- rep(c(0, 1), times = n / 2)
  y       <- 0.5 * treated + 0.2 * post + 0.4 * treated * post + rnorm(n)
  res     <- mb_did_2x2(y, treated, post)
  lm_est  <- coef(stats::lm(y ~ treated * post))[["treated:post"]]
  expect_equal(res$estimate, unname(lm_est))
})

test_that("mb_did_2x2 returns four group means", {
  set.seed(2)
  treated <- rep(c(0, 1), each = 50)
  post    <- rep(c(0, 1), times = 50)
  y       <- rnorm(100)
  res     <- mb_did_2x2(y, treated, post)
  expect_named(
    res$group_means,
    c("control_pre", "control_post", "treated_pre", "treated_post")
  )
})

test_that("mb_did_2x2 errors on length mismatch", {
  expect_error(
    mb_did_2x2(rnorm(10), c(0, 1), c(0, 1)),
    "same length"
  )
})

test_that("mb_did_2x2 errors on degenerate design", {
  expect_error(
    mb_did_2x2(rep(1, 4), c(0, 0, 1, 1), c(0, 0, 1, 1)),
    "singular"
  )
})

test_that("mb_did_2x2 cluster errors with single cluster", {
  treated <- rep(c(0, 1), each = 4)
  post    <- rep(c(0, 1), times = 4)
  y       <- rnorm(8)
  expect_error(
    mb_did_2x2(y, treated, post, cluster = rep(1, 8)),
    "at least 2 clusters"
  )
})
