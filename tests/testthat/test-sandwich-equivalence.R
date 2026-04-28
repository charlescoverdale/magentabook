# Cross-validation of mb_did_2x2 cluster-robust SEs against
# sandwich::vcovCL. Both should compute the CR1 estimator with
# Stata-style finite-sample correction (G/(G-1)) * (N-1)/(N-K).

skip_if_not_installed("sandwich")

test_that("mb_did_2x2 cluster-robust SE agrees with sandwich::vcovCL HC1", {
  set.seed(20260427)
  n <- 800
  treated <- rep(c(0, 1), each = n / 2)
  post    <- rep(c(0, 1), times = n / 2)
  cluster <- rep(seq_len(n / 8), each = 8)
  y <- 0.4 * treated * post + rnorm(n) +
       rep(rnorm(n / 8, sd = 0.3), each = 8)

  mb <- mb_did_2x2(y, treated, post, cluster = cluster)

  fit <- stats::lm(y ~ treated * post)
  vc  <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
  sw_se <- sqrt(diag(vc)[["treated:post"]])

  expect_equal(mb$se, unname(sw_se), tolerance = 1e-6)
})

test_that("mb_did_2x2 cluster-robust SE agrees with sandwich for unbalanced clusters", {
  set.seed(2)
  n_clusters <- 40
  cluster_sizes <- sample(8:24, n_clusters, replace = TRUE)
  cluster <- rep(seq_len(n_clusters), cluster_sizes)
  n <- length(cluster)
  treated <- as.integer(cluster %in% sample(seq_len(n_clusters), n_clusters / 2))
  post    <- as.integer(stats::runif(n) > 0.5)
  y <- 0.5 * treated * post + rnorm(n) +
       rep(rnorm(n_clusters, sd = 0.4), cluster_sizes)

  mb <- mb_did_2x2(y, treated, post, cluster = cluster)

  fit <- stats::lm(y ~ treated * post)
  vc  <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
  sw_se <- sqrt(diag(vc)[["treated:post"]])

  expect_equal(mb$se, unname(sw_se), tolerance = 1e-6)
})

test_that("mb_did_2x2 OLS SE agrees with conventional lm SE", {
  set.seed(3)
  n <- 200
  treated <- rep(c(0, 1), each = n / 2)
  post    <- rep(c(0, 1), times = n / 2)
  y       <- 0.3 * treated * post + rnorm(n)

  mb <- mb_did_2x2(y, treated, post)

  fit  <- stats::lm(y ~ treated * post)
  sumr <- summary(fit)
  ols_se <- sumr$coefficients["treated:post", "Std. Error"]

  expect_equal(mb$se, ols_se, tolerance = 1e-10)
})
