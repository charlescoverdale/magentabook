test_that("mb_power matches textbook two-sample power for d = 0.5", {
  # Cohen (1988): for d = 0.5, n = 64 per arm, alpha = 0.05 two-sided,
  # power approx 0.80 (large-sample normal approximation gives ~0.80).
  p <- mb_power(n_per_group = 64, effect_size = 0.5, alpha = 0.05)
  expect_gt(p, 0.78)
  expect_lt(p, 0.82)
})

test_that("mb_power increases monotonically in n", {
  ps <- vapply(c(50, 100, 200, 400, 800), function(n) {
    mb_power(n_per_group = n, effect_size = 0.3)
  }, numeric(1))
  expect_true(all(diff(ps) > 0))
})

test_that("mb_power increases monotonically in effect size", {
  es <- seq(0.1, 0.8, by = 0.1)
  ps <- vapply(es, function(d) mb_power(200, d), numeric(1))
  expect_true(all(diff(ps) > 0))
})

test_that("mb_power one-sided > two-sided for same alpha", {
  expect_gt(
    mb_power(200, 0.3, sides = 1L),
    mb_power(200, 0.3, sides = 2L)
  )
})

test_that("mb_power proportions agrees with arcsine", {
  p1 <- 0.40; p2 <- 0.50; n <- 500
  h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  via_h  <- mb_power(n, effect_size = h, type = "proportion")
  via_pp <- mb_power(n, type = "proportion", p1 = p1, p2 = p2)
  expect_equal(via_h, via_pp)
})

test_that("mb_power validates inputs", {
  expect_error(mb_power(-10, 0.3), "positive")
  expect_error(mb_power(100, 0.3, alpha = -0.1), "between")
  expect_error(mb_power(100, 0.3, sides = 3), "1 or 2")
  expect_error(mb_power(100), "Provide")
  expect_error(mb_power(100, type = "proportion", p1 = 0.5),
               "Provide")
})

test_that("mb_sample_size inverts mb_power", {
  d <- 0.3; power <- 0.8
  n <- mb_sample_size(effect_size = d, power = power)
  realised <- mb_power(n, d)
  expect_gte(realised, power - 0.01)
  expect_lte(realised, power + 0.05)
})

test_that("mb_sample_size for d = 0.5 returns ~64", {
  expect_equal(mb_sample_size(effect_size = 0.5, power = 0.8), 63)
})

test_that("mb_sample_size errors on zero effect", {
  expect_error(mb_sample_size(effect_size = 0), "infinite")
})

test_that("mb_sample_size proportions = arcsine path", {
  via_pp <- mb_sample_size(type = "proportion", p1 = 0.4, p2 = 0.5)
  h      <- 2 * asin(sqrt(0.4)) - 2 * asin(sqrt(0.5))
  via_h  <- mb_sample_size(effect_size = h, type = "proportion")
  expect_equal(via_pp, via_h)
})

test_that("mb_mde inverts mb_sample_size on d", {
  mde <- mb_mde(n_per_group = 200, power = 0.8)
  realised_n <- mb_sample_size(effect_size = mde, power = 0.8)
  expect_lte(abs(realised_n - 200), 5)
})

test_that("mb_mde decreases in n", {
  mde_small <- mb_mde(n_per_group = 50)
  mde_big   <- mb_mde(n_per_group = 500)
  expect_lt(mde_big, mde_small)
})

test_that("mb_mde proportion with baseline returns proportion-point diff", {
  mde <- mb_mde(n_per_group = 500, power = 0.8, type = "proportion",
                baseline = 0.4)
  expect_gt(mde, 0)
  expect_lt(mde, 0.2)
})

test_that("mb_cluster_design DEFF formula", {
  res <- mb_cluster_design(individuals_per_cluster = 30, icc = 0.05)
  # 1 + (30 - 1) * 0.05 = 1 + 1.45 = 2.45
  expect_equal(res$deff, 2.45)
})

test_that("mb_cluster_design returns effective N when n_clusters supplied", {
  res <- mb_cluster_design(individuals_per_cluster = 30, icc = 0.05,
                           n_clusters = 20)
  expect_equal(res$n_total_per_arm, 600)
  expect_equal(res$n_effective_per_arm, 600 / 2.45)
})

test_that("mb_cluster_design DEFF = 1 when icc = 0", {
  res <- mb_cluster_design(individuals_per_cluster = 30, icc = 0)
  expect_equal(res$deff, 1)
})

test_that("mb_cluster_design DEFF approaches m when icc = 1", {
  res <- mb_cluster_design(individuals_per_cluster = 30, icc = 1)
  expect_equal(res$deff, 30)
})

test_that("mb_cluster_design validates inputs", {
  expect_error(mb_cluster_design(0, 0.05), "positive")
  expect_error(mb_cluster_design(30, 1.5), "between 0 and 1")
})

test_that("mb_stepped_wedge returns a list with deff and total N", {
  res <- mb_stepped_wedge(steps = 5, clusters_per_step = 4,
                          individuals_per_cluster = 20, icc = 0.05)
  expect_named(res, c("deff_cluster", "correction_factor", "deff_sw",
                      "formula", "n_total"))
  expect_equal(res$n_total, 400)
  expect_equal(round(res$correction_factor, 6),
               round(3 * 0.95 / (10 * (1 - 1/25)), 6))
})

test_that("mb_stepped_wedge requires steps >= 2", {
  expect_error(
    mb_stepped_wedge(1, 4, 20, 0.05),
    "at least 2"
  )
})

test_that("mb_icc_reference returns full table by default", {
  tbl <- mb_icc_reference()
  expect_s3_class(tbl, "data.frame")
  expect_true(nrow(tbl) > 5)
  expect_true(all(c("domain", "icc_central") %in% names(tbl)))
})

test_that("mb_icc_reference filters by domain", {
  tbl <- mb_icc_reference("education")
  expect_true(all(tbl$domain == "education"))
  expect_true(nrow(tbl) >= 3)
})

test_that("mb_icc_reference rejects unknown domain", {
  expect_error(mb_icc_reference("madeup"), "must be one of")
})
