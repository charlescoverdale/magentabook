# Cross-validation against the pwr package.
#
# magentabook uses the large-sample normal approximation for two-sample
# power, MDE, and sample size; pwr uses the noncentral t-distribution.
# For typical evaluation sample sizes (n_per_group >= 30) the two
# should agree to 1-2 percentage points on power and to within 1-2 on
# integer sample size.
#
# These tests are skipped on CRAN where pwr is not in the test
# environment; locally they verify the agreement explicitly.

skip_if_not_installed("pwr")

test_that("mb_power agrees with pwr::pwr.t.test within tolerance for d in {0.2, 0.5, 0.8}", {
  for (d in c(0.2, 0.5, 0.8)) {
    for (n in c(50, 100, 200, 500)) {
      mb  <- mb_power(n_per_group = n, effect_size = d, alpha = 0.05)
      pwr <- pwr::pwr.t.test(n = n, d = d, sig.level = 0.05,
                             type = "two.sample", alternative = "two.sided")$power
      expect_lt(abs(mb - pwr), 0.03,
                label = sprintf("mb_power vs pwr (d=%.1f, n=%d)", d, n))
    }
  }
})

test_that("mb_sample_size agrees with pwr::pwr.t.test to within 5 per arm for d in {0.2, 0.5}", {
  for (d in c(0.2, 0.5, 0.8)) {
    mb  <- mb_sample_size(effect_size = d, power = 0.8, alpha = 0.05)
    pwr <- ceiling(pwr::pwr.t.test(d = d, power = 0.8, sig.level = 0.05,
                                   type = "two.sample",
                                   alternative = "two.sided")$n)
    expect_lt(abs(mb - pwr), 6,
              label = sprintf("mb_sample_size vs pwr (d=%.1f)", d))
  }
})

test_that("mb_power for proportions agrees with pwr::pwr.2p.test within tolerance", {
  cases <- list(
    list(p1 = 0.30, p2 = 0.40, n = 200),
    list(p1 = 0.40, p2 = 0.50, n = 500),
    list(p1 = 0.10, p2 = 0.15, n = 1000)
  )
  for (cc in cases) {
    h   <- 2 * asin(sqrt(cc$p1)) - 2 * asin(sqrt(cc$p2))
    mb  <- mb_power(n_per_group = cc$n, type = "proportion",
                    p1 = cc$p1, p2 = cc$p2)
    pwr <- pwr::pwr.2p.test(h = abs(h), n = cc$n, sig.level = 0.05,
                            alternative = "two.sided")$power
    expect_lt(abs(mb - pwr), 0.02,
              label = sprintf("mb_power proportion (p1=%.2f, p2=%.2f, n=%d)",
                              cc$p1, cc$p2, cc$n))
  }
})

test_that("mb_mde agrees with pwr::pwr.t.test inversion within 0.05 SD", {
  for (n in c(100, 500, 1000)) {
    mb  <- mb_mde(n_per_group = n, power = 0.8, alpha = 0.05)
    pwr <- pwr::pwr.t.test(n = n, power = 0.8, sig.level = 0.05,
                           type = "two.sample",
                           alternative = "two.sided")$d
    expect_lt(abs(mb - pwr), 0.05,
              label = sprintf("mb_mde vs pwr (n=%d)", n))
  }
})
