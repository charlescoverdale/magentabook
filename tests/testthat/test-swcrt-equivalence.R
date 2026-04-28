# Cross-validation of mb_stepped_wedge against swCRTdesign.
#
# mb_stepped_wedge implements the closed-form Hemming et al. (2015)
# approximation. swCRTdesign::swPwr computes the exact Hussey-Hughes
# (2007) variance from the design matrix.
#
# These tests document the magnitude of the approximation gap
# rather than asserting exact equivalence. For typical UK
# evaluation designs the Hemming approximation is within ~50% of
# the exact variance; for high rho or short trials the gap
# widens. Practitioners are pointed at swCRTdesign in the
# mb_stepped_wedge @details for production calculations.
#
# Convention note: this test pins down the parameter
# correspondence between the two packages. mb_stepped_wedge takes
# `clusters_per_step` as the *total* number of clusters in the
# trial (the multiplier that produces n_total = T * clusters *
# m). swCRTdesign::swDsn takes a `clusters` vector with one
# entry per sequence (step). To match an mb_stepped_wedge call
# with `clusters_per_step = N`, pass `clusters = rep(N / (T-1),
# T-1)` so that the total cluster count is preserved.

skip_if_not_installed("swCRTdesign")

# Helper: derive the SW variance of theta_hat from a swPwr power
# value. Inverts the standard normal-approximation power formula.
.sw_var_from_power <- function(power, mu_diff, alpha = 0.05) {
  z_alpha <- stats::qnorm(1 - alpha / 2)
  delta_over_se <- z_alpha - stats::qnorm(1 - power)
  se <- mu_diff / delta_over_se
  se^2
}

test_that("mb_stepped_wedge approximation tracks swCRTdesign within 0.5x to 2x for typical UK designs", {
  cases <- list(
    list(T = 5, m = 20, rho = 0.05, mu_diff = 0.10),
    list(T = 5, m = 50, rho = 0.02, mu_diff = 0.10),
    list(T = 6, m = 30, rho = 0.05, mu_diff = 0.10),
    list(T = 4, m = 40, rho = 0.10, mu_diff = 0.10)
  )
  for (cc in cases) {
    n_clusters <- cc$T - 1L  # one cluster per step
    dsn <- swCRTdesign::swDsn(clusters = rep(1L, n_clusters))
    pwr <- suppressWarnings(swCRTdesign::swPwr(
      design = dsn, distn = "gaussian", n = cc$m,
      mu0 = 0, mu1 = cc$mu_diff,
      sigma = 1, tau = sqrt(cc$rho * 1 / (1 - cc$rho)),
      alpha = 0.05
    ))[["treatment.1"]]
    sw_var_exact <- .sw_var_from_power(pwr, cc$mu_diff)

    # Total observations across the whole trial
    n_total <- n_clusters * cc$T * cc$m
    indiv_var <- 4 / n_total
    deff_exact <- sw_var_exact / indiv_var

    mb <- mb_stepped_wedge(
      steps = cc$T, clusters_per_step = n_clusters,
      individuals_per_cluster = cc$m, icc = cc$rho
    )
    expect_equal(mb$n_total, n_total)

    ratio <- mb$deff_sw / deff_exact
    expect_gt(ratio, 0.5,
              label = sprintf("Hemming-Approx / swCRT-exact at T=%d, m=%d, rho=%.2f",
                              cc$T, cc$m, cc$rho))
    expect_lt(ratio, 2.0,
              label = sprintf("Hemming-Approx / swCRT-exact at T=%d, m=%d, rho=%.2f",
                              cc$T, cc$m, cc$rho))
  }
})

test_that("mb_stepped_wedge correctly orders designs by efficiency", {
  # More steps at the same total N should give a smaller
  # correction factor (the SW efficiency improvement is larger).
  short <- mb_stepped_wedge(steps = 3, clusters_per_step = 4,
                            individuals_per_cluster = 25, icc = 0.05)
  long  <- mb_stepped_wedge(steps = 6, clusters_per_step = 2,
                            individuals_per_cluster = 25, icc = 0.05)
  expect_lt(long$correction_factor, short$correction_factor)
})
