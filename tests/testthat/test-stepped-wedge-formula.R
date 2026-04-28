test_that("mb_stepped_wedge defaults to Hemming form", {
  res <- mb_stepped_wedge(steps = 5, clusters_per_step = 4,
                          individuals_per_cluster = 20, icc = 0.05)
  expect_equal(res$formula, "hemming")
  # Hemming CF = 3 * 0.95 / (10 * (1 - 1/25))
  expected_cf <- 3 * 0.95 / (10 * (1 - 1 / 25))
  expect_equal(res$correction_factor, expected_cf)
})

test_that("mb_stepped_wedge accepts the Hussey-Hughes formula", {
  res <- mb_stepped_wedge(steps = 5, clusters_per_step = 4,
                          individuals_per_cluster = 20, icc = 0.05,
                          formula = "hussey_hughes")
  expect_equal(res$formula, "hussey_hughes")
})

test_that("mb_stepped_wedge Hemming and Hussey-Hughes diverge at high rho", {
  hem <- mb_stepped_wedge(5, 4, 20, icc = 0.30, formula = "hemming")
  hh  <- mb_stepped_wedge(5, 4, 20, icc = 0.30, formula = "hussey_hughes")
  expect_true(abs(hem$deff_sw - hh$deff_sw) > 0.01)
})

test_that("mb_stepped_wedge rejects unknown formula", {
  expect_error(
    mb_stepped_wedge(5, 4, 20, 0.05, formula = "made-up"),
    "should be one of"
  )
})

test_that("Hussey-Hughes correction factor decreases with rho (more efficient SW)", {
  cf_low  <- mb_stepped_wedge(5, 4, 20, 0.01, formula = "hussey_hughes")$correction_factor
  cf_high <- mb_stepped_wedge(5, 4, 20, 0.20, formula = "hussey_hughes")$correction_factor
  # Higher rho → SW is more efficient relative to parallel cluster
  expect_lt(cf_high, cf_low)
})
