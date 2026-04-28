test_that("ICC reference values lie in [0, 1] and respect low <= central <= high", {
  tbl <- mb_icc_reference()
  expect_true(all(tbl$icc_low >= 0 & tbl$icc_low <= 1))
  expect_true(all(tbl$icc_central >= 0 & tbl$icc_central <= 1))
  expect_true(all(tbl$icc_high >= 0 & tbl$icc_high <= 1))
  expect_true(all(tbl$icc_low <= tbl$icc_central))
  expect_true(all(tbl$icc_central <= tbl$icc_high))
})

test_that("SMS rubric levels are 1 through 5 in order", {
  tbl <- mb_schedule_table("sms")
  expect_equal(tbl$level, 1:5)
})

test_that("Confidence rubric covers all three ratings exactly", {
  tbl <- mb_schedule_table("confidence")
  expect_equal(sort(tbl$rating), c("high", "low", "medium"))
})

test_that("Question taxonomy covers all four question types", {
  tbl <- mb_schedule_table("questions")
  expect_true(all(c("process", "impact", "economic", "vfm") %in% tbl$type))
})

test_that("data versions table lists every bundled CSV", {
  declared <- sort(mb_data_versions()$dataset)
  csvs <- list.files(
    system.file("extdata", package = "magentabook"),
    pattern = "\\.csv$"
  )
  csvs <- sort(sub("\\.csv$", "", csvs))
  # data_versions itself does not need a self-row
  csvs <- setdiff(csvs, "data_versions")
  expect_setequal(declared, csvs)
})

test_that("vintage on result objects matches package version", {
  v <- utils::packageVersion("magentabook")
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  expect_equal(toc$vintage, v)
  conf <- mb_confidence("high", "q", "es", "mq", "g", "r")
  expect_equal(conf$vintage, v)
  cea <- mb_cea(100, 10)
  expect_equal(cea$vintage, v)
})

test_that("power, MDE, and sample size are mutually consistent for d = 0.4", {
  d <- 0.4; power <- 0.8; alpha <- 0.05
  n <- mb_sample_size(d, power = power, alpha = alpha)
  realised_power <- mb_power(n, d, alpha = alpha)
  realised_mde   <- mb_mde(n, power = power, alpha = alpha)
  expect_gte(realised_power, power - 0.01)
  expect_lte(realised_power, power + 0.05)
  expect_lte(realised_mde, d + 0.05)
})
