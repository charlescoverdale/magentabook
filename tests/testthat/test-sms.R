test_that("mb_sms_rate builds the rating", {
  r <- mb_sms_rate(level = 5, study = "RCT 2026", design = "Cluster RCT")
  expect_s3_class(r, "mb_sms_rating")
  expect_equal(r$level, 5L)
  expect_equal(r$label, "Strongest")
  expect_equal(r$study, "RCT 2026")
})

test_that("mb_sms_rate accepts levels 1 through 5", {
  for (lvl in 1:5) {
    r <- mb_sms_rate(level = lvl, study = "x")
    expect_equal(r$level, as.integer(lvl))
  }
})

test_that("mb_sms_rate rejects out-of-range levels", {
  expect_error(mb_sms_rate(0, "x"), "1 through 5")
  expect_error(mb_sms_rate(6, "x"), "1 through 5")
})

test_that("mb_sms_rate stores design and notes when supplied", {
  r <- mb_sms_rate(4, "study", design = "DiD", notes = "parallel trends ok")
  expect_equal(r$design, "DiD")
  expect_equal(r$notes,  "parallel trends ok")
})

test_that("mb_sms_rate adds rubric description and causal-inference text", {
  r <- mb_sms_rate(3, "study")
  expect_match(r$short_description, "comparison")
  expect_match(r$causal_inference, "Plausible")
})

test_that("mb_sms_explain returns the rubric invisibly", {
  out <- mb_sms_explain()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 5L)
})

test_that("mb_sms_explain filters by level", {
  out <- mb_sms_explain(4)
  expect_equal(nrow(out), 1L)
  expect_equal(out$level, 4L)
})

test_that("mb_sms_explain rejects out-of-range levels", {
  expect_error(mb_sms_explain(0), "1 through 5")
  expect_error(mb_sms_explain(6), "1 through 5")
})
