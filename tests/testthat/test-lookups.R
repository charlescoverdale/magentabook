test_that("mb_data_versions returns the bundled vintage table", {
  vs <- mb_data_versions()
  expect_s3_class(vs, "data.frame")
  expect_true(all(c("dataset", "source", "last_updated", "notes") %in% names(vs)))
  expect_true(nrow(vs) >= 4)
})

test_that("mb_schedule_table returns each table by name", {
  expect_s3_class(mb_schedule_table("sms"),        "data.frame")
  expect_s3_class(mb_schedule_table("confidence"), "data.frame")
  expect_s3_class(mb_schedule_table("icc"),        "data.frame")
  expect_s3_class(mb_schedule_table("questions"),  "data.frame")
})

test_that("mb_schedule_table 'sms' table has 5 rows", {
  expect_equal(nrow(mb_schedule_table("sms")), 5L)
})

test_that("mb_schedule_table 'confidence' table has 3 ratings", {
  tbl <- mb_schedule_table("confidence")
  expect_equal(nrow(tbl), 3L)
  expect_equal(sort(tbl$rating), sort(c("high", "medium", "low")))
})

test_that("mb_schedule_table rejects unknown tables", {
  expect_error(mb_schedule_table("nonexistent"), "should be one of")
})

test_that("each entry in mb_data_versions has a non-empty source", {
  vs <- mb_data_versions()
  expect_true(all(nzchar(vs$source)))
  expect_true(all(nzchar(vs$last_updated)))
})
