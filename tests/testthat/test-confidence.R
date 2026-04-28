test_that("mb_confidence builds an mb_confidence object", {
  c1 <- mb_confidence(
    rating = "high",
    question = "q",
    evidence_strength = "es",
    methodological_quality = "mq",
    generalisability = "g",
    rationale = "r"
  )
  expect_s3_class(c1, "mb_confidence")
  expect_equal(c1$rating, "high")
  expect_equal(c1$label, "High confidence")
  expect_match(c1$description, "Decision-grade")
})

test_that("mb_confidence accepts all three ratings", {
  for (rt in c("high", "medium", "low")) {
    c1 <- mb_confidence(rt, "q", "es", "mq", "g", "r")
    expect_equal(c1$rating, rt)
  }
})

test_that("mb_confidence rejects unknown ratings", {
  expect_error(
    mb_confidence("wrong", "q", "es", "mq", "g", "r"),
    "should be one of"
  )
})

test_that("mb_confidence_summary tallies ratings", {
  c1 <- mb_confidence("high",   "q1", "es", "mq", "g", "r")
  c2 <- mb_confidence("medium", "q2", "es", "mq", "g", "r")
  c3 <- mb_confidence("medium", "q3", "es", "mq", "g", "r")
  s <- mb_confidence_summary(c1, c2, c3)
  expect_s3_class(s, "mb_confidence_summary")
  expect_equal(s$n, 3L)
  expect_equal(unname(s$counts), c(1L, 2L, 0L))
  expect_equal(nrow(s$ratings), 3L)
})

test_that("mb_confidence_summary accepts a list", {
  c1 <- mb_confidence("low", "q1", "es", "mq", "g", "r")
  c2 <- mb_confidence("low", "q2", "es", "mq", "g", "r")
  s_args <- mb_confidence_summary(c1, c2)
  s_list <- mb_confidence_summary(list(c1, c2))
  expect_equal(s_args$counts, s_list$counts)
})

test_that("mb_confidence_summary rejects non-mb_confidence input", {
  expect_error(mb_confidence_summary("not"), "mb_confidence")
  expect_error(mb_confidence_summary(), "Provide at least one")
})
