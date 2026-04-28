test_that("mb_theory_of_change builds an mb_toc", {
  toc <- mb_theory_of_change(
    inputs = "GBP 50m grant", activities = "Workshops",
    outputs = "Attendees", outcomes = "Skills",
    impact = "Employment"
  )
  expect_s3_class(toc, "mb_toc")
  expect_equal(toc$inputs, "GBP 50m grant")
  expect_equal(toc$impact, "Employment")
  expect_null(toc$assumptions)
  expect_null(toc$external_factors)
  expect_null(toc$name)
})

test_that("mb_theory_of_change accepts assumptions and external factors", {
  toc <- mb_theory_of_change(
    inputs = "i", activities = "a", outputs = "o",
    outcomes = "u", impact = "z",
    assumptions = c("a1", "a2"),
    external_factors = "macro stable",
    name = "Programme X"
  )
  expect_equal(length(toc$assumptions), 2L)
  expect_equal(toc$external_factors, "macro stable")
  expect_equal(toc$name, "Programme X")
})

test_that("mb_theory_of_change validates inputs", {
  expect_error(
    mb_theory_of_change(1, "a", "o", "u", "z"),
    "must be a character"
  )
  expect_error(
    mb_theory_of_change("i", "a", "o", "u", "z", name = c("x", "y")),
    "scalar"
  )
})

test_that("mb_theory_of_change carries vintage", {
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  expect_s3_class(toc$vintage, "package_version")
})

test_that("mb_logframe pivots toc into a data frame", {
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  lf <- mb_logframe(toc)
  expect_s3_class(lf, "mb_logframe")
  expect_s3_class(lf, "data.frame")
  expect_equal(nrow(lf), 5L)
  expect_equal(lf$level,
               c("inputs", "activities", "outputs", "outcomes", "impact"))
  expect_equal(lf$description[1], "i")
})

test_that("mb_logframe accepts indicators / mov / risks", {
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  lf <- mb_logframe(
    toc,
    indicators = list(outputs = "n attendees", outcomes = "skills score"),
    mov        = list(outputs = "log", outcomes = "test"),
    risks      = list(impact  = "labour shock")
  )
  expect_true(all(c("indicator", "mov", "risk") %in% names(lf)))
  expect_equal(lf$indicator[lf$level == "outputs"], "n attendees")
  expect_true(is.na(lf$risk[lf$level == "outputs"]))
})

test_that("mb_logframe rejects bad inputs", {
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  expect_error(mb_logframe("not a toc"), "mb_toc")
  expect_error(
    mb_logframe(toc, indicators = list(badname = "x")),
    "unrecognised level"
  )
})

test_that("mb_logframe collapses multi-item levels with semicolons", {
  toc <- mb_theory_of_change(
    inputs = c("Funding", "Staff"), activities = "a",
    outputs = "o", outcomes = "u", impact = "z"
  )
  lf <- mb_logframe(toc)
  expect_equal(lf$description[lf$level == "inputs"], "Funding; Staff")
})

test_that("mb_assumptions builds a register", {
  reg <- mb_assumptions(
    level = c("activities", "outcomes"),
    description = c("a1", "a2"),
    evidence = c("e1", "e2"),
    criticality = c("medium", "high")
  )
  expect_s3_class(reg, "mb_assumption_register")
  expect_equal(nrow(reg), 2L)
  expect_equal(reg$criticality, c("medium", "high"))
})

test_that("mb_assumptions recycles scalar inputs", {
  reg <- mb_assumptions(
    level = c("inputs", "activities"),
    description = c("a", "b"),
    criticality = "high"
  )
  expect_equal(reg$criticality, c("high", "high"))
})

test_that("mb_assumptions rejects mismatched lengths and unknown levels", {
  expect_error(
    mb_assumptions(level = "outputs", description = c("a", "b")),
    "same length"
  )
  expect_error(
    mb_assumptions(level = "wrong", description = "x"),
    "unrecognised value"
  )
  expect_error(
    mb_assumptions(level = "inputs", description = "x", criticality = "wrong"),
    "unrecognised value"
  )
})
