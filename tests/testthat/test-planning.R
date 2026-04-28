test_that("mb_questions builds a tagged data frame", {
  qs <- mb_questions(
    text = c("q1", "q2"),
    type = c("impact", "process"),
    priority = c("primary", "secondary")
  )
  expect_s3_class(qs, "mb_questions")
  expect_equal(levels(qs$type), c("process", "impact", "economic", "vfm"))
  expect_equal(as.character(qs$priority), c("primary", "secondary"))
})

test_that("mb_questions recycles scalars", {
  qs <- mb_questions(text = c("q1", "q2", "q3"), type = "vfm")
  expect_equal(as.character(qs$type), c("vfm", "vfm", "vfm"))
})

test_that("mb_questions rejects unknown types and priorities", {
  expect_error(mb_questions("q", type = "wrong"), "unrecognised value")
  expect_error(mb_questions("q", priority = "wrong"), "unrecognised value")
})

test_that("mb_counterfactual constructs and prints", {
  cf <- mb_counterfactual(
    "Eligible non-applicants",
    source = "quasi-experimental",
    credibility = "Moderate"
  )
  expect_s3_class(cf, "mb_counterfactual")
  expect_equal(cf$source, "quasi-experimental")
  expect_invisible(print(cf))
})

test_that("mb_counterfactual rejects unknown source", {
  expect_error(
    mb_counterfactual("def", source = "made-up"),
    "should be one of"
  )
})

test_that("mb_stakeholders builds a register with RACI", {
  st <- mb_stakeholders(
    name = c("HMT", "DfE"), role = c("Funder", "Delivery"),
    raci = c("A", "R"), interest = c(5, 4), influence = c(5, 3)
  )
  expect_s3_class(st, "mb_stakeholders")
  expect_equal(levels(st$raci), c("R", "A", "C", "I"))
  expect_equal(nrow(st), 2L)
})

test_that("mb_stakeholders recycles interest/influence and validates ranges", {
  st <- mb_stakeholders(
    name = c("a", "b"), role = c("r", "r"), raci = c("A", "C"),
    interest = 4
  )
  expect_equal(st$interest, c(4, 4))
  expect_error(
    mb_stakeholders(name = "a", role = "r", raci = "A", interest = 10),
    "between 1 and 5"
  )
  expect_error(
    mb_stakeholders(name = "a", role = "r", raci = "wrong"),
    "unrecognised value"
  )
})

test_that("mb_evaluation_plan composes inputs", {
  qs <- mb_questions("q", type = "impact")
  plan <- mb_evaluation_plan(
    scope = "scope text",
    questions = qs,
    methods = c(impact = "RCT"),
    timing = c(start = "2026"),
    governance = "steering",
    budget = 250000
  )
  expect_s3_class(plan, "mb_plan")
  expect_equal(plan$scope, "scope text")
  expect_equal(plan$budget, 250000)
})

test_that("mb_evaluation_plan rejects non-mb_questions input", {
  expect_error(
    mb_evaluation_plan("s", questions = "q", methods = "m",
                       timing = "t", governance = "g"),
    "mb_questions"
  )
})

test_that("mb_evaluation_plan accepts no budget", {
  qs <- mb_questions("q", type = "impact")
  plan <- mb_evaluation_plan("s", qs, "m", "t", "g")
  expect_null(plan$budget)
})
