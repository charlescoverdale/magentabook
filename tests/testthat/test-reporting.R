test_that("mb_evaluation_report builds an empty report", {
  r <- mb_evaluation_report(name = "test")
  expect_s3_class(r, "mb_report")
  expect_equal(r$name, "test")
  expect_null(r$toc); expect_null(r$plan)
})

test_that("mb_evaluation_report composes components", {
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  qs  <- mb_questions("q", "impact")
  plan <- mb_evaluation_plan("scope", qs, "RCT", "2026", "steering")
  sms  <- mb_sms_rate(5, "RCT")
  conf <- mb_confidence("high", "q", "es", "mq", "g", "r")
  cea  <- mb_cea(cost = 1000, effect = 10)
  rep  <- mb_evaluation_report(plan = plan, toc = toc, sms = sms,
                               confidence = conf, cea = cea, name = "test")
  expect_s3_class(rep, "mb_report")
  expect_equal(length(rep$sms), 1L)
  expect_equal(length(rep$confidence), 1L)
  expect_equal(length(rep$cea), 1L)
})

test_that("mb_evaluation_report accepts lists of components", {
  s1 <- mb_sms_rate(4, "study1"); s2 <- mb_sms_rate(5, "study2")
  rep <- mb_evaluation_report(sms = list(s1, s2))
  expect_equal(length(rep$sms), 2L)
})

test_that("mb_evaluation_report rejects wrong types", {
  expect_error(
    mb_evaluation_report(toc = "not a toc"),
    "mb_toc"
  )
  expect_error(
    mb_evaluation_report(sms = "not a rating"),
    "mb_sms_rating"
  )
})

test_that("mb_to_latex returns a tabular string", {
  rep <- mb_evaluation_report(name = "test")
  out <- mb_to_latex(rep)
  expect_type(out, "character")
  expect_match(out, "tabular")
  expect_match(out, "test")
})

test_that("mb_to_latex wraps in table when caption supplied", {
  rep <- mb_evaluation_report(name = "test")
  out <- mb_to_latex(rep, caption = "My eval", label = "tab:eval")
  expect_match(out, "begin\\{table\\}")
  expect_match(out, "caption\\{My eval\\}")
  expect_match(out, "label\\{tab:eval\\}")
})

test_that("mb_to_word and mb_to_excel error helpfully without optional packages", {
  # When the optional packages are installed, just check the file extension guard.
  rep <- mb_evaluation_report(name = "test")
  if (requireNamespace("officer", quietly = TRUE) &&
      requireNamespace("flextable", quietly = TRUE)) {
    expect_error(mb_to_word(rep, "out.txt"), "must end in")
  } else {
    expect_error(mb_to_word(rep, "out.docx"), "officer")
  }
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    expect_error(mb_to_excel(rep, "out.txt"), "must end in")
  } else {
    expect_error(mb_to_excel(rep, "out.xlsx"), "openxlsx")
  }
})

test_that("mb_to_excel writes a workbook", {
  skip_if_not_installed("openxlsx")
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  rep <- mb_evaluation_report(toc = toc, name = "x")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  mb_to_excel(rep, tmp)
  expect_true(file.exists(tmp))
})

test_that("mb_to_word writes a docx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  rep <- mb_evaluation_report(toc = toc, name = "x")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  mb_to_word(rep, tmp)
  expect_true(file.exists(tmp))
})
