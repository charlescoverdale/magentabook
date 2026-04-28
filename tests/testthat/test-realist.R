test_that("mb_cmo builds a register", {
  cmo <- mb_cmo(
    context = c("c1", "c2"),
    mechanism = c("m1", "m2"),
    outcome = c("o1", "o2")
  )
  expect_s3_class(cmo, "mb_cmo")
  expect_equal(nrow(cmo), 2L)
  expect_true(all(is.na(cmo$evidence)))
})

test_that("mb_cmo recycles scalar evidence", {
  cmo <- mb_cmo(c("c1", "c2"), c("m1", "m2"), c("o1", "o2"),
                evidence = "Smith 2024")
  expect_equal(cmo$evidence, c("Smith 2024", "Smith 2024"))
})

test_that("mb_cmo errors on length mismatch", {
  expect_error(
    mb_cmo(context = c("c1", "c2"), mechanism = "m", outcome = c("o1", "o2")),
    "length"
  )
})

test_that("mb_contribution_claim builds a claim", {
  c <- mb_contribution_claim(
    claim = "Programme caused outcome",
    evidence_for = c("ev1", "ev2"),
    evidence_against = "ev3",
    strength = "moderate"
  )
  expect_s3_class(c, "mb_contribution_claim")
  expect_equal(c$strength, "moderate")
  expect_equal(length(c$evidence_for), 2L)
  expect_equal(length(c$evidence_against), 1L)
})

test_that("mb_contribution_claim accepts no evidence_against", {
  c <- mb_contribution_claim("c", "ev", strength = "weak")
  expect_equal(length(c$evidence_against), 0L)
})

test_that("mb_contribution_claim rejects unknown strength", {
  expect_error(
    mb_contribution_claim("c", "ev", strength = "wrong"),
    "should be one of"
  )
})
