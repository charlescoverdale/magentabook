# cli-based print methods write to stderr in non-interactive sessions, so
# we test that they run cleanly and return invisibly rather than scraping
# stdout. Family-specific tests assert on data structures.

test_that("print methods on theory-of-change objects run", {
  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  expect_invisible(print(toc))

  lf <- mb_logframe(toc)
  expect_invisible(print(lf))

  reg <- mb_assumptions("inputs", "a", criticality = "low")
  expect_invisible(print(reg))
})

test_that("print methods on planning objects run", {
  qs <- mb_questions("q", "impact")
  expect_invisible(print(qs))

  cf <- mb_counterfactual("def", source = "rct")
  expect_invisible(print(cf))
  cf2 <- mb_counterfactual("def", source = "rct", credibility = "High")
  expect_invisible(print(cf2))

  st <- mb_stakeholders("a", "r", "A")
  expect_invisible(print(st))

  plan <- mb_evaluation_plan("s", qs, "m", "t", "g")
  expect_invisible(print(plan))

  plan_named <- mb_evaluation_plan(
    "s", qs, c(impact = "RCT"), c(start = "2026"), "g", budget = 1e5
  )
  expect_invisible(print(plan_named))
})

test_that("print methods on SMS / confidence objects run", {
  sms <- mb_sms_rate(5, "study", design = "RCT", notes = "n/a")
  expect_invisible(print(sms))

  conf <- mb_confidence("high", "q", "es", "mq", "g", "r")
  expect_invisible(print(conf))

  cs <- mb_confidence_summary(conf, conf)
  expect_invisible(print(cs))
})

test_that("print methods on estimator objects run", {
  set.seed(1)
  treated <- rep(c(0, 1), each = 50)
  post    <- rep(c(0, 1), times = 50)
  y       <- rnorm(100)
  did <- mb_did_2x2(y, treated, post)
  expect_invisible(print(did))
  expect_invisible(summary(did))

  did_cl <- mb_did_2x2(y, treated, post, cluster = rep(1:25, each = 4))
  expect_invisible(print(did_cl))

  its <- mb_its(rnorm(24), 1:24, intervention_time = 12)
  expect_invisible(print(its))

  panel <- expand.grid(unit = 1:20, time = 1:8)
  panel$y <- rnorm(nrow(panel))
  panel$treated <- as.integer(panel$unit <= 10)
  ev <- mb_event_study(panel$y, panel$unit, panel$time, treatment_time = 5,
                       treated = panel$treated, leads = 2, lags = 2)
  expect_invisible(print(ev))
})

test_that("print methods on cost-effectiveness objects run", {
  cea <- mb_cea(1e6, 250)
  expect_invisible(print(cea))
  cea2 <- mb_cea(1e6, 250, label = "Programme A")
  expect_invisible(print(cea2))

  icer <- mb_icer(1e6, 200, 1.5e6, 300)
  expect_invisible(print(icer))
  icer0 <- mb_icer(1e6, 200, 1e6, 200)
  expect_invisible(print(icer0))

  ceac <- mb_ceac(rnorm(50, 50000, 10000), rnorm(50, 2, 0.5), c(0, 30000, 60000))
  expect_invisible(print(ceac))
})

test_that("print methods on realist objects run", {
  cmo <- mb_cmo("c", "m", "o")
  expect_invisible(print(cmo))
  cmo2 <- mb_cmo(c("c1", "c2"), c("m1", "m2"), c("o1", "o2"),
                 evidence = "Smith 2024")
  expect_invisible(print(cmo2))

  claim <- mb_contribution_claim("claim", "ev", strength = "strong")
  expect_invisible(print(claim))
  claim2 <- mb_contribution_claim("c", "for", evidence_against = "against",
                                  strength = "weak")
  expect_invisible(print(claim2))
})

test_that("print method on report runs in all configurations", {
  expect_invisible(print(mb_evaluation_report(name = "test")))

  toc <- mb_theory_of_change("i", "a", "o", "u", "z")
  qs  <- mb_questions("q", "impact")
  plan <- mb_evaluation_plan("s", qs, "m", "t", "g")
  sms  <- mb_sms_rate(5, "RCT")
  conf <- mb_confidence("high", "q", "es", "mq", "g", "r")
  cea  <- mb_cea(1000, 10)
  rep  <- mb_evaluation_report(plan = plan, toc = toc, sms = sms,
                               confidence = conf, cea = cea, name = "Full")
  expect_invisible(print(rep))
})
