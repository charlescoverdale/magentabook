# magentabook 0.1.0

* First release. UK HM Treasury Magenta Book policy-evaluation primitives.
* `mb_balance_table()` added for pre-treatment balance checks (mean, SD,
  standardised mean difference, Welch t / chi-squared p, imbalance flag
  at user-controlled threshold).
* `mb_stepped_wedge(formula = c("hemming", "hussey_hughes"))`: choose
  between the Woertman/Hemming closed-form correction (default) and the
  Hussey-Hughes (2007) closed form. Both assume balanced design,
  complete data, no time-by-treatment interaction; for non-standard
  designs use `swCRTdesign` or `clusterPower`.
* `quiet = FALSE` argument added to `mb_did_2x2()`, `mb_its()`, and
  `mb_event_study()`. The print method now appends a one-line reminder
  that the estimator is canonical and points to specialist packages
  (`fixest`, `did`, `sandwich`) for staggered adoption, autocorrelation,
  or production work. Set `quiet = TRUE` to suppress.
* `cluster` argument added to `mb_event_study()`, mirroring
  `mb_did_2x2()`: cluster-robust SEs via CR1 with the Stata-style
  finite-sample correction `(G/(G-1)) * (N-1)/(N-K)`.
* `mb_power()` `@details` now states the normal-approximation
  assumption explicitly and points to `pwr::pwr.t.test` for small N
  (where the noncentral-t form differs by ~1-2 percentage points).
* 35 exported functions across 10 families: theory of change, evaluation
  planning, power and design, Maryland Scientific Methods Scale,
  Magenta Book confidence rating, lightweight estimators
  (difference-in-differences, interrupted time series, event study),
  cost-effectiveness analysis (CEA, ICER, CEAC, INB, QALY, DALY),
  realist / theory-based scaffolding, reporting, lookups.
* Bundled rubric and reference tables in `inst/extdata/` covering the
  five-level Maryland SMS rubric, the three-level magentabook
  confidence rubric (synthesised across What Works Centre traditions),
  reference intra-class correlation values across UK policy domains
  (education, health, employment, local government, criminal justice,
  housing) tagged with a `value_source` flag distinguishing direct
  table quotations from researcher synthesis, and the canonical
  Magenta Book evaluation question taxonomy. Vintage and provenance
  metadata accessible via `mb_data_versions()`.
* Provenance is explicit: see the README "Bundled rubrics: provenance"
  section for what is verbatim from primary sources and what is
  magentabook synthesis.
* Cross-validated against canonical reference implementations (when
  installed): power and sample size vs `pwr`, cluster-robust SEs vs
  `sandwich`. See `tests/testthat/test-pwr-equivalence.R` and
  `tests/testthat/test-sandwich-equivalence.R`.
* Pure computation: no network calls, no API keys.
* Designed as the evaluation companion to the appraisal package
  `greenbook`. See the vignette
  *Cost-effectiveness with magentabook and greenbook* for an end-to-end
  worked example.
