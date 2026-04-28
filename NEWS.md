# magentabook 0.1.0

* First release. UK HM Treasury Magenta Book policy-evaluation primitives.
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
