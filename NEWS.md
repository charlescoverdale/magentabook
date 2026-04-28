# magentabook 0.1.0

* First release. UK HM Treasury Magenta Book policy-evaluation primitives.
* Provenance is explicit: bundled rubrics carry honest source metadata
  distinguishing direct quotations from researcher synthesis. ICC
  reference values use a `value_source` flag (`"table_quote"` vs
  `"central_estimate"`).
* DOIs added to every `@references` block where available. Framework
  functions (`mb_evaluation_plan`, `mb_questions`,
  `mb_counterfactual`, `mb_theory_of_change`, etc.) cite the
  Magenta Book (2020) chapters they correspond to.
* `inst/CITATION` extended with footer pointing to the underlying
  primary sources for the methods implemented (Sherman 1997, Cohen
  1988, Hussey-Hughes 2007, Hemming 2015, Hedges & Hedberg 2007,
  Drummond 2015, Cameron & Miller 2015, Stuart 2010).
* Cross-validated against canonical reference implementations:
    - `pwr` for two-sample power, sample size, MDE, and proportion
      power (within ~3 percentage points; `test-pwr-equivalence.R`).
    - `sandwich` for `mb_did_2x2` cluster-robust SEs (CR1 / HC1
      to within `1e-6`; `test-sandwich-equivalence.R`).
    - `swCRTdesign` for `mb_stepped_wedge` (closed-form Hemming
      approximation tracks the exact Hussey-Hughes variance to
      within roughly 0.5x to 2x for typical UK designs;
      `test-swcrt-equivalence.R`). For decision-grade sample-size
      work prefer `swCRTdesign::swPwr`.
    - `BCEA` for `mb_icer` and `mb_ceac` (floating-point agreement;
      `test-bcea-equivalence.R`).
    - `cobalt` for `mb_balance_table` SMD on balanced samples
      (within `1e-8`; `test-cobalt-equivalence.R`).
* `mb_stepped_wedge` `formula` argument removed in favour of the
  single Hemming/Woertman closed-form approximation, which is
  documented as approximate (vs the exact Hussey-Hughes variance
  computed by `swCRTdesign`). The earlier `formula = "hussey_hughes"`
  branch was researcher-derived and not externally verifiable; it
  has been removed before the package leaves disk.
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
