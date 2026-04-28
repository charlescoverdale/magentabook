# magentabook 0.1.0

* First release. UK HM Treasury Magenta Book policy-evaluation primitives.
* 35 exported functions across 10 families: theory of change, evaluation
  planning, power and design, Maryland Scientific Methods Scale,
  Magenta Book confidence rating, lightweight estimators
  (difference-in-differences, interrupted time series, event study),
  cost-effectiveness analysis (CEA, ICER, CEAC, INB, QALY, DALY),
  realist / theory-based scaffolding, reporting, lookups.
* Bundled rubric and reference tables in `inst/extdata/` covering the
  five-level Maryland SMS rubric, the three-level Magenta Book confidence
  rubric, reference intra-class correlation values across UK policy
  domains (education, health, employment, local government, criminal
  justice, housing), and the canonical Magenta Book evaluation question
  taxonomy. Vintage metadata accessible via `mb_data_versions()`.
* Pure computation: no network calls, no API keys.
* Designed as the evaluation companion to the appraisal package
  `greenbook`. See the vignette
  *Cost-effectiveness with magentabook and greenbook* for an end-to-end
  worked example.
