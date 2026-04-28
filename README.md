# magentabook

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/magentabook)](https://CRAN.R-project.org/package=magentabook)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Policy-evaluation primitives from the HM Treasury Magenta Book, in R.


## What is the Magenta Book?

The [Magenta Book](https://www.gov.uk/government/publications/the-magenta-book) is HM Treasury's guidance on how to evaluate policies, programmes, and projects funded by UK central government. It is the evaluation companion to the Green Book (which covers appraisal). Together they bookend the full ROAMEF cycle: rationale, objectives, appraisal, monitoring, evaluation, feedback. The current edition is the 2020 update.

The guidance covers four core areas:

- **Theory of change**: a structured logic model linking inputs, activities, outputs, outcomes, and impact, with explicit assumptions and external factors.
- **Evaluation design**: process, impact, and economic evaluation questions; counterfactual definition; methods chosen against the Maryland Scientific Methods Scale.
- **Quantitative methods**: power and minimum-detectable-effect calculations; randomised, quasi-experimental, and theory-based approaches.
- **Economic evaluation**: cost per outcome, incremental cost-effectiveness ratios, acceptability curves, QALYs / DALYs.

The Magenta Book is supplemented by sector-specific guidance (DESNZ, DfT, DHSC) and by the What Works Network's confidence-rating taxonomy.


## How is it used?

A practitioner planning or reporting a Magenta Book evaluation typically:

1. Builds a theory of change linking inputs, activities, outputs, outcomes, and impact.
2. Defines evaluation questions tagged by type (process, impact, economic, value-for-money).
3. Specifies a counterfactual and chooses an estimation strategy compatible with the desired Maryland SMS level.
4. Runs power / minimum-detectable-effect calculations to size the study, accounting for clustering when relevant.
5. Estimates impacts (RCT, DiD, ITS, event-study, RD, IV, synthetic control), then summarises confidence using a structured rubric.
6. Layers on cost-effectiveness analysis: cost per outcome, ICER, acceptability curves, INB.
7. Reports the bundle as a single evaluation report, including provenance for every rubric and reference value used.

Today, this is mostly assembled in Word documents and spreadsheets, with sample-size formulas hand-typed from textbooks and confidence rubrics copied from PDFs. `magentabook` puts the same primitives in R so an evaluation becomes code that can be tested, reviewed, and reproduced.


## Why this package?

No existing R or Python package implements the Magenta Book. UK evaluation practitioners hand-roll the same theory-of-change templates, sample-size formulas, and confidence rubrics every project. The arithmetic is simple but the framework is large, and the parameters change: the SMS rubric is a five-level table, ICCs vary by domain, the Magenta Book confidence rubric has three levels with explicit dimensions.

`magentabook` solves three problems:

- **Reproducibility**: every result object carries the package vintage. `mb_data_versions()` shows source and last-updated date for every bundled rubric and reference table.
- **Auditability**: evaluations are code, not Word documents. Reviewers can run the tests, inspect the inputs, verify the outputs.
- **Composability**: pairs cleanly with `greenbook` so the same R session covers appraisal and evaluation.

The package is pure computation: no network calls, no API keys. Bundled rubric and reference tables in `inst/extdata/` are refreshed via `data-raw/` scripts.


## Installation

```r
# install.packages("magentabook")  # not yet on CRAN
# Development version:
# install.packages("remotes")
remotes::install_github("charlescoverdale/magentabook")
```


## Quick start

```r
library(magentabook)

# Theory of change for a skills programme
toc <- mb_theory_of_change(
  inputs     = c("GBP 50m grant", "12 FTE programme team"),
  activities = c("Design training", "Deliver workshops"),
  outputs    = c("500 workshops delivered", "8000 attendees"),
  outcomes   = c("Improved skills", "Increased confidence"),
  impact     = "Higher employment among the target group",
  assumptions = "Workshops cause skills uplift",
  external_factors = "Macro labour market remains stable",
  name = "Skills uplift programme"
)
mb_logframe(toc)

# Power and sample size
mb_sample_size(effect_size = 0.3, power = 0.8)
mb_mde(n_per_group = 500, type = "proportion", baseline = 0.4)
mb_cluster_design(individuals_per_cluster = 30, icc = 0.05, n_clusters = 20)

# Maryland SMS rating + confidence
mb_sms_rate(level = 4, study = "DiD on admin data",
            design = "Difference-in-differences with matched comparison")
mb_confidence(
  rating                 = "medium",
  question               = "Did the policy raise employment",
  evidence_strength      = "One Level 4 DiD; one Level 3 matched cohort",
  methodological_quality = "Adequate; parallel trends plausible",
  generalisability       = "Findings established in a single region",
  rationale              = "Effect direction consistent across two studies"
)

# Cost-effectiveness
mb_cea(cost = 1e6, effect = 250, label = "Workshop programme")
mb_icer(cost_a = 1e6, effect_a = 200,
        cost_b = 1.5e6, effect_b = 300,
        label_a = "Status quo", label_b = "Enhanced")

# Quick estimators
set.seed(1)
n <- 400
treated <- rep(c(0, 1), each = n / 2)
post    <- rep(c(0, 1), times = n / 2)
y       <- 0.4 * treated * post + rnorm(n)
mb_did_2x2(y, treated, post)

# Inspect bundled vintages
mb_data_versions()
```


## Function inventory

| Family | Functions |
|---|---|
| Theory of change | `mb_theory_of_change()`, `mb_logframe()`, `mb_assumptions()` |
| Planning | `mb_evaluation_plan()`, `mb_questions()`, `mb_counterfactual()`, `mb_stakeholders()`, `mb_balance_table()` |
| Power and design | `mb_power()`, `mb_mde()`, `mb_sample_size()`, `mb_cluster_design()`, `mb_stepped_wedge()`, `mb_icc_reference()` |
| Maryland SMS | `mb_sms_rate()`, `mb_sms_explain()` |
| Confidence | `mb_confidence()`, `mb_confidence_summary()` |
| Estimators | `mb_did_2x2()`, `mb_its()`, `mb_event_study()` |
| Cost-effectiveness | `mb_cea()`, `mb_icer()`, `mb_ceac()`, `mb_inb()`, `mb_qaly()`, `mb_daly()` |
| Realist / theory-based | `mb_cmo()`, `mb_contribution_claim()` |
| Reporting | `mb_evaluation_report()`, `mb_to_word()`, `mb_to_excel()`, `mb_to_latex()` |
| Lookups | `mb_data_versions()`, `mb_schedule_table()` |


## Bundled data sources

| Dataset | Source | Notes |
|---|---|---|
| Maryland SMS rubric | Sherman et al. (1997); Magenta Book (2020) | 1-5 rubric: design examples, causal inference, typical uses |
| Confidence rubric | Synthesis across What Works Centre traditions | 3-level rubric: evidence strength, methodological quality, generalisability |
| ICC reference values | Hedges & Hedberg (2007); Adams et al. (2004); Campbell et al. (2000); EEF / DfE / DWP / MHCLG / MoJ | Reference low / central / high ICCs across UK policy domains |
| Question taxonomy | Magenta Book (2020) | 19 canonical evaluation questions tagged by type and method |

All datasets are refreshed via the scripts in `data-raw/`. Vintages are visible via `mb_data_versions()`.


## Bundled rubrics: provenance

Decision-grade use depends on knowing what is a direct quotation and what is a researcher synthesis. magentabook is explicit about this:

| Bundled item | Status | What is verbatim | What is magentabook synthesis |
|---|---|---|---|
| **Maryland SMS levels 1-5** | Verbatim numeric scale | The five-level structure is direct from Sherman et al. (1997) | Word labels (Weakest / Weak / Moderate / Strong / Strongest) follow What Works UK / EEF convention. The design-examples and typical-use columns are practitioner-oriented synthesis. |
| **Magenta Book confidence rubric** | Synthesis | The three-level high / medium / low structure aligns with the Magenta Book (2020) supplementary value-for-money framing | The full rubric is *not* a direct quotation from the Magenta Book. It is synthesised from EEF (5 padlocks), Early Intervention Foundation (Foundation Standards), College of Policing (1-5 scale), and Justice Data Lab (red / amber / green) confidence traditions. |
| **ICC reference values** | Mixed | Each row carries a `value_source` flag: `"table_quote"` for direct extraction with table number, `"central_estimate"` for researcher synthesis within the published range. | At v0.1.0 every row is `central_estimate`. Future versions will upgrade individual rows to `table_quote` as exact citations are added. Always compute domain-specific ICCs from baseline data before relying on these in a published power calculation. |
| **Question taxonomy** | Verbatim structure | The four types (process, impact, economic, value-for-money) and their canonical questions are from Magenta Book (2020) chapters | Sub-types (e.g. "attribution", "fidelity") are conventional categories used across HMG evaluation practice. |

Practitioner rule: use the *structure* of the bundled rubrics with confidence; substitute your project-specific *content* (rubric values, ICC estimates) where decision-grade reporting requires it.


## Cross-validation

The arithmetic primitives are cross-validated against the canonical
reference implementations on every `R CMD check` (when the optional
packages are installed):

- **Power and sample size** vs `pwr` (`pwr.t.test`, `pwr.2p.test`):
  agreement within ~2-3 percentage points of power, ~5 per arm on
  required N. Discrepancy reflects magentabook's normal-approximation
  vs `pwr`'s noncentral t.
- **Cluster-robust SEs** in `mb_did_2x2` vs `sandwich::vcovCL` with
  `type = "HC1"`: agreement to within `1e-6`. The CR1 estimator and
  the Stata-style finite-sample correction `(G/(G-1)) * (N-1)/(N-K)`
  are implemented identically.
- **DiD point estimate** vs `lm(y ~ treated * post)$coefficients`:
  agreement to floating-point precision.

See `tests/testthat/test-pwr-equivalence.R` and
`tests/testthat/test-sandwich-equivalence.R` for the test grid.


## What this package is not

`magentabook` provides framework primitives plus light-weight versions of the most common quantitative methods. For production-grade quasi-experimental estimation, use the specialist packages:

- Heterogeneous-treatment-effect difference-in-differences: `did`, `didimputation`, `fixest::feols(... sunab(...))`
- Synthetic control: `Synth`, `tidysynth`, `augsynth`
- Regression discontinuity: `rdrobust`, `rddtools`
- Instrumental variables: `ivreg`, `fixest::feols(... | ... ~ ... )`, `ivcheck` for diagnostics
- Conformal prediction: `predictset`
- Cluster-robust inference: `sandwich`, `clubSandwich`

The light-weight implementations of `mb_did_2x2`, `mb_its`, and `mb_event_study` are deliberately canonical: they are useful for sanity checks, teaching, and headline estimates, and each docstring points to the right specialist package for production work.


## Companion package

`greenbook` provides UK Green Book appraisal primitives (STPR, NPV, optimism bias, distributional weights, METB, DESNZ carbon values, VPF, WELLBYs). Together, `greenbook` + `magentabook` cover the full appraisal-to-evaluation spine.

```r
# Appraisal: discount future net benefits to present value
greenbook::gb_npv(cashflow = c(-100, 30, 30, 30, 30, 30))

# Evaluation: did the realised effect justify the cost?
magentabook::mb_icer(cost_a = 1e6, effect_a = 200,
                     cost_b = 1.5e6, effect_b = 300)
```

See the vignette "Cost-effectiveness with magentabook and greenbook" for a worked end-to-end example.


## References

HM Treasury (2020). *The Magenta Book: Central Government Guidance on Evaluation.* London: HMSO.

Sherman, L. W., Gottfredson, D. C., MacKenzie, D. L., Eck, J., Reuter, P., Bushway, S. (1997). *Preventing Crime: What Works, What Doesn't, What's Promising.* Report to the US Congress.

Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum.

Drummond, M. F., Sculpher, M. J., Claxton, K., Stoddart, G. L., Torrance, G. W. (2015). *Methods for the Economic Evaluation of Health Care Programmes* (4th ed.). Oxford University Press.

Hemming, K., Haines, T. P., Chilton, P. J., Girling, A. J., Lilford, R. J. (2015). The stepped wedge cluster randomised trial: rationale, design, analysis, and reporting. *BMJ* 350.


## Source documents

- [HM Treasury Magenta Book (2020)](https://www.gov.uk/government/publications/the-magenta-book)
- [Magenta Book supplementary guides (HMT, 2020)](https://www.gov.uk/government/publications/the-magenta-book) — feasibility, experimental, quasi-experimental, theory-based, value for money, qualitative, mixed methods
- [Sherman et al. (1997). Preventing Crime: What Works, What Doesn't, What's Promising](https://www.ojp.gov/ncjrs/virtual-library/abstracts/preventing-crime-what-works-what-doesnt-whats-promising) — original Maryland SMS
- [Cabinet Office Evaluation Task Force](https://www.gov.uk/government/organisations/evaluation-task-force) — central HMG evaluation guidance
- [Hedges & Hedberg (2007). Intraclass Correlation Values for Planning Group-Randomized Trials in Education. EEPA 29(1)](https://doi.org/10.3102/0162373707299706)
- [Hemming et al. (2015). The stepped wedge cluster randomised trial. BMJ 350](https://doi.org/10.1136/bmj.h391)
- [Drummond, Sculpher, Claxton, Stoddart, Torrance (2015). Methods for the Economic Evaluation of Health Care Programmes (4e). OUP](https://global.oup.com/academic/product/methods-for-the-economic-evaluation-of-health-care-programmes-9780199665884)


## Citation

If you use `magentabook` in published work, please cite via:

```r
citation("magentabook")
```

The package citation and the underlying HM Treasury Magenta Book are both returned.


## Issues

Report bugs or request features at [GitHub Issues](https://github.com/charlescoverdale/magentabook/issues).


## Keywords

policy-evaluation, magenta-book, hm-treasury, theory-of-change, logframe, evaluation-design, power-analysis, sample-size, minimum-detectable-effect, cluster-rct, stepped-wedge, intra-class-correlation, maryland-sms, scientific-methods-scale, what-works, confidence-rating, cost-effectiveness, icer, ceac, qaly, daly, difference-in-differences, interrupted-time-series, event-study, realist-evaluation, contribution-analysis, cabinet-office-evaluation-task-force


## Licence

MIT (c) 2026 Charles Coverdale.
