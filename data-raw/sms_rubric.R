# Build inst/extdata/sms_rubric.csv
#
# Provenance:
#   - Numeric levels 1-5 are direct from Sherman et al. (1997)
#     Preventing Crime: What Works, What Doesn't, What's Promising.
#   - Word labels (Weakest / Weak / Moderate / Strong / Strongest)
#     follow What Works UK / Education Endowment Foundation
#     convention and are not direct quotations from Sherman et al.
#   - The "design_examples" and "typical_uses" columns are
#     magentabook synthesis, intended as a practitioner reference.
#
# Re-run this script to regenerate the bundled CSV. The CSV is
# checked in so that magentabook ships without this script.

sms_rubric <- data.frame(
  level = 1:5,
  label = c("Weakest", "Weak", "Moderate", "Strong", "Strongest"),
  short_description = c(
    "Outcomes measured at one point in time, or before-and-after with no comparison group",
    "Outcomes measured before and after intervention with a non-equivalent comparison group",
    "Outcomes measured in multiple treatment and comparison units, with comparison units selected to be similar",
    "Comparison between treatment and comparison units accounting for unobservable differences",
    "Random assignment of units to treatment and control conditions"
  ),
  design_examples = c(
    "Cross-sectional survey; pre-post case study",
    "Pre-post with comparator; matched cohort without rigorous matching",
    "Multi-site comparison; basic propensity-score matching; cohort study with covariate adjustment",
    "Difference-in-differences; regression discontinuity; instrumental variables; interrupted time series; synthetic control",
    "Randomised controlled trial; cluster RCT; stepped-wedge RCT"
  ),
  causal_inference = c(
    "No basis for causal inference",
    "Limited; selection bias and time effects confounded",
    "Plausible if comparison group is well-matched",
    "Strong if identifying assumptions hold",
    "Highest internal validity available"
  ),
  typical_uses = c(
    "Programme description, hypothesis generation",
    "Early-stage programme review, formative evaluation",
    "Mid-stage evaluation when randomisation is infeasible",
    "Quasi-experimental impact evaluation",
    "Pilot evaluation, programme that can be randomised"
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  sms_rubric,
  file = "inst/extdata/sms_rubric.csv",
  row.names = FALSE
)
