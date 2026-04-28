# Build inst/extdata/confidence_rubric.csv
#
# Source: HM Treasury Magenta Book (2020) Annex A; What Works Centre
# confidence-rating guidance. Three-level rubric (high / medium / low).

confidence_rubric <- data.frame(
  rating = c("high", "medium", "low"),
  label = c("High confidence", "Medium confidence", "Low confidence"),
  evidence_strength = c(
    "Multiple independent SMS Level 4 or 5 studies",
    "At least one SMS Level 3 or 4 study",
    "SMS Level 1 or 2 only, or conflicting Level 3+ studies"
  ),
  methodological_quality = c(
    "Strong; identifying assumptions credible",
    "Adequate; some methodological caveats",
    "Weak; selection bias and confounding plausible"
  ),
  generalisability = c(
    "Findings replicated across settings or contexts",
    "Findings established in one or a few settings",
    "Findings not yet established beyond original setting"
  ),
  description = c(
    "Decision-grade evidence; suitable for scaling and policy commitment",
    "Indicative evidence; supports continued investment with monitoring",
    "Exploratory evidence; further evaluation needed before scaling"
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  confidence_rubric,
  file = "inst/extdata/confidence_rubric.csv",
  row.names = FALSE
)
