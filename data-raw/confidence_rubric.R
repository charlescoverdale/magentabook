# Build inst/extdata/confidence_rubric.csv
#
# Provenance: this rubric is NOT a direct quotation from the
# Magenta Book. It is a magentabook synthesis of cross-What-Works-
# Centre confidence-rating traditions:
#   - Education Endowment Foundation (5 padlocks)
#   - Early Intervention Foundation (Foundation Standards)
#   - College of Policing (1-5 scale)
#   - Justice Data Lab (red / amber / green)
#
# The three-level high / medium / low structure is designed for HMG
# decision-grade reporting and aligns with the value-for-money
# framing of the Magenta Book (2020) supplementary guidance.

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
