# Build inst/extdata/data_versions.csv
#
# Vintage metadata for every CSV bundled in inst/extdata/. Critical
# for reproducibility AND for being honest about which content is a
# direct quotation from a primary source vs. a researcher synthesis.

data_versions <- data.frame(
  dataset = c(
    "sms_rubric",
    "confidence_rubric",
    "icc_reference",
    "question_taxonomy"
  ),
  source = c(
    paste(
      "Sherman, Gottfredson, MacKenzie, Eck, Reuter & Bushway (1997).",
      "Preventing Crime: What Works, What Doesn't, What's Promising.",
      "Numeric levels 1-5 are the original Maryland Scientific Methods Scale."
    ),
    paste(
      "Synthesised from What Works Centre confidence-rating traditions:",
      "Education Endowment Foundation (5 padlocks), Early Intervention",
      "Foundation (Foundation Standards), College of Policing (1-5 scale),",
      "and the Justice Data Lab (red / amber / green). Three-level high /",
      "medium / low structure adopted to align with HM Treasury Magenta",
      "Book (2020) supplementary value-for-money guidance."
    ),
    paste(
      "Hedges & Hedberg (2007); Adams, Gulliford, Ukoumunne, Eldridge,",
      "Chinn & Campbell (2004); Campbell, Mollison & Grimshaw (2000);",
      "EEF / DfE / DWP / MHCLG / MoJ impact-evaluation reports."
    ),
    paste(
      "HM Treasury Magenta Book (2020) chapters on process, impact, and",
      "economic evaluation; supplementary Magenta Book guides on value",
      "for money and theory-based evaluation."
    )
  ),
  last_updated = "2026-04-27",
  notes = c(
    paste(
      "Numeric levels 1-5 are direct from Sherman et al. (1997). Word",
      "labels (Weakest / Weak / Moderate / Strong / Strongest) follow",
      "What Works UK / Education Endowment Foundation convention.",
      "Design examples and typical-use columns are magentabook synthesis."
    ),
    paste(
      "Not a direct quotation from the Magenta Book. magentabook",
      "synthesis of cross-What-Works-Centre confidence-rating traditions.",
      "Three-level structure designed for Treasury / consultancy",
      "decision-grade reporting."
    ),
    paste(
      "Reference intra-class correlation coefficients across UK policy",
      "domains. Each row is tagged in the bundled CSV with value_source",
      "= 'table_quote' (direct extraction with table number) or",
      "'central_estimate' (researcher synthesis within published range).",
      "Practitioners should compute domain-specific ICCs from baseline",
      "data wherever feasible."
    ),
    paste(
      "Magenta Book canonical evaluation question taxonomy with methods",
      "and chapter references. Sub-types (e.g. 'attribution',",
      "'fidelity') are conventional categories used across HMG",
      "evaluation practice."
    )
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  data_versions,
  file = "inst/extdata/data_versions.csv",
  row.names = FALSE
)
