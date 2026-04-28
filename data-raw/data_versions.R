# Build inst/extdata/data_versions.csv
#
# Vintage metadata for every CSV bundled in inst/extdata/. Critical
# for reproducibility: every magentabook object can record the
# vintage of the parameter tables it consumed.

data_versions <- data.frame(
  dataset = c(
    "sms_rubric",
    "confidence_rubric",
    "icc_reference",
    "question_taxonomy"
  ),
  source = c(
    "Sherman, Gottfredson, MacKenzie, Eck, Reuter & Bushway (1997). Preventing Crime: What Works, What Doesn't, What's Promising. Maryland SMS as adopted by HM Treasury Magenta Book and What Works Network",
    "HM Treasury Magenta Book (2020) Annex A; What Works Centre confidence-rating guidance",
    "Hedges & Hedberg (2007); Adams, Gulliford, Ukoumunne, Eldridge, Chinn & Campbell (2004); Campbell, Mollison & Grimshaw (2000); EEF / DfE / DWP / MHCLG / MoJ impact-evaluation reports",
    "HM Treasury Magenta Book (2020) chapters on process, impact, and economic evaluation"
  ),
  last_updated = "2026-04-27",
  notes = c(
    "Five-level Maryland Scientific Methods Scale rubric with design examples and causal-inference notes",
    "Three-level confidence rubric (high / medium / low) covering evidence strength, methodological quality, and generalisability",
    "Reference intra-class correlation coefficients across UK policy domains and units of clustering, low / central / high",
    "Magenta Book canonical evaluation question taxonomy with methods and chapter references"
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  data_versions,
  file = "inst/extdata/data_versions.csv",
  row.names = FALSE
)
