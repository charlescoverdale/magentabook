# Build inst/extdata/question_taxonomy.csv
#
# Source: HM Treasury Magenta Book (2020), chapters on process,
# impact, and economic evaluation. Canonical evaluation question
# taxonomy used to tag questions in mb_questions().

question_taxonomy <- data.frame(
  type = c(
    rep("process", 5),
    rep("impact", 5),
    rep("economic", 5),
    rep("vfm", 4)
  ),
  sub_type = c(
    "implementation", "reach", "fidelity", "adaptation", "context",
    "attribution", "magnitude", "heterogeneity", "duration", "unintended",
    "cea", "cba", "bcr", "distributional", "affordability",
    "economy", "efficiency", "effectiveness", "equity"
  ),
  canonical_question = c(
    "Was the policy implemented as intended",
    "Did the policy reach the intended population",
    "To what extent did delivery match the design",
    "How was the policy adapted in practice",
    "In what contexts did the policy work better or worse",
    "What change in outcomes was caused by the policy",
    "How large was the effect",
    "For whom did it work",
    "How long did the effect last",
    "What unintended consequences occurred",
    "What was the cost per unit of outcome",
    "Did benefits outweigh costs in monetary terms",
    "What is the benefit cost ratio",
    "How were costs and benefits distributed across groups",
    "Can the policy be sustained at scale",
    "Were inputs procured at lowest reasonable cost",
    "Were outputs produced from inputs at the right rate",
    "Did outputs translate into intended outcomes",
    "Were outcomes fairly distributed"
  ),
  methods = c(
    "Implementation fidelity assessment, document review, interviews",
    "Administrative data analysis, monitoring data review",
    "Observation, fidelity scoring, contractor reporting",
    "Realist evaluation, qualitative interviews",
    "Realist evaluation, comparative case study",
    "RCT, quasi-experimental, contribution analysis",
    "RCT or QED with effect size and confidence interval",
    "Subgroup analysis, interaction terms, machine learning",
    "Longitudinal follow-up, event-study",
    "Mixed methods, theory-based evaluation, qualitative scoping",
    "Cost-effectiveness analysis",
    "Cost-benefit analysis",
    "Cost-benefit analysis",
    "Distributional analysis, weighted CBA",
    "Fiscal sustainability analysis, opportunity cost review",
    "Procurement review, benchmarking",
    "Productivity analysis, comparator benchmarking",
    "Logic-model audit, outcome tracking",
    "Distributional analysis, equality impact assessment"
  ),
  magenta_chapter = c(
    rep("Process evaluation", 5),
    rep("Impact evaluation", 5),
    rep("Value for money", 5),
    rep("Value for money", 4)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  question_taxonomy,
  file = "inst/extdata/question_taxonomy.csv",
  row.names = FALSE
)
