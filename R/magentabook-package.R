#' magentabook: HM Treasury Magenta Book Policy Evaluation Primitives
#'
#' Implements the framework set out in HM Treasury's Magenta Book
#' (2020): theory of change, evaluation planning, power analysis,
#' Maryland SMS ratings, structured confidence ratings, light-weight
#' difference-in-differences and interrupted-time-series estimators,
#' and cost-effectiveness analysis. Designed as the evaluation
#' companion to the appraisal package `greenbook`.
#'
#' @section Function families:
#' - **Theory of change**: [mb_theory_of_change()], [mb_logframe()],
#'   [mb_assumptions()].
#' - **Planning**: [mb_evaluation_plan()], [mb_questions()],
#'   [mb_counterfactual()], [mb_stakeholders()].
#' - **Power and design**: [mb_power()], [mb_mde()],
#'   [mb_sample_size()], [mb_cluster_design()],
#'   [mb_stepped_wedge()], [mb_icc_reference()].
#' - **Maryland SMS**: [mb_sms_rate()], [mb_sms_explain()].
#' - **Confidence**: [mb_confidence()], [mb_confidence_summary()].
#' - **Estimators**: [mb_did_2x2()], [mb_its()], [mb_event_study()].
#' - **Cost-effectiveness**: [mb_cea()], [mb_icer()], [mb_ceac()],
#'   [mb_inb()], [mb_qaly()], [mb_daly()].
#' - **Realist**: [mb_cmo()], [mb_contribution_claim()].
#' - **Reporting**: [mb_evaluation_report()], [mb_to_word()],
#'   [mb_to_excel()], [mb_to_latex()].
#' - **Lookups**: [mb_data_versions()], [mb_schedule_table()].
#'
#' @section Reproducibility:
#' Bundled rubric and reference tables in `inst/extdata/` carry
#' vintage metadata accessible via [mb_data_versions()]. Every result
#' object records the package version it was produced under.
#'
#' @references
#' HM Treasury (2020). The Magenta Book: Central Government Guidance
#' on Evaluation. London: HMSO.
#'
#' @keywords internal
#' @importFrom cli cli_abort cli_text cli_h1 cli_h2 cli_alert_success cli_alert_danger cli_bullets cli_inform cli_warn
#' @importFrom stats qnorm pnorm pt qt model.matrix
#' @importFrom utils read.csv packageVersion
"_PACKAGE"
