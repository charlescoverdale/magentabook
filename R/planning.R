#' Tag and structure evaluation questions
#'
#' Stores a set of evaluation questions tagged by Magenta Book type
#' (process, impact, economic, value-for-money) and by priority
#' (primary or secondary). The Magenta Book canonical taxonomy is
#' bundled in [mb_schedule_table()] under `"questions"`.
#'
#' @param text Character vector of evaluation questions.
#' @param type Character vector. One of `"process"`, `"impact"`,
#'   `"economic"`, `"vfm"`. Length 1 or `length(text)`.
#' @param priority Character vector. `"primary"` or `"secondary"`.
#'   Length 1 or `length(text)`.
#'
#' @return An `mb_questions` data frame with columns `text`,
#'   `type`, `priority`.
#'
#' @references
#' HM Treasury (2020). The Magenta Book: Central Government
#' Guidance on Evaluation, chapters on process, impact, and
#' economic evaluation.
#' <https://www.gov.uk/government/publications/the-magenta-book>.
#'
#' @family planning
#' @seealso [mb_evaluation_plan()], [mb_schedule_table()].
#'
#' @export
#' @examples
#' mb_questions(
#'   text     = c("Did the policy cause employment to rise",
#'                "Was implementation faithful to the design"),
#'   type     = c("impact", "process"),
#'   priority = c("primary", "secondary")
#' )
mb_questions <- function(text,
                         type     = "impact",
                         priority = "primary") {
  valid_type <- c("process", "impact", "economic", "vfm")
  valid_pri  <- c("primary", "secondary")
  validate_character(text, arg = "text")

  if (length(type) == 1L)     type     <- rep(type, length(text))
  if (length(priority) == 1L) priority <- rep(priority, length(text))
  if (length(type)     != length(text)) {
    cli::cli_abort("{.arg type} must be length 1 or length(text).")
  }
  if (length(priority) != length(text)) {
    cli::cli_abort("{.arg priority} must be length 1 or length(text).")
  }
  bad_t <- setdiff(type, valid_type)
  if (length(bad_t)) {
    cli::cli_abort("{.arg type} has unrecognised value(s): {.val {bad_t}}. Use one of {.val {valid_type}}.")
  }
  bad_p <- setdiff(priority, valid_pri)
  if (length(bad_p)) {
    cli::cli_abort("{.arg priority} has unrecognised value(s): {.val {bad_p}}. Use one of {.val {valid_pri}}.")
  }

  out <- data.frame(
    text     = text,
    type     = factor(type,     levels = valid_type),
    priority = factor(priority, levels = valid_pri),
    stringsAsFactors = FALSE
  )
  class(out) <- c("mb_questions", "data.frame")
  out
}

#' Define a counterfactual
#'
#' Records the comparison condition against which the policy effect
#' is to be measured. The Magenta Book stresses that no impact
#' evaluation is possible without an explicit counterfactual.
#'
#' @param definition Character scalar describing the counterfactual:
#'   what would have happened in the absence of the policy.
#' @param source Character scalar. Mechanism by which the
#'   counterfactual is constructed. One of `"rct"`,
#'   `"quasi-experimental"`, `"theory-based"`, `"comparator"`,
#'   `"historical"`.
#' @param credibility Character scalar. Plain-English assessment of
#'   how credible the counterfactual is.
#'
#' @return An `mb_counterfactual` object.
#'
#' @references
#' HM Treasury (2020). The Magenta Book: Central Government
#' Guidance on Evaluation, supplementary guidance on
#' quasi-experimental and theory-based methods.
#' <https://www.gov.uk/government/publications/the-magenta-book>.
#'
#' @family planning
#' @seealso [mb_evaluation_plan()].
#'
#' @export
#' @examples
#' mb_counterfactual(
#'   definition  = "Eligible non-applicants in the same year",
#'   source      = "quasi-experimental",
#'   credibility = "Moderate; selection on observables only"
#' )
mb_counterfactual <- function(definition,
                              source = c("rct", "quasi-experimental",
                                         "theory-based", "comparator",
                                         "historical"),
                              credibility = NA_character_) {
  validate_character(definition,  arg = "definition")
  validate_scalar(definition,     arg = "definition")
  source <- match.arg(source)
  if (!is.na(credibility)) {
    validate_character(credibility, arg = "credibility")
    validate_scalar(credibility,    arg = "credibility")
  }
  out <- list(
    definition  = definition,
    source      = source,
    credibility = credibility,
    vintage     = .mb_vintage()
  )
  class(out) <- c("mb_counterfactual", "list")
  out
}

#' RACI-style stakeholder register
#'
#' Records who is Responsible, Accountable, Consulted, or Informed
#' for an evaluation, with optional interest and influence ratings
#' for use in a stakeholder map.
#'
#' @param name Character vector of stakeholder names.
#' @param role Character vector of stakeholder roles.
#' @param raci Character vector. One of `"R"`, `"A"`, `"C"`, `"I"`.
#' @param interest Optional numeric vector in `[1, 5]`. Higher
#'   means more interest in the evaluation.
#' @param influence Optional numeric vector in `[1, 5]`. Higher
#'   means more influence over the evaluation.
#'
#' @return An `mb_stakeholders` data frame with columns `name`,
#'   `role`, `raci`, `interest`, `influence`.
#'
#' @family planning
#' @seealso [mb_evaluation_plan()].
#'
#' @export
#' @examples
#' mb_stakeholders(
#'   name      = c("HMT", "DfE", "What Works Centre"),
#'   role      = c("Funder", "Delivery", "Synthesis"),
#'   raci      = c("A", "R", "C"),
#'   interest  = c(5, 5, 4),
#'   influence = c(5, 4, 2)
#' )
mb_stakeholders <- function(name,
                            role,
                            raci,
                            interest  = NA_real_,
                            influence = NA_real_) {
  valid_raci <- c("R", "A", "C", "I")
  validate_character(name, arg = "name")
  validate_character(role, arg = "role")
  validate_character(raci, arg = "raci")

  n <- length(name)
  if (length(role) != n) cli::cli_abort("{.arg role} must be length {.val {n}}.")
  if (length(raci) != n) cli::cli_abort("{.arg raci} must be length {.val {n}}.")
  bad_r <- setdiff(raci, valid_raci)
  if (length(bad_r)) {
    cli::cli_abort("{.arg raci} has unrecognised value(s): {.val {bad_r}}. Use one of {.val {valid_raci}}.")
  }
  if (length(interest)  == 1L) interest  <- rep(interest,  n)
  if (length(influence) == 1L) influence <- rep(influence, n)
  if (length(interest)  != n) cli::cli_abort("{.arg interest} must be length 1 or {.val {n}}.")
  if (length(influence) != n) cli::cli_abort("{.arg influence} must be length 1 or {.val {n}}.")
  if (any(!is.na(interest)  & (interest  < 1 | interest  > 5))) {
    cli::cli_abort("{.arg interest} values must be between 1 and 5 (or NA).")
  }
  if (any(!is.na(influence) & (influence < 1 | influence > 5))) {
    cli::cli_abort("{.arg influence} values must be between 1 and 5 (or NA).")
  }

  out <- data.frame(
    name      = name,
    role      = role,
    raci      = factor(raci, levels = valid_raci),
    interest  = interest,
    influence = influence,
    stringsAsFactors = FALSE
  )
  class(out) <- c("mb_stakeholders", "data.frame")
  out
}

#' Aggregate evaluation plan
#'
#' Composes the evaluation scope, questions, methods, timing,
#' governance, and (optionally) budget into a single object suitable
#' for review and export.
#'
#' @param scope Character scalar describing what the evaluation
#'   does and does not cover.
#' @param questions An `mb_questions` object.
#' @param methods Character vector of methods chosen for each type
#'   of question (e.g. `c(impact = "RCT", process = "Realist
#'   interviews")`). Names are used in the print method.
#' @param timing Character vector or list describing the evaluation
#'   timeline (baseline, midline, endline, follow-up).
#' @param governance Character vector or list describing oversight:
#'   steering group composition, peer review, data access.
#' @param budget Optional numeric scalar (GBP) for total evaluation
#'   cost.
#'
#' @return An `mb_plan` object.
#'
#' @references
#' HM Treasury (2020). The Magenta Book: Central Government
#' Guidance on Evaluation, chapter on planning and managing an
#' evaluation.
#' <https://www.gov.uk/government/publications/the-magenta-book>.
#'
#' @family planning
#' @seealso [mb_questions()], [mb_counterfactual()],
#'   [mb_stakeholders()], [mb_evaluation_report()].
#'
#' @export
#' @examples
#' qs <- mb_questions(
#'   text = c("Did employment rise", "Was the policy implemented faithfully"),
#'   type = c("impact", "process")
#' )
#' mb_evaluation_plan(
#'   scope      = "GBP 50m skills programme, 2026-2029",
#'   questions  = qs,
#'   methods    = c(impact = "RCT", process = "Mixed methods"),
#'   timing     = c(baseline = "2026-Q1", endline = "2029-Q2"),
#'   governance = "Joint HMT / DfE steering group; peer review by What Works"
#' )
mb_evaluation_plan <- function(scope,
                               questions,
                               methods,
                               timing,
                               governance,
                               budget = NULL) {
  validate_character(scope,      arg = "scope")
  validate_scalar(scope,         arg = "scope")
  if (!inherits(questions, "mb_questions")) {
    cli::cli_abort("{.arg questions} must be an {.cls mb_questions} from {.fn mb_questions}.")
  }
  validate_character(methods,    arg = "methods")
  validate_character(governance, arg = "governance")
  if (!is.null(budget)) {
    validate_numeric(budget, arg = "budget", require_positive = TRUE)
    validate_scalar(budget,  arg = "budget")
  }

  out <- list(
    scope      = scope,
    questions  = questions,
    methods    = methods,
    timing     = timing,
    governance = governance,
    budget     = budget,
    vintage    = .mb_vintage()
  )
  class(out) <- c("mb_plan", "list")
  out
}
