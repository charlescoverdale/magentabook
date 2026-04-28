#' Build a Magenta Book theory of change
#'
#' Constructs a five-level logic model in the form set out by the
#' HM Treasury Magenta Book: inputs → activities → outputs →
#' outcomes → impact, with optional assumptions and external
#' factors.
#'
#' @param inputs Character vector of resources committed to the
#'   policy: funding, staff, infrastructure, partnerships.
#' @param activities Character vector of what the policy does with
#'   those inputs: design, delivery, communication, enforcement.
#' @param outputs Character vector of direct, countable products of
#'   the activities: training sessions delivered, leaflets posted,
#'   payments made.
#' @param outcomes Character vector of changes the outputs produce
#'   in the target population, typically over months to a few years:
#'   behaviour change, attitudes, take-up.
#' @param impact Character vector of long-term, ultimate goals the
#'   outcomes contribute to: poverty reduction, decarbonisation,
#'   improved health.
#' @param assumptions Optional character vector of assumptions that
#'   must hold for each level to translate into the next.
#' @param external_factors Optional character vector of contextual
#'   factors outside the policy's control that may affect outcomes.
#' @param name Optional character scalar naming the policy or
#'   programme.
#'
#' @return An `mb_toc` object: a list with one element per level
#'   plus optional `assumptions`, `external_factors`, `name`, and
#'   `vintage`.
#'
#' @details
#' The Magenta Book theory of change is the foundation for every
#' subsequent evaluation step. It makes the implicit causal chain
#' explicit so that evaluation questions can be tied to specific
#' levels and indicators can be defined.
#'
#' @references
#' HM Treasury (2020). The Magenta Book: Central Government
#' Guidance on Evaluation, chapter on theory-based evaluation.
#'
#' @family theory of change
#' @seealso [mb_logframe()], [mb_assumptions()].
#'
#' @export
#' @examples
#' toc <- mb_theory_of_change(
#'   inputs     = c("GBP 50m grant", "12 FTE programme team"),
#'   activities = c("Design training", "Deliver workshops"),
#'   outputs    = c("500 workshops delivered", "8000 attendees"),
#'   outcomes   = c("Improved skills", "Increased confidence"),
#'   impact     = "Higher employment among target group",
#'   assumptions = "Workshops cause skills uplift",
#'   external_factors = "Macro labour market remains stable",
#'   name = "Skills uplift programme"
#' )
#' toc
mb_theory_of_change <- function(inputs,
                                activities,
                                outputs,
                                outcomes,
                                impact,
                                assumptions = NULL,
                                external_factors = NULL,
                                name = NULL) {
  validate_character(inputs,     arg = "inputs")
  validate_character(activities, arg = "activities")
  validate_character(outputs,    arg = "outputs")
  validate_character(outcomes,   arg = "outcomes")
  validate_character(impact,     arg = "impact")
  if (!is.null(assumptions))      validate_character(assumptions,      arg = "assumptions")
  if (!is.null(external_factors)) validate_character(external_factors, arg = "external_factors")
  if (!is.null(name)) {
    validate_character(name, arg = "name")
    validate_scalar(name,    arg = "name")
  }

  out <- list(
    name             = name,
    inputs           = inputs,
    activities       = activities,
    outputs          = outputs,
    outcomes         = outcomes,
    impact           = impact,
    assumptions      = assumptions,
    external_factors = external_factors,
    vintage          = .mb_vintage()
  )
  class(out) <- c("mb_toc", "list")
  out
}

#' Convert a theory of change into a logframe
#'
#' Pivots an `mb_toc` into the canonical Magenta Book logframe
#' table: one row per level, with optional indicators, means of
#' verification, and risks columns.
#'
#' @param toc An `mb_toc` object from [mb_theory_of_change()].
#' @param indicators Optional named list. Names must be one of
#'   `"inputs"`, `"activities"`, `"outputs"`, `"outcomes"`, or
#'   `"impact"`. Each element is a character vector of indicators.
#' @param mov Optional named list, same convention. Means of
#'   verification per level (data source, survey, administrative
#'   record).
#' @param risks Optional named list, same convention. Risks per
#'   level.
#'
#' @return An `mb_logframe` object: a data frame with columns
#'   `level`, `description`, and (if supplied) `indicator`, `mov`,
#'   `risk`. Multiple items per level are concatenated with `"; "`.
#'
#' @family theory of change
#' @seealso [mb_theory_of_change()].
#'
#' @export
#' @examples
#' toc <- mb_theory_of_change(
#'   inputs = "Funding", activities = "Workshops",
#'   outputs = "Attendees", outcomes = "Skills",
#'   impact = "Employment"
#' )
#' mb_logframe(
#'   toc,
#'   indicators = list(outputs = "n attendees", outcomes = "skills score"),
#'   mov        = list(outputs = "attendance log", outcomes = "post-test")
#' )
mb_logframe <- function(toc, indicators = NULL, mov = NULL, risks = NULL) {
  if (!inherits(toc, "mb_toc")) {
    cli::cli_abort("{.arg toc} must be an {.cls mb_toc} from {.fn mb_theory_of_change}.")
  }
  levels <- c("inputs", "activities", "outputs", "outcomes", "impact")
  .check_named_list <- function(x, arg) {
    if (is.null(x)) return(invisible(NULL))
    if (!is.list(x) || is.null(names(x))) {
      cli::cli_abort("{.arg {arg}} must be a named list.")
    }
    bad <- setdiff(names(x), levels)
    if (length(bad)) {
      cli::cli_abort("{.arg {arg}} has unrecognised level(s): {.val {bad}}.")
    }
  }
  .check_named_list(indicators, "indicators")
  .check_named_list(mov,        "mov")
  .check_named_list(risks,      "risks")

  collapse <- function(x) paste(x, collapse = "; ")
  pull <- function(lst, lvl) {
    v <- lst[[lvl]]
    if (is.null(v)) NA_character_ else collapse(v)
  }

  df <- data.frame(
    level       = levels,
    description = vapply(levels, function(l) collapse(toc[[l]]), character(1)),
    stringsAsFactors = FALSE
  )
  if (!is.null(indicators)) df$indicator <- vapply(levels, pull, character(1), lst = indicators)
  if (!is.null(mov))        df$mov       <- vapply(levels, pull, character(1), lst = mov)
  if (!is.null(risks))      df$risk      <- vapply(levels, pull, character(1), lst = risks)

  attr(df, "name")    <- toc$name
  attr(df, "vintage") <- toc$vintage
  class(df) <- c("mb_logframe", "data.frame")
  df
}

#' Build a structured assumption register
#'
#' Captures one or more assumptions from a theory of change in a
#' tidy register, with the level they sit at, the supporting
#' evidence (or its absence), and a criticality rating.
#'
#' @param level Character vector. The theory-of-change level the
#'   assumption sits at. One of `"inputs"`, `"activities"`,
#'   `"outputs"`, `"outcomes"`, `"impact"`.
#' @param description Character vector. Plain-English statement of
#'   the assumption.
#' @param evidence Optional character vector. Source or rationale
#'   for believing the assumption holds. Defaults to `NA`.
#' @param criticality Character vector. One of `"low"`, `"medium"`,
#'   `"high"`. Failure of high-criticality assumptions invalidates
#'   the causal chain.
#'
#' @return An `mb_assumption_register` data frame with columns
#'   `level`, `description`, `evidence`, `criticality`.
#'
#' @family theory of change
#' @seealso [mb_theory_of_change()], [mb_logframe()].
#'
#' @export
#' @examples
#' mb_assumptions(
#'   level       = c("activities", "outcomes"),
#'   description = c("Workshops are well-attended",
#'                   "Skills uplift translates into job entry"),
#'   evidence    = c("Pilot attendance 80%",
#'                   "Indirect: similar programmes show 0.3 SD effect"),
#'   criticality = c("medium", "high")
#' )
mb_assumptions <- function(level,
                           description,
                           evidence = NA_character_,
                           criticality = "medium") {
  valid_levels <- c("inputs", "activities", "outputs", "outcomes", "impact")
  valid_crit   <- c("low", "medium", "high")
  validate_character(level,       arg = "level")
  validate_character(description, arg = "description")

  if (length(level) != length(description)) {
    cli::cli_abort("{.arg level} and {.arg description} must be the same length.")
  }
  bad_l <- setdiff(level, valid_levels)
  if (length(bad_l)) {
    cli::cli_abort("{.arg level} has unrecognised value(s): {.val {bad_l}}. Use one of {.val {valid_levels}}.")
  }
  bad_c <- setdiff(setdiff(criticality, NA), valid_crit)
  if (length(bad_c)) {
    cli::cli_abort("{.arg criticality} has unrecognised value(s): {.val {bad_c}}. Use one of {.val {valid_crit}}.")
  }

  if (length(evidence) == 1L)    evidence    <- rep(evidence, length(level))
  if (length(criticality) == 1L) criticality <- rep(criticality, length(level))
  if (length(evidence)    != length(level)) {
    cli::cli_abort("{.arg evidence} must be length 1 or length(level).")
  }
  if (length(criticality) != length(level)) {
    cli::cli_abort("{.arg criticality} must be length 1 or length(level).")
  }

  out <- data.frame(
    level       = level,
    description = description,
    evidence    = evidence,
    criticality = criticality,
    stringsAsFactors = FALSE
  )
  class(out) <- c("mb_assumption_register", "data.frame")
  out
}
