#' Score a study against the Maryland Scientific Methods Scale
#'
#' Records an evidence rating against the 1-5 Maryland SMS, the
#' What Works Network's standard for grading impact evidence.
#'
#' @param level Integer in `1:5`. The Maryland SMS level.
#' @param study Character scalar. Reference for the study being
#'   rated (citation, URL, internal ID).
#' @param design Optional character scalar. Brief description of
#'   the design (e.g. `"Cluster RCT, 80 schools"`).
#' @param notes Optional character scalar. Additional notes on
#'   methodological strengths and weaknesses.
#'
#' @return An `mb_sms_rating` object: a list capturing the level,
#'   study, design, notes, the corresponding rubric row, and
#'   `vintage`.
#'
#' @details
#' The Maryland SMS, originally developed by Sherman et al. (1997)
#' for crime-prevention research, is the foundation for evidence
#' ratings used by the College of Policing What Works Centre,
#' the Education Endowment Foundation, the Early Intervention
#' Foundation, and others. The Magenta Book adopts SMS as its
#' default for grading impact evidence.
#'
#' Level 1: cross-sectional or before-after with no comparison.
#' Level 2: before-after with a non-equivalent comparison group.
#' Level 3: well-matched comparison across multiple units.
#' Level 4: comparison adjusting for unobservables (DiD, RD, IV,
#' ITS, synthetic control).
#' Level 5: random assignment.
#'
#' Provenance note: numeric levels 1-5 are direct from Sherman et
#' al. (1997). The word labels (Weakest / Weak / Moderate / Strong
#' / Strongest) follow What Works UK / Education Endowment
#' Foundation convention and are not direct quotations from the
#' original report. The design-examples and typical-use columns of
#' the bundled rubric are magentabook synthesis, intended as a
#' practitioner reference rather than a verbatim reproduction.
#'
#' @references
#' Sherman, L. W., Gottfredson, D. C., MacKenzie, D. L., Eck, J.,
#' Reuter, P., Bushway, S. (1997). Preventing Crime: What Works,
#' What Doesn't, What's Promising. Report to the US Congress.
#'
#' HM Treasury (2020). The Magenta Book: Central Government
#' Guidance on Evaluation.
#'
#' @family Maryland SMS
#' @seealso [mb_sms_explain()], [mb_confidence()].
#'
#' @export
#' @examples
#' mb_sms_rate(
#'   level  = 5,
#'   study  = "Card & Krueger (1994) NJ minimum wage",
#'   design = "Difference-in-differences with PA comparison",
#'   notes  = "Large N, but contested measurement"
#' )
mb_sms_rate <- function(level, study, design = NULL, notes = NULL) {
  validate_numeric(level, arg = "level")
  validate_scalar(level,  arg = "level")
  if (!level %in% 1:5) cli::cli_abort("{.arg level} must be an integer 1 through 5.")
  level <- as.integer(level)
  validate_character(study, arg = "study")
  validate_scalar(study,    arg = "study")
  if (!is.null(design)) {
    validate_character(design, arg = "design")
    validate_scalar(design,    arg = "design")
  }
  if (!is.null(notes)) {
    validate_character(notes, arg = "notes")
    validate_scalar(notes,    arg = "notes")
  }

  rubric <- .read_sms_rubric()
  row <- rubric[rubric$level == level, , drop = FALSE]
  if (nrow(row) != 1L) {
    cli::cli_abort("Internal error: rubric lookup for level {.val {level}} returned {.val {nrow(row)}} rows.")
  }
  out <- list(
    level             = level,
    label             = row$label,
    study             = study,
    design            = design,
    notes             = notes,
    short_description = row$short_description,
    causal_inference  = row$causal_inference,
    vintage           = .mb_vintage()
  )
  class(out) <- c("mb_sms_rating", "list")
  out
}

#' Explain the Maryland SMS rubric
#'
#' Prints the bundled Maryland SMS rubric. Use this when scoring
#' studies, training reviewers, or presenting evidence ratings to
#' stakeholders.
#'
#' @param level Optional integer in `1:5`. If supplied, prints the
#'   rubric for that single level. If `NULL` (default), prints the
#'   full rubric.
#'
#' @return Invisibly, the rubric data frame (filtered to `level` if
#'   supplied). Called for the side-effect of printing.
#'
#' @family Maryland SMS
#' @seealso [mb_sms_rate()].
#'
#' @export
#' @examples
#' mb_sms_explain()
#' mb_sms_explain(4)
mb_sms_explain <- function(level = NULL) {
  rubric <- .read_sms_rubric()
  if (!is.null(level)) {
    validate_numeric(level, arg = "level")
    validate_scalar(level,  arg = "level")
    if (!level %in% 1:5) cli::cli_abort("{.arg level} must be an integer 1 through 5.")
    rubric <- rubric[rubric$level == as.integer(level), , drop = FALSE]
  }

  cli::cli_h1("Maryland Scientific Methods Scale")
  for (i in seq_len(nrow(rubric))) {
    r <- rubric[i, , drop = FALSE]
    cli::cli_h2("Level {r$level}: {r$label}")
    cli::cli_text("{.strong Description}: {r$short_description}")
    cli::cli_text("{.strong Examples}: {r$design_examples}")
    cli::cli_text("{.strong Causal inference}: {r$causal_inference}")
    cli::cli_text("{.strong Typical use}: {r$typical_uses}")
  }
  invisible(rubric)
}
