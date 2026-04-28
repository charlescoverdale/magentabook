#' Aggregate evaluation report
#'
#' Composes the components produced by other `magentabook`
#' functions into a single report object: theory of change,
#' evaluation plan, SMS ratings, confidence ratings,
#' cost-effectiveness analyses. Any component may be omitted.
#'
#' @param plan Optional `mb_plan` from [mb_evaluation_plan()].
#' @param toc Optional `mb_toc` from [mb_theory_of_change()].
#' @param sms Optional `mb_sms_rating` or list of them.
#' @param confidence Optional `mb_confidence`,
#'   `mb_confidence_summary`, or list of `mb_confidence`.
#' @param cea Optional `mb_cea`, `mb_icer`, or list of them.
#' @param name Optional character scalar naming the evaluation.
#'
#' @return An `mb_report` object.
#'
#' @family reporting
#' @seealso [mb_to_word()], [mb_to_excel()], [mb_to_latex()].
#'
#' @export
#' @examples
#' toc <- mb_theory_of_change(
#'   inputs = "Funding", activities = "Workshops",
#'   outputs = "Attendees", outcomes = "Skills",
#'   impact = "Employment"
#' )
#' mb_evaluation_report(toc = toc, name = "Skills uplift evaluation")
mb_evaluation_report <- function(plan       = NULL,
                                 toc        = NULL,
                                 sms        = NULL,
                                 confidence = NULL,
                                 cea        = NULL,
                                 name       = NULL) {
  if (!is.null(plan)       && !inherits(plan, "mb_plan")) cli::cli_abort("{.arg plan} must be an {.cls mb_plan}.")
  if (!is.null(toc)        && !inherits(toc,  "mb_toc"))  cli::cli_abort("{.arg toc} must be an {.cls mb_toc}.")
  if (!is.null(name)) { validate_character(name, arg = "name"); validate_scalar(name, arg = "name") }

  .normalise <- function(x, classes) {
    if (is.null(x)) return(NULL)
    if (any(vapply(classes, function(cl) inherits(x, cl), logical(1)))) return(list(x))
    if (is.list(x)) {
      ok <- vapply(x, function(el) any(vapply(classes, function(cl) inherits(el, cl), logical(1))), logical(1))
      if (!all(ok)) cli::cli_abort("All elements must be one of {.cls {classes}}.")
      return(x)
    }
    cli::cli_abort("Input must be one of {.cls {classes}} or a list of them.")
  }
  sms_list <- .normalise(sms,        c("mb_sms_rating"))
  cnf_list <- .normalise(confidence, c("mb_confidence", "mb_confidence_summary"))
  cea_list <- .normalise(cea,        c("mb_cea", "mb_icer"))

  out <- list(
    name       = name,
    plan       = plan,
    toc        = toc,
    sms        = sms_list,
    confidence = cnf_list,
    cea        = cea_list,
    vintage    = .mb_vintage()
  )
  class(out) <- c("mb_report", "list")
  out
}

#' Export an evaluation report to Word
#'
#' Writes a one- to two-page Word document summarising an
#' `mb_report`: name, theory of change, evaluation plan, SMS
#' ratings, confidence ratings, and cost-effectiveness.
#'
#' @param report An `mb_report` object.
#' @param file Output file path (must end in `.docx`).
#'
#' @return Invisibly, the file path.
#'
#' @details
#' Requires the \pkg{officer} and \pkg{flextable} packages (both in
#' Suggests).
#'
#' @family reporting
#' @seealso [mb_evaluation_report()], [mb_to_excel()],
#'   [mb_to_latex()].
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   toc <- mb_theory_of_change(
#'     inputs = "Funding", activities = "Workshops",
#'     outputs = "Attendees", outcomes = "Skills",
#'     impact = "Employment"
#'   )
#'   rep <- mb_evaluation_report(toc = toc, name = "Skills uplift")
#'   tmp <- tempfile(fileext = ".docx")
#'   mb_to_word(rep, tmp)
#' }
#' }
mb_to_word <- function(report, file) {
  if (!requireNamespace("officer", quietly = TRUE) ||
      !requireNamespace("flextable", quietly = TRUE)) {
    cli::cli_abort(
      "Packages {.pkg officer} and {.pkg flextable} are required for {.fn mb_to_word}."
    )
  }
  if (!inherits(report, "mb_report")) {
    cli::cli_abort("{.arg report} must be an {.cls mb_report}.")
  }
  if (!grepl("\\.docx$", file)) {
    cli::cli_abort("{.arg file} must end in {.val .docx}.")
  }

  doc <- officer::read_docx()
  title <- report$name %||% "Magenta Book evaluation"
  doc <- officer::body_add_par(doc, title, style = "heading 1")

  if (!is.null(report$toc)) {
    doc <- officer::body_add_par(doc, "Theory of change", style = "heading 2")
    lf <- mb_logframe(report$toc)
    doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(lf)))
  }
  if (!is.null(report$plan)) {
    doc <- officer::body_add_par(doc, "Evaluation plan", style = "heading 2")
    doc <- officer::body_add_par(doc, paste("Scope:", report$plan$scope))
    doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(report$plan$questions)))
  }
  if (!is.null(report$sms)) {
    doc <- officer::body_add_par(doc, "Maryland SMS ratings", style = "heading 2")
    sms_df <- do.call(rbind, lapply(report$sms, function(s) {
      data.frame(level = s$level, label = s$label, study = s$study,
                 design = s$design %||% NA_character_,
                 stringsAsFactors = FALSE)
    }))
    doc <- flextable::body_add_flextable(doc, flextable::flextable(sms_df))
  }
  if (!is.null(report$confidence)) {
    doc <- officer::body_add_par(doc, "Confidence ratings", style = "heading 2")
    cnf_df <- do.call(rbind, lapply(report$confidence, function(c) {
      if (inherits(c, "mb_confidence_summary")) return(c$ratings)
      data.frame(question = c$question, rating = c$rating,
                 rationale = c$rationale, stringsAsFactors = FALSE)
    }))
    doc <- flextable::body_add_flextable(doc, flextable::flextable(cnf_df))
  }
  if (!is.null(report$cea)) {
    doc <- officer::body_add_par(doc, "Cost-effectiveness", style = "heading 2")
    cea_df <- do.call(rbind, lapply(report$cea, function(x) {
      if (inherits(x, "mb_cea")) {
        data.frame(type = "CEA", label = x$label %||% NA, cost = x$total_cost,
                   effect = x$total_effect, ratio = x$cost_per_unit,
                   stringsAsFactors = FALSE)
      } else {
        data.frame(type = "ICER", label = sprintf("%s vs %s", x$label_b, x$label_a),
                   cost = x$delta_cost, effect = x$delta_effect, ratio = x$icer,
                   stringsAsFactors = FALSE)
      }
    }))
    doc <- flextable::body_add_flextable(doc, flextable::flextable(cea_df))
  }

  print(doc, target = file)
  invisible(file)
}

#' Export an evaluation report to Excel
#'
#' Writes a multi-sheet workbook with one sheet per component:
#' summary, theory of change, plan, SMS ratings, confidence
#' ratings, cost-effectiveness, provenance.
#'
#' @inheritParams mb_to_word
#' @param file Output file path (must end in `.xlsx`).
#'
#' @return Invisibly, the file path.
#'
#' @details
#' Requires the \pkg{openxlsx} package (in Suggests).
#'
#' @family reporting
#' @seealso [mb_to_word()], [mb_to_latex()].
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   toc <- mb_theory_of_change(
#'     inputs = "Funding", activities = "Workshops",
#'     outputs = "Attendees", outcomes = "Skills",
#'     impact = "Employment"
#'   )
#'   rep <- mb_evaluation_report(toc = toc, name = "Skills uplift")
#'   tmp <- tempfile(fileext = ".xlsx")
#'   mb_to_excel(rep, tmp)
#' }
#' }
mb_to_excel <- function(report, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg openxlsx} is required for {.fn mb_to_excel}. Install with {.code install.packages('openxlsx')}."
    )
  }
  if (!inherits(report, "mb_report")) {
    cli::cli_abort("{.arg report} must be an {.cls mb_report}.")
  }
  if (!grepl("\\.xlsx$", file)) {
    cli::cli_abort("{.arg file} must end in {.val .xlsx}.")
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Summary")
  openxlsx::writeData(wb, "Summary", data.frame(
    component = c("name", "vintage", "has_toc", "has_plan", "n_sms", "n_confidence", "n_cea"),
    value     = c(
      report$name %||% "",
      as.character(report$vintage),
      !is.null(report$toc),
      !is.null(report$plan),
      length(report$sms %||% list()),
      length(report$confidence %||% list()),
      length(report$cea %||% list())
    ),
    stringsAsFactors = FALSE
  ))

  if (!is.null(report$toc)) {
    openxlsx::addWorksheet(wb, "Theory of change")
    openxlsx::writeData(wb, "Theory of change", as.data.frame(mb_logframe(report$toc)))
  }
  if (!is.null(report$plan)) {
    openxlsx::addWorksheet(wb, "Plan")
    openxlsx::writeData(wb, "Plan", as.data.frame(report$plan$questions))
  }
  if (!is.null(report$sms)) {
    openxlsx::addWorksheet(wb, "SMS")
    sms_df <- do.call(rbind, lapply(report$sms, function(s) {
      data.frame(level = s$level, label = s$label, study = s$study,
                 design = s$design %||% NA_character_,
                 notes = s$notes %||% NA_character_,
                 stringsAsFactors = FALSE)
    }))
    openxlsx::writeData(wb, "SMS", sms_df)
  }
  if (!is.null(report$confidence)) {
    openxlsx::addWorksheet(wb, "Confidence")
    cnf_df <- do.call(rbind, lapply(report$confidence, function(c) {
      if (inherits(c, "mb_confidence_summary")) return(c$ratings)
      data.frame(question = c$question, rating = c$rating,
                 rationale = c$rationale, stringsAsFactors = FALSE)
    }))
    openxlsx::writeData(wb, "Confidence", cnf_df)
  }
  if (!is.null(report$cea)) {
    openxlsx::addWorksheet(wb, "Cost-effectiveness")
    cea_df <- do.call(rbind, lapply(report$cea, function(x) {
      if (inherits(x, "mb_cea")) {
        data.frame(type = "CEA", label = x$label %||% NA_character_,
                   cost = x$total_cost, effect = x$total_effect,
                   ratio = x$cost_per_unit, stringsAsFactors = FALSE)
      } else {
        data.frame(type = "ICER",
                   label = sprintf("%s vs %s", x$label_b, x$label_a),
                   cost = x$delta_cost, effect = x$delta_effect,
                   ratio = x$icer, stringsAsFactors = FALSE)
      }
    }))
    openxlsx::writeData(wb, "Cost-effectiveness", cea_df)
  }

  openxlsx::addWorksheet(wb, "Provenance")
  openxlsx::writeData(wb, "Provenance", mb_data_versions())

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

#' Render an evaluation report as a LaTeX table
#'
#' Returns a single LaTeX `tabular` summarising the report.
#' Multi-sheet Word/Excel exports are richer; LaTeX is intended for
#' insertion into a one-pager.
#'
#' @inheritParams mb_to_word
#' @param caption Optional table caption.
#' @param label Optional LaTeX label for cross-referencing.
#'
#' @return A character scalar containing a LaTeX `tabular`
#'   environment.
#'
#' @family reporting
#' @seealso [mb_to_word()], [mb_to_excel()].
#'
#' @export
#' @examples
#' toc <- mb_theory_of_change(
#'   inputs = "Funding", activities = "Workshops",
#'   outputs = "Attendees", outcomes = "Skills",
#'   impact = "Employment"
#' )
#' rep <- mb_evaluation_report(toc = toc, name = "Skills uplift")
#' cat(mb_to_latex(rep))
mb_to_latex <- function(report, caption = NULL, label = NULL) {
  if (!inherits(report, "mb_report")) {
    cli::cli_abort("{.arg report} must be an {.cls mb_report}.")
  }
  rows <- c(
    sprintf("Name & %s \\\\", report$name %||% "n/a"),
    sprintf("Vintage & magentabook %s \\\\", as.character(report$vintage)),
    sprintf("Has theory of change & %s \\\\", if (!is.null(report$toc)) "yes" else "no"),
    sprintf("Has plan & %s \\\\", if (!is.null(report$plan)) "yes" else "no"),
    sprintf("SMS ratings & %d \\\\", length(report$sms %||% list())),
    sprintf("Confidence ratings & %d \\\\", length(report$confidence %||% list())),
    sprintf("Cost-effectiveness items & %d \\\\", length(report$cea %||% list()))
  )
  body <- paste(rows, collapse = "\n")
  preamble <- "\\begin{tabular}{ll}\n\\hline\nComponent & Value \\\\\n\\hline\n"
  closing  <- "\n\\hline\n\\end{tabular}"
  out <- paste0(preamble, body, closing)
  if (!is.null(caption) || !is.null(label)) {
    cap <- if (!is.null(caption)) sprintf("\\caption{%s}\n", caption) else ""
    lab <- if (!is.null(label))   sprintf("\\label{%s}\n",   label)   else ""
    out <- sprintf("\\begin{table}[h]\n\\centering\n%s%s%s\n\\end{table}",
                   out, cap, lab)
  }
  out
}
