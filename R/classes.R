# S3 print and summary methods for magentabook result classes

#' @export
print.mb_toc <- function(x, ...) {
  cli::cli_h1("Theory of change{if (!is.null(x$name)) paste0(': ', x$name) else ''}")
  cli::cli_text("{.strong Inputs}: {paste(x$inputs, collapse = '; ')}")
  cli::cli_text("{.strong Activities}: {paste(x$activities, collapse = '; ')}")
  cli::cli_text("{.strong Outputs}: {paste(x$outputs, collapse = '; ')}")
  cli::cli_text("{.strong Outcomes}: {paste(x$outcomes, collapse = '; ')}")
  cli::cli_text("{.strong Impact}: {paste(x$impact, collapse = '; ')}")
  if (!is.null(x$assumptions)) {
    cli::cli_text("{.strong Assumptions}: {paste(x$assumptions, collapse = '; ')}")
  }
  if (!is.null(x$external_factors)) {
    cli::cli_text("{.strong External factors}: {paste(x$external_factors, collapse = '; ')}")
  }
  cli::cli_text("Vintage: magentabook {.val {as.character(x$vintage)}}")
  invisible(x)
}

#' @export
print.mb_logframe <- function(x, ...) {
  nm <- attr(x, "name")
  cli::cli_h1("Logframe{if (!is.null(nm)) paste0(': ', nm) else ''}")
  print.data.frame(x)
  invisible(x)
}

#' @export
print.mb_assumption_register <- function(x, ...) {
  cli::cli_h1("Assumption register ({nrow(x)} item{if (nrow(x) != 1L) 's' else ''})")
  print.data.frame(x)
  invisible(x)
}

#' @export
print.mb_questions <- function(x, ...) {
  cli::cli_h1("Evaluation questions ({nrow(x)} item{if (nrow(x) != 1L) 's' else ''})")
  print.data.frame(x)
  invisible(x)
}

#' @export
print.mb_counterfactual <- function(x, ...) {
  cli::cli_h1("Counterfactual")
  cli::cli_text("{.strong Definition}: {x$definition}")
  cli::cli_text("{.strong Source}: {x$source}")
  if (!is.na(x$credibility)) {
    cli::cli_text("{.strong Credibility}: {x$credibility}")
  }
  invisible(x)
}

#' @export
print.mb_stakeholders <- function(x, ...) {
  cli::cli_h1("Stakeholders ({nrow(x)} item{if (nrow(x) != 1L) 's' else ''})")
  print.data.frame(x)
  invisible(x)
}

#' @export
print.mb_plan <- function(x, ...) {
  cli::cli_h1("Evaluation plan")
  cli::cli_text("{.strong Scope}: {x$scope}")
  cli::cli_text("{.strong Questions}: {.val {nrow(x$questions)}} (primary: {.val {sum(x$questions$priority == 'primary')}})")
  if (length(x$methods)) {
    if (!is.null(names(x$methods))) {
      for (i in seq_along(x$methods)) {
        cli::cli_text("{.strong Method ({names(x$methods)[i]})}: {x$methods[[i]]}")
      }
    } else {
      cli::cli_text("{.strong Methods}: {paste(x$methods, collapse = '; ')}")
    }
  }
  if (length(x$timing)) {
    cli::cli_text("{.strong Timing}: {paste(unlist(x$timing), collapse = '; ')}")
  }
  if (length(x$governance)) {
    cli::cli_text("{.strong Governance}: {paste(x$governance, collapse = '; ')}")
  }
  if (!is.null(x$budget)) {
    budget_str <- .format_gbp(x$budget)
    cli::cli_text("{.strong Budget}: {.val {budget_str}}")
  }
  cli::cli_text("Vintage: magentabook {.val {as.character(x$vintage)}}")
  invisible(x)
}

#' @export
print.mb_sms_rating <- function(x, ...) {
  cli::cli_h1("Maryland SMS Level {x$level}: {x$label}")
  cli::cli_text("{.strong Study}: {x$study}")
  if (!is.null(x$design)) cli::cli_text("{.strong Design}: {x$design}")
  if (!is.null(x$notes))  cli::cli_text("{.strong Notes}: {x$notes}")
  cli::cli_text("{.strong Description}: {x$short_description}")
  cli::cli_text("{.strong Causal inference}: {x$causal_inference}")
  invisible(x)
}

#' @export
print.mb_confidence <- function(x, ...) {
  cli::cli_h1("{x$label}")
  cli::cli_text("{.strong Question}: {x$question}")
  cli::cli_text("{.strong Evidence strength}: {x$evidence_strength}")
  cli::cli_text("{.strong Methodological quality}: {x$methodological_quality}")
  cli::cli_text("{.strong Generalisability}: {x$generalisability}")
  cli::cli_text("{.strong Rationale}: {x$rationale}")
  cli::cli_text("{.strong Decision implication}: {x$description}")
  invisible(x)
}

#' @export
print.mb_confidence_summary <- function(x, ...) {
  cli::cli_h1("Confidence summary ({x$n} rating{if (x$n != 1L) 's' else ''})")
  for (lvl in x$levels) {
    cli::cli_text("{.strong {lvl}}: {.val {x$counts[[lvl]]}}")
  }
  cli::cli_h2("Ratings")
  print.data.frame(x$ratings)
  invisible(x)
}

#' @export
print.mb_did <- function(x, ...) {
  est_str  <- .format_es(x$estimate)
  se_str   <- .format_es(x$se)
  cil_str  <- .format_es(x$ci_low)
  cih_str  <- .format_es(x$ci_high)
  p_str    <- .format_es(x$p_value, 4)
  se_label <- if (x$cluster_robust) "cluster-robust" else "OLS"
  cli::cli_h1("Difference-in-differences (2x2)")
  cli::cli_text("Estimate: {.val {est_str}}")
  cli::cli_text("SE ({se_label}): {.val {se_str}}")
  cli::cli_text("95% CI: [{.val {cil_str}}, {.val {cih_str}}]")
  cli::cli_text("p-value: {.val {p_str}}")
  cli::cli_text("N = {.val {x$n}}, df = {.val {x$df}}")
  if (!isTRUE(x$quiet)) {
    cli::cli_alert_info(
      "Canonical 2x2 DiD. For staggered adoption or heterogeneous effects, use {.pkg fixest} or {.pkg did}."
    )
  }
  invisible(x)
}

#' @export
summary.mb_did <- function(object, ...) {
  cat("Difference-in-differences (2x2)\n")
  cat("--------------------------------\n")
  cat(sprintf("Estimate         : %.4f\n", object$estimate))
  cat(sprintf("SE (%s) : %.4f\n",
              if (object$cluster_robust) "cluster-robust" else "OLS           ",
              object$se))
  cat(sprintf("t                : %.3f\n", object$t_stat))
  cat(sprintf("p                : %.4f\n", object$p_value))
  cat(sprintf("95%% CI            : [%.4f, %.4f]\n", object$ci_low, object$ci_high))
  cat("\nGroup means:\n")
  print(object$group_means)
  invisible(object)
}

#' @export
print.mb_its <- function(x, ...) {
  level_str <- .format_es(x$level_change)
  slope_str <- .format_es(x$slope_change)
  level_se  <- .format_es(x$se['post'])
  slope_se  <- .format_es(x$se['post_time'])
  cli::cli_h1("Interrupted time series")
  cli::cli_text("Intervention at: {.val {x$intervention_time}}")
  cli::cli_text("Level change: {.val {level_str}} (SE {.val {level_se}})")
  cli::cli_text("Slope change: {.val {slope_str}} (SE {.val {slope_se}})")
  cli::cli_text("N = {.val {x$n}} (pre: {.val {x$n_pre}}, post: {.val {x$n_post}})")
  if (!isTRUE(x$quiet)) {
    cli::cli_alert_info(
      "Single-group segmented regression with OLS SEs. For autocorrelated series use {.pkg sandwich} (Newey-West) or {.pkg nlme} / {.pkg forecast} (ARIMA-error)."
    )
  }
  invisible(x)
}

#' @export
print.mb_event_study <- function(x, ...) {
  se_label <- if (isTRUE(x$cluster_robust)) "cluster-robust" else "OLS"
  cli::cli_h1("Event study")
  cli::cli_text("Treatment at: {.val {x$treatment_time}}")
  cli::cli_text("Units: {.val {x$n_units}}, periods: {.val {x$n_periods}}, N = {.val {x$n}}")
  cli::cli_text("SE: {se_label}")
  df <- data.frame(
    event_time = x$event_time,
    estimate   = x$estimate,
    se         = x$se
  )
  print(df, row.names = FALSE)
  if (!isTRUE(x$quiet)) {
    cli::cli_alert_info(
      "Fixed-treatment-time event study. For staggered adoption use {.pkg fixest} sunab() or the {.pkg did} package (Callaway-Sant'Anna)."
    )
  }
  invisible(x)
}

#' @export
print.mb_cea <- function(x, ...) {
  cost_str   <- .format_gbp(x$total_cost)
  effect_str <- .format_n(x$total_effect)
  unit_str   <- .format_gbp(x$cost_per_unit)
  title <- if (!is.null(x$label)) paste0("Cost-effectiveness: ", x$label) else "Cost-effectiveness"
  cli::cli_h1(title)
  cli::cli_text("Total cost: {.val {cost_str}}")
  cli::cli_text("Total effect: {.val {effect_str}}")
  cli::cli_text("Cost per unit: {.val {unit_str}}")
  invisible(x)
}

#' @export
print.mb_icer <- function(x, ...) {
  dc_str <- .format_gbp(x$delta_cost)
  de_str <- .format_n(x$delta_effect)
  cli::cli_h1("ICER: {x$label_b} vs {x$label_a}")
  cli::cli_text("Delta cost: {.val {dc_str}}")
  cli::cli_text("Delta effect: {.val {de_str}}")
  if (is.finite(x$icer)) {
    icer_str <- .format_gbp(x$icer)
    cli::cli_text("ICER: {.val {icer_str}} per unit")
  } else {
    cli::cli_text("ICER: {.val {x$icer}}")
  }
  cli::cli_text("Dominance: {.val {x$dominance}}")
  invisible(x)
}

#' @export
print.mb_ceac <- function(x, ...) {
  cli::cli_h1("Cost-effectiveness acceptability curve")
  cli::cli_text("Draws: {.val {x$n_draws}}; WTP grid: {.val {length(x$wtp)}} points")
  df <- data.frame(wtp = x$wtp, prob_cost_effective = x$prob_cost_effective)
  print(df, row.names = FALSE)
  invisible(x)
}

#' @export
print.mb_cmo <- function(x, ...) {
  cli::cli_h1("CMO configurations ({nrow(x)} item{if (nrow(x) != 1L) 's' else ''})")
  print.data.frame(x)
  invisible(x)
}

#' @export
print.mb_contribution_claim <- function(x, ...) {
  cli::cli_h1("Contribution claim ({x$strength})")
  cli::cli_text("{.strong Claim}: {x$claim}")
  cli::cli_h2("Evidence for ({.val {length(x$evidence_for)}})")
  for (e in x$evidence_for) cli::cli_bullets(c("v" = e))
  if (length(x$evidence_against)) {
    cli::cli_h2("Evidence against ({.val {length(x$evidence_against)}})")
    for (e in x$evidence_against) cli::cli_bullets(c("x" = e))
  }
  invisible(x)
}

#' @export
print.mb_balance_table <- function(x, ...) {
  thr <- attr(x, "threshold") %||% 0.10
  n_imb <- sum(x$imbalanced, na.rm = TRUE)
  cli::cli_h1("Pre-treatment balance ({nrow(x)} covariate{if (nrow(x) != 1L) 's' else ''})")
  cli::cli_text("|SMD| threshold: {.val {thr}}")
  cli::cli_text("Imbalanced rows: {.val {n_imb}} of {.val {nrow(x)}}")
  display <- x
  display$mean_treated <- round(display$mean_treated, 3)
  display$mean_control <- round(display$mean_control, 3)
  display$sd_treated   <- round(display$sd_treated, 3)
  display$sd_control   <- round(display$sd_control, 3)
  display$smd          <- round(display$smd, 3)
  display$p_value      <- round(display$p_value, 4)
  print(as.data.frame(display), row.names = FALSE)
  invisible(x)
}

#' @export
print.mb_report <- function(x, ...) {
  cli::cli_h1("Magenta Book evaluation report{if (!is.null(x$name)) paste0(': ', x$name) else ''}")
  cli::cli_text("Theory of change: {if (!is.null(x$toc)) 'present' else 'not set'}")
  cli::cli_text("Plan: {if (!is.null(x$plan)) 'present' else 'not set'}")
  cli::cli_text("SMS ratings: {.val {length(x$sms %||% list())}}")
  cli::cli_text("Confidence ratings: {.val {length(x$confidence %||% list())}}")
  cli::cli_text("Cost-effectiveness items: {.val {length(x$cea %||% list())}}")
  cli::cli_text("Vintage: magentabook {.val {as.character(x$vintage)}}")
  invisible(x)
}
