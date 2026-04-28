#' Pre-treatment balance table
#'
#' Computes a Magenta Book-standard balance check for pre-treatment
#' covariates: by-arm mean and standard deviation, standardised
#' mean difference (SMD), and a two-sample test of equality. The
#' SMD is the unitless effect size most evaluators report; rules
#' of thumb flag `|SMD| > 0.10` as a meaningful imbalance and
#' `|SMD| > 0.25` as a serious imbalance.
#'
#' @param treated Logical or 0/1 numeric vector identifying the
#'   treated unit. `TRUE` / `1` means treated.
#' @param ... Numeric or factor covariates to balance check. Names
#'   become row labels. May be passed as a data frame via the
#'   `data` argument.
#' @param data Optional data frame. If supplied, `...` is ignored
#'   and every column other than `treated` is checked. Pass the
#'   `treated` argument as a column reference (e.g. `data$treat`)
#'   or as the column name in the data frame.
#' @param threshold Numeric scalar. Absolute SMD threshold above
#'   which a row is flagged as imbalanced. Default `0.10`.
#'
#' @return An `mb_balance_table` data frame with columns
#'   `covariate`, `mean_treated`, `mean_control`, `sd_treated`,
#'   `sd_control`, `n_treated`, `n_control`, `smd`, `p_value`,
#'   `imbalanced`. Numeric and binary covariates use the
#'   pooled-SD SMD and a Welch two-sample t-test. Factor covariates
#'   are decomposed into one row per non-reference level using the
#'   level-indicator and a chi-squared test on the original
#'   factor.
#'
#' @details
#' For a numeric or 0/1 covariate \eqn{X} with treated mean
#' \eqn{\bar X_T}, control mean \eqn{\bar X_C}, treated SD
#' \eqn{s_T}, and control SD \eqn{s_C}, the standardised mean
#' difference is
#' \deqn{\text{SMD} = \frac{\bar X_T - \bar X_C}{\sqrt{(s_T^2 + s_C^2)/2}}.}
#'
#' This is the equal-weighted pooled-SD form recommended by Stuart
#' (2010) and Austin (2009) for propensity-score balance
#' diagnostics. It differs from Cohen's d, which uses the
#' degrees-of-freedom-weighted pooled SD
#' \eqn{\sqrt{(s_T^2(n_T-1) + s_C^2(n_C-1))/(n_T+n_C-2)}}; the two
#' agree when \eqn{n_T = n_C}. magentabook ships a cross-validation
#' test against `cobalt::bal.tab` which uses the same averaged-SD
#' form.
#'
#' Rules of thumb (Cohen 1988; Stuart 2010):
#' - `|SMD| < 0.10`: well balanced
#' - `0.10 <= |SMD| < 0.25`: meaningful imbalance, consider
#'   covariate adjustment
#' - `|SMD| >= 0.25`: serious imbalance, matching or weighting
#'   recommended
#'
#' Magenta Book impact evaluation guidance recommends a balance
#' table for any quasi-experimental design and as a sense-check
#' even for randomised designs.
#'
#' @references
#' Stuart, E. A. (2010). Matching methods for causal inference: A
#' review and a look forward. Statistical Science 25(1).
#' <doi:10.1214/09-STS313>.
#'
#' Austin, P. C. (2009). Balance diagnostics for comparing the
#' distribution of baseline covariates between treatment groups
#' in propensity-score matched samples. Statistics in Medicine
#' 28(25). <doi:10.1002/sim.3697>.
#'
#' HM Treasury (2020). The Magenta Book, supplementary guidance on
#' quasi-experimental methods.
#' <https://www.gov.uk/government/publications/the-magenta-book>.
#'
#' @family planning
#' @seealso [mb_did_2x2()], [mb_questions()].
#'
#' @export
#' @examples
#' set.seed(20260427)
#' n <- 400
#' treated <- rep(c(0, 1), each = n / 2)
#' age     <- rnorm(n, mean = 45 + 2 * treated, sd = 10)
#' female  <- rbinom(n, 1, 0.5)
#' income  <- rnorm(n, mean = 30000 + 1500 * treated, sd = 8000)
#' mb_balance_table(treated = treated, age = age, female = female, income = income)
mb_balance_table <- function(treated, ..., data = NULL, threshold = 0.10) {
  validate_proportion(threshold, arg = "threshold")
  validate_scalar(threshold,     arg = "threshold")

  if (!is.null(data)) {
    if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
    covars <- as.list(data)
    if (length(treated) == 1L && is.character(treated) && treated %in% names(data)) {
      treated <- data[[treated]]
      covars[[treated]] <- NULL
    }
  } else {
    covars <- list(...)
    if (length(covars) == 0L) {
      cli::cli_abort("Provide at least one covariate via {.arg ...} or {.arg data}.")
    }
    if (is.null(names(covars)) || any(!nzchar(names(covars)))) {
      cli::cli_abort("All covariates passed via {.arg ...} must be named.")
    }
  }

  if (length(treated) == 0L) cli::cli_abort("{.arg treated} must be non-empty.")
  treated <- as.integer(as.logical(treated))
  if (anyNA(treated)) cli::cli_abort("{.arg treated} contains {.val NA}.")
  if (!all(treated %in% 0:1)) cli::cli_abort("{.arg treated} must coerce to 0/1.")
  if (length(unique(treated)) < 2L) {
    cli::cli_abort("{.arg treated} must contain both 0 and 1 values.")
  }
  n_check <- length(treated)
  bad_len <- vapply(covars, function(x) length(x) != n_check, logical(1))
  if (any(bad_len)) {
    cli::cli_abort("All covariates must have length {.val {n_check}}.")
  }

  rows <- list()
  for (nm in names(covars)) {
    x <- covars[[nm]]
    if (is.factor(x) || (is.character(x) && !is.numeric(x))) {
      x <- as.factor(x)
      lvls <- levels(x)
      if (length(lvls) < 2L) next
      # Chi-square p once on the factor, applied to every level row.
      tab <- suppressWarnings(stats::chisq.test(x, treated))
      p_chi <- tab$p.value
      for (lv in lvls[-1L]) {
        ind <- as.integer(x == lv)
        rows[[length(rows) + 1L]] <- .balance_row(
          paste0(nm, ":", lv), ind, treated, threshold,
          p_value = p_chi, test = "chi-squared"
        )
      }
    } else {
      rows[[length(rows) + 1L]] <- .balance_row(
        nm, x, treated, threshold, p_value = NA_real_, test = "welch_t"
      )
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "threshold") <- threshold
  class(out) <- c("mb_balance_table", "data.frame")
  out
}

# Internal: build a single row of the balance table.
.balance_row <- function(name, x, treated, threshold,
                         p_value = NA_real_, test = "welch_t") {
  x <- as.numeric(x)
  if (anyNA(x)) {
    keep <- !is.na(x)
    x <- x[keep]; treated <- treated[keep]
  }
  xt <- x[treated == 1L]; xc <- x[treated == 0L]
  m_t <- mean(xt); m_c <- mean(xc)
  s_t <- stats::sd(xt); s_c <- stats::sd(xc)
  pooled_sd <- sqrt((s_t^2 + s_c^2) / 2)
  smd <- if (is.finite(pooled_sd) && pooled_sd > 0) (m_t - m_c) / pooled_sd else NA_real_

  if (test == "welch_t" && is.na(p_value)) {
    p_value <- tryCatch(
      stats::t.test(xt, xc, var.equal = FALSE)$p.value,
      error = function(e) NA_real_
    )
  }

  data.frame(
    covariate    = name,
    mean_treated = m_t,
    mean_control = m_c,
    sd_treated   = s_t,
    sd_control   = s_c,
    n_treated    = length(xt),
    n_control    = length(xc),
    smd          = smd,
    p_value      = p_value,
    imbalanced   = !is.na(smd) & abs(smd) >= threshold,
    stringsAsFactors = FALSE
  )
}
