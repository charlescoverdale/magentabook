#' Simple event-study coefficients
#'
#' Estimates a panel event-study with unit and time fixed effects
#' and event-time dummies. Treatment time is fixed across treated
#' units (no staggered adoption). Returns coefficients for `leads`
#' periods before and `lags` periods after treatment, with the
#' period immediately before treatment (`event_time = -1`) omitted
#' as the reference category.
#'
#' @param y Numeric vector of outcomes.
#' @param unit Vector identifying units (panel ID).
#' @param time Numeric vector of time indices.
#' @param treatment_time Numeric scalar. The first treated period.
#'   Units with `treated = 0` (never-treated) are pure controls.
#' @param treated Logical or 0/1 numeric vector indicating
#'   whether each observation belongs to a treated unit. The design
#'   requires at least some never-treated control units; without
#'   them the event-time dummies are collinear with the time fixed
#'   effects.
#' @param leads Integer >= 0. Number of pre-treatment periods to
#'   include. Default `3`.
#' @param lags Integer >= 0. Number of post-treatment periods.
#'   Default `3`.
#'
#' @return An `mb_event_study` object: a list with `event_time`,
#'   `estimate`, `se`, plus `n`, `n_units`, `n_periods`,
#'   `treatment_time`, and `vintage`.
#'
#' @details
#' Implements the canonical two-way fixed-effects event study:
#' \deqn{Y_{it} = \alpha_i + \gamma_t + \sum_{k \neq -1} \beta_k \mathbf{1}\{t - t^* = k, D_i = 1\} + \epsilon_{it}}
#'
#' For staggered adoption (units treated at different times), this
#' specification is biased under treatment-effect heterogeneity. Use
#' the heterogeneity-robust estimators of Callaway & Sant'Anna
#' (2021) or de Chaisemartin & D'Haultfoeuille (2020), available in
#' the \pkg{did}, \pkg{didimputation}, or \pkg{fixest} packages
#' (`fixest::feols` with `sunab()`).
#'
#' Standard errors are conventional OLS; for clustered inference
#' use \pkg{sandwich} or \pkg{fixest}.
#'
#' @references
#' Callaway, B., Sant'Anna, P. H. C. (2021). Difference-in-Differences
#' with Multiple Time Periods. Journal of Econometrics 225(2).
#'
#' de Chaisemartin, C., D'Haultfoeuille, X. (2020). Two-Way Fixed
#' Effects Estimators with Heterogeneous Treatment Effects. American
#' Economic Review 110(9).
#'
#' @family estimators
#' @seealso [mb_did_2x2()], [mb_its()].
#'
#' @export
#' @examples
#' set.seed(3)
#' n_units <- 50; n_periods <- 10; treat_time <- 6
#' panel <- expand.grid(unit = 1:n_units, time = 1:n_periods)
#' panel$treated <- as.integer(panel$unit <= 25)
#' panel$post    <- as.integer(panel$time >= treat_time)
#' panel$y <- 0.1 * panel$time + 0.5 * (panel$treated * panel$post) +
#'            rnorm(nrow(panel))
#' mb_event_study(
#'   y = panel$y, unit = panel$unit, time = panel$time,
#'   treatment_time = treat_time, treated = panel$treated,
#'   leads = 3, lags = 3
#' )
mb_event_study <- function(y, unit, time, treatment_time,
                           treated, leads = 3L, lags = 3L) {
  validate_numeric(y,    arg = "y")
  validate_numeric(time, arg = "time")
  if (length(unit) != length(y) || length(time) != length(y)) {
    cli::cli_abort("{.arg y}, {.arg unit}, and {.arg time} must be the same length.")
  }
  validate_numeric(treatment_time, arg = "treatment_time")
  validate_scalar(treatment_time,  arg = "treatment_time")
  validate_numeric(leads, arg = "leads", require_non_negative = TRUE)
  validate_scalar(leads,  arg = "leads")
  validate_numeric(lags,  arg = "lags",  require_non_negative = TRUE)
  validate_scalar(lags,   arg = "lags")
  if (leads != as.integer(leads)) cli::cli_abort("{.arg leads} must be a non-negative integer.")
  if (lags  != as.integer(lags))  cli::cli_abort("{.arg lags} must be a non-negative integer.")

  if (length(treated) != length(y)) {
    cli::cli_abort("{.arg treated} must be the same length as {.arg y}.")
  }
  treated <- as.integer(as.logical(treated))
  if (sum(treated == 0L) == 0L) {
    cli::cli_abort(
      "Need at least one never-treated control unit; otherwise event-time dummies are collinear with time fixed effects."
    )
  }

  event_time <- time - treatment_time
  ks <- setdiff(seq(-leads, lags), -1L)
  D_event <- vapply(ks, function(k) as.integer(event_time == k & treated == 1L), integer(length(y)))
  colnames(D_event) <- sprintf("k_%s", ks)

  unit_f <- as.factor(unit)
  time_f <- as.factor(time)

  X_unit <- stats::model.matrix(~ unit_f - 1)[, -1, drop = FALSE]
  X_time <- stats::model.matrix(~ time_f - 1)[, -1, drop = FALSE]
  X <- cbind(1, X_unit, X_time, D_event)

  XtX_inv <- tryCatch(
    solve(crossprod(X)),
    error = function(e) cli::cli_abort("Design matrix is singular. Check unit/time coverage and event-time grid.")
  )
  beta <- drop(XtX_inv %*% crossprod(X, y))
  resid <- y - drop(X %*% beta)
  n <- length(y); k <- ncol(X)
  sigma2 <- sum(resid^2) / max(n - k, 1L)
  vcov_b <- sigma2 * XtX_inv

  event_idx <- (k - length(ks) + 1L):k
  estimate <- unname(beta[event_idx])
  se       <- sqrt(diag(vcov_b)[event_idx])

  out <- list(
    event_time     = ks,
    estimate       = estimate,
    se             = se,
    n              = n,
    n_units        = nlevels(unit_f),
    n_periods      = nlevels(time_f),
    treatment_time = treatment_time,
    vintage        = .mb_vintage()
  )
  class(out) <- c("mb_event_study", "list")
  out
}
