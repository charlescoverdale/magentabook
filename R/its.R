#' Interrupted time series via segmented regression
#'
#' Fits a single-group interrupted time series model:
#' \deqn{Y_t = \beta_0 + \beta_1 t + \beta_2 P_t + \beta_3 (t - t^*) P_t + \epsilon_t}
#' where `P_t` is 1 for `t >= t*` and `t*` is the intervention time.
#' `beta_2` is the immediate level change at the intervention;
#' `beta_3` is the change in slope.
#'
#' @param y Numeric vector of outcomes ordered by `time`.
#' @param time Numeric vector of time indices, same length as `y`.
#' @param intervention_time Numeric scalar. The first time point
#'   considered post-intervention.
#' @param lag Integer >= 0. Number of pre-intervention observations
#'   to drop near the intervention (transition period). Default `0`.
#'
#' @return An `mb_its` object: a list with `coefficients` (named
#'   numeric), `se` (named numeric), `level_change`, `slope_change`,
#'   `intervention_time`, `n`, `n_pre`, `n_post`, and `vintage`.
#'
#' @details
#' Segmented regression assumes residuals are independent. For
#' autocorrelated series, fit a Newey-West, Prais-Winsten, or
#' ARIMA-error specification using \pkg{sandwich}, \pkg{nlme}, or
#' \pkg{forecast}. This function is the canonical baseline.
#'
#' @references
#' Bernal, J. L., Cummins, S., Gasparrini, A. (2017). Interrupted
#' time series regression for the evaluation of public health
#' interventions: a tutorial. International Journal of
#' Epidemiology 46(1).
#'
#' Wagner, A. K., Soumerai, S. B., Zhang, F., Ross-Degnan, D.
#' (2002). Segmented regression analysis of interrupted time series
#' studies in medication use research. Journal of Clinical Pharmacy
#' and Therapeutics 27.
#'
#' @family estimators
#' @seealso [mb_did_2x2()], [mb_event_study()].
#'
#' @export
#' @examples
#' set.seed(2)
#' time <- 1:48
#' y    <- 10 + 0.05 * time + ifelse(time >= 25, 2 + 0.1 * (time - 25), 0) + rnorm(48, sd = 0.5)
#' mb_its(y, time, intervention_time = 25)
mb_its <- function(y, time, intervention_time, lag = 0L) {
  validate_numeric(y,    arg = "y")
  validate_numeric(time, arg = "time")
  if (length(y) != length(time)) {
    cli::cli_abort("{.arg y} and {.arg time} must be the same length.")
  }
  validate_numeric(intervention_time, arg = "intervention_time")
  validate_scalar(intervention_time,  arg = "intervention_time")
  validate_numeric(lag, arg = "lag", require_non_negative = TRUE)
  validate_scalar(lag,  arg = "lag")
  if (lag != as.integer(lag)) cli::cli_abort("{.arg lag} must be a non-negative integer.")

  P  <- as.numeric(time >= intervention_time)
  t_post <- pmax(time - intervention_time, 0) * P
  if (lag > 0L) {
    drop_idx <- which(time >= (intervention_time - lag) & time < intervention_time)
    if (length(drop_idx)) {
      y      <- y[-drop_idx]
      time   <- time[-drop_idx]
      P      <- P[-drop_idx]
      t_post <- t_post[-drop_idx]
    }
  }

  X <- cbind(1, time, P, t_post)
  colnames(X) <- c("(Intercept)", "time", "post", "post_time")
  XtX_inv <- tryCatch(
    solve(crossprod(X)),
    error = function(e) cli::cli_abort("Design matrix is singular; ensure both pre- and post-intervention observations are present.")
  )
  beta <- drop(XtX_inv %*% crossprod(X, y))
  resid <- y - drop(X %*% beta)
  n <- length(y); k <- ncol(X)
  sigma2 <- sum(resid^2) / (n - k)
  se     <- sqrt(diag(sigma2 * XtX_inv))
  names(beta) <- names(se) <- colnames(X)

  out <- list(
    coefficients      = beta,
    se                = se,
    level_change      = unname(beta["post"]),
    slope_change      = unname(beta["post_time"]),
    intervention_time = intervention_time,
    n                 = n,
    n_pre             = sum(P == 0),
    n_post            = sum(P == 1),
    vintage           = .mb_vintage()
  )
  class(out) <- c("mb_its", "list")
  out
}
