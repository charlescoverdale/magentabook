#' Canonical 2x2 difference-in-differences estimator
#'
#' Returns the simple two-period, two-group DiD estimate of an
#' average treatment effect on the treated, with optional
#' cluster-robust standard errors.
#'
#' @param y Numeric vector of outcomes.
#' @param treated Logical or 0/1 numeric vector. `TRUE` / `1` if
#'   the unit is in the treated group, regardless of period.
#' @param post Logical or 0/1 numeric vector. `TRUE` / `1` if the
#'   observation is in the post-treatment period.
#' @param cluster Optional vector identifying clusters for
#'   cluster-robust standard errors (CR1 with finite-sample
#'   correction). If `NULL`, conventional OLS SEs are returned.
#' @param alpha Numeric in `(0, 1)`. Significance level for the
#'   confidence interval. Default `0.05`.
#' @param quiet Logical. If `FALSE` (default), the print method
#'   appends a one-line reminder that this is a canonical 2x2 DiD
#'   and points to specialist tooling for staggered adoption or
#'   heterogeneous treatment effects. Set to `TRUE` to suppress.
#'
#' @return An `mb_did` object: a list with `estimate`, `se`,
#'   `t_stat`, `p_value`, `ci_low`, `ci_high`, group means,
#'   `cluster_robust`, `n`, `quiet`, and `vintage`.
#'
#' @details
#' Computes
#' \deqn{\hat{\tau} = (\bar{Y}_{T,1} - \bar{Y}_{T,0}) - (\bar{Y}_{C,1} - \bar{Y}_{C,0})}
#' which equals the coefficient on the `treated:post` interaction in
#' \eqn{Y = \beta_0 + \beta_1 T + \beta_2 P + \tau (T \times P) + \epsilon}.
#'
#' Cluster-robust SEs use the CR1 sandwich estimator with
#' finite-sample correction \eqn{(G/(G-1)) \cdot (N-1)/(N-K)}, where
#' \eqn{G} is the number of clusters, \eqn{N} the number of
#' observations, and \eqn{K} the number of regressors (4).
#'
#' For staggered adoption, heterogeneous treatment effects, or
#' production estimation, use \pkg{fixest}, \pkg{did}, or
#' \pkg{Synth}. This function is for the canonical 2x2 case only.
#'
#' @references
#' Card, D., Krueger, A. B. (1994). Minimum Wages and Employment:
#' A Case Study of the Fast-Food Industry in New Jersey and
#' Pennsylvania. American Economic Review 84(4).
#'
#' Cameron, A. C., Miller, D. L. (2015). A Practitioner's Guide to
#' Cluster-Robust Inference. Journal of Human Resources 50(2).
#'
#' @family estimators
#' @seealso [mb_its()], [mb_event_study()].
#'
#' @export
#' @examples
#' set.seed(1)
#' n <- 400
#' treated <- rep(c(0, 1), each = n / 2)
#' post    <- rep(c(0, 1), times = n / 2)
#' y       <- 0.5 * treated + 0.2 * post + 0.4 * treated * post + rnorm(n)
#' mb_did_2x2(y, treated, post)
mb_did_2x2 <- function(y, treated, post, cluster = NULL, alpha = 0.05,
                       quiet = FALSE) {
  validate_numeric(y, arg = "y")
  if (length(treated) != length(y)) {
    cli::cli_abort("{.arg treated} must be the same length as {.arg y}.")
  }
  if (length(post) != length(y)) {
    cli::cli_abort("{.arg post} must be the same length as {.arg y}.")
  }
  validate_proportion(alpha, arg = "alpha", strict = TRUE)
  treated <- as.numeric(as.logical(treated))
  post    <- as.numeric(as.logical(post))
  if (anyNA(treated) || anyNA(post)) {
    cli::cli_abort("{.arg treated} and {.arg post} must be logical or 0/1 numeric without NA.")
  }
  if (!all(treated %in% 0:1)) cli::cli_abort("{.arg treated} must coerce to 0/1.")
  if (!all(post    %in% 0:1)) cli::cli_abort("{.arg post} must coerce to 0/1.")

  n <- length(y)
  X <- cbind(1, treated, post, treated * post)
  colnames(X) <- c("(Intercept)", "treated", "post", "treated:post")

  XtX_inv <- tryCatch(
    solve(crossprod(X)),
    error = function(e) cli::cli_abort("Design matrix is singular; check that all four cells are populated.")
  )
  beta <- drop(XtX_inv %*% crossprod(X, y))
  resid <- y - drop(X %*% beta)
  k <- ncol(X)

  if (is.null(cluster)) {
    sigma2  <- sum(resid^2) / (n - k)
    vcov_b  <- sigma2 * XtX_inv
    cluster_robust <- FALSE
  } else {
    if (length(cluster) != n) {
      cli::cli_abort("{.arg cluster} must have length {.val {n}}.")
    }
    g <- as.factor(cluster)
    G <- nlevels(g)
    if (G < 2L) cli::cli_abort("Need at least 2 clusters for cluster-robust SEs.")
    meat <- matrix(0, nrow = k, ncol = k)
    for (lvl in levels(g)) {
      idx <- which(g == lvl)
      X_g <- X[idx, , drop = FALSE]
      u_g <- resid[idx]
      Xu  <- crossprod(X_g, u_g)
      meat <- meat + tcrossprod(Xu)
    }
    correction <- (G / (G - 1)) * ((n - 1) / (n - k))
    vcov_b <- correction * XtX_inv %*% meat %*% XtX_inv
    cluster_robust <- TRUE
  }

  estimate <- unname(beta[4])
  se       <- sqrt(unname(vcov_b[4, 4]))
  t_stat   <- estimate / se
  df       <- if (cluster_robust) (length(unique(cluster)) - 1L) else (n - k)
  p_value  <- 2 * (1 - stats::pt(abs(t_stat), df = df))
  cv       <- stats::qt(1 - alpha / 2, df = df)
  ci_low   <- estimate - cv * se
  ci_high  <- estimate + cv * se

  group_means <- c(
    control_pre   = mean(y[treated == 0 & post == 0]),
    control_post  = mean(y[treated == 0 & post == 1]),
    treated_pre   = mean(y[treated == 1 & post == 0]),
    treated_post  = mean(y[treated == 1 & post == 1])
  )

  out <- list(
    estimate       = estimate,
    se             = se,
    t_stat         = t_stat,
    p_value        = p_value,
    ci_low         = ci_low,
    ci_high        = ci_high,
    df             = df,
    group_means    = group_means,
    cluster_robust = cluster_robust,
    n              = n,
    quiet          = isTRUE(quiet),
    vintage        = .mb_vintage()
  )
  class(out) <- c("mb_did", "list")
  out
}
