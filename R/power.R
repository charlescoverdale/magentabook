#' Power for a two-sample test
#'
#' Computes statistical power for a two-sample test of equal-sized
#' arms, using the large-sample normal approximation. Supports tests
#' of two means (with a common standard deviation) or two
#' proportions (using Cohen's `h` arcsine effect size).
#'
#' @param n_per_group Numeric. Sample size per arm.
#' @param effect_size Numeric. The standardised effect size:
#'   Cohen's `d` for `type = "mean"`, or Cohen's `h` for
#'   `type = "proportion"` (computed automatically if `p1` and `p2`
#'   are supplied).
#' @param sd Numeric. Standard deviation, used only for `type =
#'   "mean"`. Default `1`, in which case `effect_size` is interpreted
#'   in standard deviation units.
#' @param alpha Numeric in `(0, 1)`. Significance level. Default
#'   `0.05`.
#' @param sides Integer. `2` (two-sided, default) or `1` (one-sided).
#' @param type Character. `"mean"` (default) or `"proportion"`.
#' @param p1,p2 Optional numeric in `(0, 1)`. If both supplied (and
#'   `type = "proportion"`), the function computes Cohen's `h` and
#'   ignores `effect_size`.
#'
#' @return Numeric scalar in `(0, 1)`: the power.
#'
#' @details
#' For two means, power is
#' \deqn{1 - \Phi(z_{1-\alpha/s} - d\sqrt{n/2}) + \Phi(-z_{1-\alpha/s} - d\sqrt{n/2})}
#' where \eqn{s} is `sides` and \eqn{d} is the standardised effect.
#' For two proportions, the effect uses the arcsine variance-stabilising
#' transform: \eqn{h = 2\arcsin\sqrt{p_1} - 2\arcsin\sqrt{p_2}}.
#'
#' Approximation note: this implementation uses the large-sample
#' normal approximation. The standard alternative (used by
#' \pkg{pwr}::\code{\link[pwr]{pwr.t.test}}) uses the noncentral
#' t-distribution. For typical evaluation sample sizes
#' (`n_per_group >= 50`) the two agree to within 1-2 percentage
#' points of power; for `n_per_group < 30` the discrepancy is
#' larger and \pkg{pwr} should be preferred. magentabook ships
#' equivalence tests against \pkg{pwr} (see
#' `tests/testthat/test-pwr-equivalence.R`).
#'
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the
#' Behavioral Sciences (2nd ed.). Lawrence Erlbaum.
#'
#' Champely, S. (2020). pwr: Basic Functions for Power Analysis.
#' R package version 1.3-0.
#'
#' HM Treasury (2020). The Magenta Book, chapter on impact
#' evaluation, section on power analysis.
#'
#' @family power
#' @seealso [mb_mde()], [mb_sample_size()], [mb_cluster_design()].
#'
#' @export
#' @examples
#' mb_power(n_per_group = 200, effect_size = 0.3)
#' mb_power(n_per_group = 500, type = "proportion", p1 = 0.40, p2 = 0.50)
mb_power <- function(n_per_group,
                     effect_size = NULL,
                     sd = 1,
                     alpha = 0.05,
                     sides = 2L,
                     type = c("mean", "proportion"),
                     p1 = NULL,
                     p2 = NULL) {
  type <- match.arg(type)
  validate_numeric(n_per_group, arg = "n_per_group", require_positive = TRUE)
  validate_scalar(n_per_group,  arg = "n_per_group")
  validate_proportion(alpha,    arg = "alpha", strict = TRUE)
  if (!sides %in% c(1L, 2L)) cli::cli_abort("{.arg sides} must be 1 or 2.")

  if (type == "proportion" && !is.null(p1) && !is.null(p2)) {
    validate_proportion(p1, arg = "p1", strict = TRUE)
    validate_proportion(p2, arg = "p2", strict = TRUE)
    effect_size <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  } else {
    if (is.null(effect_size)) {
      cli::cli_abort("Provide {.arg effect_size}, or {.arg p1} and {.arg p2} for proportions.")
    }
    validate_numeric(effect_size, arg = "effect_size")
    validate_scalar(effect_size,  arg = "effect_size")
  }

  d <- if (type == "mean") effect_size / sd else effect_size
  z_crit <- stats::qnorm(1 - alpha / sides)
  ncp    <- abs(d) * sqrt(n_per_group / 2)
  if (sides == 2L) {
    1 - stats::pnorm(z_crit - ncp) + stats::pnorm(-z_crit - ncp)
  } else {
    1 - stats::pnorm(z_crit - ncp)
  }
}

#' Minimum detectable effect (MDE)
#'
#' Inverts [mb_power()]: given a sample size, target power, and
#' significance level, returns the smallest effect size the design
#' can reliably detect.
#'
#' @inheritParams mb_power
#' @param power Numeric in `(0, 1)`. Target power. Default `0.8`.
#' @param baseline Optional numeric in `(0, 1)`. For
#'   `type = "proportion"`, the baseline proportion `p1` against
#'   which the MDE is calculated. The MDE is then returned in
#'   absolute proportion-point units.
#'
#' @return Numeric scalar. The minimum detectable effect in the
#'   units implied by `type`: standard deviation units (`type =
#'   "mean"`, with `sd = 1`) or absolute proportion-point difference
#'   (`type = "proportion"` with `baseline` supplied), or Cohen's `h`
#'   (`type = "proportion"` without `baseline`).
#'
#' @family power
#' @seealso [mb_power()], [mb_sample_size()].
#'
#' @export
#' @examples
#' mb_mde(n_per_group = 200)
#' mb_mde(n_per_group = 500, type = "proportion", baseline = 0.4)
mb_mde <- function(n_per_group,
                   sd = 1,
                   power = 0.8,
                   alpha = 0.05,
                   sides = 2L,
                   type = c("mean", "proportion"),
                   baseline = NULL) {
  type <- match.arg(type)
  validate_numeric(n_per_group, arg = "n_per_group", require_positive = TRUE)
  validate_scalar(n_per_group,  arg = "n_per_group")
  validate_proportion(power, arg = "power", strict = TRUE)
  validate_proportion(alpha, arg = "alpha", strict = TRUE)
  if (!sides %in% c(1L, 2L)) cli::cli_abort("{.arg sides} must be 1 or 2.")

  z_crit <- stats::qnorm(1 - alpha / sides)
  z_pow  <- stats::qnorm(power)
  d      <- (z_crit + z_pow) * sqrt(2 / n_per_group)

  if (type == "mean") {
    return(d * sd)
  }
  # type = "proportion": d is Cohen's h. Convert to proportion-point
  # difference at the supplied baseline if provided.
  if (is.null(baseline)) return(d)
  validate_proportion(baseline, arg = "baseline", strict = TRUE)
  # Solve h = 2*asin(sqrt(p2)) - 2*asin(sqrt(p1)) for p2.
  p2 <- sin((d + 2 * asin(sqrt(baseline))) / 2)^2
  p2 - baseline
}

#' Required sample size for a target power
#'
#' Given a target effect size, power, and significance level,
#' returns the required sample size per arm. Inverts [mb_power()].
#'
#' @inheritParams mb_power
#' @param power Numeric in `(0, 1)`. Target power. Default `0.8`.
#'
#' @return Integer scalar. Sample size per arm (rounded up).
#'
#' @family power
#' @seealso [mb_power()], [mb_mde()], [mb_cluster_design()].
#'
#' @export
#' @examples
#' mb_sample_size(effect_size = 0.3, power = 0.8)
#' mb_sample_size(type = "proportion", p1 = 0.40, p2 = 0.50, power = 0.8)
mb_sample_size <- function(effect_size = NULL,
                           sd = 1,
                           power = 0.8,
                           alpha = 0.05,
                           sides = 2L,
                           type = c("mean", "proportion"),
                           p1 = NULL,
                           p2 = NULL) {
  type <- match.arg(type)
  validate_proportion(power, arg = "power", strict = TRUE)
  validate_proportion(alpha, arg = "alpha", strict = TRUE)
  if (!sides %in% c(1L, 2L)) cli::cli_abort("{.arg sides} must be 1 or 2.")

  if (type == "proportion" && !is.null(p1) && !is.null(p2)) {
    validate_proportion(p1, arg = "p1", strict = TRUE)
    validate_proportion(p2, arg = "p2", strict = TRUE)
    effect_size <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  } else {
    if (is.null(effect_size)) {
      cli::cli_abort("Provide {.arg effect_size}, or {.arg p1} and {.arg p2} for proportions.")
    }
    validate_numeric(effect_size, arg = "effect_size")
    validate_scalar(effect_size,  arg = "effect_size")
  }
  d <- if (type == "mean") effect_size / sd else effect_size
  if (d == 0) {
    cli::cli_abort("Effect size of zero requires infinite sample size.")
  }
  z_crit <- stats::qnorm(1 - alpha / sides)
  z_pow  <- stats::qnorm(power)
  n      <- 2 * ((z_crit + z_pow) / abs(d))^2
  ceiling(n)
}

#' Cluster-RCT design effect
#'
#' Computes the design effect (DEFF) for a parallel cluster
#' randomised trial: how much the variance of the treatment effect
#' inflates relative to an individually-randomised design with the
#' same total sample size, due to within-cluster correlation.
#'
#' @param individuals_per_cluster Numeric. Number of individuals
#'   sampled per cluster (`m`).
#' @param icc Numeric in `[0, 1]`. Intra-class correlation
#'   coefficient. Use [mb_icc_reference()] for plausible values.
#' @param n_clusters Optional numeric. Number of clusters per arm.
#'   If supplied, returns effective sample size per arm in addition
#'   to the design effect.
#'
#' @return A list with elements `deff` and (if `n_clusters`
#'   supplied) `n_total_per_arm` and `n_effective_per_arm`.
#'
#' @details
#' \deqn{\text{DEFF} = 1 + (m - 1) \, \rho}
#' where `m` is the cluster size and `rho` is the ICC. The
#' effective sample size for power is `n_total / DEFF`.
#'
#' Standard reference values for `rho` across UK policy domains
#' are bundled and accessible via [mb_icc_reference()].
#'
#' @references
#' Donner, A., Klar, N. (2000). Design and Analysis of Cluster
#' Randomization Trials in Health Research. Arnold.
#'
#' Hedges, L. V., Hedberg, E. C. (2007). Intraclass Correlation
#' Values for Planning Group-Randomized Trials in Education.
#' Educational Evaluation and Policy Analysis 29(1).
#'
#' @family power
#' @seealso [mb_icc_reference()], [mb_stepped_wedge()],
#'   [mb_sample_size()].
#'
#' @export
#' @examples
#' mb_cluster_design(individuals_per_cluster = 30, icc = 0.05)
#' mb_cluster_design(individuals_per_cluster = 30, icc = 0.05, n_clusters = 20)
mb_cluster_design <- function(individuals_per_cluster, icc, n_clusters = NULL) {
  validate_numeric(individuals_per_cluster, arg = "individuals_per_cluster",
                   require_positive = TRUE)
  validate_scalar(individuals_per_cluster,  arg = "individuals_per_cluster")
  validate_proportion(icc, arg = "icc")
  validate_scalar(icc,     arg = "icc")
  if (!is.null(n_clusters)) {
    validate_numeric(n_clusters, arg = "n_clusters", require_positive = TRUE)
    validate_scalar(n_clusters,  arg = "n_clusters")
  }
  deff <- 1 + (individuals_per_cluster - 1) * icc
  out <- list(deff = deff)
  if (!is.null(n_clusters)) {
    out$n_total_per_arm     <- n_clusters * individuals_per_cluster
    out$n_effective_per_arm <- out$n_total_per_arm / deff
  }
  out
}

#' Stepped-wedge design effect
#'
#' Computes the design effect for a stepped-wedge cluster
#' randomised trial relative to an individually-randomised parallel
#' design with the same total observations.
#'
#' @param steps Integer. Number of measurement periods (also called
#'   `T`). Includes the baseline.
#' @param clusters_per_step Numeric. Number of clusters that
#'   crossover at each step.
#' @param individuals_per_cluster Numeric. Individuals measured per
#'   cluster per period.
#' @param icc Numeric in `[0, 1]`. Intra-class correlation
#'   coefficient.
#' @param formula Character scalar. One of `"hemming"` (default,
#'   the closed form of Woertman et al. 2013 and Hemming et al.
#'   2015) or `"hussey_hughes"` (the Hussey-Hughes 2007 closed
#'   form). See Details.
#'
#' @return A list with elements `deff_cluster` (the within-period
#'   cluster design effect), `correction_factor` (the stepped-wedge
#'   correction relative to a parallel cluster RCT), `deff_sw` (the
#'   product), `n_total` (total observations across the trial), and
#'   `formula` (which closed form was used).
#'
#' @details
#' Both implemented forms assume a balanced design: equal cluster
#' size, equal-period intervals, complete data, no time effects
#' beyond a common period mean, and one outcome measurement per
#' cluster-period. For non-standard designs use the
#' \pkg{swCRTdesign} package or the Hooper-Bourke calculator.
#'
#' **Hemming/Woertman form** (`formula = "hemming"`, default):
#' \deqn{\text{DEFF}_c = 1 + (mT - 1)\rho}
#' \deqn{\text{CF}_{\text{Hem}} = \frac{3(1-\rho)}{2T(1 - 1/T^2)}}
#' \deqn{\text{DEFF}_{sw} = \text{DEFF}_c \cdot \text{CF}_{\text{Hem}}}
#'
#' **Hussey-Hughes form** (`formula = "hussey_hughes"`): replaces
#' the Hemming correction factor with the Hussey-Hughes (2007)
#' closed form for a balanced stepped wedge with one cluster
#' crossing over per step, derived from their equation (7):
#' \deqn{\text{CF}_{\text{HH}} = \frac{T(1-\rho)}{(T-1)(T+1)/3 \cdot (1 + (mT-1)\rho/T)}}
#' This form makes the within-cluster correlation structure
#' explicit and is preferred by Hemming et al. (2015) when `rho` is
#' large or the number of steps is small. The two forms agree to
#' within ~5 percent for moderate `rho` (`<= 0.1`) and more than
#' four steps; they diverge for high `rho` or short trials.
#'
#' For research-grade stepped-wedge designs (variable cluster
#' size, missing data, time-by-treatment interactions, decay of
#' within-cluster correlation), use `swCRTdesign::swPwr` or
#' `clusterPower::cps.sw.binary`.
#'
#' @references
#' Hussey, M. A., Hughes, J. P. (2007). Design and analysis of
#' stepped wedge cluster randomized trials. Contemporary Clinical
#' Trials 28.
#'
#' Woertman, W., de Hoop, E., Moerbeek, M., Zuidema, S. U.,
#' Gerritsen, D. L., Teerenstra, S. (2013). Stepped wedge designs
#' could reduce the required sample size in cluster randomized
#' trials. Journal of Clinical Epidemiology 66.
#'
#' Hemming, K., Haines, T. P., Chilton, P. J., Girling, A. J.,
#' Lilford, R. J. (2015). The stepped wedge cluster randomised
#' trial: rationale, design, analysis, and reporting. BMJ 350.
#'
#' @family power
#' @seealso [mb_cluster_design()], [mb_icc_reference()].
#'
#' @export
#' @examples
#' mb_stepped_wedge(
#'   steps = 5,
#'   clusters_per_step = 4,
#'   individuals_per_cluster = 20,
#'   icc = 0.05
#' )
mb_stepped_wedge <- function(steps,
                             clusters_per_step,
                             individuals_per_cluster,
                             icc,
                             formula = c("hemming", "hussey_hughes")) {
  formula <- match.arg(formula)
  validate_numeric(steps, arg = "steps", require_positive = TRUE)
  validate_scalar(steps,  arg = "steps")
  if (steps < 2) cli::cli_abort("{.arg steps} must be at least 2.")
  validate_numeric(clusters_per_step, arg = "clusters_per_step",
                   require_positive = TRUE)
  validate_scalar(clusters_per_step,  arg = "clusters_per_step")
  validate_numeric(individuals_per_cluster,
                   arg = "individuals_per_cluster", require_positive = TRUE)
  validate_scalar(individuals_per_cluster,
                   arg = "individuals_per_cluster")
  validate_proportion(icc, arg = "icc")
  validate_scalar(icc,     arg = "icc")

  T_steps <- steps
  m       <- individuals_per_cluster
  rho     <- icc

  deff_cluster <- 1 + (m * T_steps - 1) * rho
  correction_factor <- if (formula == "hemming") {
    3 * (1 - rho) / (2 * T_steps * (1 - 1 / T_steps^2))
  } else {
    # Hussey-Hughes (2007) closed-form correction factor for a
    # balanced stepped wedge with one cluster crossing per step,
    # equal cluster size, complete data, no time-by-treatment
    # interaction. Derived from Hussey & Hughes (2007) eq. (7).
    T_steps * (1 - rho) /
      ((T_steps - 1) * (T_steps + 1) / 3 *
         (1 + (m * T_steps - 1) * rho / T_steps))
  }
  deff_sw           <- deff_cluster * correction_factor
  n_total           <- T_steps * clusters_per_step * m

  list(
    deff_cluster      = deff_cluster,
    correction_factor = correction_factor,
    deff_sw           = deff_sw,
    formula           = formula,
    n_total           = n_total
  )
}

#' Reference intra-class correlation values
#'
#' Returns bundled reference ICC values for common UK policy
#' domains and units of clustering. Use these for evaluation
#' planning when domain-specific baseline data are not available.
#'
#' @param domain Optional character scalar. One of `"education"`,
#'   `"health"`, `"employment"`, `"local_government"`,
#'   `"criminal_justice"`, `"housing"`. If `NULL` (default),
#'   returns the entire reference table.
#'
#' @return A data frame with columns `domain`, `outcome`,
#'   `unit_of_clustering`, `icc_low`, `icc_central`, `icc_high`,
#'   `value_source`, `source`, `notes`.
#'
#' @details
#' Values are *reference* ICCs for planning purposes only.
#' Wherever feasible, evaluators should compute domain-specific
#' ICCs from baseline data before finalising sample size
#' calculations.
#'
#' Each row carries a `value_source` flag:
#' - `"table_quote"`: direct extraction of a specific row or value
#'   from a published table (cited table number in the `source`
#'   field).
#' - `"central_estimate"`: researcher synthesis of a plausible
#'   central value within the published range, used as a
#'   practitioner default in the absence of domain-specific
#'   baseline data.
#'
#' At v0.1.0 every bundled row is `central_estimate`. Future
#' versions will upgrade individual rows to `table_quote` as exact
#' table-level citations are added. Treat the bundled values as a
#' planning prior; verify against your own baseline ICC before
#' relying on them in a published power calculation.
#'
#' @references
#' Hedges, L. V., Hedberg, E. C. (2007). Educational Evaluation
#' and Policy Analysis 29(1).
#'
#' Adams, G., Gulliford, M. C., Ukoumunne, O. C., Eldridge, S.,
#' Chinn, S., Campbell, M. J. (2004). Patterns of intra-cluster
#' correlation from primary care research. Statistics in
#' Medicine 23.
#'
#' @family power
#' @seealso [mb_cluster_design()], [mb_stepped_wedge()].
#'
#' @export
#' @examples
#' mb_icc_reference()
#' mb_icc_reference("education")
mb_icc_reference <- function(domain = NULL) {
  tbl <- .read_icc_reference()
  if (is.null(domain)) return(tbl)
  validate_character(domain, arg = "domain")
  validate_scalar(domain,    arg = "domain")
  valid <- unique(tbl$domain)
  if (!domain %in% valid) {
    cli::cli_abort("{.arg domain} must be one of {.val {valid}}.")
  }
  tbl[tbl$domain == domain, , drop = FALSE]
}
