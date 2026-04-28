#' Cost per unit of outcome
#'
#' Computes a simple cost-effectiveness ratio: total cost divided
#' by total outcomes delivered. Use [mb_icer()] for two-option
#' comparisons.
#'
#' @param cost Numeric scalar or vector. Total cost (or per-period
#'   costs that will be summed).
#' @param effect Numeric scalar or vector. Total outcomes delivered
#'   (or per-period outcomes that will be summed).
#' @param label Optional character scalar. Name of the option.
#'
#' @return An `mb_cea` object.
#'
#' @family cost-effectiveness
#' @seealso [mb_icer()], [mb_ceac()], [mb_inb()].
#'
#' @export
#' @examples
#' mb_cea(cost = 1e6, effect = 250, label = "Workshop programme")
mb_cea <- function(cost, effect, label = NULL) {
  validate_numeric(cost,   arg = "cost")
  validate_numeric(effect, arg = "effect")
  if (!is.null(label)) {
    validate_character(label, arg = "label")
    validate_scalar(label,    arg = "label")
  }
  total_cost   <- sum(cost)
  total_effect <- sum(effect)
  if (total_effect == 0) {
    cli::cli_abort("Total {.arg effect} is zero; cost per unit is undefined.")
  }
  out <- list(
    label             = label,
    total_cost        = total_cost,
    total_effect      = total_effect,
    cost_per_unit     = total_cost / total_effect,
    vintage           = .mb_vintage()
  )
  class(out) <- c("mb_cea", "list")
  out
}

#' Incremental cost-effectiveness ratio with dominance handling
#'
#' Computes the ICER comparing option B to option A, with explicit
#' handling of the four dominance regions:
#' - **A dominates** B (B costs more, delivers less): no ICER.
#' - **B dominates** A (B costs less, delivers more): no ICER; B is
#'   the obvious choice.
#' - **B more costly, more effective**: standard ICER positive.
#' - **B less costly, less effective**: ICER negative — B saves
#'   money at the expense of effect.
#'
#' @param cost_a,effect_a Numeric scalars. Cost and effect of
#'   option A.
#' @param cost_b,effect_b Numeric scalars. Cost and effect of
#'   option B.
#' @param label_a,label_b Character scalars. Labels for the two
#'   options.
#'
#' @return An `mb_icer` object: a list with `delta_cost`,
#'   `delta_effect`, `icer`, `dominance` (one of `"a_dominates"`,
#'   `"b_dominates"`, `"b_more_costly_more_effective"`,
#'   `"b_less_costly_less_effective"`), and labels.
#'
#' @details
#' The ICER is the cost per additional unit of outcome from
#' switching from A to B:
#' \deqn{\text{ICER} = (C_B - C_A) / (E_B - E_A)}
#'
#' If `delta_effect` is zero, the ICER is reported as `Inf`
#' (when costs differ) or `NaN` (when costs are equal).
#'
#' @references
#' HM Treasury (2020). The Magenta Book, Annex A on cost-effectiveness.
#'
#' Drummond, M. F., Sculpher, M. J., Claxton, K., Stoddart, G. L.,
#' Torrance, G. W. (2015). Methods for the Economic Evaluation of
#' Health Care Programmes (4th ed.). Oxford University Press.
#'
#' @family cost-effectiveness
#' @seealso [mb_cea()], [mb_ceac()], [mb_inb()].
#'
#' @export
#' @examples
#' mb_icer(cost_a = 1e6, effect_a = 200, cost_b = 1.5e6, effect_b = 300,
#'         label_a = "Status quo", label_b = "Enhanced")
mb_icer <- function(cost_a, effect_a, cost_b, effect_b,
                    label_a = "A", label_b = "B") {
  validate_numeric(cost_a,   arg = "cost_a");   validate_scalar(cost_a,   "cost_a")
  validate_numeric(effect_a, arg = "effect_a"); validate_scalar(effect_a, "effect_a")
  validate_numeric(cost_b,   arg = "cost_b");   validate_scalar(cost_b,   "cost_b")
  validate_numeric(effect_b, arg = "effect_b"); validate_scalar(effect_b, "effect_b")
  validate_character(label_a, arg = "label_a"); validate_scalar(label_a, "label_a")
  validate_character(label_b, arg = "label_b"); validate_scalar(label_b, "label_b")

  delta_cost   <- cost_b - cost_a
  delta_effect <- effect_b - effect_a
  icer <- if (delta_effect == 0) {
    if (delta_cost == 0) NaN else Inf * sign(delta_cost)
  } else {
    delta_cost / delta_effect
  }
  dominance <- if (delta_cost <= 0 && delta_effect >= 0 &&
                   !(delta_cost == 0 && delta_effect == 0)) {
    "b_dominates"
  } else if (delta_cost >= 0 && delta_effect <= 0 &&
             !(delta_cost == 0 && delta_effect == 0)) {
    "a_dominates"
  } else if (delta_cost > 0 && delta_effect > 0) {
    "b_more_costly_more_effective"
  } else if (delta_cost < 0 && delta_effect < 0) {
    "b_less_costly_less_effective"
  } else {
    "no_difference"
  }

  out <- list(
    label_a       = label_a,
    label_b       = label_b,
    cost_a        = cost_a,
    cost_b        = cost_b,
    effect_a      = effect_a,
    effect_b      = effect_b,
    delta_cost    = delta_cost,
    delta_effect  = delta_effect,
    icer          = icer,
    dominance     = dominance,
    vintage       = .mb_vintage()
  )
  class(out) <- c("mb_icer", "list")
  out
}

#' Cost-effectiveness acceptability curve
#'
#' For a single A-vs-B comparison with sampled (`delta_cost`,
#' `delta_effect`) draws (e.g. from a probabilistic sensitivity
#' analysis), returns the probability that B is cost-effective at
#' each willingness-to-pay (WTP) value in `wtp_grid`.
#'
#' @param delta_cost Numeric vector. Sampled incremental costs of B
#'   relative to A.
#' @param delta_effect Numeric vector, same length as `delta_cost`.
#'   Sampled incremental effects.
#' @param wtp_grid Numeric vector of willingness-to-pay values
#'   (cost per unit of effect) at which to evaluate the curve.
#'
#' @return An `mb_ceac` object: a data-frame-like list with columns
#'   `wtp`, `prob_cost_effective`, plus `n_draws` and `vintage`.
#'
#' @details
#' At each WTP value `lambda`, B is cost-effective if the
#' incremental net benefit `lambda * delta_effect - delta_cost > 0`.
#' The CEAC is the proportion of draws for which this is true.
#'
#' @references
#' Fenwick, E., Claxton, K., Sculpher, M. (2001). Representing
#' uncertainty: the role of cost-effectiveness acceptability
#' curves. Health Economics 10(8).
#'
#' @family cost-effectiveness
#' @seealso [mb_inb()], [mb_icer()].
#'
#' @export
#' @examples
#' set.seed(4)
#' delta_cost   <- rnorm(1000, mean = 50000, sd = 10000)
#' delta_effect <- rnorm(1000, mean = 2,     sd = 0.5)
#' mb_ceac(delta_cost, delta_effect, wtp_grid = seq(0, 100000, by = 10000))
mb_ceac <- function(delta_cost, delta_effect, wtp_grid) {
  validate_numeric(delta_cost,   arg = "delta_cost")
  validate_numeric(delta_effect, arg = "delta_effect")
  validate_numeric(wtp_grid,     arg = "wtp_grid", require_non_negative = TRUE)
  if (length(delta_cost) != length(delta_effect)) {
    cli::cli_abort("{.arg delta_cost} and {.arg delta_effect} must be the same length.")
  }
  prob <- vapply(wtp_grid, function(lambda) {
    inb <- lambda * delta_effect - delta_cost
    mean(inb > 0)
  }, numeric(1))
  out <- list(
    wtp                 = wtp_grid,
    prob_cost_effective = prob,
    n_draws             = length(delta_cost),
    vintage             = .mb_vintage()
  )
  class(out) <- c("mb_ceac", "list")
  out
}

#' Incremental net benefit
#'
#' Computes the incremental net benefit (INB) of B over A at a
#' single willingness-to-pay threshold. Equivalent to the ICER
#' framing on a monetary scale.
#'
#' @param delta_cost Numeric scalar. Incremental cost of B over A.
#' @param delta_effect Numeric scalar. Incremental effect of B
#'   over A.
#' @param wtp Numeric scalar. Willingness-to-pay per unit of effect
#'   (e.g. the NICE QALY threshold in a health context).
#'
#' @return Numeric scalar. INB in the units of `delta_cost`. INB > 0
#'   means B is cost-effective at the supplied WTP.
#'
#' @details
#' \deqn{\text{INB} = \lambda \cdot \Delta E - \Delta C}
#' Equivalent to ICER comparison: INB > 0 iff ICER < WTP (when
#' effect change is positive).
#'
#' @family cost-effectiveness
#' @seealso [mb_ceac()], [mb_icer()].
#'
#' @export
#' @examples
#' mb_inb(delta_cost = 50000, delta_effect = 2, wtp = 30000)
mb_inb <- function(delta_cost, delta_effect, wtp) {
  validate_numeric(delta_cost,   arg = "delta_cost");   validate_scalar(delta_cost,   "delta_cost")
  validate_numeric(delta_effect, arg = "delta_effect"); validate_scalar(delta_effect, "delta_effect")
  validate_numeric(wtp, arg = "wtp", require_non_negative = TRUE)
  validate_scalar(wtp, arg = "wtp")
  wtp * delta_effect - delta_cost
}

#' Quality-adjusted life years (QALYs) accumulator
#'
#' Sums utility-weighted years lived across persons, with optional
#' annual discounting.
#'
#' @param utility Numeric scalar or vector in `[0, 1]`. Utility
#'   weight per year. Length 1 or `years`.
#' @param persons Numeric scalar. Number of persons. Default `1`.
#' @param years Numeric scalar. Number of years. Default `1`.
#' @param discount_rate Optional numeric in `[0, 1)`. Annual
#'   discount rate. If supplied, returns the discounted QALY total.
#'   Default `NULL` (undiscounted).
#'
#' @return Numeric scalar. Total QALYs.
#'
#' @details
#' Without discounting:
#' \deqn{\text{QALY} = \text{persons} \cdot \sum_{t=0}^{T-1} u_t}
#'
#' With annual discount rate `r`:
#' \deqn{\text{QALY} = \text{persons} \cdot \sum_{t=0}^{T-1} \frac{u_t}{(1+r)^t}}
#'
#' Compatible with `greenbook::gb_qaly`: when `utility` is scalar and
#' `discount_rate` is `NULL`, this returns `persons * utility * years`.
#'
#' @references
#' Drummond, M. F. et al. (2015). Methods for the Economic
#' Evaluation of Health Care Programmes (4th ed.). OUP.
#'
#' NICE (2022). Guide to the methods of technology appraisal.
#'
#' @family cost-effectiveness
#' @seealso [mb_daly()], [mb_cea()].
#'
#' @export
#' @examples
#' mb_qaly(utility = 0.8, persons = 100, years = 5)
#' mb_qaly(utility = 0.8, persons = 100, years = 5, discount_rate = 0.035)
#' mb_qaly(utility = c(0.5, 0.7, 0.9), persons = 50)
mb_qaly <- function(utility, persons = 1, years = 1, discount_rate = NULL) {
  validate_numeric(utility, arg = "utility")
  if (any(utility < 0 | utility > 1)) {
    cli::cli_abort("{.arg utility} must be between 0 and 1.")
  }
  validate_numeric(persons, arg = "persons", require_positive = TRUE)
  validate_scalar(persons,  arg = "persons")
  validate_numeric(years,   arg = "years",   require_positive = TRUE)
  validate_scalar(years,    arg = "years")

  if (length(utility) == 1L) {
    if (is.null(discount_rate)) {
      return(persons * utility * years)
    }
    utility <- rep(utility, ceiling(years))
  }
  if (length(utility) != as.integer(ceiling(years))) {
    if (length(utility) > 1L && years != 1) {
      cli::cli_abort("Length of {.arg utility} ({.val {length(utility)}}) must equal {.arg years} ({.val {years}}) when both > 1.")
    }
    years <- length(utility)
  }
  if (is.null(discount_rate)) {
    return(persons * sum(utility))
  }
  validate_proportion(discount_rate, arg = "discount_rate")
  validate_scalar(discount_rate,     arg = "discount_rate")
  if (discount_rate >= 1) cli::cli_abort("{.arg discount_rate} must be less than 1.")
  weights <- 1 / (1 + discount_rate)^seq.int(0, length(utility) - 1L)
  persons * sum(utility * weights)
}

#' Disability-adjusted life years (DALYs) accumulator
#'
#' Sums years lived with disability (YLD) and years of life lost
#' (YLL) across persons. DALY is the global-health analogue of
#' QALY: lower is better.
#'
#' @param yld Numeric scalar or vector. Years lived with disability
#'   per person.
#' @param yll Numeric scalar or vector. Years of life lost per
#'   person (e.g. life expectancy minus age at death).
#' @param persons Numeric scalar. Number of persons. Default `1`.
#'
#' @return Numeric scalar. Total DALYs (YLD + YLL summed across
#'   persons).
#'
#' @details
#' \deqn{\text{DALY} = \text{persons} \cdot \sum (YLD + YLL)}
#'
#' This implementation follows the Global Burden of Disease
#' definition. Age-weighting and discounting are not applied by
#' default (the IHME GBD removed both in the 2010 update); add a
#' discount factor manually if your guidance still requires it.
#'
#' @references
#' Murray, C. J. L., Lopez, A. D. (1996). The Global Burden of
#' Disease. Harvard University Press.
#'
#' GBD 2019 Diseases and Injuries Collaborators (2020). Global
#' burden of 369 diseases and injuries in 204 countries and
#' territories, 1990-2019. The Lancet 396.
#'
#' @family cost-effectiveness
#' @seealso [mb_qaly()].
#'
#' @export
#' @examples
#' mb_daly(yld = 2.5, yll = 8.0, persons = 100)
mb_daly <- function(yld, yll, persons = 1) {
  validate_numeric(yld,     arg = "yld",     require_non_negative = TRUE)
  validate_numeric(yll,     arg = "yll",     require_non_negative = TRUE)
  validate_numeric(persons, arg = "persons", require_positive = TRUE)
  validate_scalar(persons,  arg = "persons")
  persons * (sum(yld) + sum(yll))
}
