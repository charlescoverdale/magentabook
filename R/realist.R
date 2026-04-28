#' Context-mechanism-outcome (CMO) configuration
#'
#' Records one or more CMO configurations from a realist evaluation:
#' the *contexts* in which a *mechanism* fires to produce an
#' *outcome*, with optional supporting evidence.
#'
#' @param context Character vector. The contextual conditions
#'   needed for the mechanism to fire.
#' @param mechanism Character vector. The underlying generative
#'   mechanism (typically a change in reasoning or resources).
#' @param outcome Character vector. The observed outcome pattern.
#' @param evidence Character vector. Citation, quote, or other
#'   evidence supporting the configuration. Default `NA`.
#'
#' @return An `mb_cmo` data frame with columns `context`,
#'   `mechanism`, `outcome`, `evidence`.
#'
#' @details
#' Realist evaluation, developed by Pawson and Tilley (1997), seeks
#' to answer "what works for whom in what circumstances and why" by
#' surfacing CMO configurations rather than estimating average
#' treatment effects. The Magenta Book lists realist evaluation as
#' the principal theory-based approach for context-dependent
#' interventions.
#'
#' @references
#' Pawson, R., Tilley, N. (1997). Realistic Evaluation. SAGE.
#'
#' HM Treasury (2020). The Magenta Book, chapter on theory-based
#' evaluation.
#' <https://www.gov.uk/government/publications/the-magenta-book>.
#'
#' @family realist
#' @seealso [mb_contribution_claim()].
#'
#' @export
#' @examples
#' mb_cmo(
#'   context   = c("High trust GP-patient relationships",
#'                 "Low trust GP-patient relationships"),
#'   mechanism = c("Patients accept advice", "Patients ignore advice"),
#'   outcome   = c("Improved adherence", "No change in adherence"),
#'   evidence  = c("Smith et al. 2024 cohort study", "Smith et al. 2024")
#' )
mb_cmo <- function(context, mechanism, outcome, evidence = NA_character_) {
  validate_character(context,   arg = "context")
  validate_character(mechanism, arg = "mechanism")
  validate_character(outcome,   arg = "outcome")
  n <- length(context)
  if (length(mechanism) != n) cli::cli_abort("{.arg mechanism} must be length {.val {n}}.")
  if (length(outcome)   != n) cli::cli_abort("{.arg outcome} must be length {.val {n}}.")
  if (length(evidence)  == 1L) evidence <- rep(evidence, n)
  if (length(evidence)  != n) cli::cli_abort("{.arg evidence} must be length 1 or {.val {n}}.")

  out <- data.frame(
    context   = context,
    mechanism = mechanism,
    outcome   = outcome,
    evidence  = evidence,
    stringsAsFactors = FALSE
  )
  class(out) <- c("mb_cmo", "data.frame")
  out
}

#' Contribution-analysis claim
#'
#' Records a contribution claim with supporting and refuting
#' evidence and an overall strength rating. Used in
#' contribution-analysis-style theory-based evaluation, where causal
#' inference comes from triangulating multiple evidence streams
#' against a contribution story rather than from a counterfactual.
#'
#' @param claim Character scalar. The contribution claim being
#'   tested.
#' @param evidence_for Character vector. Evidence supporting the
#'   claim.
#' @param evidence_against Character vector. Evidence against the
#'   claim. Default `character(0)`.
#' @param strength Character scalar. One of `"weak"`, `"moderate"`,
#'   `"strong"`. Reflects the analyst's overall judgement after
#'   weighing evidence for and against.
#'
#' @return An `mb_contribution_claim` object.
#'
#' @references
#' Mayne, J. (2008). Contribution Analysis: An approach to exploring
#' cause and effect. ILAC Brief No. 16.
#'
#' HM Treasury (2020). The Magenta Book, chapter on theory-based
#' evaluation.
#'
#' @family realist
#' @seealso [mb_cmo()].
#'
#' @export
#' @examples
#' mb_contribution_claim(
#'   claim            = "The training programme contributed to higher employment",
#'   evidence_for     = c("Pre-post outcomes improved",
#'                        "Theory of change pathways visible in interviews"),
#'   evidence_against = "Macro labour market also improved",
#'   strength         = "moderate"
#' )
mb_contribution_claim <- function(claim,
                                  evidence_for,
                                  evidence_against = character(0),
                                  strength = c("weak", "moderate", "strong")) {
  validate_character(claim, arg = "claim"); validate_scalar(claim, "claim")
  validate_character(evidence_for, arg = "evidence_for")
  if (length(evidence_against)) validate_character(evidence_against, arg = "evidence_against")
  strength <- match.arg(strength)

  out <- list(
    claim            = claim,
    evidence_for     = evidence_for,
    evidence_against = evidence_against,
    strength         = strength,
    vintage          = .mb_vintage()
  )
  class(out) <- c("mb_contribution_claim", "list")
  out
}
