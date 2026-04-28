#' Structured Magenta Book confidence rating
#'
#' Records a single confidence rating against the bundled rubric:
#' high / medium / low, with explicit assessments of evidence
#' strength, methodological quality, and generalisability, and a
#' free-text rationale.
#'
#' @param rating Character scalar. One of `"high"`, `"medium"`,
#'   `"low"`.
#' @param question Character scalar. The evaluation question this
#'   rating refers to.
#' @param evidence_strength Character scalar. Plain-English
#'   description of the volume and quality of underlying studies.
#' @param methodological_quality Character scalar. Plain-English
#'   description of design rigour and identifying assumptions.
#' @param generalisability Character scalar. Plain-English
#'   description of how widely the findings travel across settings.
#' @param rationale Character scalar. Free-text justification for
#'   the chosen rating.
#'
#' @return An `mb_confidence` object: a list with the supplied
#'   fields plus the bundled-rubric `description` for the chosen
#'   rating, and `vintage`.
#'
#' @details
#' Magenta Book confidence ratings translate evidence into
#' decision-grade summaries for ministers and senior officials. The
#' bundled rubric (see [mb_schedule_table()] with table
#' `"confidence"`) is *not a direct quotation from the Magenta
#' Book*. It is a magentabook synthesis of cross-What-Works-Centre
#' confidence-rating traditions: Education Endowment Foundation
#' (5 padlocks), Early Intervention Foundation (Foundation
#' Standards), College of Policing (1-5 scale), and the Justice
#' Data Lab (red / amber / green). The three-level high / medium /
#' low structure is designed for HMG decision-grade reporting and
#' aligns with the value-for-money framing of the Magenta Book
#' (2020) supplementary guidance.
#'
#' @references
#' HM Treasury (2020). The Magenta Book: Central Government
#' Guidance on Evaluation. Supplementary guidance on value for
#' money.
#'
#' Education Endowment Foundation. Padlock evidence ratings.
#'
#' Early Intervention Foundation (2021). Foundation Standards of
#' Evidence.
#'
#' @family confidence
#' @seealso [mb_confidence_summary()], [mb_sms_rate()].
#'
#' @export
#' @examples
#' mb_confidence(
#'   rating                 = "medium",
#'   question               = "Did the policy raise employment",
#'   evidence_strength      = "One Level 4 DiD; one Level 3 matched cohort",
#'   methodological_quality = "Adequate; parallel trends plausible but limited pre-period",
#'   generalisability       = "Findings established in a single region",
#'   rationale              = "Effect direction consistent across two studies but limited replication"
#' )
mb_confidence <- function(rating = c("high", "medium", "low"),
                          question,
                          evidence_strength,
                          methodological_quality,
                          generalisability,
                          rationale) {
  rating <- match.arg(rating)
  validate_character(question,               arg = "question")
  validate_scalar(question,                  arg = "question")
  validate_character(evidence_strength,      arg = "evidence_strength")
  validate_scalar(evidence_strength,         arg = "evidence_strength")
  validate_character(methodological_quality, arg = "methodological_quality")
  validate_scalar(methodological_quality,    arg = "methodological_quality")
  validate_character(generalisability,       arg = "generalisability")
  validate_scalar(generalisability,          arg = "generalisability")
  validate_character(rationale,              arg = "rationale")
  validate_scalar(rationale,                 arg = "rationale")

  rubric <- .read_confidence_rubric()
  row <- rubric[rubric$rating == rating, , drop = FALSE]
  out <- list(
    rating                 = rating,
    label                  = row$label,
    question               = question,
    evidence_strength      = evidence_strength,
    methodological_quality = methodological_quality,
    generalisability       = generalisability,
    rationale              = rationale,
    description            = row$description,
    vintage                = .mb_vintage()
  )
  class(out) <- c("mb_confidence", "list")
  out
}

#' One-page confidence summary across multiple ratings
#'
#' Aggregates several `mb_confidence` ratings into a single summary
#' object with a confidence count and the underlying ratings as a
#' data frame.
#'
#' @param ... One or more `mb_confidence` objects, or a single
#'   list of them.
#'
#' @return An `mb_confidence_summary` object: a list with `n`
#'   (total ratings), `counts` (named integer vector by rating),
#'   `ratings` (data frame), and `vintage`.
#'
#' @family confidence
#' @seealso [mb_confidence()].
#'
#' @export
#' @examples
#' c1 <- mb_confidence(
#'   "high",   "Did employment rise",
#'   "Two Level 5 RCTs", "Strong; randomisation worked",
#'   "Tested in two regions", "Two RCTs both positive"
#' )
#' c2 <- mb_confidence(
#'   "medium", "Did wages rise",
#'   "One Level 4 DiD",  "Adequate; parallel trends plausible",
#'   "Single region",    "DiD effect positive but no replication"
#' )
#' mb_confidence_summary(c1, c2)
mb_confidence_summary <- function(...) {
  args <- list(...)
  if (length(args) == 1L && is.list(args[[1]]) && !inherits(args[[1]], "mb_confidence")) {
    args <- args[[1]]
  }
  if (length(args) == 0L) {
    cli::cli_abort("Provide at least one {.cls mb_confidence} object.")
  }
  ok <- vapply(args, inherits, logical(1), what = "mb_confidence")
  if (!all(ok)) {
    cli::cli_abort("All arguments must be {.cls mb_confidence} objects.")
  }

  ratings <- data.frame(
    question  = vapply(args, `[[`, character(1), "question"),
    rating    = vapply(args, `[[`, character(1), "rating"),
    rationale = vapply(args, `[[`, character(1), "rationale"),
    stringsAsFactors = FALSE
  )
  counts <- table(factor(ratings$rating, levels = c("high", "medium", "low")))
  out <- list(
    n       = nrow(ratings),
    counts  = as.integer(counts),
    levels  = c("high", "medium", "low"),
    ratings = ratings,
    vintage = .mb_vintage()
  )
  names(out$counts) <- out$levels
  class(out) <- c("mb_confidence_summary", "list")
  out
}
