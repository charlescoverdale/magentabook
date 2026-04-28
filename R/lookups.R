#' Vintage of bundled rubric and reference tables
#'
#' Returns a data frame describing the source and last-updated date
#' of every CSV bundled in `inst/extdata/`. Critical for
#' reproducibility: every evaluation report can record the vintage
#' of the rubrics and reference values used.
#'
#' @return A data frame with columns `dataset`, `source`,
#'   `last_updated`, `notes`.
#'
#' @family lookups
#' @seealso [mb_schedule_table()].
#'
#' @export
#' @examples
#' mb_data_versions()
mb_data_versions <- function() {
  .read_data_versions()
}

#' Expose internal lookup tables
#'
#' Returns one of the bundled lookup tables: the Maryland SMS
#' rubric, the Magenta Book confidence rubric, the ICC reference
#' table, or the evaluation question taxonomy.
#'
#' @param table Character scalar. One of `"sms"`, `"confidence"`,
#'   `"icc"`, or `"questions"`.
#'
#' @return A data frame.
#'
#' @family lookups
#' @seealso [mb_data_versions()].
#'
#' @export
#' @examples
#' mb_schedule_table("sms")
#' mb_schedule_table("confidence")
#' mb_schedule_table("icc")
#' mb_schedule_table("questions")
mb_schedule_table <- function(table = c("sms", "confidence", "icc", "questions")) {
  table <- match.arg(table)
  switch(
    table,
    sms        = .read_sms_rubric(),
    confidence = .read_confidence_rubric(),
    icc        = .read_icc_reference(),
    questions  = .read_question_taxonomy()
  )
}
