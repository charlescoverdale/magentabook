# Internal helpers (not exported)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Path to bundled CSV in inst/extdata
.mb_extdata <- function(file) {
  path <- system.file("extdata", file, package = "magentabook")
  if (!nzchar(path)) {
    cli::cli_abort("Bundled data file {.val {file}} not found in magentabook extdata.")
  }
  path
}

# Lazy readers for bundled tables
.read_sms_rubric <- function() {
  utils::read.csv(.mb_extdata("sms_rubric.csv"), stringsAsFactors = FALSE)
}

.read_confidence_rubric <- function() {
  utils::read.csv(.mb_extdata("confidence_rubric.csv"), stringsAsFactors = FALSE)
}

.read_icc_reference <- function() {
  utils::read.csv(.mb_extdata("icc_reference.csv"), stringsAsFactors = FALSE)
}

.read_question_taxonomy <- function() {
  utils::read.csv(.mb_extdata("question_taxonomy.csv"), stringsAsFactors = FALSE)
}

.read_data_versions <- function() {
  utils::read.csv(.mb_extdata("data_versions.csv"), stringsAsFactors = FALSE)
}

# Validate numeric vector input
validate_numeric <- function(x, arg = "x", allow_na = FALSE,
                             require_positive = FALSE,
                             require_non_negative = FALSE) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector.")
  }
  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} contains {.val NA} values.")
  }
  if (require_positive && any(x <= 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg {arg}} must be strictly positive.")
  }
  if (require_non_negative && any(x < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg {arg}} must be non-negative.")
  }
  invisible(x)
}

# Validate scalar
validate_scalar <- function(x, arg = "x") {
  if (length(x) != 1L) {
    cli::cli_abort("{.arg {arg}} must be a scalar (length 1).")
  }
  invisible(x)
}

# Validate proportion in (0, 1) or [0, 1]
validate_proportion <- function(x, arg = "x", strict = FALSE) {
  validate_numeric(x, arg = arg)
  if (strict) {
    if (any(x <= 0 | x >= 1)) {
      cli::cli_abort("{.arg {arg}} must be strictly between 0 and 1.")
    }
  } else {
    if (any(x < 0 | x > 1)) {
      cli::cli_abort("{.arg {arg}} must be between 0 and 1.")
    }
  }
  invisible(x)
}

# Validate character vector
validate_character <- function(x, arg = "x", allow_na = FALSE) {
  if (!is.character(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector.")
  }
  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} contains {.val NA} values.")
  }
  invisible(x)
}

# Generic compact-list utility (drops NULL elements)
.drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

# Format an effect size in a print method
.format_es <- function(x, digits = 3) {
  if (length(x) != 1L || !is.finite(x)) return(format(x))
  sprintf(paste0("%.", digits, "f"), x)
}

# Format a count compactly
.format_n <- function(x) {
  if (length(x) != 1L || !is.finite(x)) return(format(x))
  if (abs(x) >= 1e6) return(sprintf("%.2fm", x / 1e6))
  if (abs(x) >= 1e3) return(sprintf("%.1fk", x / 1e3))
  format(x, big.mark = ",", scientific = FALSE)
}

# Currency formatter mirroring greenbook house style
.format_gbp <- function(x, digits = 1) {
  if (length(x) != 1L || !is.finite(x)) return(format(x))
  if (abs(x) >= 1e9) return(sprintf("GBP %.2fbn", x / 1e9))
  if (abs(x) >= 1e6) return(sprintf("GBP %.2fm", x / 1e6))
  if (abs(x) >= 1e3) return(sprintf("GBP %.1fk", x / 1e3))
  sprintf(paste0("GBP %.", digits, "f"), x)
}

# Internal: package vintage stamped onto every result object
.mb_vintage <- function() {
  utils::packageVersion("magentabook")
}
