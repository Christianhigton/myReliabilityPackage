#' Generate a Narrative Report on Missing Data
#'
#' This function calculates the percent of missing data in a dataset, classifies
#' the level of missingness (e.g., minimal, moderate), performs an MCAR test using
#' \code{naniar::mcar_test()}, and returns a paragraph summary along with a table
#' of missing data per variable.
#'
#' @param data A data frame containing the dataset.
#' @param digits Number of decimal places for rounding percentages (default = 1).
#' @param deletion_strategy Either "listwise" or "pairwise" to describe your analysis plan.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{paragraph}}{A human-readable narrative summary.}
#'   \item{\code{summary_table}}{A table of missing values per variable.}
#'   \item{\code{mcar_test}}{The result of the MCAR test.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   var1 = c(1, 2, NA, 4),
#'   var2 = c(NA, 2, 3, 4),
#'   var3 = c(1, NA, NA, 4)
#' )
#' missing_data_report(df)
#'
#' @export
missing_data_report <- function(data, digits = 1, deletion_strategy = c("listwise", "pairwise")) {
  if (!requireNamespace("naniar", quietly = TRUE)) {
    stop("Please install the 'naniar' package: install.packages('naniar')")
  }

  deletion_strategy <- match.arg(deletion_strategy)

  # --- Classify Missingness ---
  total_cells <- prod(dim(data))
  num_missing <- sum(is.na(data))
  percent_missing <- round((num_missing / total_cells) * 100, digits)

  classification <- dplyr::case_when(
    percent_missing < 5   ~ "minimal",
    percent_missing < 10  ~ "low",
    percent_missing < 20  ~ "moderate",
    percent_missing < 50  ~ "high",
    TRUE                  ~ "severe"
  )

  # --- Summary Table per Variable ---
  missing_summary <- naniar::miss_var_summary(data)
  missing_summary <- missing_summary[, c("variable", "n_miss", "pct_miss")]

  # --- MCAR Test ---
  mcar_result <- naniar::mcar_test(data)
  chi_sq <- round(mcar_result$statistic, 2)
  df     <- mcar_result$parameter
  p_val  <- round(mcar_result$p.value, 3)
  p_text <- ifelse(p_val < .001, "< .001", paste0("= ", p_val))
  mcar_interpretation <- if (p_val > 0.05) "missing completely at random" else "not missing completely at random"

  # --- Paragraph Output ---
  paragraph <- paste0(
    "Missing data were classified as ", classification, ", with approximately ",
    percent_missing, "% of responses missing across all variables. ",
    "MCAR test results suggested that the data were ", mcar_interpretation,
    ", χ²(", df, ") = ", chi_sq, ", p ", p_text, ". ",
    "Given the low proportion of missingness, analyses were conducted using ", deletion_strategy, " deletion."
  )

  return(list(
    paragraph = paragraph,
    summary_table = missing_summary,
    mcar_test = mcar_result
  ))
}
