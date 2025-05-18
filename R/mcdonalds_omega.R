
#' Generate a McDonald's Omega Summary Paragraph
#'
#' Calculates McDonald's omega total for a set of scale items and returns a formatted
#' summary paragraph. The paragraph includes a placeholder for a reference to the
#' original scale and its reported omega. The function also includes a qualitative
#' interpretation of the omega value based on commonly accepted thresholds.
#'
#' @param data A data frame containing responses to the scale items.
#' @param items A character vector of column names to include in the reliability calculation.
#'   If NULL, all columns in the data frame are used.
#' @param digits Number of decimal places to round the omega coefficient (default is 2).
#'
#' @return A character string summarizing McDonald's omega and its interpretation.
#'
#' @examples
#' df <- data.frame(
#'   item1 = rnorm(100),
#'   item2 = rnorm(100),
#'   item3 = rnorm(100)
#' )
#' mcdonalds_omega_report(df, items = c("item1", "item2", "item3"))
#'
#' @export



mcdonalds_omega_report <- function(data,
                                   items = NULL,
                                   digits = 2) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("The 'psych' package is required. Please install it using install.packages('psych').")
  }

  if (!is.null(items)) {
    missing_items <- items[!items %in% colnames(data)]
    if (length(missing_items) > 0) {
      stop(paste("The following items were not found in the data frame:",
                 paste(missing_items, collapse = ", ")))
    }
    data <- data[, items]
  }

  omega_result <- suppressWarnings(psych::omega(data, nfactors = 1, warnings = FALSE))
  omega_value <- round(omega_result$omega.tot, digits)

  interpretation <- dplyr::case_when(
    omega_value >= 0.90 ~ "excellent",
    omega_value >= 0.80 ~ "good",
    omega_value >= 0.70 ~ "acceptable",
    omega_value >= 0.60 ~ "questionable",
    omega_value >= 0.50 ~ "poor",
    TRUE ~ "unacceptable"
  )

  text <- paste0(
    "According to [INSERT CITATION HERE], [INSERT SCALE NAME HERE] has ", interpretation,
    " internal consistency, with a McDonald’s omega coefficient reported of [INSERT ORIGINAL OMEGA]. ",
    "In the current study, McDonald’s omega was ", omega_value,
    ", based on the current sample."
  )

  return(text)
}
