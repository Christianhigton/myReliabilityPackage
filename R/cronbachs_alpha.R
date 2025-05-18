#' Generate a Cronbach's Alpha Summary Paragraph
#'
#' Calculates Cronbach's alpha for a set of scale items and returns a formatted
#' summary paragraph. The paragraph includes a placeholder for a reference to the
#' original scale and its reported reliability. The function also automatically
#' provides a qualitative interpretation (e.g., "good", "acceptable") based on the
#' current sample's alpha value.
#'
#' @param data A data frame containing responses to the scale items.
#' @param items A character vector of column names to include in the reliability calculation.
#'   If NULL, all columns in the data frame are used.
#' @param digits Number of decimal places to round the alpha coefficient (default is 2).
#'
#' @return A character string summarizing Cronbach's alpha and its interpretation.
#'
#' @examples
#' df <- data.frame(
#'   item1 = rnorm(100),
#'   item2 = rnorm(100),
#'   item3 = rnorm(100)
#' )
#' cronbachs_alpha_report(df, items = c("item1", "item2", "item3"))
#'
#' @export



cronbachs_alpha_report <- function(data,
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

  alpha_result <- psych::alpha(data)
  alpha_value <- round(alpha_result$total$raw_alpha, digits)

  interpretation <- dplyr::case_when(
    alpha_value >= 0.90 ~ "excellent",
    alpha_value >= 0.80 ~ "good",
    alpha_value >= 0.70 ~ "acceptable",
    alpha_value >= 0.60 ~ "questionable",
    alpha_value >= 0.50 ~ "poor",
    TRUE ~ "unacceptable"
  )

  text <- paste0(
    "According to [INSERT CITATION HERE], [INSERT SCALE NAME HERE] has ", interpretation,
    " internal consistency, with a Cronbach’s alpha coefficient reported of [INSERT ORIGINAL ALPHA]. ",
    "In the current study, the Cronbach’s alpha coefficient was ", alpha_value,
    ", based on the current sample."
  )

  return(text)
}
