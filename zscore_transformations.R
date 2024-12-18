#' Z-Score Standardization
#'
#' Standardizes data by centering it at zero and scaling to unit variance.
#'
#' @param x A numeric vector.
#' @return A numeric vector where each value is transformed to its z-score.
#' @examples
#' zscore_transform(c(1, 2, 3, 4, 5))
#' @export
zscore_transform <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
