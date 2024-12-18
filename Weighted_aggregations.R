#' Weighted Aggregation
#'
#' Computes the weighted sum and weighted average of a numeric vector.
#'
#' @param x A numeric vector.
#' @param weights A numeric vector of weights. Must be the same length as `x`.
#' @return A list with two elements: `weighted_sum` and `weighted_avg`.
#' @examples
#' weighted_aggregation(c(1, 2, 3), weights = c(0.1, 0.3, 0.6))
#' @export
weighted_aggregation <- function(x, weights) {
  if (length(x) != length(weights)) stop("x and weights must have the same length.")
  weighted_sum <- sum(x * weights, na.rm = TRUE)
  weighted_avg <- weighted_sum / sum(weights, na.rm = TRUE)
  return(list(weighted_sum = weighted_sum, weighted_avg = weighted_avg))
}
