#' Min-Max Normalization
#'
#' Rescales data to a range between 0 and 1. This is useful for algorithms that rely on distance-based metrics.
#'
#' @param x A numeric vector.
#' @return A numeric vector scaled to the range [0, 1].
#' @examples
#' minmax_normalization(c(1, 2, 3, 4, 5))
#' @export
minmax_normalization <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
