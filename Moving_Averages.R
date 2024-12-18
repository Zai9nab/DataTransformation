#' Moving Average
#'
#' Calculates a simple moving average over a specified window size.
#'
#' @param x A numeric vector.
#' @param window_size An integer specifying the window size for the moving average. Must be 1 or greater.
#' @return A numeric vector of the same length as `x`, with moving average values.
#' @examples
#' moving_average(c(1, 2, 3, 4, 5), window_size = 3)
#' @export
moving_average <- function(x, window_size) {
  if (window_size < 1) stop("Window size must be 1 or greater.")
  return(as.numeric(stats::filter(x, rep(1 / window_size, window_size), sides = 1)))
}
