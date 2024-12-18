#' Logarithmic Transformation
#'
#' Applies a logarithmic transformation to a numeric vector. This is useful for reducing skewness and stabilizing variance in regression modeling.
#'
#' @param x A numeric vector. All values must be positive.
#' @return A numeric vector with the logarithm applied to each value.
#' @examples
#' log_transform(c(1, 10, 100))
#' @export
log_transform <- function(x) {
  if (any(x <= 0)) stop("Values must be positive for log transformation.")
  return(log(x))
}
