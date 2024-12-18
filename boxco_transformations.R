#' Box-Cox Transformation
#'
#' Transforms data to stabilize variance and make the data more normal. The optimal value of lambda can be determined using maximum likelihood.
#'
#' @param x A numeric vector. All values must be positive.
#' @param lambda A numeric value for the Box-Cox transformation parameter. Defaults to 0.
#' @return A numeric vector with the Box-Cox transformation applied.
#' @examples
#' boxcox_transform(c(1, 10, 100), lambda = 0.5)
#' @export
boxcox_transform <- function(x, lambda = 0) {
  if (any(x <= 0)) stop("Values must be positive for Box-Cox transformation.")
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda - 1) / lambda)
  }
}
