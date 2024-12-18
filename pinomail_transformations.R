#' Polynomial Transformation
#'
#' Expands a numeric vector into polynomial terms up to the specified degree.
#'
#' @param x A numeric vector.
#' @param degree An integer specifying the degree of the polynomial. Must be 1 or greater.
#' @return A matrix where each column represents a polynomial term of the input vector.
#' @examples
#' polynomial_transform(c(1, 2, 3), degree = 2)
#' @export
polynomial_transform <- function(x, degree = 2) {
  if (degree < 1) stop("Degree must be 1 or greater.")
  return(as.matrix(sapply(0:degree, function(d) x^d)))
}
