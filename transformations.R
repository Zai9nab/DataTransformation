#Logarithmic Transformation:

log_transform <- function(y) {
  if (any(y <= 0)) stop("Log transformation requires positive values.")
  return(log(y))
}


#' Logarithmic Transformation
#' @param y Numeric vector. Input data.
#' @return Transformed data.
#' @examples
#' log_transform(c(1, 10, 100))
#' @export
log_transform <- function(y) {
  if (any(y <= 0)) stop("Log transformation requires positive values.")
  return(log(y))
}


#Box-Cox Transformation:
  library(MASS)
box_cox_transform <- function(y, lambda = NULL) {
  if (is.null(lambda)) {
    boxcox_result <- boxcox(lm(y ~ 1), lambda = seq(-2, 2, by = 0.1))
    lambda <- boxcox_result$x[which.max(boxcox_result$y)]
  }
  return(ifelse(lambda == 0, log(y), (y^lambda - 1) / lambda))
}

#'  Box-Cox Transformation
#'
#' @description Performs Box-Cox transformation on input data to stabilize variance and normalize data.
#'
#' @param y A numeric vector. Input data must be positive.
#' @param lambda Optional transformation parameter. If NULL, an optimal value is estimated.
#' @return A numeric vector of transformed data.
#' @examples
#' box_cox_transform(c(1, 2, 3))
#' @export
box_cox_transform <- function(y, lambda = NULL) {
  if (is.null(lambda)) {
    lambda <- boxcox(lm(y ~ 1), lambda = seq(-2, 2, by = 0.1))$x[which.max(boxcox(lm(y ~ 1), lambda = seq(-2, 2, by = 0.1))$y)]
  }
  return(ifelse(lambda == 0, log(y), (y^lambda - 1) / lambda))
}



#Z-score Standardization:
z_score <- function(y) {
  return((y - mean(y)) / sd(y))
}

#'  Z-Score Standardization
#'
#' @description Standardizes the input data to have a mean of 0 and a standard deviation of 1.
#'
#' @param y A numeric vector. Input data.
#' @return A numeric vector of standardized data.
#' @examples
#' z_score(c(1, 2, 3, 4, 5))
#' @export
z_score <- function(y) {
  return((y - mean(y)) / sd(y))
}


#Min-Max Normalization:
min_max_normalize <- function(y) {
  return((y - min(y)) / (max(y) - min(y)))
}

#'  Min-Max Normalization
#'
#' @description Scales the input data to the range [0, 1].
#'
#' @param y A numeric vector. Input data.
#' @return A numeric vector of normalized data.
#' @examples
#' min_max_normalize(c(1, 2, 3, 4, 5))
#' @export
min_max_normalize <- function(y) {
  return((y - min(y)) / (max(y) - min(y)))
}


