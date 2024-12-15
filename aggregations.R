#Summing and Averaging:
library(dplyr)
sum_average <- function(data, group_col, value_col) {
  aggregated <- data %>%
    group_by({{ group_col }}) %>%
    summarize(sum = sum({{ value_col }}), average = mean({{ value_col }}), .groups = 'drop')
  return(aggregated)
}

#'  Summing and Averaging by Group
#'
#' @description Aggregates data by summing and averaging within groups.
#'
#' @param data A data frame containing the input data.
#' @param group_col The column for grouping the data.
#' @param value_col The column containing values to aggregate.
#' @return A data frame with aggregated sums and averages.
#' @examples
#' df <- data.frame(group = c('A', 'A', 'B'), value = c(1, 2, 3))
#' sum_average(df, group, value)
#' @export
sum_average <- function(data, group_col, value_col) {
  aggregated <- data %>%
    group_by({{ group_col }}) %>%
    summarize(sum = sum({{ value_col }}), average = mean({{ value_col }}), .groups = 'drop')
  return(aggregated)
}



#Weighted Aggregation:
weighted_average <- function(data, group_col, value_col, weight_col) {
  aggregated <- data %>%
    group_by({{ group_col }}) %>%
    summarize(weighted_avg = sum({{ value_col }} * {{ weight_col }}) / sum({{ weight_col }}), .groups = 'drop')
  return(aggregated)
}


moving_average <- function(y, k) {
  if (k <= 0 || k > length(y)) stop("Invalid window size.")
  dplyr::filter <- rep(1/k, k)
  return(dplyr::filter(y, dplyr::filter, sides = 1))
}


#'  Moving Averages
#'
#' @description Calculates a simple moving average over a specified window size.
#'
#' @param y A numeric vector. Input data.
#' @param k An integer. Window size for the moving average.
#' @return A numeric vector of moving averages.
#' @examples
#' moving_average(c(1, 2, 3, 4, 5), 3)
#' @export
moving_average <- function(y, k) {
  if (k <= 0 || k > length(y)) stop("Invalid window size.")
  dplyr::filter <- rep(1/k, k)
  return(dplyr::filter(y, dplyr::filter, sides = 1))
}


