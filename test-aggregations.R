test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#Summing and Averaging:
test_that("Min-Max Normalization scales correctly", {
  input <- c(1, 2, 3)
  result <- min_max_normalize(input)
  expect_equal(result, c(0, 0.5, 1))
})

#Moving Averages:
library(testthat)
library(dplyr)
library(datatransform)

test_that("Moving average computes correctly", {
  input <- c(1, 2, 3, 4, 5)
  result <- moving_average(input, 3)
  expect_equal(as.numeric(result), c(NA, NA, 2, 3, 4))
})


