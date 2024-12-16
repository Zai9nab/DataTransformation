

#Logarithmic Transformation:
library(testthat)
test_that("Log Transformation works correctly", {
  expect_equal(log_transform(c(1, 10, 100)), c(0, log(10), log(100)))
})
test_that("Box-Cox Transformation handles positive values correctly", {
  expect_error(box_cox_transform(c(-1, 0)), "negative values")
})


#Box-Cox Transformation:
test_that("Box-Cox Transformation handles positive values correctly", {
  expect_error(box_cox_transform(c(-1, 0)), "negative values")
})

#Z-score Standardization:
test_that("Z-score standardization computes correctly", {
  input <- c(1, 2, 3)
  result <- z_score(input)
  expected <- (input - mean(input)) / sd(input)
  expect_equal(result, expected)
})

#Min-Max Normalization:
test_that("Min-Max Normalization scales correctly", {
  input <- c(1, 2, 3)
  result <- min_max_normalize(input)
  expect_equal(result, c(0, 0.5, 1))
})
