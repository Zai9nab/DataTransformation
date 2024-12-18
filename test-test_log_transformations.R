test_that("log_transform works correctly", {
  # Positive cases
  expect_equal(log_transform(c(1, 10, 100)), log(c(1, 10, 100)))

  # Error cases
  expect_error(log_transform(c(-1, 0, 10)), "Values must be positive")
})
