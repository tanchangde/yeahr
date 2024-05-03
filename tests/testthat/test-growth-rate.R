test_that("正确处理特殊除法", {
  current_period1 <- c(10, NA, 30, 0, 60)
  base_period1 <- c(5, 10, NA, 20, 0)
  expected1 <- c(1, -1, 1, -1, 1)
  result1 <- ya_growth_rate(current_period1, base_period1)
  expect_equal(result1, expected1)

  current_period2 <- c(NA, NA, 0, 0)
  base_period2 <- c(NA, 0, NA, 0)
  expected2 <- c(NA_real_, NA_real_, NA_real_, NA_real_)
  result2 <- ya_growth_rate(current_period2, base_period2)
  expect_equal(result2, expected2)
})
