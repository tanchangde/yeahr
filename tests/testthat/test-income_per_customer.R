test_data <- tibble(
  year = c(2020, 2020, 2021, 2021),
  group = c("A", "B", "A", "B"),
  income = c(100, 200, 150, 250),
  customer_id = c(1, 2, 1, 3)
)

test_that("ya_income_per_customer calculates income per customer correctly", {
  result <- ya_income_per_customer(
    test_data, periods = 1, income_col = income, customer_id_col = customer_id,
    year, group)

  expected <- tibble(
    year = c(2020, 2020, 2021, 2021),
    group = c("A", "B", "A", "B"),
    income_per_customer = c(100, 200, 150, 250)
  )

  expect_equal(result, expected)
})

test_that("ya_income_per_customer handles periods correctly", {
  result <- ya_income_per_customer(
    test_data, periods = 2, income_col = income, customer_id_col = customer_id,
    year, group)

  expected <- tibble(
    year = c(2020, 2020, 2021, 2021),
    group = c("A", "B", "A", "B"),
    income_per_customer = c(50, 100, 75, 125)
  )

  expect_equal(result, expected)
})
