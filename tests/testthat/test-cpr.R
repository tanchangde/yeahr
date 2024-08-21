test_that("测试 ya_cpr", {
  base_df <- data.frame(
    customer_id = c(1, 2, 3),
    product = c("A", "B", "C")
  )

  calculate_df <- data.frame(
    customer_id = c(1, 2, 3, 4, 5),
    product = c("A", "B", "D", "E", "F")
  )

  result <- ya_cpr(
    base_df, calculate_df,
    customer_id_col = "customer_id", product_col = "product"
  )

  new_customers <- result %>% filter(cpr == "C")
  expect_equal(nrow(new_customers), 2)
  expect_true(all(new_customers$customer_id %in% c(4, 5)))

  new_business <- result %>% filter(cpr == "P")
  expect_equal(nrow(new_business), 1)
  expect_true(all(new_business$customer_id == 3 & new_business$product == "D"))

  old_business <- result %>% filter(cpr == "R")
  expect_equal(nrow(old_business), 2)
  expect_true(all(old_business$customer_id %in% c(1, 2)))
  expect_true(all(old_business$product %in% c("A", "B")))
})
