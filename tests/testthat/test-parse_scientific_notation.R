test_that("parse_sci_str 正确解析科学计数法字符串", {
  test_cases <-
    c("0917214240", "044760E000", "67699262E1", "BXW011", "760472000A",
      "MA59TLU19A")
  expected_results <-
    c("917214240", "44760", "676992620", "BXW011", "760472000A", "MA59TLU19A")
  results <- ya_parse_sci_str(test_cases)

  expect_equal(results, expected_results)
})
