#' 客户单产
#'
#' 根据指定的分组列，对数据进行分组，并计算每个分组的客户单产。
#'
#' @param data 数据框。
#' @param periods 期数。
#' @param income_col 收入列名。
#' @param customer_id_col 用于区分客户计数的列名。
#' @param ... 分组列名。
#'
#' @return 一个数据框，含分组列、客户单产。
#'
#' @export
ya_income_per_customer <- function(data, periods, income_col, customer_id_col, ...) {
  group_cols <- enquos(...)

  result <- data %>%
    group_by(!!!group_cols) %>%
    summarise(
      income_sum = sum({{ income_col }}, na.rm = TRUE),
      cust_amount = n_distinct({{ customer_id_col }}),
      .groups = "drop"
    ) %>%
    mutate(
      income_per_customer = .data$income_sum / .data$cust_amount / periods
      ) %>%
    select(!!!group_cols, "income_per_customer")

  return(result)
}
