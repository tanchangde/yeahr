#' CPR
#'
#' 计算 CPR
#'
#' 该函数用于根据客户和产品信息将客户分类为新客户、老客户新业务和老客户老业务。
#'
#' @param base_df 数据框。基期数据，上年度有营收客户及产品清单。必需包含产品及客
#'   户 ID，如结算代码。
#' @param calculate_df  数据框。需要计算客户数据，必要列同 `base_df`，且需按必要
#'   列分组求和。
#' @param customer_id_col 客户区分列。
#' @param product_col 产品列。
#'
#' @return  数据框，含 `customer_id_col`，`product_col` 及 CPR 计算结果。
#'
#' @export
ya_cpr <- function(base_df, calculate_df, customer_id_col, product_col) {
  customer_id_col <- ensym(customer_id_col)
  product_col <- ensym(product_col)

  tb_new_customer <- calculate_df %>%
    distinct({{ customer_id_col }}) %>%
    anti_join(
      base_df %>%
        distinct({{ customer_id_col }}),
      by = as_string(customer_id_col)
    ) %>%
    mutate(cpr = "C")

  tb_exist_customer_new_business <- calculate_df %>%
    anti_join(tb_new_customer, by = as_string(customer_id_col)) %>%
    anti_join(
      base_df,
      by = c(as_string(customer_id_col), as_string(product_col))
    ) %>%
    mutate(cpr = "P")

  tb_exist_customer_business <- calculate_df %>%
    anti_join(tb_new_customer, by = as_string(customer_id_col)) %>%
    anti_join(
      tb_exist_customer_new_business,
      by = c(as_string(customer_id_col), as_string(product_col))
    ) %>%
    mutate(cpr = "R")

  result <- bind_rows(
    tb_new_customer, tb_exist_customer_new_business, tb_exist_customer_business
  )

  return(result)
}
