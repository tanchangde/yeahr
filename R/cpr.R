#' CPR
#'
#' 计算 CPR
#'
#' 该函数用于根据客户和产品信息将客户分类为新客户、老客户新业务和老客户老业务。
#'
#' @param base_df data.frame 基期对照数据，需包含客户 ID（如结算代），以及产品
#' @param calculate_df data.frame 需要计算客户数据，需包含列同 `base_df`
#' @param customer_id_col 客户区分列
#' @param product_col 产品列
#'
#' @return data.frame 返回客户分类结果
#'
#' @export
ya_cpr <- function(base_df, calculate_df, customer_id_col, product_col) {
  customer_id_col <- ensym(customer_id_col)
  product_col <- ensym(product_col)

  tb_new_customer <- calculate_df %>%
    select({{ customer_id_col }}) %>%
    distinct() %>%
    anti_join(
      base_df %>%
        select({{ customer_id_col }}) %>%
        distinct(),
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
