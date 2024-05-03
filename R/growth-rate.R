#' 增长率
#'
#' 依据两个非负数值向量计算增长率，如果当期或基期为零或 NA，函数会返回特定值。适
#' 用于同比、环比增长率计算。
#'
#' @param current_period 当期值，一个非负数值向量。
#' @param base_period 基期值，一个非负数值向量。
#'
#' @return 一个数值向量，表示增长率。特殊情况处理如下：
#'
#' * 如果当期为零或 NA，且基期大于零，返回 -1
#' * 如果当期大于零，且基期为零或 NA，返回 1
#' * 如果当期和基期都不为零或 NA，返回当期除以基期减一
#' * 如果当期和基期都为零或 NA，返回 NA
#'
#' @export
ya_growth_rate <- function(current_period, base_period) {
  cond1 <- (is.na(current_period) | current_period == 0) &
    (!is.na(base_period) & base_period > 0)
  cond2 <- (!is.na(current_period) & current_period > 0) &
    (is.na(base_period) | base_period == 0)
  cond3 <- (!is.na(current_period) & current_period != 0) &
    (!is.na(base_period) & base_period != 0)
  cond4 <- (is.na(current_period) | current_period == 0) &
    (is.na(base_period) | base_period == 0)

  result <- numeric(length(current_period))
  result[cond1] <- -1
  result[cond2] <- 1
  result[cond3] <- current_period[cond3] / base_period[cond3] - 1
  result[cond4] <- NA_real_

  return(result)
}
