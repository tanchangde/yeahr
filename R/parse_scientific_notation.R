#' 解析科学计数法字符串为数值字符串
#'
#' 该函数接受一个字符串向量，尝试将其解析为数值，并返回解析后的值作为字符串。如
#'   果解析失败，则返回原字符串。
#'
#' @param x 包含待解析字符串的字符向量。
#'
#' @return 一个字符向量，包含解析后数值字符串，或解析失败的原字符串。
#'
#' @export
ya_parse_sci_str <- function(x) {
  parsed_values <- suppressWarnings(as.numeric(x))
  result <- ifelse(is.na(parsed_values), x, as.character(parsed_values))
  return(result)
}
