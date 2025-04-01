#' @title 重命名文件
#'
#' @description 根据提供的命名向量，重命名文件夹中文件。向量名为要匹配的文本内容，
#'   向量值为重命名后的文件名。重命名后文件名，不需要包含文件格式后缀。
#'
#' @param input_dir 字符串，输入目录的路径。
#' @param output_dir 字符串，输出目录的路径。
#' @param rename_vector 命名向量，向量名为匹配的文本内容，向量值为重命名后的文件
#'   名。
#'
#' @return 无返回值。
#'
#' @export
yeahr_rename <- function(input_dir, output_dir, rename_vector) {
  if (!is.character(input_dir) || length(input_dir) != 1) {
    stop("`input_dir` must be a single string.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("`output_dir` must be a single string.")
  }
  if (!is_named(rename_vector) || !is.character(rename_vector)) {
    stop("`rename_vector` must be a named character vector.")
  }
  if (anyDuplicated(names(rename_vector)) > 0) {
    stop("The names in the rename vector contain duplicates.")
  }

  input_dir <- path_abs(input_dir)
  output_dir <- path_abs(output_dir)

  if (!dir_exists(input_dir)) {
    stop(paste("Input directory does not exist:", input_dir))
  }

  dir_create(output_dir)
  files <- dir_ls(input_dir, type = "file")
  walk(files, function(file) {
    file_name <- path_file(file)
    file_ext <- path_ext(file_name)
    matched_keys <- names(rename_vector)[str_detect(file_name, names(rename_vector))]

    if (length(matched_keys) == 1) {
      new_name <- paste0(rename_vector[matched_keys], ".", file_ext)
      new_path <- path(output_dir, new_name)
      file_copy(file, new_path, overwrite = TRUE)
    } else if (length(matched_keys) > 1) {
      warning(paste("The file", file_name, "has multiple matches and has been skipped."))
    } else {
      warning(paste("The file", file_name, "has no matches and has been skipped."))
    }
  })
}
