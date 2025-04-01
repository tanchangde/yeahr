test_that("ya_rename handles duplicates in rename_vector", {
  input_dir <- tempdir()
  output_dir <- file.path(tempdir(), "output")
  rename_vector <- c("file1" = "new_file1", "file1" = "new_file2")

  expect_error(
    ya_rename(input_dir, output_dir, rename_vector),
    "The names in the rename vector contain duplicates."
  )
})

test_that("ya_rename renames files correctly", {
  input_dir <- file.path(tempdir(), "input")
  output_dir <- file.path(tempdir(), "output")
  dir_create(input_dir)
  dir_create(output_dir)

  file_create(file.path(input_dir, "file1.txt"))
  file_create(file.path(input_dir, "file2.txt"))

  rename_vector <- c("file1" = "new_file1", "file2" = "new_file2")

  ya_rename(input_dir, output_dir, rename_vector)

  expect_true(file_exists(file.path(output_dir, "new_file1.txt")))
  expect_true(file_exists(file.path(output_dir, "new_file2.txt")))
})

test_that("ya_rename skips files with no matches", {
  input_dir <- file.path(tempdir(), "input")
  output_dir <- file.path(tempdir(), "output")
  dir_create(input_dir)
  dir_create(output_dir)

  file_create(file.path(input_dir, "file3.txt"))

  rename_vector <- c("file1" = "new_file1")

  expect_warning(
    ya_rename(input_dir, output_dir, rename_vector),
    "The file file3.txt has no matches and has been skipped."
  )

  expect_false(file_exists(file.path(output_dir, "file3.txt")))
})

test_that("ya_rename skips files with multiple matches", {
  input_dir <- file.path(tempdir(), "input")
  output_dir <- file.path(tempdir(), "output")
  dir_create(input_dir)
  dir_create(output_dir)

  # Create test file
  file_create(file.path(input_dir, "file1.txt"))

  rename_vector <- c("file1" = "new_file1", "file" = "new_file_generic")

  expect_warning(
    ya_rename(input_dir, output_dir, rename_vector),
    "The file file1.txt has multiple matches and has been skipped."
  )

  expect_false(file_exists(file.path(output_dir, "file1.txt")))
})
