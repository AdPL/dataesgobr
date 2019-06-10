context("test-load-data-from-csv-file")

test_that("Load from csv file works", {
  file1 <- system.file("extdata", "datos1.csv", package="dataesgobr")
  file2 <- system.file("extdata", "datos2.csv", package="dataesgobr")
  expect_true(check_csv_file(file1))
  expect_true(check_csv_file(file2))
})

test_that("Error if the file does not exist", {
  file3 <- system.file("extdata", "datos5.csv", package="dataesgobr")
  expect_error(check_csv_file(file3))
})

test_that("Warning if the file is empty", {
  file4 <- system.file("extdata", "vacio.csv", package="dataesgobr")
 expect_warning(check_csv_file(file4))
})
