context("test-load-data-from-csv-file")

test_that("Load from csv file works", {
  expect_true(check_csv_file("data/datos1.csv"))
  expect_true(check_csv_file("data/datos2.csv"))
})

test_that("False if the file is incorrect", {

})

test_that("Error if the file does not exist", {
  expect_error(check_csv_file("data/datos5.csv"))
})

test_that("Warning if the file is empty", {
 expect_warning(check_csv_file("data/vacio.csv"))
})
