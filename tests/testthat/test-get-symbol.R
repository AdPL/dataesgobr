context("test-get-symbol")

test_that("Get symbol works", {
  file1 <- system.file("extdata", "fichero.csv", package="dataesgobr")
  file2 <- system.file("extdata", "datos4.csv", package="dataesgobr")
  expect_equal(dataesgobr:::get_symbol(file1), ",")
  expect_equal(dataesgobr:::get_symbol(file2), ";")
})
