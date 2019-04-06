context("test-get-symbol")

test_that("Get symbol works", {
  expect_equal(dataesgobr:::get_symbol("data/datos1.csv"), ",")
  expect_equal(dataesgobr:::get_symbol("data/datos4.csv"), ";")
})
