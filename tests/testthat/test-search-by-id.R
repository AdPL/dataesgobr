context("test-search-by-id")

test_that("Search by id works", {
  expect_is(search_by_id("l01280148-contratos-mayores-4-trimestre-20161"),
              "dataesgobr")
})

test_that("Warning if dataset not found and return NULL", {
  expect_null(search_by_id("l01280148-not-found"))
})

test_that("ID is not a character", {
  expect_error(search_by_id(123), "*is not TRUE")
  expect_error(search_by_id(list()), "*is not TRUE")
})
