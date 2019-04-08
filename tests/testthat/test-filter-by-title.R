context("test-filter-by-title")

test_that("Filter by title works", {
  expect_is(filter_by_title(data.frame(), character()), class(data.frame()))
})

test_that("Data or q are invalid", {
  expect_error(filter_by_title(list(), character()), "*is not TRUE")
  expect_error(filter_by_title(data.frame(), numeric()), "*is not TRUE")
})
