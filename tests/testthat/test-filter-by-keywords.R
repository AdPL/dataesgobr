context("test-filter-by-keywords")

test_that("Filter by keywords works", {
  expect_is(filter_by_keywords(data.frame(), character()), class(data.frame()))
})

test_that("Data or q are invalid", {
  expect_error(filter_by_keywords(list(), character()), "*is not TRUE")
  expect_error(filter_by_keywords(data.frame(), numeric()), "*is not TRUE")
})
