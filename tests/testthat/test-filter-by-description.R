context("test-filter-by-description")

test_that("Filter by description works", {
  expect_is(filter_by_description(data.frame(), character()), class(data.frame()))
})

test_that("Data or q are invalid", {
  expect_error(filter_by_description(list(), character()), "*is not TRUE")
  expect_error(filter_by_description(data.frame(), numeric()), "*is not TRUE")
})
