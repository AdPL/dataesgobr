context("test-search-by-id")

test_that("ID is not a character", {
  expect_error(search_by_id(123), "*is not TRUE")
  expect_error(search_by_id(list()), "*is not TRUE")
})
