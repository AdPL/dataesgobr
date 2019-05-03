context("test-get-name")

test_that("Get file name works", {
  expect_equal(get_name("archivo-de-datos1.csv", "text/csv"), "archivo-de-datos1.csv")

  expect_equal(get_name("/archivo-de-datos2.csv", "text/csv"), "archivo-de-datos2.csv")

  expect_equal(get_name("/archivo-de-datos3.csv&rnd=1234", "text/csv"), "archivo-de-datos3.csv")

  expect_equal(get_name("/data/datos1.csv", "text/csv"), "datos1.csv")

})

test_that("Get file from url works", {
  url <- "https://datosabiertos.ayto-arganda.es/dataset/c56c0a6a-14af-4ce3-9cd5-0089389437a4/resource/ee752b9b-0900-43b7-bd7a-fadf0e1032dd/download/contratos-mayores-4-trimestre2016.csv"
  expect_that(dataesgobr:::get_name(url, "text/csv"),
              equals("contratos-mayores-4-trimestre2016.csv"))
})
