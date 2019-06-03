context("test-get-id")

test_that("Get id works", {
  expect_is(get_id("https://datos.gob.es/apidata/catalog/dataset?_sort=title&_pageSize=10&_page=1"), "character")
  expect_equal(is.na(get_id("https://datos.gob.es/apidata/catalog/dataset?_sort=title&_pageSize=10&_page=1")), TRUE)
  expect_equal(get_id("https://datos.gob.es/apidata/catalog/dataset/l01281230-calidad-del-aire?_sort=title&_pageSize=10&_page=0"), "l01281230-calidad-del-aire")
  expect_equal(get_id("https://datos.gob.es/es/catalogo/l01290672-informacion-sig-mapa-estrategico-de-ruido-de-malaga-ruido-total-indice-lnoche1"), "l01290672-informacion-sig-mapa-estrategico-de-ruido-de-malaga-ruido-total-indice-lnoche1")
})
