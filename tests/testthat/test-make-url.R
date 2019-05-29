context("test-make-url")

test_that("Make url works", {
  expect_error(make_url())
  expect_equal(make_url("id", "l01281230-calidad-del-aire"), "https://datos.gob.es/apidata/catalog/dataset/l01281230-calidad-del-aire")
  expect_equal(make_url("title", "mirador"), "https://datos.gob.es/apidata/catalog/dataset/title/mirador")
  expect_equal(make_url("publisher", "L01280066"), "https://datos.gob.es/apidata/catalog/dataset/publisher/L01280066")
  expect_equal(make_url("theme", "sector-publico"), "https://datos.gob.es/apidata/catalog/dataset/theme/sector-publico")
  expect_equal(make_url("format", "json"), "https://datos.gob.es/apidata/catalog/dataset/format/json")
  expect_equal(make_url("keyword", "turismo"), "https://datos.gob.es/apidata/catalog/dataset/keyword/turismo")
  expect_equal(make_url("id", "ea0010587-correspondencias-cruzadas-de-las-superficies-de-regadio-entre-las-comunidades-autonomas-y-las-demarcaciones-hidrograficas-ano-2011-comunidades-y-ciudades-autonomas-estadisticas-sobre-el-uso-del-agua-identificador-api-t26-p067-p05-l0-01001-px1"), "https://datos.gob.es/apidata/catalog/dataset/ea0010587-correspondencias-cruzadas-de-las-superficies-de-regadio-entre-las-comunidades-autonomas-y-las-demarcaciones-hidrograficas-ano-2011-comunidades-y-ciudades-autonomas-estadisticas-sobre-el-uso-del-agua-identificador-api-t26-p067-p05-l0-01001-px1")
})
