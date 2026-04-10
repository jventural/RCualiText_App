library(testthat)

# Sample data used across tests
sample_textos <- c("the quick brown fox jumps over the lazy dog",
                   "the dog runs fast and catches the fox")
sample_nombres <- c("doc1", "doc2")

sample_tabla_fm <- tibble::tibble(
  Archivo = c("doc1", "doc1", "doc2"),
  Codigo = c("animal", "action", "animal"),
  Extracto = c("quick brown fox", "jumps over", "dog runs fast"),
  FragmentId = c("f1", "f2", "f3"),
  Categoria = c("c1", "c2", "c1"),
  Color = c("#ff0000", "#00ff00", "#ff0000")
)

test_that("calcular_frecuencia_palabras returns a tibble with word and n columns", {
  # min_freq = 1 because sample is small
  result <- calcular_frecuencia_palabras(sample_textos, idioma = "en", min_freq = 1)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("word", "n") %in% names(result)))
  expect_true(nrow(result) >= 1)
  expect_type(result$word, "character")
})

test_that("kwic_search returns a tibble with expected columns", {
  result <- kwic_search(sample_textos, sample_nombres, keyword = "fox", window = 3)
  expect_s3_class(result, "tbl_df")
  expected_cols <- c("Documento", "Contexto_Izq", "Keyword", "Contexto_Der", "Posicion")
  expect_true(all(expected_cols %in% names(result)))
  # "fox" appears in both documents
  expect_true(nrow(result) >= 1)
})

test_that("kwic_search returns an empty tibble for empty keyword", {
  result <- kwic_search(sample_textos, sample_nombres, keyword = "", window = 3)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("matrix_coding_query returns a wide-format tibble", {
  result <- matrix_coding_query(sample_tabla_fm)
  expect_s3_class(result, "tbl_df")
  expect_true("Codigo" %in% names(result))
  # Documents should become columns
  expect_true("doc1" %in% names(result))
  expect_true("doc2" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("generar_framework_matrix returns a tibble with Documento and Codigo columns", {
  result <- generar_framework_matrix(sample_tabla_fm, documentos = unique(sample_tabla_fm$Archivo))
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Documento", "Codigo") %in% names(result)))
  # Expand grid: 2 docs x 2 codes = 4 rows
  expect_equal(nrow(result), 4)
  expect_true("Contenido" %in% names(result))
  expect_true("N" %in% names(result))
})
