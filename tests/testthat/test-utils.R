library(testthat)

# Sample data used across tests
sample_df <- tibble::tibble(
  Codigo = c("A", "A", "B", "C"),
  Archivo = c("f1.txt", "f1.txt", "f2.txt", "f2.txt"),
  Categoria = c("Cat1", "Cat1", "Cat2", "Cat2")
)

test_that("crear_fragment_id returns a unique character ID with 'fragment_' prefix", {
  id1 <- crear_fragment_id()
  expect_type(id1, "character")
  expect_length(id1, 1)
  expect_true(startsWith(id1, "fragment_"))

  # Two successive calls should differ (time + runif seed)
  id2 <- crear_fragment_id()
  expect_false(identical(id1, id2))
})

test_that("APP_VERSION exists and is a character string", {
  expect_true(exists("APP_VERSION"))
  expect_type(APP_VERSION, "character")
  expect_length(APP_VERSION, 1)
  expect_true(nzchar(APP_VERSION))
})

test_that("generar_css_multiples returns background-color for a single color", {
  css <- generar_css_multiples(c("#ff0000"))
  expect_type(css, "character")
  expect_match(css, "background-color:\\s*#ff0000")
})

test_that("generar_css_multiples returns a linear-gradient for multiple colors", {
  css <- generar_css_multiples(c("#ff0000", "#00ff00"))
  expect_type(css, "character")
  expect_match(css, "linear-gradient")
  expect_match(css, "#ff0000")
  expect_match(css, "#00ff00")
})

test_that("prepare_code_counts returns a tibble with Frecuencia column", {
  result <- prepare_code_counts(sample_df, fill = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_true("Frecuencia" %in% names(result))
  expect_true(all(c("Archivo", "Codigo") %in% names(result)))
  expect_true(nrow(result) >= 1)
  expect_type(result$Frecuencia, "integer")
})
