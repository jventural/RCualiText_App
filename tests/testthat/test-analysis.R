library(testthat)

# Sample data used across tests
set.seed(2026)
sample_embeddings <- matrix(rnorm(12), nrow = 3, ncol = 4)
sample_tabla <- tibble::tibble(
  Codigo = c("A", "B", "A"),
  Extracto = c("text1", "text2", "text3"),
  Categoria = c("Cat1", "Cat2", "Cat1")
)

test_that("calcular_similitud_coseno returns a symmetric matrix with diagonal = 1", {
  sim <- calcular_similitud_coseno(sample_embeddings)
  expect_true(is.matrix(sim))
  expect_equal(dim(sim), c(3, 3))
  # Diagonal should be ~1
  expect_equal(as.numeric(diag(sim)), rep(1, 3), tolerance = 1e-8)
  # Should be symmetric
  expect_equal(sim, t(sim), tolerance = 1e-8)
})

test_that("clustering_semantico returns a list with clusters and n_clusters", {
  result <- clustering_semantico(sample_embeddings, n_clusters = 2, metodo = "kmeans")
  expect_type(result, "list")
  expect_true("clusters" %in% names(result))
  expect_true("n_clusters" %in% names(result))
  expect_equal(result$n_clusters, 2)
  expect_length(result$clusters, 3)
})

test_that("detectar_similares_diferente_codigo returns a tibble", {
  # Build a similarity matrix with high similarity between different codes
  sim_mat <- matrix(c(
    1.0, 0.95, 0.2,
    0.95, 1.0, 0.1,
    0.2, 0.1, 1.0
  ), nrow = 3, byrow = TRUE)

  result <- detectar_similares_diferente_codigo(sample_tabla, sim_mat, umbral = 0.8)
  expect_s3_class(result, "tbl_df")
  # The 0.95 pair is between rows 1 (A) and 2 (B) - different codes
  expect_true(nrow(result) >= 1)
  expect_true(all(c("Fragmento1", "Codigo1", "Fragmento2", "Codigo2", "Similitud") %in% names(result)))
})

test_that("analizar_coherencia_codigos returns a tibble with expected columns", {
  sim_mat <- calcular_similitud_coseno(sample_embeddings)
  result <- analizar_coherencia_codigos(sample_tabla, sim_mat)
  expect_s3_class(result, "tbl_df")
  expected_cols <- c("Codigo", "N_Fragmentos", "Coherencia_Media",
                     "Coherencia_Min", "Coherencia_Max", "Coherencia_SD", "Evaluacion")
  expect_true(all(expected_cols %in% names(result)))
  expect_true(nrow(result) >= 1)
})
