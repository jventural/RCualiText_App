# R/analysis.R
# Analysis and computation functions for RCualiText App
# Includes: similarity, clustering, coherence, semantic network

#' @title Compute cosine similarity matrix
#' @description Computes the pairwise cosine similarity between the rows of an embedding matrix after L2 normalization.
#' @param embeddings_matrix Numeric matrix. Rows correspond to observations and columns to embedding dimensions.
#' @return A square numeric similarity matrix of size `nrow(embeddings_matrix)`, or NULL if the input is invalid or has fewer than 2 rows.
calcular_similitud_coseno <- function(embeddings_matrix) {
  if (is.null(embeddings_matrix) || nrow(embeddings_matrix) < 2) return(NULL)
  normas <- sqrt(rowSums(embeddings_matrix^2))
  normas[normas == 0] <- 1
  embeddings_norm <- embeddings_matrix / normas
  similitud <- embeddings_norm %*% t(embeddings_norm)
  return(similitud)
}

#' @title Semantic clustering of embeddings
#' @description Performs clustering on an embedding matrix using either k-means or hierarchical clustering (Ward's method), choosing an automatic number of clusters if not specified.
#' @param embeddings_matrix Numeric matrix. Rows are observations, columns are embedding dimensions.
#' @param n_clusters Integer or NULL. Desired number of clusters; if NULL it is estimated from the number of observations.
#' @param metodo Character. Clustering method, either "kmeans" or "hclust" (default "kmeans").
#' @return A list with cluster assignments and method-specific fields (e.g. centers, within/between SS, or the hclust object), or NULL if the input is invalid.
clustering_semantico <- function(embeddings_matrix, n_clusters = NULL, metodo = "kmeans") {
  if (is.null(embeddings_matrix) || nrow(embeddings_matrix) < 2) return(NULL)
  n_obs <- nrow(embeddings_matrix)
  if (is.null(n_clusters)) {
    n_clusters <- min(max(2, floor(sqrt(n_obs / 2))), n_obs - 1)
  }
  n_clusters <- min(n_clusters, n_obs - 1)
  if (metodo == "kmeans") {
    set.seed(2026)
    km <- kmeans(embeddings_matrix, centers = n_clusters, nstart = 25, iter.max = 100)
    list(clusters = km$cluster, centros = km$centers, total_ss = km$totss,
         within_ss = km$tot.withinss, between_ss = km$betweenss, n_clusters = n_clusters)
  } else if (metodo == "hclust") {
    dist_matrix <- dist(embeddings_matrix)
    hc <- hclust(dist_matrix, method = "ward.D2")
    clusters <- cutree(hc, k = n_clusters)
    list(clusters = clusters, hclust_obj = hc, n_clusters = n_clusters)
  }
}

#' @title Detect similar fragments with different codes
#' @description Scans the upper triangle of a similarity matrix to find fragment pairs above a threshold that were assigned different codes, which may indicate inconsistent coding.
#' @param tabla Data frame of fragments. Must contain `Extracto` and `Codigo` columns aligned with the rows of `similitud_matrix`.
#' @param similitud_matrix Numeric square matrix of pairwise similarities (e.g. cosine similarity).
#' @param umbral Numeric. Minimum similarity threshold to consider a pair suspicious (default 0.8).
#' @param labels Named list with strings `alta` and `moderada` used in the suggestion column.
#' @return A tibble with columns `Fragmento1`, `Codigo1`, `Fragmento2`, `Codigo2`, `Similitud`, `Sugerencia`, sorted by descending similarity.
detectar_similares_diferente_codigo <- function(tabla, similitud_matrix, umbral = 0.8,
    labels = list(alta = "High similarity - review coding", moderada = "Moderate similarity - consider merging")) {
  if (is.null(similitud_matrix) || nrow(tabla) < 2) return(tibble::tibble())
  n <- nrow(similitud_matrix)
  # Vectorized: get upper triangle indices where similarity >= threshold
  upper_tri <- which(upper.tri(similitud_matrix) & similitud_matrix >= umbral, arr.ind = TRUE)
  if (nrow(upper_tri) == 0) return(tibble::tibble())
  # Filter to pairs with different codes
  i_idx <- upper_tri[, 1]
  j_idx <- upper_tri[, 2]
  diff_code <- tabla$Codigo[i_idx] != tabla$Codigo[j_idx]
  if (!any(diff_code)) return(tibble::tibble())
  i_idx <- i_idx[diff_code]
  j_idx <- j_idx[diff_code]
  sims <- similitud_matrix[cbind(i_idx, j_idx)]
  result <- tibble::tibble(
    Fragmento1 = tabla$Extracto[i_idx],
    Codigo1 = tabla$Codigo[i_idx],
    Fragmento2 = tabla$Extracto[j_idx],
    Codigo2 = tabla$Codigo[j_idx],
    Similitud = round(sims, 3),
    Sugerencia = ifelse(sims > 0.9, labels$alta, labels$moderada)
  )
  result %>% dplyr::arrange(dplyr::desc(Similitud))
}

#' @title Analyze intra-code coherence
#' @description For each unique code, computes similarity statistics (mean, min, max, SD) among its fragments and classifies the code's internal coherence into evaluation categories.
#' @param tabla Data frame of fragments. Must contain a `Codigo` column aligned with the rows of `similitud_matrix`.
#' @param similitud_matrix Numeric square matrix of pairwise similarities.
#' @return A tibble with one row per code, columns `Codigo`, `N_Fragmentos`, `Coherencia_Media/Min/Max/SD`, and `Evaluacion`, sorted by mean coherence.
analizar_coherencia_codigos <- function(tabla, similitud_matrix) {
  if (is.null(similitud_matrix) || nrow(tabla) < 2) return(tibble::tibble())
  codigos_unicos <- unique(tabla$Codigo)
  coherencia_por_codigo <- list()
  for (codigo in codigos_unicos) {
    indices <- which(tabla$Codigo == codigo)
    if (length(indices) >= 2) {
      sub_sim <- similitud_matrix[indices, indices, drop = FALSE]
      valores <- sub_sim[lower.tri(sub_sim)]
      coherencia_por_codigo[[length(coherencia_por_codigo) + 1]] <- tibble::tibble(
        Codigo = codigo, N_Fragmentos = length(indices),
        Coherencia_Media = round(mean(valores), 3), Coherencia_Min = round(min(valores), 3),
        Coherencia_Max = round(max(valores), 3), Coherencia_SD = round(sd(valores), 3),
        Evaluacion = dplyr::case_when(
          mean(valores) >= 0.8 ~ "excellent", mean(valores) >= 0.6 ~ "good",
          mean(valores) >= 0.4 ~ "moderate", TRUE ~ "low_review"
        )
      )
    } else {
      coherencia_por_codigo[[length(coherencia_por_codigo) + 1]] <- tibble::tibble(
        Codigo = codigo, N_Fragmentos = length(indices),
        Coherencia_Media = NA_real_, Coherencia_Min = NA_real_,
        Coherencia_Max = NA_real_, Coherencia_SD = NA_real_,
        Evaluacion = "insufficient"
      )
    }
  }
  dplyr::bind_rows(coherencia_por_codigo) %>% dplyr::arrange(dplyr::desc(Coherencia_Media))
}

#' @title Build a semantic network of codes
#' @description Computes centroid embeddings per code, derives pairwise cosine similarities between codes, and builds an undirected tidygraph with nodes sized by frequency and edges filtered by a connection threshold.
#' @param embeddings_matrix Numeric matrix of per-fragment embeddings aligned with rows of `tabla`.
#' @param tabla Data frame of fragments with columns `Codigo` and `Categoria`.
#' @param umbral_conexion Numeric. Minimum cosine similarity required to create an edge between two codes (default 0.5).
#' @return A named list with elements `grafo` (tbl_graph), `nodos`, `edges`, `similitud_matrix`, `n_codigos`, and `n_conexiones`, or NULL if inputs are insufficient.
calcular_red_semantica_codigos <- function(embeddings_matrix, tabla, umbral_conexion = 0.5) {
  if (is.null(embeddings_matrix) || nrow(tabla) < 2) return(NULL)
  codigos_unicos <- unique(tabla$Codigo)
  if (length(codigos_unicos) < 2) return(NULL)

  centroides <- list()
  frecuencias <- list()
  categorias <- list()
  for (codigo in codigos_unicos) {
    indices <- which(tabla$Codigo == codigo)
    if (length(indices) > 0) {
      centroides[[codigo]] <- if (length(indices) == 1) embeddings_matrix[indices, ] else colMeans(embeddings_matrix[indices, , drop = FALSE])
      frecuencias[[codigo]] <- length(indices)
      categorias[[codigo]] <- tabla$Categoria[indices[1]]
    }
  }
  centroide_matrix <- do.call(rbind, centroides)
  rownames(centroide_matrix) <- codigos_unicos
  normas <- sqrt(rowSums(centroide_matrix^2))
  normas[normas == 0] <- 1
  centroide_norm <- centroide_matrix / normas
  similitud_codigos <- centroide_norm %*% t(centroide_norm)

  nodos <- tibble::tibble(
    name = codigos_unicos,
    frecuencia = unlist(frecuencias[codigos_unicos]),
    categoria = unlist(categorias[codigos_unicos]),
    size = scales::rescale(unlist(frecuencias[codigos_unicos]), to = c(5, 25))
  )

  # Vectorized edge detection
  n <- length(codigos_unicos)
  upper_idx <- which(upper.tri(similitud_codigos) & similitud_codigos >= umbral_conexion, arr.ind = TRUE)
  if (nrow(upper_idx) > 0) {
    sims <- similitud_codigos[upper_idx]
    edges_df <- tibble::tibble(
      from = codigos_unicos[upper_idx[, 1]],
      to = codigos_unicos[upper_idx[, 2]],
      weight = round(sims, 3),
      width = scales::rescale(sims, to = c(0.5, 4), from = c(umbral_conexion, 1))
    )
  } else {
    edges_df <- tibble::tibble(from = character(), to = character(), weight = numeric(), width = numeric())
  }

  grafo <- tidygraph::tbl_graph(nodes = nodos, edges = edges_df, directed = FALSE)

  list(grafo = grafo, nodos = nodos, edges = edges_df, similitud_matrix = similitud_codigos,
       n_codigos = length(codigos_unicos), n_conexiones = nrow(edges_df))
}
# Note: plot_embeddings_semantico is in R/plots.R
