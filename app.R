# app.R

# ========================================
# Cargar librerías
# ========================================
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(colourpicker)
library(officer)
library(DT)
library(openxlsx)
library(bslib)
library(shinycssloaders)
library(shinyWidgets)
# para análisis y visualización
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(patchwork)
library(plotly)
# para análisis con IA
library(httr)
library(jsonlite)
library(readxl)
# para análisis semántico
library(digest)  # Para cache hash
options(shiny.maxRequestSize = 50 * 1024^2)

# Asegurar que pipe viene de magrittr/dplyr
`%>%` <- magrittr::`%>%`

# ========================================
# Función para leer archivos (.txt, .docx)
# ========================================
leer_archivo <- function(archivo) {
  ext <- tools::file_ext(archivo$datapath)
  if (ext == "txt") {
    lineas <- readLines(archivo$datapath, encoding = "UTF-8")
    # Eliminar líneas vacías del inicio y final, mantener las internas
    lineas_limpias <- lineas[lineas != ""]
    texto_final <- paste(lineas_limpias, collapse = "\n")
    # Limpiar espacios al inicio y final
    str_trim(texto_final)
  } else if (ext %in% c("docx","doc")) {
    doc <- read_docx(archivo$datapath)
    df  <- docx_summary(doc)
    df  <- df[!is.na(df$text) & df$text != "", ]
    df  <- df[!duplicated(df$text), ]
    texto_final <- paste(df$text, collapse = "\n")
    # Limpiar espacios al inicio y final
    str_trim(texto_final)
  } else {
    "Formato no soportado"
  }
}

# ========================================
# Funciones auxiliares para resaltado múltiple
# ========================================

# Función para crear un ID único para cada fragmento
crear_fragment_id <- function() {
  paste0("fragment_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
}

# Función para generar CSS de resaltado múltiple
generar_css_multiples <- function(colores) {
  if (length(colores) == 1) {
    return(paste0("background-color: ", colores[1], "; box-shadow: 0 2px 8px rgba(0,0,0,0.15);"))
  }
  
  # Para múltiples colores, crear un gradiente lineal con franjas
  n_colores <- length(colores)
  porcentaje <- 100 / n_colores
  
  stops <- character()
  for (i in seq_along(colores)) {
    inicio <- (i - 1) * porcentaje
    fin <- i * porcentaje
    stops <- c(stops, paste0(colores[i], " ", inicio, "% ", fin, "%"))
  }
  
  gradient <- paste0("linear-gradient(135deg, ", paste(stops, collapse = ", "), ")")
  return(paste0("background: ", gradient, "; box-shadow: 0 3px 12px rgba(0,0,0,0.2); border-radius: 6px;"))
}

# Función para aplicar resaltado múltiple al texto
aplicar_resaltado_multiple <- function(texto_original, fragmentos_df) {
  if (nrow(fragmentos_df) == 0) return(texto_original)
  
  texto_procesado <- texto_original
  
  # Agrupar fragmentos por texto exacto
  fragmentos_agrupados <- fragmentos_df %>%
    group_by(Extracto, Archivo) %>%
    summarise(
      Codigos = list(Codigo),
      Colores = list(Color),
      FragmentId = first(FragmentId),
      .groups = "drop"
    )
  
  # Aplicar cada grupo de fragmentos
  for (i in seq_len(nrow(fragmentos_agrupados))) {
    frag <- fragmentos_agrupados[i, ]
    texto_buscar <- frag$Extracto
    colores <- unlist(frag$Colores)
    fragment_id <- frag$FragmentId
    codigos_aplicados <- paste(unlist(frag$Codigos), collapse = ", ")
    
    # Generar CSS para múltiples colores
    css_style <- generar_css_multiples(colores)
    
    # Crear el span con el resaltado y tooltip
    span_text <- paste0(
      "<span class='highlight-multiple' data-fragment-id='", fragment_id, "' ",
      "title='Códigos: ", codigos_aplicados, "' ",
      "style='", css_style, " padding: 4px 8px; margin: 2px; color: #fff; font-weight: 500; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);'>",
      texto_buscar,
      "</span>"
    )
    
    # Reemplazar solo si el texto no está ya resaltado
    if (!grepl(paste0("data-fragment-id='", fragment_id, "'"), texto_procesado)) {
      texto_procesado <- sub(texto_buscar, span_text, texto_procesado, fixed = TRUE)
    }
  }
  
  return(texto_procesado)
}

# ========================================
# Funciones de análisis y visualización (versiones optimizadas)
# ========================================
plot_codigos <- function(df, fill = TRUE, code_colors = NULL) {
  # Limpiar datos de categorías vacías o NA
  df <- df %>%
    mutate(Categoria = case_when(
      is.na(Categoria) | Categoria == "" ~ "Sin categoría",
      TRUE ~ Categoria
    ))

  # Pre-calcular conteos
  if (fill && "Categoria" %in% names(df)) {
    df_counts <- df %>%
      count(Archivo, Codigo, Categoria, name = "Frecuencia") %>%
      group_by(Archivo) %>%
      mutate(Codigo = factor(Codigo, levels = Codigo[order(Frecuencia)])) %>%
      ungroup()
  } else {
    df_counts <- df %>%
      count(Archivo, Codigo, name = "Frecuencia") %>%
      group_by(Archivo) %>%
      mutate(Codigo = factor(Codigo, levels = Codigo[order(Frecuencia)])) %>%
      ungroup()
  }

  # Usar plotly nativo con textposition = "outside"
  if (fill && "Categoria" %in% names(df_counts)) {
    p <- plotly::plot_ly(
      data = df_counts,
      y = ~Codigo,
      x = ~Frecuencia,
      color = ~Categoria,
      type = "bar",
      orientation = "h",
      text = ~Frecuencia,
      textposition = "outside",
      textfont = list(size = 12, color = "#2c3e50", family = "Arial Black"),
      hovertemplate = "<b>%{y}</b><br>Frecuencia: %{x}<extra></extra>"
    )
  } else {
    # Aplicar colores personalizados si existen
    if (!is.null(code_colors)) {
      df_counts <- df_counts %>%
        mutate(Color = code_colors[as.character(Codigo)])

      p <- plotly::plot_ly(
        data = df_counts,
        y = ~Codigo,
        x = ~Frecuencia,
        type = "bar",
        orientation = "h",
        text = ~Frecuencia,
        textposition = "outside",
        textfont = list(size = 12, color = "#2c3e50", family = "Arial Black"),
        marker = list(color = ~Color),
        hovertemplate = "<b>%{y}</b><br>Frecuencia: %{x}<extra></extra>"
      )
    } else {
      p <- plotly::plot_ly(
        data = df_counts,
        y = ~Codigo,
        x = ~Frecuencia,
        color = ~Codigo,
        type = "bar",
        orientation = "h",
        text = ~Frecuencia,
        textposition = "outside",
        textfont = list(size = 12, color = "#2c3e50", family = "Arial Black"),
        hovertemplate = "<b>%{y}</b><br>Frecuencia: %{x}<extra></extra>"
      )
    }
  }

  # Layout común
  p <- p %>%
    plotly::layout(
      xaxis = list(
        title = list(text = "Frecuencia", font = list(size = 12, family = "sans-serif")),
        tickfont = list(size = 10)
      ),
      yaxis = list(
        title = list(text = "Códigos", font = list(size = 12, family = "sans-serif")),
        tickfont = list(size = 10),
        categoryorder = "total ascending"
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        font = list(size = 10)
      ),
      margin = list(l = 120, r = 80, t = 40, b = 80),
      barmode = "stack",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    plotly::config(displayModeBar = FALSE)

  return(p)
}

# Función ggplot para descarga JPG (mantener para exportación estática)
plot_codigos_ggplot <- function(df, fill = TRUE, code_colors = NULL) {
  df <- df %>%
    mutate(Categoria = case_when(
      is.na(Categoria) | Categoria == "" ~ "Sin categoría",
      TRUE ~ Categoria
    ))

  if (fill && "Categoria" %in% names(df)) {
    df_counts <- df %>%
      count(Archivo, Codigo, Categoria, name = "Frecuencia") %>%
      group_by(Archivo) %>%
      mutate(Codigo = factor(Codigo, levels = Codigo[order(Frecuencia)])) %>%
      ungroup()

    p <- ggplot(df_counts, aes(x = Codigo, y = Frecuencia, fill = Categoria)) +
      geom_col() +
      geom_text(aes(label = Frecuencia), hjust = -0.3, size = 4, fontface = "bold", color = "#2c3e50") +
      facet_wrap(~ Archivo, scales = "free_y") +
      coord_flip() +
      labs(x = "Códigos", y = "Frecuencia", fill = "Categoría")
  } else {
    df_counts <- df %>%
      count(Archivo, Codigo, name = "Frecuencia") %>%
      group_by(Archivo) %>%
      mutate(Codigo = factor(Codigo, levels = Codigo[order(Frecuencia)])) %>%
      ungroup()

    p <- ggplot(df_counts, aes(x = Codigo, y = Frecuencia, fill = Codigo)) +
      geom_col() +
      geom_text(aes(label = Frecuencia), hjust = -0.3, size = 4, fontface = "bold", color = "#2c3e50") +
      facet_wrap(~ Archivo, scales = "free_y") +
      coord_flip() +
      labs(x = "Códigos", y = "Frecuencia", fill = "Código")
  }

  p <- p +
    theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

  if (!fill && !is.null(code_colors)) {
    p <- p + scale_fill_manual(values = code_colors)
  }
  p
}

plot_network_and_centrality <- function(df, code_colors = NULL) {
  dtm <- df %>%
    count(Archivo, Codigo, name="freq") %>%
    pivot_wider(names_from=Codigo, values_from=freq, values_fill=0)
  mat <- dtm %>% select(-Archivo) %>% as.matrix() %>% crossprod()
  
  graph_tbl <- mat %>%
    graph_from_adjacency_matrix(weighted=TRUE) %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    mutate(
      full_name   = name,
      label_abbr  = substr(name,1,3),
      strength    = centrality_degree(weights=weight),
      closeness   = centrality_closeness(),
      betweenness = centrality_betweenness()
    )
  
  # Establecer semilla aleatoria fija para layout consistente
  set.seed(2026)
  
  # Configurar plot de red - optimizado para descarga
  net_plot <- ggraph(graph_tbl, layout="fr") +
    geom_edge_link(aes(width=weight), color="gray80", alpha=0.6) +
    scale_edge_width(range = c(0.5, 3), guide = "none") +
    geom_node_point(aes(fill=full_name), shape=21, size=12, color="white") +
    { if (!is.null(code_colors))
      scale_fill_manual(name="Código", values=code_colors)
      else
        scale_fill_brewer(name="Código", palette="Set3") } +
    geom_node_text(aes(label=label_abbr), size=3.5) +
    guides(fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0.5
    )) +
    theme_void(base_size = 11) +
    theme(
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9)
    )
  
  cents <- graph_tbl %>%
    as_tibble() %>%
    select(full_name, strength) %>%
    pivot_longer(cols = -full_name, names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(zscore = round((value - mean(value)) / sd(value), 2)) %>%
    ungroup()
  
  # Plot de centralidad - optimizado para descarga
  cent_plot <- cents %>%
    filter(metric == "strength") %>%
    ggplot(aes(full_name, zscore, group = metric)) +
    geom_line(color = "gray40", linewidth = 1) +
    geom_point(aes(fill = full_name), shape = 21, size = 5, color = "white", stroke = 1.5) +
    { if (!is.null(code_colors))
      scale_fill_manual(values = code_colors, guide = "none")
      else
        scale_fill_brewer(palette = "Set3", guide = "none") } +
    coord_flip() +
    labs(y = "Centralidad (z-score)", x = "Código") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      axis.title = element_text(size = 10, face = "bold")
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7)
  
  # Combinar plots con leyenda compartida en la parte inferior centrada
  combined <- net_plot + cent_plot +
    plot_layout(ncol=2, widths=c(2.5,1), guides="collect")
  
  # Aplicar tema global para centrar la leyenda
  combined <- combined +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 8),
        legend.margin = ggplot2::margin(t = 8, b = 5),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 15, l = 5)
      )
    )
  
  list(plot = combined, table = cents)
}

# ========================================
# Funciones para Análisis Semántico (OpenAI API)
# ========================================

# Embeddings via OpenAI
obtener_embeddings_openai <- function(textos, api_key, modelo = "text-embedding-3-small") {
  all_embeddings <- list()
  batch_size <- 100
  n_batches <- ceiling(length(textos) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(textos))
    batch_textos <- textos[start_idx:end_idx]

    resp <- httr::POST(
      "https://api.openai.com/v1/embeddings",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      httr::timeout(120),
      body = jsonlite::toJSON(list(
        model = modelo,
        input = batch_textos
      ), auto_unbox = TRUE)
    )

    if (httr::status_code(resp) == 200) {
      content <- httr::content(resp)
      for (item in content$data) {
        all_embeddings[[length(all_embeddings) + 1]] <- unlist(item$embedding)
      }
    } else {
      stop(paste("Error OpenAI Embeddings:", httr::status_code(resp)))
    }
  }

  embeddings_matrix <- do.call(rbind, all_embeddings)
  return(embeddings_matrix)
}

# Función para calcular similitud coseno
calcular_similitud_coseno <- function(embeddings_matrix) {
  if (is.null(embeddings_matrix) || nrow(embeddings_matrix) < 2) {
    return(NULL)
  }

  # Normalizar vectores
  normas <- sqrt(rowSums(embeddings_matrix^2))
  normas[normas == 0] <- 1  # Evitar división por cero
  embeddings_norm <- embeddings_matrix / normas

  # Calcular matriz de similitud coseno
  similitud <- embeddings_norm %*% t(embeddings_norm)

  return(similitud)
}

# Función para clustering semántico
clustering_semantico <- function(embeddings_matrix, n_clusters = NULL, metodo = "kmeans") {
  if (is.null(embeddings_matrix) || nrow(embeddings_matrix) < 2) {
    return(NULL)
  }

  n_obs <- nrow(embeddings_matrix)

  # Determinar número óptimo de clusters si no se especifica
  if (is.null(n_clusters)) {
    n_clusters <- min(max(2, floor(sqrt(n_obs / 2))), n_obs - 1)
  }

  n_clusters <- min(n_clusters, n_obs - 1)

  if (metodo == "kmeans") {
    set.seed(2026)
    km <- kmeans(embeddings_matrix, centers = n_clusters, nstart = 25, iter.max = 100)

    resultado <- list(
      clusters = km$cluster,
      centros = km$centers,
      total_ss = km$totss,
      within_ss = km$tot.withinss,
      between_ss = km$betweenss,
      n_clusters = n_clusters
    )
  } else if (metodo == "hclust") {
    dist_matrix <- dist(embeddings_matrix)
    hc <- hclust(dist_matrix, method = "ward.D2")
    clusters <- cutree(hc, k = n_clusters)

    resultado <- list(
      clusters = clusters,
      hclust_obj = hc,
      n_clusters = n_clusters
    )
  }

  return(resultado)
}

# Función para detectar fragmentos similares con diferente código
detectar_similares_diferente_codigo <- function(tabla, similitud_matrix, umbral = 0.8) {
  if (is.null(similitud_matrix) || nrow(tabla) < 2) {
    return(tibble())
  }

  n <- nrow(similitud_matrix)
  inconsistencias <- list()

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      sim <- similitud_matrix[i, j]
      if (sim >= umbral) {
        codigo_i <- tabla$Codigo[i]
        codigo_j <- tabla$Codigo[j]

        if (codigo_i != codigo_j) {
          inconsistencias[[length(inconsistencias) + 1]] <- tibble(
            Fragmento1 = tabla$Extracto[i],
            Codigo1 = codigo_i,
            Fragmento2 = tabla$Extracto[j],
            Codigo2 = codigo_j,
            Similitud = round(sim, 3),
            Sugerencia = ifelse(sim > 0.9, "Alta similitud - revisar codificación", "Similitud moderada - considerar unificar")
          )
        }
      }
    }
  }

  if (length(inconsistencias) > 0) {
    return(bind_rows(inconsistencias) %>% arrange(desc(Similitud)))
  }

  return(tibble())
}

# Función para analizar coherencia de códigos
analizar_coherencia_codigos <- function(tabla, similitud_matrix) {
  if (is.null(similitud_matrix) || nrow(tabla) < 2) {
    return(tibble())
  }

  codigos_unicos <- unique(tabla$Codigo)
  coherencia_por_codigo <- list()

  for (codigo in codigos_unicos) {
    indices <- which(tabla$Codigo == codigo)

    if (length(indices) >= 2) {
      # Obtener submatriz de similitud para este código
      sub_sim <- similitud_matrix[indices, indices, drop = FALSE]

      # Calcular estadísticas de coherencia
      valores <- sub_sim[lower.tri(sub_sim)]

      coherencia_por_codigo[[length(coherencia_por_codigo) + 1]] <- tibble(
        Codigo = codigo,
        N_Fragmentos = length(indices),
        Coherencia_Media = round(mean(valores), 3),
        Coherencia_Min = round(min(valores), 3),
        Coherencia_Max = round(max(valores), 3),
        Coherencia_SD = round(sd(valores), 3),
        Evaluacion = case_when(
          mean(valores) >= 0.8 ~ "Excelente",
          mean(valores) >= 0.6 ~ "Buena",
          mean(valores) >= 0.4 ~ "Moderada",
          TRUE ~ "Baja - revisar"
        )
      )
    } else {
      coherencia_por_codigo[[length(coherencia_por_codigo) + 1]] <- tibble(
        Codigo = codigo,
        N_Fragmentos = length(indices),
        Coherencia_Media = NA_real_,
        Coherencia_Min = NA_real_,
        Coherencia_Max = NA_real_,
        Coherencia_SD = NA_real_,
        Evaluacion = "Insuficiente (< 2 fragmentos)"
      )
    }
  }

  return(bind_rows(coherencia_por_codigo) %>% arrange(desc(Coherencia_Media)))
}

# Función para calcular red semántica de códigos
calcular_red_semantica_codigos <- function(embeddings_matrix, tabla, umbral_conexion = 0.5) {
  if (is.null(embeddings_matrix) || nrow(tabla) < 2) {
    return(NULL)
  }

  # Obtener códigos únicos
  codigos_unicos <- unique(tabla$Codigo)

  if (length(codigos_unicos) < 2) {
    return(NULL)
  }

  # Calcular centroide (embedding promedio) para cada código
  centroides <- list()
  frecuencias <- list()
  categorias <- list()

  for (codigo in codigos_unicos) {
    indices <- which(tabla$Codigo == codigo)
    if (length(indices) > 0) {
      # Centroide = promedio de embeddings
      if (length(indices) == 1) {
        centroides[[codigo]] <- embeddings_matrix[indices, ]
      } else {
        centroides[[codigo]] <- colMeans(embeddings_matrix[indices, , drop = FALSE])
      }
      frecuencias[[codigo]] <- length(indices)
      categorias[[codigo]] <- tabla$Categoria[indices[1]]
    }
  }

  # Crear matriz de centroides
  centroide_matrix <- do.call(rbind, centroides)
  rownames(centroide_matrix) <- codigos_unicos

  # Calcular similitud coseno entre centroides
  normas <- sqrt(rowSums(centroide_matrix^2))
  normas[normas == 0] <- 1
  centroide_norm <- centroide_matrix / normas
  similitud_codigos <- centroide_norm %*% t(centroide_norm)

  # Crear nodos

  nodos <- tibble(
    name = codigos_unicos,
    frecuencia = unlist(frecuencias[codigos_unicos]),
    categoria = unlist(categorias[codigos_unicos]),
    size = scales::rescale(unlist(frecuencias[codigos_unicos]), to = c(5, 25))
  )

  # Crear edges (conexiones) basadas en umbral de similitud
  edges <- list()
  n <- length(codigos_unicos)

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      sim <- similitud_codigos[i, j]
      if (sim >= umbral_conexion) {
        edges[[length(edges) + 1]] <- tibble(
          from = codigos_unicos[i],
          to = codigos_unicos[j],
          weight = round(sim, 3),
          width = scales::rescale(sim, to = c(0.5, 4), from = c(umbral_conexion, 1))
        )
      }
    }
  }

  edges_df <- if (length(edges) > 0) bind_rows(edges) else tibble(from = character(), to = character(), weight = numeric(), width = numeric())

  # Crear grafo con tidygraph
  if (nrow(edges_df) > 0) {
    grafo <- tidygraph::tbl_graph(nodes = nodos, edges = edges_df, directed = FALSE)
  } else {
    # Sin conexiones, solo nodos
    grafo <- tidygraph::tbl_graph(nodes = nodos, edges = edges_df, directed = FALSE)
  }

  return(list(
    grafo = grafo,
    nodos = nodos,
    edges = edges_df,
    similitud_matrix = similitud_codigos,
    n_codigos = length(codigos_unicos),
    n_conexiones = nrow(edges_df)
  ))
}

# Función para visualización de embeddings (t-SNE/PCA)
plot_embeddings_semantico <- function(embeddings_matrix, tabla, metodo = "pca") {
  if (is.null(embeddings_matrix) || nrow(embeddings_matrix) < 3) {
    return(NULL)
  }

  n_obs <- nrow(embeddings_matrix)

  if (metodo == "tsne" && requireNamespace("Rtsne", quietly = TRUE)) {
    set.seed(2026)
    perplexity <- min(30, floor((n_obs - 1) / 3))
    perplexity <- max(perplexity, 1)

    tsne_result <- Rtsne::Rtsne(embeddings_matrix, dims = 2, perplexity = perplexity,
                                 verbose = FALSE, max_iter = 500)
    coords <- data.frame(
      X = tsne_result$Y[, 1],
      Y = tsne_result$Y[, 2]
    )
  } else if (metodo == "umap" && requireNamespace("umap", quietly = TRUE)) {
    set.seed(2026)
    n_neighbors <- min(15, n_obs - 1)
    umap_result <- umap::umap(embeddings_matrix, n_neighbors = n_neighbors)
    coords <- data.frame(
      X = umap_result$layout[, 1],
      Y = umap_result$layout[, 2]
    )
  } else {
    # PCA como fallback
    pca_result <- prcomp(embeddings_matrix, scale. = TRUE)
    coords <- data.frame(
      X = pca_result$x[, 1],
      Y = pca_result$x[, 2]
    )
    metodo <- "pca"
  }

  # Preparar datos para el gráfico
  plot_data <- coords %>%
    mutate(
      Codigo = tabla$Codigo[1:nrow(coords)],
      Extracto = stringr::str_trunc(tabla$Extracto[1:nrow(coords)], 50),
      Categoria = tabla$Categoria[1:nrow(coords)]
    )

  # Crear gráfico
  p <- ggplot(plot_data, aes(x = X, y = Y, color = Codigo, text = Extracto)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = paste("Visualización de Embeddings (", toupper(metodo), ")", sep = ""),
      x = paste(toupper(metodo), "Dimensión 1"),
      y = paste(toupper(metodo), "Dimensión 2"),
      color = "Código"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

  return(p)
}

# Función para validar codificación con LLM (OpenAI)
validar_codificacion_llm <- function(fragmentos, codigos, api_key) {
  if (is.null(fragmentos) || length(fragmentos) == 0) {
    stop("No hay fragmentos para validar")
  }

  if (is.null(api_key) || !nzchar(api_key)) {
    stop("API Key de OpenAI no proporcionada")
  }

  # Preparar prompt para validación
  fragmentos_texto <- paste(
    sapply(seq_along(fragmentos), function(i) {
      paste0("Fragmento ", i, " [Código: ", codigos[i], "]: \"", fragmentos[i], "\"")
    }),
    collapse = "\n"
  )

  prompt <- paste0(
    "Actúa como un panel de 3 expertos en análisis cualitativo. Evalúa si los siguientes fragmentos están correctamente codificados:\n\n",
    fragmentos_texto,
    "\n\nPara cada fragmento, proporciona:\n",
    "1. Evaluación (Correcto/Revisar/Incorrecto)\n",
    "2. Justificación breve\n",
    "3. Código alternativo sugerido (si aplica)\n\n",
    "Responde en formato estructurado."
  )

  # Llamar a OpenAI
  resultado <- call_openai_api(prompt, api_key)

  if (is.null(resultado) || !nzchar(resultado)) {
    stop("No se recibió respuesta de OpenAI")
  }

  return(resultado)
}

# ========================================
# ========================================
# Configuración de OpenAI API (GPT-4.1)
# ========================================
OPENAI_MODEL <- "gpt-4.1"  # Modelo principal

# ========================================
# Función principal para llamar a OpenAI
# ========================================
call_openai_api <- function(prompt, api_key, system_prompt = NULL) {
  if (is.null(system_prompt)) {
    system_prompt <- "Eres un experto en análisis cualitativo de datos textuales y codificación temática."
  }

  resp <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    httr::timeout(180),
    body = jsonlite::toJSON(list(
      model = OPENAI_MODEL,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = prompt)
      ),
      temperature = 0.3,
      max_tokens = 4096
    ), auto_unbox = TRUE)
  )

  if (httr::status_code(resp) == 200) {
    content <- httr::content(resp)
    return(content$choices[[1]]$message$content)
  } else {
    error_content <- httr::content(resp, "text", encoding = "UTF-8")
    stop(paste("Error OpenAI (", httr::status_code(resp), "):", error_content))
  }
}

# ========================================
# UI (actualizado con controles de descarga personalizados)
# ========================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = div(
      style = "font-weight: bold; font-size: 18px; color: #fff; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);",
      icon("microscope", style = "margin-right: 8px;"),
      "RCualiText"
    ), 
    titleWidth = 280
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Documento", tabName = "texto", icon = icon("file-text")),
      menuItem("Códigos", tabName = "codigos", icon = icon("tags")),
      menuItem("Categorías", tabName = "categorias", icon = icon("folder-open")),
      menuItem("Extractos", tabName = "resaltes", icon = icon("highlighter")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-bar")),
      menuItem("Análisis IA (opcional)", tabName = "analisis_ia", icon = icon("robot")),
      menuItem("Análisis Semántico (experimental)", tabName = "analisis_semantico", icon = icon("brain")),
      menuItem("Reporte con IA", tabName = "reporte_ia", icon = icon("file-alt")),
      menuItem("Proyecto", tabName = "estado", icon = icon("save")),
      menuItem("Citar", tabName = "citar", icon = icon("quote-right")),
      menuItem("Ayuda", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    theme = bs_theme(
      bootswatch = "flatly",
      base_font = font_google("Source Sans Pro"),
      primary = "#2c3e50",
      secondary = "#7f8c8d",
      success = "#27ae60",
      warning = "#d35400",
      danger = "#c0392b",
      info = "#2980b9"
    ),
    useShinyjs(),
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&family=Source+Serif+Pro:wght@400;600;700&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        /* ===== RCualiText - Diseño Académico Formal ===== */

        /* Variables de color académicas */
        :root {
          --primary: #2c3e50;
          --primary-light: #34495e;
          --secondary: #7f8c8d;
          --accent: #2980b9;
          --success: #27ae60;
          --warning: #d35400;
          --danger: #c0392b;
          --text-dark: #1a1a2e;
          --text-medium: #4a4a4a;
          --text-light: #6c757d;
          --bg-light: #f8f9fa;
          --bg-white: #ffffff;
          --border-color: #dee2e6;
        }

        /* Estilos generales - Diseño claro académico */
        body {
          font-family: 'Source Sans Pro', -apple-system, BlinkMacSystemFont, sans-serif;
          background: var(--bg-light);
          color: var(--text-dark);
          min-height: 100vh;
        }

        .content-wrapper {
          background: #f4f6f9;
          min-height: 100vh;
          padding: 20px;
        }

        /* === SIDEBAR - Estilos forzados === */
        .main-sidebar,
        .left-side,
        .skin-black .main-sidebar,
        .skin-black .left-side {
          background: #f4f6f9 !important;
          border-right: 1px solid #e0e4e8 !important;
          box-shadow: none !important;
        }

        .skin-black .sidebar-menu > li > a,
        .main-sidebar .sidebar-menu > li > a,
        .sidebar-menu > li > a {
          color: #2c3e50 !important;
          background: transparent !important;
          border-left: 3px solid transparent !important;
          padding: 12px 15px !important;
          font-size: 14px !important;
          transition: all 0.2s ease;
        }

        .skin-black .sidebar-menu > li > a:hover,
        .main-sidebar .sidebar-menu > li > a:hover,
        .sidebar-menu > li > a:hover {
          background: #ebeef2 !important;
          color: #1a252f !important;
          border-left-color: #2c3e50 !important;
        }

        .skin-black .sidebar-menu > li.active > a,
        .main-sidebar .sidebar-menu > li.active > a,
        .sidebar-menu > li.active > a {
          background: #e0e4e8 !important;
          color: #1a252f !important;
          border-left-color: #2c3e50 !important;
        }

        .sidebar-menu > li > a > .fa,
        .sidebar-menu > li > a > .glyphicon,
        .sidebar-menu > li > a > i {
          color: #5a6c7d !important;
        }

        .sidebar-menu > li.active > a > .fa,
        .sidebar-menu > li.active > a > .glyphicon,
        .sidebar-menu > li.active > a > i {
          color: #2c3e50 !important;
        }

        /* Separador del sidebar */
        .skin-black .sidebar-menu > li.header,
        .sidebar-menu > li.header {
          color: #7f8c8d !important;
          background: transparent !important;
          padding: 10px 15px !important;
          font-size: 11px !important;
          text-transform: uppercase !important;
          letter-spacing: 1px !important;
        }

        /* === HEADER - Franja superior oscura uniforme === */
        .main-header .navbar,
        .skin-black .main-header .navbar {
          background: #2c3e50 !important;
          border-bottom: 1px solid #1a252f !important;
          box-shadow: none !important;
        }

        .main-header .navbar .nav > li > a,
        .skin-black .main-header .navbar .nav > li > a {
          color: #ffffff !important;
        }

        .main-header .navbar .nav > li > a:hover,
        .skin-black .main-header .navbar .nav > li > a:hover {
          background: #1a252f !important;
        }

        .main-header .logo,
        .skin-black .main-header .logo {
          background: #2c3e50 !important;
          color: #ffffff !important;
          border-right: 1px solid #1a252f !important;
          border-bottom: 1px solid #1a252f !important;
          font-family: 'Source Serif Pro', Georgia, serif;
          font-weight: 700;
        }

        .main-header .logo:hover,
        .skin-black .main-header .logo:hover {
          background: #1a252f !important;
        }

        .main-header .sidebar-toggle,
        .skin-black .main-header .sidebar-toggle {
          color: #ffffff !important;
          background: transparent !important;
        }

        .main-header .sidebar-toggle:hover,
        .skin-black .main-header .sidebar-toggle:hover {
          background: #1a252f !important;
        }

        /* Cajas - Diseño limpio y profesional */
        .box {
          border-radius: 6px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.06);
          border: 1px solid #e0e4e8;
          overflow: hidden;
          transition: box-shadow 0.2s ease;
          background: var(--bg-white);
        }

        .box:hover {
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        }

        .box-header {
          background: #ffffff;
          color: #2c3e50;
          border-radius: 6px 6px 0 0;
          padding: 14px 18px;
          border-bottom: 1px solid #e0e4e8;
        }

        .box-header.with-border {
          border-bottom: 1px solid #e0e4e8;
        }

        .box-title {
          font-family: 'Source Serif Pro', Georgia, serif;
          font-weight: 600;
          font-size: 15px;
          letter-spacing: 0.2px;
          color: #2c3e50;
        }

        .box-body {
          padding: 20px;
          color: var(--text-dark);
          background: #ffffff;
        }


        /* Estilos para resaltado - Más sutiles */
        .highlight-multiple {
          position: relative;
          padding: 2px 6px;
          border-radius: 3px;
          transition: all 0.2s ease;
          cursor: pointer;
          display: inline;
          border-bottom: 2px solid currentColor;
        }

        .highlight-multiple:hover {
          opacity: 0.85;
          z-index: 10;
        }

        /* Indicador visual para modo deselección */
        #document-viewer.deselect-mode .highlight-multiple:hover {
          box-shadow: 0 0 0 2px #c0392b;
        }

        /* Tooltip */
        .highlight-multiple[title]:hover::after {
          content: attr(title);
          position: absolute;
          top: -35px;
          left: 50%;
          transform: translateX(-50%);
          background: #2c3e50;
          color: white;
          padding: 6px 12px;
          border-radius: 3px;
          font-size: 11px;
          white-space: nowrap;
          z-index: 1000;
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        }

        .highlight-multiple[title]:hover::before {
          content: '';
          position: absolute;
          top: -8px;
          left: 50%;
          transform: translateX(-50%);
          border-left: 6px solid transparent;
          border-right: 6px solid transparent;
          border-top: 6px solid #2c3e50;
          z-index: 1001;
        }

        /* Botones de modo */
        .mode-button {
          margin: 6px 4px;
          border-radius: 4px;
          padding: 8px 16px;
          font-weight: 600;
          transition: all 0.2s ease;
          border: 1px solid #e0e4e8;
        }

        .mode-button:hover {
          background: #f4f6f9;
          border-color: #d0d4d8;
        }

        .mode-button.active {
          background: #2c3e50 !important;
          color: white !important;
          border-color: #2c3e50 !important;
        }

        .deselect-active {
          background: #5a6c7d !important;
          border-color: #5a6c7d !important;
          color: white !important;
        }

        /* Visor de texto */
        #document-viewer.content {
          background: var(--bg-white);
          padding: 24px 28px;
          border-radius: 4px;
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.05);
          border: 1px solid var(--border-color);
          position: relative;
          color: var(--text-dark);
          line-height: 1.7;
        }

        #document-viewer.content::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 3px;
          background: #5a6c7d;
          border-radius: 6px 6px 0 0;
        }

        /* Eliminar márgenes del contenido del visor */
        #contenido {
          margin: 0 !important;
          padding: 0 !important;
        }

        #document-viewer .content > div {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }

        #document-viewer .sk-folding-cube {
          margin-top: 10px !important;
        }

        #contenido > * {
          margin-top: 0 !important;
        }

        #contenido > *:first-child {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }

        /* Botones - Diseño limpio */
        .btn {
          border-radius: 4px;
          font-weight: 600;
          transition: all 0.2s ease;
          border: 1px solid transparent;
          box-shadow: none;
          text-transform: none;
          letter-spacing: 0.3px;
        }

        .btn:hover {
          transform: none;
        }

        .btn-primary {
          background: #2c3e50;
          border-color: #2c3e50;
          color: white;
        }

        .btn-primary:hover, .btn-primary:focus {
          background: #1a252f;
          border-color: #1a252f;
        }

        .btn-success {
          background: #27ae60;
          border-color: #27ae60;
          color: white;
        }

        .btn-success:hover {
          background: #219a52;
          border-color: #219a52;
        }

        .btn-danger {
          background: #c0392b;
          border-color: #c0392b;
          color: white;
        }

        .btn-danger:hover {
          background: #a93226;
          border-color: #a93226;
        }

        .btn-info {
          background: #5a6c7d;
          border-color: #5a6c7d;
          color: white;
        }

        .btn-info:hover {
          background: #4a5a69;
          border-color: #4a5a69;
        }

        .btn-warning {
          background: #d35400;
          border-color: #d35400;
          color: white;
        }

        .btn-warning:hover {
          background: #b94700;
          border-color: #b94700;
        }

        .btn-default {
          background: #ffffff;
          border-color: #e0e4e8;
          color: #2c3e50;
        }

        .btn-default:hover {
          background: #f4f6f9;
          border-color: #d0d4d8;
        }

        /* Inputs */
        .form-control {
          border-radius: 4px;
          border: 1px solid var(--border-color);
          transition: border-color 0.2s ease;
          padding: 10px 12px;
          background: var(--bg-white);
        }

        .form-control:focus {
          border-color: #2980b9;
          box-shadow: 0 0 0 2px rgba(41,128,185,0.15);
        }

        .form-control::placeholder {
          color: #adb5bd;
        }

        /* Labels */
        label, .control-label {
          color: var(--text-dark);
          font-weight: 600;
          font-size: 13px;
          margin-bottom: 6px;
        }

        /* Selectores y dropdowns */
        .selectize-input, .selectize-dropdown {
          background: var(--bg-white) !important;
          border-color: var(--border-color) !important;
          color: var(--text-dark) !important;
          border-radius: 4px !important;
        }

        .selectize-input.focus {
          border-color: #2980b9 !important;
          box-shadow: 0 0 0 2px rgba(41,128,185,0.15) !important;
        }

        /* File input */
        .btn-file {
          background: #2c3e50;
          border: none;
          color: white;
          border-radius: 4px;
        }

        /* Progress bar */
        .progress-bar {
          background: #2980b9;
        }

        /* Tablas - Diseño académico claro */
        .dataTables_wrapper {
          border-radius: 6px;
          overflow: hidden;
        }

        table.dataTable thead th {
          background: #f4f6f9;
          color: #2c3e50;
          border: none;
          border-bottom: 2px solid #e0e4e8;
          font-weight: 600;
          font-size: 13px;
          padding: 12px 15px;
        }

        table.dataTable tbody tr:hover {
          background: #f8f9fa;
        }

        table.dataTable tbody td {
          color: var(--text-dark);
          border-color: #e0e4e8;
          padding: 10px 15px;
          font-size: 13px;
        }

        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          background: var(--bg-white);
          color: var(--text-dark);
          border: 1px solid #e0e4e8;
          border-radius: 4px;
        }

        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          color: var(--text-light);
          font-size: 13px;
        }

        /* Info panels - Diseño sobrio */
        .info-panel {
          background: #f8f9fa;
          border: 1px solid #e0e4e8;
          border-left: 3px solid #5a6c7d;
          border-radius: 0 4px 4px 0;
          padding: 15px 18px;
          margin: 12px 0;
        }

        .danger-panel {
          background: #fdf8f8;
          border-color: #e8d4d4;
          border-left-color: #c0392b;
        }

        /* Controles de descarga */
        .download-controls-container {
          background: #f8f9fa;
          border: 1px solid #e0e4e8;
          border-left: 3px solid #5a6c7d;
          border-radius: 0 4px 4px 0;
          padding: 15px 18px;
          margin: 10px 0;
        }

        .download-controls-container::before {
          display: none;
        }

        .download-controls-grid {
          display: grid;
          grid-template-columns: 1fr 1fr 1fr auto;
          gap: 15px;
          align-items: end;
        }

        /* Spinner */
        .sk-folding-cube {
          margin: 20px auto;
          width: 30px;
          height: 30px;
        }

        /* Box status - Borde superior neutro */
        .box.box-primary {
          border-top-color: #5a6c7d !important;
        }

        .box.box-primary > .box-header {
          background: #ffffff !important;
          color: #2c3e50 !important;
        }

        .box.box-solid.box-primary {
          border: 1px solid #e0e4e8 !important;
        }

        .box.box-solid.box-primary > .box-header {
          background: #f4f6f9 !important;
          color: #2c3e50 !important;
        }

        .box.box-solid.box-primary > .box-header > .box-title {
          color: #2c3e50 !important;
        }

        .box.box-solid.box-primary > .box-header .btn {
          color: #2c3e50 !important;
        }

        /* Tarjetas de análisis semántico */
        .semantico-card {
          background: var(--bg-white) !important;
          border: 1px solid #e0e4e8 !important;
          border-radius: 6px !important;
        }

        .semantico-card h5 {
          color: var(--text-dark) !important;
          font-family: 'Source Serif Pro', Georgia, serif;
        }

        /* Tabs */
        .nav-pills > li > a {
          border-radius: 4px;
          color: var(--text-dark);
          font-weight: 600;
          border: 1px solid transparent;
        }

        .nav-pills > li > a:hover {
          background: #f4f6f9;
        }

        .nav-pills > li.active > a,
        .nav-pills > li.active > a:hover,
        .nav-pills > li.active > a:focus {
          background: #2c3e50;
          color: white;
          border-color: #2c3e50;
        }

        /* Encabezados */
        h4, h5, h6 {
          font-family: 'Source Serif Pro', Georgia, serif;
          color: var(--text-dark);
        }

        /* Badges */
        .badge {
          font-weight: 600;
          font-size: 10px;
          padding: 3px 8px;
          border-radius: 3px;
        }
      "))
    ),
    tags$script(HTML("
      // Variables globales para el modo de deselección
      var isDeselectMode = false;
      
      // Script mejorado para selección de texto - SOLO en el visor de documento
      document.addEventListener('mouseup', function(e){
        if (!isDeselectMode) {
          // Verificar que la selección está dentro del visor de documento
          var documentViewer = document.getElementById('document-viewer');
          var selection = window.getSelection();
          
          if (selection.rangeCount > 0 && documentViewer) {
            var range = selection.getRangeAt(0);
            var selectedText = selection.toString().trim();
            
            // Verificar que el rango de selección está contenido dentro del visor de documento
            if (selectedText.length > 0 && documentViewer.contains(range.commonAncestorContainer)) {
              Shiny.setInputValue('selectedText', selectedText);
            }
          }
        }
      });
      
      // Función para manejar clics en fragmentos resaltados
      document.addEventListener('click', function(e) {
        if (e.target.classList.contains('highlight-multiple')) {
          var fragmentId = e.target.getAttribute('data-fragment-id');
          
          if (isDeselectMode) {
            // Modo deselección: enviar ID del fragmento para eliminar
            Shiny.setInputValue('deselectFragment', {
              id: fragmentId,
              text: e.target.textContent,
              timestamp: Date.now()
            });
          } else {
            // Modo normal: mostrar información
            Shiny.setInputValue('clickedFragment', fragmentId);
          }
          
          e.preventDefault();
          e.stopPropagation();
        }
      });
      
      // Función para activar/desactivar modo deselección
      function toggleDeselectMode(active) {
        isDeselectMode = active;
        var textViewer = document.getElementById('document-viewer');
        
        if (active) {
          if (textViewer) textViewer.classList.add('deselect-mode');
        } else {
          if (textViewer) textViewer.classList.remove('deselect-mode');
        }
      }
      
      // Escuchar cambios en el modo desde Shiny
      Shiny.addCustomMessageHandler('setDeselectMode', function(active) {
        toggleDeselectMode(active);
      });
    ")),
    tabItems(
      # ---- Texto (diseño mejorado) ----
      tabItem("texto",
              fluidRow(
                box(
                  width = 4, 
                  title = "Panel de Control", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  fileInput("archivo", 
                            div(icon("upload"), " Cargar Documentos"), 
                            multiple = TRUE, 
                            accept = c(".txt", ".docx"),
                            buttonLabel = "Examinar...",
                            placeholder = "Ningún archivo seleccionado"),
                  
                  # Panel de modos con diseño moderno
                  div(
                    class = "info-panel",
                    h5(icon("cog"), " Modo de Trabajo", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    div(
                      style = "display: flex; flex-wrap: wrap; gap: 8px; justify-content: center;",
                      actionButton("modeSelect",
                                   div(icon("mouse-pointer"), " Seleccionar"),
                                   class = "btn-primary btn-sm mode-button active"),
                      actionButton("modeDeselect",
                                   div(icon("eraser"), " Deseleccionar"),
                                   class = "btn-default btn-sm mode-button")
                    )
                  ),
                  
                  # Controles de codificación mejorados
                  conditionalPanel(
                    condition = "input.modeSelect",
                    div(
                      style = "margin: 20px 0;",
                      selectInput("codigoTexto", 
                                  div(icon("tag"), " Código a Aplicar"), 
                                  choices = NULL),
                      div(
                        class = "info-panel",
                        checkboxInput("modoAcumulativo", 
                                      div(icon("layer-group"), " Modo Acumulativo"), 
                                      value = TRUE),
                        helpText("Permite aplicar múltiples códigos al mismo fragmento", 
                                 style = "color: #7f8c8d; font-size: 12px;")
                      )
                    )
                  ),
                  
                  # Controles de navegación mejorados
                  div(
                    style = "margin: 20px 0;",
                    h5(icon("arrows-alt-h"), " Navegación", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    fluidRow(
                      column(6, actionButton("prev_doc",
                                             div(icon("chevron-left"), " Anterior"),
                                             class = "btn-default btn-sm btn-block")),
                      column(6, actionButton("next_doc",
                                             div(icon("chevron-right"), " Siguiente"),
                                             class = "btn-default btn-sm btn-block"))
                    )
                  ),
                  
                  # Botones de acción mejorados
                  div(
                    style = "margin: 20px 0;",
                    h5(icon("tools"), " Acciones", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    div(
                      style = "display: flex; flex-wrap: wrap; gap: 8px;",
                      actionButton("limpiarResaltados",
                                   div(icon("broom"), " Limpiar"),
                                   class = "btn-default btn-sm"),
                      actionButton("ayuda",
                                   div(icon("question-circle"), " Ayuda"),
                                   class = "btn-info btn-sm")
                    )
                  ),
                  
                  # Info del documento
                  div(
                    class = "info-panel",
                    textOutput("doc_info")
                  )
                ),
                box(
                  width = 8, 
                  title = "Visor de Documento", 
                  status = "primary", 
                  solidHeader = TRUE,
                  div(
                    id = "document-viewer",
                    class = "content",
                    style = "max-height: 600px; overflow-y: auto; line-height: 1.8;",
                    withSpinner(
                      div(
                        style = "white-space: pre-wrap; font-family: 'Inter', sans-serif; font-size: 15px; color: #2c3e50; margin: 0; padding: 0;",
                        uiOutput("contenido")
                      ),
                      type = 6,
                      color = "#3498db"
                    )
                  ),
                  div(
                    id = "currentModeInfo",
                    class = "info-panel",
                    style = "margin-top: 15px;"
                  )
                )
              )
      ),
      
      # ---- Códigos (diseño mejorado) ----
      tabItem("codigos",
              fluidRow(
                box(
                  width = 4, 
                  title = "Gestión de Códigos", 
                  status = "primary", 
                  solidHeader = TRUE,
                  div(
                    style = "space-y: 20px;",
                    textInput("new_codigo", 
                              div(icon("tag"), " Nombre del Código"), 
                              value = "",
                              placeholder = "Ej: Emociones positivas"),
                    
                    div(
                      style = "margin: 20px 0;",
                      h5(icon("palette"), " Color del Código", style = "color: #2c3e50; margin-bottom: 10px;"),
                      colourInput("new_color", 
                                  label = NULL, 
                                  value = "#3498db",
                                  palette = "limited",
                                  allowedCols = c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", 
                                                  "#9b59b6", "#1abc9c", "#34495e", "#e67e22",
                                                  "#f1c40f", "#c0392b", "#8e44ad", "#16a085",
                                                  "#2c3e50", "#d35400", "#27ae60"))
                    ),
                    
                    div(
                      style = "display: flex; gap: 10px; margin-top: 25px;",
                      actionButton("addOrUpdateCodigo",
                                   div(icon("save"), " Guardar"),
                                   class = "btn-primary btn-sm"),
                      actionButton("deleteCodigo",
                                   div(icon("trash"), " Eliminar"),
                                   class = "btn-default btn-sm")
                    )
                  )
                ),
                box(
                  width = 8, 
                  title = "Lista de Códigos", 
                  status = "primary", 
                  solidHeader = TRUE,
                  DTOutput("tablaCodigos") %>% withSpinner(type = 4, color = "#5a6c7d")
                )
              )
      ),
      
      # ---- Categorías (diseño mejorado) ----
      tabItem("categorias",
              fluidRow(
                box(
                  width = 4, 
                  height = 600,
                  title = "Gestión de Categorías", 
                  status = "primary", 
                  solidHeader = TRUE,
                  div(
                    textInput("new_categoria", 
                              div(icon("folder"), " Nombre de Categoría"), 
                              value = "",
                              placeholder = "Ej: Aspectos emocionales"),
                    
                    div(
                      style = "margin: 20px 0;",
                      selectizeInput("codigos_for_categoria", 
                                     div(icon("tags"), " Códigos Asociados"), 
                                     choices = NULL, 
                                     multiple = TRUE,
                                     options = list(placeholder = "Selecciona códigos..."))
                    ),
                    
                    div(
                      style = "display: flex; gap: 10px; margin-top: 25px;",
                      actionButton("addOrUpdateCategoria",
                                   div(icon("save"), " Guardar"),
                                   class = "btn-primary btn-sm"),
                      actionButton("deleteCategoria",
                                   div(icon("trash"), " Eliminar"),
                                   class = "btn-default btn-sm")
                    )
                  )
                ),
                box(
                  width = 8, 
                  height = 600,
                  title = "Categorías Definidas", 
                  status = "primary", 
                  solidHeader = TRUE,
                  DTOutput("tablaCategorias") %>% withSpinner(type = 4, color = "#5a6c7d")
                )
              )
      ),
      
      # ---- Resaltes (diseño mejorado) ----
      tabItem("resaltes",
              fluidRow(
                box(
                  width = 12, 
                  title = "Gestión de Extractos Codificados", 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  # Panel de controles mejorado
                  div(
                    class = "info-panel",
                    h5(icon("cogs"), " Herramientas de Gestión", style = "color: #2c3e50; margin-bottom: 15px;"),
                    fluidRow(
                      column(4,
                             downloadButton("descarga",
                                            div(icon("download"), " Exportar XLSX"),
                                            class = "btn-primary btn-sm btn-block")),
                      column(4,
                             actionButton("eliminarResalte",
                                          div(icon("minus-circle"), " Eliminar Seleccionado"),
                                          class = "btn-default btn-sm btn-block")),
                      column(4,
                             actionButton("eliminarTodosResaltes",
                                          div(icon("trash-alt"), " Limpiar Todo"),
                                          class = "btn-default btn-sm btn-block"))
                    )
                  ),
                  
                  # Tabla de resaltados
                  DTOutput("tablaResaltes") %>% withSpinner(type = 4, color = "#5a6c7d"),
                  
                  # Panel informativo mejorado
                  div(
                    class = "info-panel",
                    style = "margin-top: 20px;",
                    h5(icon("info-circle"), " Guía de Resaltados", style = "color: #2c3e50; margin-bottom: 15px;"),
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
                      div(
                        h6(icon("paint-brush"), " Visualización", style = "color: #2c3e50; margin-bottom: 8px;"),
                        tags$ul(
                          style = "font-size: 13px; color: #7f8c8d;",
                          tags$li("Gradientes indican múltiples códigos"),
                          tags$li("Hover muestra códigos aplicados"),
                          tags$li("Cada fila = un código por fragmento")
                        )
                      ),
                      div(
                        h6(icon("edit"), " Edición", style = "color: #2c3e50; margin-bottom: 8px;"),
                        tags$ul(
                          style = "font-size: 13px; color: #7f8c8d;",
                          tags$li("Modo deseleccionar para eliminar"),
                          tags$li("Selección múltiple disponible"),
                          tags$li("Exportación completa a Excel")
                        )
                      )
                    )
                  )
                )
              )
      ),
      
      # ---- Análisis (diseño mejorado con controles de descarga personalizados) ----
      tabItem("analisis",
              fluidRow(
                box(
                  width = 3, 
                  title = "Configuración", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("chart-bar"), " Opciones Visuales", style = "color: #2c3e50; margin-bottom: 15px;"),
                    prettySwitch("fillToggle", 
                                 "Colorear por Categoría", 
                                 value = TRUE, 
                                 status = "primary",
                                 fill = TRUE)
                  ),
                  
                  # Controles de descarga personalizados
                  div(
                    class = "download-controls-container",
                    h5(icon("cogs"), " Configuración de Descarga", style = "color: #2c3e50; margin-bottom: 15px;"),
                    
                    fluidRow(
                      column(6,
                             numericInput("plot_width", 
                                          div(icon("arrows-alt-h"), " Ancho (pulg)"), 
                                          value = 12, 
                                          min = 5, 
                                          max = 20, 
                                          step = 0.5)
                      ),
                      column(6,
                             numericInput("plot_height", 
                                          div(icon("arrows-alt-v"), " Alto (pulg)"), 
                                          value = 8, 
                                          min = 4, 
                                          max = 16, 
                                          step = 0.5)
                      )
                    ),
                    
                    numericInput("plot_dpi", 
                                 div(icon("expand"), " Resolución (DPI)"), 
                                 value = 600, 
                                 min = 150, 
                                 max = 1200, 
                                 step = 50),
                    
                    helpText("Configuración aplicada a ambos gráficos", 
                             style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;")
                  )
                ),
                box(
                  width = 9,
                  height = 650,
                  title = "Distribución de Códigos",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  plotlyOutput("plotCodigos", height = "580px") %>%
                    withSpinner(type = 6, color = "#5a6c7d")
                ),
                box(
                  width = 3,
                  title = "Exportar",
                  status = "primary",
                  solidHeader = TRUE,
                  downloadButton("download_distribucion_jpg",
                                 div(icon("download"), " Distribución (JPG)"),
                                 class = "btn-primary btn-sm btn-block")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Exportar Red",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  downloadButton("download_red_jpg",
                                 div(icon("download"), " Red de Coocurrencia (JPG)"),
                                 class = "btn-primary btn-sm")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  height = 750,
                  title = "Red de Coocurrencia y Análisis de Centralidad",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  plotOutput("plotRedCentralidad", height = "680px") %>%
                    withSpinner(type = 6, color = "#5a6c7d")
                )
              )
      ),
      
      # ---- Análisis IA (nuevo) ----
      tabItem("analisis_ia",
              fluidRow(
                box(
                  width = 4,
                  title = "Configuración del Análisis IA",
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("key"), " Configuración de OpenAI", style = "color: #2c3e50; margin-bottom: 15px;"),
                    passwordInput("openai_api_key",
                                  div(icon("lock"), " API Key de OpenAI"),
                                  placeholder = "sk-..."),
                    helpText("Usa el modelo GPT-4.1 de OpenAI. Obtén tu API Key en platform.openai.com",
                             style = "color: #7f8c8d; font-size: 12px;"),
                    div(
                      style = "margin-top: 10px; padding: 10px; background: #e8f4f8; border-radius: 5px;",
                      tags$small(
                        icon("info-circle"),
                        " El análisis requiere una API Key válida de OpenAI.",
                        style = "color: #2980b9;"
                      )
                    )
                  ),

                  div(
                    class = "info-panel",
                    style = "margin-top: 20px;",
                    h5(icon("book"), " Diccionario de Códigos", style = "color: #2c3e50; margin-bottom: 15px;"),
                    fileInput("dict_ia",
                              div(icon("upload"), " Cargar Diccionario"),
                              accept = c(".csv", ".xlsx"),
                              buttonLabel = "Examinar...",
                              placeholder = "Archivo .csv o .xlsx"),
                    helpText("Debe tener columnas: Categoría, Código, Definición",
                             style = "color: #7f8c8d; font-size: 12px;")
                  ),
                  
                  div(
                    style = "margin-top: 25px;",
                    actionButton("run_ia_analysis",
                                 div(icon("play"), " Ejecutar Análisis IA"),
                                 class = "btn-primary btn-block", style = "margin-bottom: 10px;"),
                    downloadButton("download_ia_results",
                                   div(icon("download"), " Descargar Resultados (.xlsx)"),
                                   class = "btn-default btn-block"),
                    helpText("Los resultados se mostrarán abajo. Descarga la tabla en Excel con el botón de arriba.",
                             style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;")
                  )
                ),
                box(
                  width = 8,
                  title = "Resultados del Análisis IA",
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), " Instrucciones", style = "color: #2c3e50;"),
                    p("1. Asegúrate de tener documentos cargados en la pestaña 'Documento'",
                      style = "color: #7f8c8d;"),
                    p("2. Ingresa tu API Key de OpenAI",
                      style = "color: #7f8c8d;"),
                    p("3. Carga un diccionario de códigos con las columnas: Categoría, Código, Definición",
                      style = "color: #7f8c8d;"),
                    p("4. Ejecuta el análisis y revisa los resultados",
                      style = "color: #7f8c8d;"),
                    p("5. Si estás satisfecho, integra los resultados al análisis manual",
                      style = "color: #7f8c8d;")
                  ),
                  DTOutput("tabla_ia_results") %>% withSpinner(type = 6, color = "#5a6c7d"),
                  div(
                    style = "margin-top: 15px;",
                    downloadButton("download_tabla_ia_excel",
                                   div(icon("file-excel"), " Descargar Tabla (Excel)"),
                                   class = "btn-success btn-sm")
                  )
                )
              ),

              # Análisis visual de resultados IA
              fluidRow(
                box(
                  width = 12,
                  title = "Visualización de Resultados IA",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  fluidRow(
                    column(6,
                           h5(icon("chart-bar"), " Distribución de Códigos", style = "color: #2c3e50; margin-bottom: 15px;"),
                           plotlyOutput("plot_ia_distribucion", height = "400px") %>%
                             withSpinner(type = 6, color = "#5a6c7d"),
                           div(style = "margin-top: 10px;",
                               downloadButton("download_ia_distribucion_png",
                                              div(icon("image"), " PNG"),
                                              class = "btn-primary btn-sm"))
                    ),
                    column(6,
                           h5(icon("chart-pie"), " Fragmentos por Categoría", style = "color: #2c3e50; margin-bottom: 15px;"),
                           plotlyOutput("plot_ia_categorias", height = "400px") %>%
                             withSpinner(type = 6, color = "#5a6c7d"),
                           div(style = "margin-top: 10px;",
                               downloadButton("download_ia_categorias_png",
                                              div(icon("image"), " PNG"),
                                              class = "btn-primary btn-sm"))
                    )
                  )
                )
              )
      ),

      # ---- Análisis Semántico (OpenAI) ----
      tabItem("analisis_semantico",
              fluidRow(
                # Panel de Configuración
                box(
                  width = 4,
                  title = "Configuración del Análisis Semántico",
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    class = "info-panel",
                    h5(icon("key"), " Configuración", style = "color: #2c3e50; margin-bottom: 15px;"),
                    p("Este módulo utiliza la API de OpenAI para generar embeddings y análisis semántico.",
                      style = "color: #7f8c8d; font-size: 12px;"),
                    tags$small(
                      icon("info-circle"),
                      " Modelo de embeddings: text-embedding-3-small",
                      style = "color: #2980b9; display: block; margin-top: 10px;"
                    ),
                    tags$small(
                      icon("info-circle"),
                      " Ingresa tu API Key de OpenAI en la pestaña 'Análisis IA'",
                      style = "color: #2980b9; display: block; margin-top: 5px;"
                    )
                  ),

                  div(
                    style = "margin-top: 20px;",
                    actionButton("btn_generar_embeddings",
                                 div(icon("brain"), " Generar Embeddings"),
                                 class = "btn-primary btn-block"),
                    helpText("Genera representaciones vectoriales de los fragmentos codificados usando OpenAI",
                             style = "color: #7f8c8d; font-size: 11px; margin-top: 8px;")
                  ),

                  # Estado de embeddings
                  div(
                    class = "info-panel",
                    style = "margin-top: 15px;",
                    uiOutput("estado_embeddings")
                  )
                ),

                # Panel de Funcionalidades
                box(
                  width = 8,
                  title = "Herramientas de Análisis Semántico",
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), " Requisitos", style = "color: #2c3e50; margin-bottom: 10px;"),
                    p("1. Ten fragmentos codificados (usa 'Análisis IA' o codifica manualmente)",
                      style = "color: #7f8c8d; margin: 3px 0;"),
                    p("2. Genera los embeddings (usa tokens internos automáticamente)",
                      style = "color: #7f8c8d; margin: 3px 0;"),
                    p("3. Explora las herramientas de análisis semántico",
                      style = "color: #7f8c8d; margin: 3px 0;")
                  ),

                  # Grid de botones de funcionalidades
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 20px;",

                    # Clustering
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("object-group"), " Clustering Semántico", style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p("Agrupa fragmentos similares automáticamente", style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      numericInput("n_clusters_semantico", "Número de clusters", value = 3, min = 2, max = 20, step = 1),
                      actionButton("btn_clustering", div(icon("sitemap"), " Ejecutar Clustering"),
                                   class = "btn-primary btn-sm btn-block")
                    ),

                    # Similitud
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("exchange-alt"), " Detección de Similitud", style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p("Encuentra fragmentos similares con diferente código", style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      sliderInput("umbral_similitud", "Umbral de similitud", min = 0.5, max = 0.95, value = 0.8, step = 0.05),
                      actionButton("btn_similitud", div(icon("search"), " Detectar Similares"),
                                   class = "btn-primary btn-sm btn-block")
                    ),

                    # Visualización
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("project-diagram"), " Visualización 2D", style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p("Visualiza la distribución semántica de fragmentos", style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      selectInput("metodo_visualizacion", "Método de reducción",
                                  choices = c("PCA" = "pca", "t-SNE" = "tsne", "UMAP" = "umap"),
                                  selected = "pca"),
                      actionButton("btn_visualizar", div(icon("chart-area"), " Visualizar"),
                                   class = "btn-primary btn-sm btn-block")
                    ),

                    # Coherencia
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("check-double"), " Análisis de Coherencia", style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p("Evalúa la homogeneidad semántica de cada código", style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      br(),
                      actionButton("btn_coherencia", div(icon("tasks"), " Analizar Coherencia"),
                                   class = "btn-primary btn-sm btn-block")
                    )
                  ),

                  # Validación LLM (fila completa)
                  div(
                    style = "margin-top: 15px;",
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("user-check"), " Validación con LLM (Panel de Expertos Virtual)", style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p("Un modelo de lenguaje evalúa la calidad de tu codificación", style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      fluidRow(
                        column(8,
                               numericInput("n_fragmentos_validar", "Fragmentos a validar (muestra)", value = 10, min = 1, max = 50, step = 1)
                        ),
                        column(4,
                               actionButton("btn_validacion", div(icon("gavel"), " Validar"),
                                            class = "btn-primary btn-sm btn-block")
                        )
                      )
                    )
                  )
                )
              ),

              # Configuración de descarga de figuras
              fluidRow(
                box(
                  width = 12,
                  title = div(icon("download"), " Configuración de Descarga de Figuras"),
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,

                  fluidRow(
                    column(3,
                      numericInput("sem_plot_width",
                                   div(icon("arrows-alt-h"), " Ancho (pulg)"),
                                   value = 10, min = 5, max = 20, step = 0.5)
                    ),
                    column(3,
                      numericInput("sem_plot_height",
                                   div(icon("arrows-alt-v"), " Alto (pulg)"),
                                   value = 8, min = 4, max = 16, step = 0.5)
                    ),
                    column(3,
                      numericInput("sem_plot_dpi",
                                   div(icon("expand"), " Resolución (DPI)"),
                                   value = 300, min = 150, max = 600, step = 50)
                    ),
                    column(3,
                      div(style = "margin-top: 25px;",
                        helpText(icon("info-circle"), " Ajusta las dimensiones antes de descargar",
                                 style = "color: #7f8c8d; font-size: 11px;")
                      )
                    )
                  )
                )
              ),

              # Área de resultados
              fluidRow(
                box(
                  width = 12,
                  title = "Resultados del Análisis Semántico",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  tabsetPanel(
                    id = "tabs_resultados_semantico",
                    type = "pills",

                    tabPanel(
                      title = div(icon("object-group"), " Clustering"),
                      value = "tab_clustering",
                      div(
                        style = "padding: 15px;",
                        DTOutput("tabla_clustering_semantico") %>% withSpinner(type = 6, color = "#5a6c7d"),
                        plotlyOutput("plot_clustering_semantico", height = "400px") %>% withSpinner(type = 6, color = "#5a6c7d"),
                        div(
                          style = "margin-top: 15px; display: flex; gap: 10px;",
                          downloadButton("download_clustering_excel", div(icon("file-excel"), " Tabla Excel"), class = "btn-success btn-sm"),
                          downloadButton("download_clustering_png", div(icon("image"), " Figura PNG"), class = "btn-primary btn-sm")
                        )
                      )
                    ),

                    tabPanel(
                      title = div(icon("exchange-alt"), " Similitud"),
                      value = "tab_similitud",
                      div(
                        style = "padding: 15px;",
                        DTOutput("tabla_similitud_semantico") %>% withSpinner(type = 6, color = "#5a6c7d"),
                        div(
                          style = "margin-top: 15px;",
                          downloadButton("download_similitud_excel", div(icon("file-excel"), " Tabla Excel"), class = "btn-success btn-sm")
                        )
                      )
                    ),

                    tabPanel(
                      title = div(icon("project-diagram"), " Visualización"),
                      value = "tab_visualizacion",
                      div(
                        style = "padding: 15px;",
                        plotlyOutput("plot_embeddings_2d", height = "500px") %>% withSpinner(type = 6, color = "#5a6c7d"),
                        div(
                          style = "margin-top: 15px;",
                          downloadButton("download_visualizacion_png", div(icon("image"), " Figura PNG"), class = "btn-primary btn-sm")
                        )
                      )
                    ),

                    tabPanel(
                      title = div(icon("check-double"), " Coherencia"),
                      value = "tab_coherencia",
                      div(
                        style = "padding: 15px;",
                        DTOutput("tabla_coherencia_semantico") %>% withSpinner(type = 6, color = "#5a6c7d"),
                        plotOutput("plot_coherencia_semantico", height = "350px") %>% withSpinner(type = 6, color = "#5a6c7d"),
                        div(
                          style = "margin-top: 15px; display: flex; gap: 10px;",
                          downloadButton("download_coherencia_excel", div(icon("file-excel"), " Tabla Excel"), class = "btn-success btn-sm"),
                          downloadButton("download_coherencia_png", div(icon("image"), " Figura PNG"), class = "btn-primary btn-sm")
                        )
                      )
                    ),

                    tabPanel(
                      title = div(icon("project-diagram"), " Red Semántica"),
                      value = "tab_red_semantica",
                      div(
                        style = "padding: 15px;",
                        fluidRow(
                          column(4,
                            div(
                              class = "info-panel",
                              style = "padding: 15px; margin-bottom: 15px;",
                              h5(icon("sliders-h"), " Configuración", style = "color: #2c3e50; margin-bottom: 15px;"),
                              sliderInput("umbral_red_semantica",
                                          "Umbral de conexión (similitud)",
                                          min = 0.2, max = 0.9, value = 0.4, step = 0.05),
                              helpText("Códigos con similitud mayor al umbral se conectan",
                                       style = "color: #7f8c8d; font-size: 11px;"),
                              selectInput("color_red_semantica",
                                          "Colorear por",
                                          choices = c("Categoría" = "categoria", "Comunidad (detectada)" = "comunidad"),
                                          selected = "categoria"),
                              actionButton("btn_generar_red",
                                           div(icon("project-diagram"), " Generar Red"),
                                           class = "btn-primary btn-block",
                                           style = "margin-top: 15px;"),
                              div(style = "margin-top: 15px;",
                                  downloadButton("download_red_semantica_png",
                                                 div(icon("image"), " Descargar PNG"),
                                                 class = "btn-success btn-sm btn-block"))
                            )
                          ),
                          column(8,
                            div(
                              class = "info-panel",
                              style = "padding: 10px;",
                              uiOutput("info_red_semantica"),
                              plotOutput("plot_red_semantica", height = "500px") %>% withSpinner(type = 6, color = "#5a6c7d")
                            )
                          )
                        )
                      )
                    ),

                    tabPanel(
                      title = div(icon("user-check"), " Validación LLM"),
                      value = "tab_validacion",
                      div(
                        style = "padding: 15px;",
                        div(
                          class = "info-panel",
                          uiOutput("resultado_validacion_llm")
                        )
                      )
                    )
                  )
                )
              )
      ),

      # ---- Reporte con IA ----
      tabItem("reporte_ia",
              fluidRow(
                box(
                  width = 4,
                  title = "Configuración del Reporte",
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), " Información", style = "color: #2c3e50; margin-bottom: 15px;"),
                    p("Genera un reporte interpretativo automático basado en los análisis realizados.",
                      style = "color: #7f8c8d; margin-bottom: 10px;"),
                    p("Usa el modelo GPT-4.1 de OpenAI para generar reportes interpretativos.",
                      style = "color: #7f8c8d; font-size: 12px;")
                  ),

                  hr(),

                  selectInput("idioma_reporte",
                              div(icon("language"), " Idioma del reporte"),
                              choices = c("Español" = "es", "English" = "en"),
                              selected = "es"),

                  selectInput("estilo_reporte",
                              div(icon("file-alt"), " Estilo de redacción"),
                              choices = c(
                                "Académico (tesis/artículo)" = "academico",
                                "Técnico (informe)" = "tecnico",
                                "Divulgativo (general)" = "divulgativo"
                              ),
                              selected = "academico"),

                  checkboxGroupInput("secciones_reporte",
                                     div(icon("list-check"), " Secciones a incluir"),
                                     choices = c(
                                       "Resumen de codificación" = "codificacion",
                                       "Análisis de frecuencias" = "frecuencias",
                                       "Clustering semántico" = "clustering",
                                       "Coherencia de códigos" = "coherencia",
                                       "Red semántica" = "red",
                                       "Hallazgos principales" = "hallazgos",
                                       "Limitaciones" = "limitaciones"
                                     ),
                                     selected = c("codificacion", "frecuencias", "hallazgos")),

                  hr(),

                  div(
                    class = "info-panel",
                    style = "background: #fff3cd; border-color: #ffc107;",
                    h5(icon("exclamation-triangle"), " Requisitos", style = "color: #856404; margin-bottom: 10px;"),
                    tags$ul(
                      style = "color: #856404; font-size: 12px; padding-left: 20px;",
                      tags$li("Tener fragmentos codificados (manual o IA)"),
                      tags$li("Para análisis semántico: haber generado embeddings")
                    )
                  ),

                  hr(),

                  actionButton("btn_generar_reporte",
                               div(icon("magic"), " Generar Reporte"),
                               class = "btn-primary btn-block btn-lg"),

                  div(
                    style = "margin-top: 15px;",
                    downloadButton("btn_descargar_reporte",
                                   div(icon("file-word"), " Descargar (.docx)"),
                                   class = "btn-success btn-block")
                  )
                ),

                box(
                  width = 8,
                  title = "Reporte Generado",
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    style = "min-height: 500px;",
                    uiOutput("reporte_ia_output") %>% withSpinner(type = 6, color = "#5a6c7d")
                  )
                )
              )
      ),

      # ---- Estado (diseño mejorado) ----
      tabItem("estado",
              fluidRow(
                box(
                  width = 6, 
                  title = "Guardar Proyecto", 
                  status = "primary", 
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("save"), " Respaldo de Datos", style = "color: #2c3e50; margin-bottom: 15px;"),
                    p("Guarda todo tu trabajo incluyendo códigos, categorías y resaltados.",
                      style = "color: #7f8c8d; margin-bottom: 20px;"),
                    downloadButton("saveState",
                                   div(icon("download"), " Descargar Estado (.rds)"),
                                   class = "btn-primary")
                  )
                ),
                box(
                  width = 6, 
                  title = "Cargar Proyecto", 
                  status = "primary", 
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("upload"), " Restaurar Datos", style = "color: #2c3e50; margin-bottom: 15px;"),
                    p("Carga un proyecto previamente guardado para continuar trabajando.",
                      style = "color: #7f8c8d; margin-bottom: 20px;"),
                    fileInput("loadState", 
                              div(icon("folder-open"), " Seleccionar Archivo"), 
                              accept = ".rds",
                              buttonLabel = "Buscar...",
                              placeholder = "Archivo .rds no seleccionado")
                  )
                )
              )
      ),
      
      # ---- Cómo citar (diseño mejorado) ----
      tabItem("citar",
              fluidRow(
                box(
                  width = 12, 
                  title = "Cómo Citar RCualiText", 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  # Header visual mejorado
                  div(
                    style = "text-align: center; padding: 30px; background: #2c3e50; margin: -25px -25px 25px -25px; color: white;",
                    div(
                      style = "font-size: 64px; margin-bottom: 15px;",
                      icon("quote-right")
                    ),
                    h2("Reconocimiento Académico", style = "margin: 0; font-weight: 600;")
                  ),
                  
                  # Cita principal
                  div(
                    class = "info-panel",
                    h3(icon("graduation-cap"), " Cita en formato APA 7ª edición", 
                       style = "color: #2c3e50; margin-bottom: 20px; font-weight: 600;"),
                    
                    div(
                      style = "background: #f4f6f9; padding: 25px; border-left: 5px solid #2c3e50; margin: 20px 0; font-family: 'Georgia', serif; font-size: 16px; line-height: 2; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.06);",
                      HTML("Ventura-León, J. (2026). <em>RCualiText</em> (v1.0) [Shiny app]. GitHub. https://github.com/jventural/RCualiText_App")
                    ),
                    
                    div(
                      style = "text-align: center; margin: 25px 0;",
                      actionButton("copycitation",
                                   div(icon("copy"), " Copiar Cita"),
                                   class = "btn-primary")
                    )
                  ),
                  
                  hr(),
                  
                  # Información adicional mejorada
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 25px; margin-top: 30px;",
                    
                    # Información del software
                    div(
                      class = "info-panel",
                      h4(icon("info-circle"), " Información del Software", style = "color: #2c3e50; margin-bottom: 15px;"),
                      div(
                        style = "space-y: 10px;",
                        div(strong("Autor: "), "Dr. José Ventura-León"),
                        div(strong("Año: "), "2026"),
                        div(strong("Versión: "), "2.0"),
                        div(strong("Tipo: "), "Aplicación Shiny para análisis cualitativo"),
                        div(strong("Repositorio: "), 
                            tags$a("GitHub", 
                                   href = "https://github.com/jventural/RCualiText_App", 
                                   target = "_blank", 
                                   style = "color: #2c3e50; text-decoration: none; font-weight: 500;"))
                      )
                    ),
                    
                    # Nota importante
                    div(
                      class = "danger-panel",
                      h4(icon("exclamation-triangle"), " Importante", style = "color: #c0392b; margin-bottom: 15px;"),
                      p("Si utilizas RCualiText en tu investigación o trabajo académico, te agradecemos que incluyas esta cita para reconocer el trabajo del autor y permitir que otros investigadores puedan acceder a esta herramienta.", 
                        style = "color: #c0392b; margin-bottom: 0; line-height: 1.6;")
                    )
                  )
                )
              )
      ),
      
      # ---- Info/Ayuda (diseño mejorado) ----
      tabItem("info",
              fluidRow(
                box(
                  width = 12, 
                  title = "Acerca de RCualiText", 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  # Header mejorado
                  div(
                    style = "text-align: center; padding: 30px; background: #2c3e50; margin: -25px -25px 25px -25px; color: white;",
                    div(
                      style = "font-size: 64px; margin-bottom: 15px;",
                      icon("microscope")
                    ),
                    h2("Análisis Cualitativo Avanzado", style = "margin: 0; font-weight: 600;")
                  ),
                  
                  # Descripción principal
                  div(
                    class = "info-panel",
                    p("RCualiText es una aplicación avanzada para la codificación cualitativa de textos que permite cargar documentos (.txt y .docx), definir códigos y categorías, resaltar extractos de interés y visualizar frecuencias y redes de coocurrencia de códigos.", 
                      style = "font-size: 16px; line-height: 1.8; color: #2c3e50;"),
                    p("Con RCualiText puedes gestionar de manera interactiva tu lista de códigos, agruparlos en categorías, exportar tus resaltados a Excel y analizar gráficamente tus datos cualitativos mediante visualizaciones modernas y análisis de redes.", 
                      style = "font-size: 16px; line-height: 1.8; color: #2c3e50;")
                  ),
                  
                  # Funcionalidades en grid
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 25px; margin: 30px 0;",
                    
                    # Funcionalidades de Resaltado
                    div(
                      class = "info-panel",
                      h4(icon("highlighter"), " Resaltado Inteligente", style = "color: #2c3e50; margin-bottom: 15px;"),
                      tags$ul(
                        style = "line-height: 1.8; color: #2c3e50;",
                        tags$li(strong("Modo Seleccionar:"), " Aplica códigos a fragmentos seleccionados"),
                        tags$li(strong("Modo Deseleccionar:"), " Elimina códigos específicos con un clic"),
                        tags$li(strong("Modo Acumulativo:"), " Múltiples códigos por fragmento"),
                        tags$li(strong("Gradientes Visuales:"), " Códigos múltiples con efectos visuales"),
                        tags$li(strong("Tooltips:"), " Información al pasar el mouse"),
                        tags$li(strong("Exportación:"), " Datos detallados a Excel")
                      )
                    ),
                    
                    # Guía de uso
                    div(
                      class = "info-panel",
                      h4(icon("user-graduate"), " Guía de Deselección", style = "color: #2c3e50; margin-bottom: 15px;"),
                      tags$ol(
                        style = "line-height: 1.8; color: #2c3e50;",
                        tags$li("Activa el modo 'Deseleccionar' en el panel de controles"),
                        tags$li("Haz clic directamente sobre el texto resaltado"),
                        tags$li("Selecciona qué código específico eliminar"),
                        tags$li("Vuelve al modo 'Seleccionar' para continuar")
                      )
                    )
                  ),
                  
                  # Autor y versión
                  div(
                    class = "info-panel",
                    style = "text-align: center; margin-top: 30px;",
                    h4("Dr. José Ventura-León", style = "color: #2c3e50; margin-bottom: 10px;")
                  )
                )
              )
      )
    )
  )
)

# ========================================
# SERVER (con downloadHandlers personalizados)
# ========================================
server <- function(input, output, session) {
  rv <- reactiveValues(
    codigosDF    = tibble(Codigo = character(), Color = character()),
    categoriasDF = tibble(Categoria = character(), Codigos = character()),
    docs         = NULL,
    idx          = 0,
    texto        = "",
    tabla        = tibble(
      Extracto   = character(),
      Codigo     = character(),
      Categoria  = character(),
      Color      = character(),
      Archivo    = character(),
      FragmentId = character(),
      Timestamp  = as.POSIXct(character())
    ),
    deselectMode = FALSE,
    ia_results   = tibble(
      Archivo    = character(),
      Categoria  = character(),
      Codigo     = character(),
      Definicion = character(),
      Extracto   = character()
    ),
    # Análisis Semántico (OpenAI)
    hf_embeddings       = NULL,    # Matriz de embeddings
    hf_similitud        = NULL,    # Matriz de similitud coseno
    hf_cache_hash       = NULL,    # Hash para detectar cambios
    datos_embedding_ref = NULL,    # Referencia a datos usados para embeddings
    semantico_clusters  = NULL,    # Resultado clustering
    semantico_validacion = NULL,   # Resultado validación LLM
    semantico_coherencia = NULL,   # Resultado coherencia
    similares_encontrados = NULL,  # Fragmentos similares encontrados
    red_semantica = NULL,          # Red semántica de códigos
    visualizacion_2d = NULL        # Datos de visualización 2D (coordenadas reducidas)
  )
  
  get_code_colors <- reactive({
    set_names(rv$codigosDF$Color, rv$codigosDF$Codigo)
  })

  # ========================================
  # Datos para Análisis Semántico (prioriza IA sobre manual)
  # ========================================
  datos_semantico <- reactive({
    # Priorizar resultados del Análisis IA
    if (!is.null(rv$ia_results) && nrow(rv$ia_results) > 0) {
      # Normalizar columnas para compatibilidad
      datos <- rv$ia_results %>%
        select(
          Extracto = Extracto,
          Codigo = Codigo,
          Categoria = Categoria,
          Archivo = Archivo
        ) %>%
        filter(!is.na(Extracto) & nchar(trimws(Extracto)) > 0)

      if (nrow(datos) > 0) {
        return(list(
          datos = datos,
          fuente = "ia",
          n = nrow(datos)
        ))
      }
    }

    # Fallback: codificación manual
    if (!is.null(rv$tabla) && nrow(rv$tabla) > 0) {
      datos <- rv$tabla %>%
        select(Extracto, Codigo, Categoria, Archivo) %>%
        filter(!is.na(Extracto) & nchar(trimws(Extracto)) > 0)

      if (nrow(datos) > 0) {
        return(list(
          datos = datos,
          fuente = "manual",
          n = nrow(datos)
        ))
      }
    }

    # Sin datos
    return(list(
      datos = NULL,
      fuente = "ninguno",
      n = 0
    ))
  })

  # ========================================
  # Descarga JPG del gráfico de distribución con controles personalizados
  # ========================================
  
  output$download_distribucion_jpg <- downloadHandler(
    filename = function() {
      paste("distribucion_codigos_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      if (nrow(rv$tabla) > 0) {
        # Obtener parámetros de descarga
        ancho <- input$plot_width
        alto <- input$plot_height
        dpi <- input$plot_dpi

        # Obtener colores de códigos
        code_colors <- get_code_colors()

        # Usar plot_codigos_ggplot para exportación estática
        p <- plot_codigos_ggplot(rv$tabla,
                                  fill = input$fillToggle,
                                  code_colors = code_colors)

        # Guardar como JPG con parámetros personalizados
        ggsave(file, plot = p, device = "jpeg",
               width = ancho, height = alto, dpi = dpi, bg = "white")

        showNotification(
          paste0("Gráfico de distribución descargado (", ancho, "×", alto, " pulg, ", dpi, " DPI)"),
          type = "message", duration = 4
        )
      } else {
        showNotification("No hay datos para descargar", type = "error", duration = 3)
      }
    }
  )
  
  # ========================================
  # Descarga JPG del gráfico de red con controles personalizados
  # ========================================
  
  output$download_red_jpg <- downloadHandler(
    filename = function() {
      paste("red_codigos_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      if (nrow(rv$tabla) > 0) {
        # Obtener parámetros de descarga
        ancho <- input$plot_width
        alto <- input$plot_height
        dpi <- input$plot_dpi
        
        # Crear el gráfico de red
        result <- plot_network_and_centrality(rv$tabla, 
                                              code_colors = get_code_colors())
        
        # Guardar como JPG con parámetros personalizados
        ggsave(file, plot = result$plot, device = "jpeg", 
               width = ancho, height = alto, dpi = dpi, bg = "white")
        
        showNotification(
          paste0("Gráfico de red descargado (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), 
          type = "message", duration = 4
        )
      } else {
        showNotification("No hay datos para descargar", type = "error", duration = 3)
      }
    }
  )
  
  # ========================================
  # Manejo de modos (Seleccionar/Deseleccionar)
  # ========================================
  
  observeEvent(input$modeSelect, {
    rv$deselectMode <- FALSE
    
    # Actualizar estilos de botones
    shinyjs::removeClass(id = "modeSelect", class = "deselect-active")
    shinyjs::removeClass(id = "modeDeselect", class = "deselect-active")
    shinyjs::addClass(id = "modeSelect", class = "active")
    shinyjs::removeClass(id = "modeDeselect", class = "active")
    
    # Enviar mensaje al JavaScript
    session$sendCustomMessage("setDeselectMode", FALSE)
    
    # Actualizar instrucciones
    output$currentModeInfo <- renderUI({
      div(
        class = "info-panel",
        h5(icon("mouse-pointer"), " Modo Seleccionar Activo", style = "color: #2c3e50; margin-bottom: 15px;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            h6("Acciones disponibles:", style = "color: #2c3e50; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #7f8c8d; margin: 0;",
              tags$li("Selecciona texto en el visor de documento"),
              tags$li("Aplica códigos automáticamente")
            )
          ),
          div(
            h6("Características:", style = "color: #2c3e50; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #7f8c8d; margin: 0;",
              tags$li("Modo acumulativo disponible"),
              tags$li("Clic en resaltado = info")
            )
          )
        )
      )
    })
    
    showNotification("Modo Seleccionar activado", type = "message", duration = 2)
  })
  
  observeEvent(input$modeDeselect, {
    rv$deselectMode <- TRUE
    
    # Actualizar estilos de botones
    shinyjs::removeClass(id = "modeSelect", class = "active")
    shinyjs::addClass(id = "modeDeselect", class = "active deselect-active")
    
    # Enviar mensaje al JavaScript
    session$sendCustomMessage("setDeselectMode", TRUE)
    
    # Actualizar instrucciones
    output$currentModeInfo <- renderUI({
      div(
        class = "danger-panel",
        h5(icon("eraser"), " Modo Deseleccionar Activo", style = "color: #c0392b; margin-bottom: 15px;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            h6("Instrucciones:", style = "color: #c0392b; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #a93226; margin: 0;",
              tags$li("Clic directo en texto resaltado"),
              tags$li("Elige código específico a eliminar")
            )
          ),
          div(
            h6("Importante:", style = "color: #c0392b; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #a93226; margin: 0;",
              tags$li("Cursor cambia en resaltado"),
              tags$li("Vuelve a 'Seleccionar' después")
            )
          )
        )
      )
    })
    
    showNotification("Modo Deseleccionar activado - Haz clic en texto resaltado del visor", 
                     type = "warning", duration = 4)
  })
  
  # ========================================
  # Manejo de deselección de fragmentos
  # ========================================
  
  observeEvent(input$deselectFragment, {
    req(rv$deselectMode, input$deselectFragment)
    
    fragment_data <- input$deselectFragment
    fragment_id <- fragment_data$id
    fragment_text <- fragment_data$text
    
    # Buscar todos los códigos aplicados a este fragmento
    fragmentos_asociados <- rv$tabla %>%
      filter(FragmentId == fragment_id)
    
    if (nrow(fragmentos_asociados) == 0) {
      showNotification("No se encontraron códigos para este fragmento", type = "error", duration = 3)
      return()
    }
    
    if (nrow(fragmentos_asociados) == 1) {
      # Solo un código - eliminar directamente
      codigo_eliminar <- fragmentos_asociados$Codigo[1]
      
      showModal(modalDialog(
        title = div(icon("exclamation-triangle"), " Confirmar Eliminación"),
        div(
          h4("¿Eliminar este resaltado?", style = "color: #c0392b;"),
          div(
            class = "info-panel",
            strong("Texto: "), str_trunc(fragment_text, 50), br(),
            strong("Código: "), span(codigo_eliminar, style = paste0("background:", fragmentos_asociados$Color[1], "; padding: 4px 8px; border-radius: 4px; color: white;")), br(),
            strong("Archivo: "), fragmentos_asociados$Archivo[1]
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirmarEliminacionUnica", "Eliminar", class = "btn-default")
        )
      ))
      
      # Guardar el fragmento para eliminación
      rv$fragmento_a_eliminar <- fragmentos_asociados[1, ]
      
    } else {
      # Múltiples códigos - mostrar opciones
      showModal(modalDialog(
        title = div(icon("list"), " Seleccionar Código a Eliminar"),
        div(
          h4("Este fragmento tiene múltiples códigos:", style = "color: #2c3e50;"),
          div(
            class = "info-panel",
            strong("Texto: "), str_trunc(fragment_text, 50)
          ),
          h5("Códigos aplicados:"),
          DTOutput("tablaCodigosFragmento")
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("eliminarCodigoSeleccionado", "Eliminar Seleccionado", class = "btn-default")
        ),
        size = "m"
      ))
      
      # Mostrar tabla de códigos del fragmento
      output$tablaCodigosFragmento <- renderDT({
        tabla_codigos <- fragmentos_asociados %>%
          select(Codigo, Categoria, Timestamp) %>%
          mutate(Timestamp = format(Timestamp, "%H:%M:%S")) %>%
          arrange(Codigo)
        
        datatable(
          tabla_codigos,
          selection = "single",
          options = list(
            pageLength = 5,
            searching = FALSE,
            info = FALSE,
            lengthChange = FALSE,
            dom = 't'
          ),
          colnames = c("Código", "Categoría", "Aplicado")
        ) %>%
          formatStyle(
            "Codigo",
            backgroundColor = styleEqual(
              fragmentos_asociados$Codigo,
              fragmentos_asociados$Color
            )
          )
      }, server = FALSE)
      
      # Guardar fragmentos para posible eliminación
      rv$fragmentos_multiples <- fragmentos_asociados
    }
  })
  
  # Manejar eliminación de código único
  observeEvent(input$confirmarEliminacionUnica, {
    req(rv$fragmento_a_eliminar)
    
    frag <- rv$fragmento_a_eliminar
    
    # Eliminar el fragmento específico
    rv$tabla <- rv$tabla %>%
      filter(!(Extracto == frag$Extracto & 
                 Codigo == frag$Codigo & 
                 Archivo == frag$Archivo &
                 Timestamp == frag$Timestamp))
    
    removeModal()
    showNotification(paste("Código", frag$Codigo, "eliminado del fragmento"), 
                     type = "message", duration = 3)
    
    # Limpiar variable temporal
    rv$fragmento_a_eliminar <- NULL
  })
  
  # Manejar eliminación de código seleccionado de múltiples
  observeEvent(input$eliminarCodigoSeleccionado, {
    sel <- input$tablaCodigosFragmento_rows_selected
    req(length(sel) == 1, rv$fragmentos_multiples)
    
    frag_eliminar <- rv$fragmentos_multiples[sel, ]
    
    # Eliminar el fragmento específico
    rv$tabla <- rv$tabla %>%
      filter(!(Extracto == frag_eliminar$Extracto & 
                 Codigo == frag_eliminar$Codigo & 
                 Archivo == frag_eliminar$Archivo &
                 Timestamp == frag_eliminar$Timestamp))
    
    removeModal()
    showNotification(paste("Código", frag_eliminar$Codigo, "eliminado del fragmento"), 
                     type = "message", duration = 3)
    
    # Limpiar variables temporales
    rv$fragmentos_multiples <- NULL
  })
  
  # ========================================
  # CRUD códigos
  # ========================================
  
  output$tablaCodigos <- renderDT({
    datatable(
      rv$codigosDF, 
      selection = "single", 
      options = list(
        pageLength = 8,
        dom = 'frtip',
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ códigos",
          info = "Mostrando _START_ a _END_ de _TOTAL_ códigos",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      ),
      colnames = c("Código", "Color")
    ) %>%
      formatStyle(
        "Color",
        backgroundColor = styleEqual(rv$codigosDF$Color, rv$codigosDF$Color),
        color = "white",
        fontWeight = "bold"
      )
  })
  
  observeEvent(input$tablaCodigos_rows_selected, {
    sel <- input$tablaCodigos_rows_selected; req(length(sel)==1)
    updateTextInput(session, "new_codigo", value = rv$codigosDF$Codigo[sel])
    updateColourInput(session, "new_color", value = rv$codigosDF$Color[sel])
  })
  
  observeEvent(input$addOrUpdateCodigo, {
    req(input$new_codigo)
    df <- rv$codigosDF
    if (input$new_codigo %in% df$Codigo) {
      df <- df %>% mutate(Color = if_else(Codigo==input$new_codigo, input$new_color, Color))
      showNotification(paste("Código", input$new_codigo, "actualizado"), type = "message", duration = 2)
    } else {
      df <- bind_rows(df, tibble(Codigo=input$new_codigo, Color=input$new_color))
      showNotification(paste("Código", input$new_codigo, "añadido"), type = "message", duration = 2)
    }
    rv$codigosDF <- df
    updateSelectInput(session, "codigoTexto", choices = df$Codigo)
    updateSelectizeInput(session, "codigos_for_categoria", choices = df$Codigo, server = TRUE)
  })
  
  observeEvent(input$deleteCodigo, {
    sel <- input$tablaCodigos_rows_selected; req(length(sel)==1)
    codigo_eliminar <- rv$codigosDF$Codigo[sel]
    
    # Mostrar confirmación con información de impacto
    resaltados_afectados <- rv$tabla %>% filter(Codigo == codigo_eliminar) %>% nrow()
    
    showModal(modalDialog(
      title = div(icon("exclamation-triangle"), " Confirmar Eliminación de Código"),
      div(
        h4(paste("¿Eliminar el código:", codigo_eliminar, "?"), style = "color: #c0392b;"),
        if(resaltados_afectados > 0) {
          div(
            class = "danger-panel",
            p(paste("Esto también eliminará", resaltados_afectados, "resaltado(s) asociado(s) a este código."),
              style = "color: #c0392b; font-weight: bold;")
          )
        } else {
          div(
            class = "info-panel",
            p("Este código no tiene resaltados asociados.", style = "color: #7f8c8d;")
          )
        }
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarEliminarCodigo", "Eliminar", class = "btn-default")
      )
    ))
    
    # Guardar código a eliminar para usarlo en la confirmación
    rv$codigo_a_eliminar <- codigo_eliminar
  })
  
  # Manejar confirmación de eliminación de código
  observeEvent(input$confirmarEliminarCodigo, {
    req(rv$codigo_a_eliminar)
    
    codigo_eliminar <- rv$codigo_a_eliminar
    
    # Eliminar código de la lista
    sel <- which(rv$codigosDF$Codigo == codigo_eliminar)
    df <- rv$codigosDF[-sel, ]
    rv$codigosDF <- df
    
    # Eliminar todos los resaltados asociados a este código
    resaltados_eliminados <- rv$tabla %>% filter(Codigo == codigo_eliminar) %>% nrow()
    rv$tabla <- rv$tabla %>% filter(Codigo != codigo_eliminar)
    
    # Actualizar interfaz
    updateSelectInput(session, "codigoTexto", choices = df$Codigo)
    updateSelectizeInput(session, "codigos_for_categoria", choices = df$Codigo, server = TRUE)
    
    # Limpiar variable temporal
    rv$codigo_a_eliminar <- NULL
    
    removeModal()
    
    if(resaltados_eliminados > 0) {
      showNotification(
        paste("Código", codigo_eliminar, "eliminado junto con", resaltados_eliminados, "resaltado(s)"), 
        type = "warning", 
        duration = 4
      )
    } else {
      showNotification(
        paste("Código", codigo_eliminar, "eliminado"), 
        type = "warning", 
        duration = 2
      )
    }
  })
  
  # ========================================
  # CRUD categorías
  # ========================================
  
  output$tablaCategorias <- renderDT({
    datatable(
      rv$categoriasDF, 
      selection = "single", 
      options = list(
        pageLength = 8,
        dom = 'frtip',
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ categorías",
          info = "Mostrando _START_ a _END_ de _TOTAL_ categorías",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      ),
      colnames = c("Categoría", "Códigos Asociados")
    )
  })
  
  observeEvent(input$tablaCategorias_rows_selected, {
    sel <- input$tablaCategorias_rows_selected; req(length(sel)==1)
    cats <- str_split(rv$categoriasDF$Codigos[sel], ",\\s*")[[1]]
    updateTextInput(session, "new_categoria", value = rv$categoriasDF$Categoria[sel])
    updateSelectizeInput(session, "codigos_for_categoria", selected = cats)
  })
  
  observeEvent(input$addOrUpdateCategoria, {
    req(input$new_categoria)
    df <- rv$categoriasDF
    nueva <- tibble(Categoria=input$new_categoria, Codigos=paste(input$codigos_for_categoria, collapse=", "))
    if (input$new_categoria %in% df$Categoria) {
      df <- df %>% filter(Categoria!=input$new_categoria) %>% bind_rows(nueva)
      showNotification(paste("Categoría", input$new_categoria, "actualizada"), type = "message", duration = 2)
    } else {
      df <- bind_rows(df, nueva)
      showNotification(paste("Categoría", input$new_categoria, "añadida"), type = "message", duration = 2)
    }
    rv$categoriasDF <- df
  })
  
  observeEvent(input$deleteCategoria, {
    sel <- input$tablaCategorias_rows_selected; req(length(sel)==1)
    categoria_eliminar <- rv$categoriasDF$Categoria[sel]
    df <- rv$categoriasDF[-sel, ]
    rv$categoriasDF <- df
    showNotification(paste("Categoría", categoria_eliminar, "eliminada"), type = "warning", duration = 2)
  })
  
  # ========================================
  # Carga y navegación de documentos
  # ========================================
  
  observeEvent(input$archivo, {
    files <- input$archivo
    docs <- map(seq_len(nrow(files)), ~{
      txt <- leer_archivo(files[., ])
      list(name = files$name[.], original = txt, modified = txt)
    })
    rv$docs  <- docs; rv$idx <- 1
    actualizar_texto_mostrado()
    showNotification(paste(nrow(files), "documento(s) cargado(s)"), type = "message", duration = 3)
  })
  
  observeEvent(input$prev_doc, {
    req(rv$idx > 1)
    rv$idx <- rv$idx - 1
    actualizar_texto_mostrado()
  })
  
  observeEvent(input$next_doc, {
    req(rv$idx < length(rv$docs))
    rv$idx <- rv$idx + 1
    actualizar_texto_mostrado()
  })
  
  # Función para actualizar el texto mostrado con resaltados
  actualizar_texto_mostrado <- reactive({
    req(rv$docs, rv$idx > 0)
    
    # Obtener texto original del documento actual
    texto_original <- rv$docs[[rv$idx]]$original
    archivo_actual <- rv$docs[[rv$idx]]$name
    
    # Limpiar espacios iniciales del texto original
    texto_original <- str_trim(texto_original)
    
    # Filtrar fragmentos del archivo actual
    fragmentos_archivo <- rv$tabla %>%
      filter(Archivo == archivo_actual)
    
    # Aplicar resaltados múltiples
    if (nrow(fragmentos_archivo) > 0) {
      rv$texto <- aplicar_resaltado_multiple(texto_original, fragmentos_archivo)
    } else {
      rv$texto <- texto_original
    }
    
    # Actualizar UI
    output$contenido <- renderUI(HTML(rv$texto))
  })
  
  # Observar cambios para actualizar texto
  observe({
    actualizar_texto_mostrado()
  })
  
  output$doc_info <- renderText({
    if (!is.null(rv$docs) && rv$idx>0) {
      paste0("Documento ", rv$idx, " de ", length(rv$docs), ": ", rv$docs[[rv$idx]]$name)
    } else "No hay documentos cargados"
  })
  
  # ========================================
  # Sistema de resaltar texto mejorado
  # ========================================
  
  observeEvent(input$selectedText, {
    # Solo procesar si no estamos en modo deselección
    req(!rv$deselectMode)
    
    txt <- str_trim(input$selectedText)
    req(txt != "", input$codigoTexto != "", rv$idx > 0)
    
    code <- input$codigoTexto
    col <- get_code_colors()[code]
    archivo_actual <- rv$docs[[rv$idx]]$name
    
    # Obtener categoría del código
    cat_sel <- rv$categoriasDF %>% 
      filter(str_detect(Codigos, fixed(code))) %>% 
      pull(Categoria) %>% 
      first()
    
    # Si no hay categoría asignada, usar "Sin categoría"
    if (is.null(cat_sel) || is.na(cat_sel) || cat_sel == "") {
      cat_sel <- "Sin categoría"
    }
    
    # Verificar si el fragmento ya existe
    fragmento_existente <- rv$tabla %>%
      filter(Extracto == txt, Archivo == archivo_actual)
    
    if (nrow(fragmento_existente) > 0 && input$modoAcumulativo) {
      # Modo acumulativo: verificar si el código ya está aplicado
      if (!code %in% fragmento_existente$Codigo) {
        # Usar el mismo FragmentId para el nuevo código
        fragment_id <- fragmento_existente$FragmentId[1]
        
        newrow <- tibble(
          Extracto = txt,
          Codigo = code,
          Categoria = cat_sel,
          Color = col,
          Archivo = archivo_actual,
          FragmentId = fragment_id,
          Timestamp = Sys.time()
        )
        
        rv$tabla <- bind_rows(rv$tabla, newrow)
        
        showNotification(
          paste("Código", code, "añadido al fragmento"),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste("El código", code, "ya está aplicado"),
          type = "warning",
          duration = 3
        )
      }
    } else {
      # Crear nuevo fragmento o reemplazar si no está en modo acumulativo
      fragment_id <- crear_fragment_id()
      
      if (!input$modoAcumulativo && nrow(fragmento_existente) > 0) {
        # Eliminar fragmentos existentes del mismo texto
        rv$tabla <- rv$tabla %>%
          filter(!(Extracto == txt & Archivo == archivo_actual))
      }
      
      newrow <- tibble(
        Extracto = txt,
        Codigo = code,
        Categoria = cat_sel,
        Color = col,
        Archivo = archivo_actual,
        FragmentId = fragment_id,
        Timestamp = Sys.time()
      )
      
      rv$tabla <- bind_rows(rv$tabla, newrow)
      
      if (!input$modoAcumulativo && nrow(fragmento_existente) > 0) {
        showNotification(
          paste("Fragmento recodificado con", code),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste("Fragmento codificado con", code),
          type = "message",
          duration = 3
        )
      }
    }
    
    # Limpiar selección (solo en el visor de documento)
    shinyjs::runjs("
      var documentViewer = document.getElementById('document-viewer');
      if (documentViewer && window.getSelection) {
        var selection = window.getSelection();
        if (selection.rangeCount > 0) {
          var range = selection.getRangeAt(0);
          if (documentViewer.contains(range.commonAncestorContainer)) {
            selection.removeAllRanges();
          }
        }
      }
    ")
  })
  
  # ========================================
  # Funciones de limpieza
  # ========================================
  
  # Función para limpiar resaltados del documento actual
  observeEvent(input$limpiarResaltados, {
    req(rv$idx > 0)
    archivo_actual <- rv$docs[[rv$idx]]$name
    
    showModal(modalDialog(
      title = div(icon("exclamation-triangle"), " Confirmar Limpieza"),
      div(
        h4("¿Estás seguro?", style = "color: #2c3e50;"),
        div(
          class = "info-panel",
          p(paste("Se eliminarán todos los resaltados del documento:", strong(archivo_actual)),
            style = "color: #e67e22; margin: 0;")
        )
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarLimpieza", "Sí, limpiar", class = "btn-default")
      )
    ))
  })
  
  observeEvent(input$confirmarLimpieza, {
    archivo_actual <- rv$docs[[rv$idx]]$name
    rv$tabla <- rv$tabla %>%
      filter(Archivo != archivo_actual)
    
    removeModal()
    showNotification(
      paste("Resaltados eliminados de", archivo_actual),
      type = "message",
      duration = 3
    )
  })
  
  # Función para eliminar todos los resaltados
  observeEvent(input$eliminarTodosResaltes, {
    showModal(modalDialog(
      title = div(icon("exclamation-triangle"), " Confirmación"),
      div(
        class = "danger-panel",
        h4("¿Eliminar TODOS los resaltados?", style = "color: #c0392b;"),
        p("Esta acción eliminará todos los códigos aplicados en todos los documentos.", style = "color: #a93226;"),
        p(strong("Esta acción no se puede deshacer."), style = "color: #8b1538; font-size: 16px;")
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarEliminacionTotal", "Sí, eliminar todo", class = "btn-default")
      )
    ))
  })
  
  observeEvent(input$confirmarEliminacionTotal, {
    rv$tabla <- rv$tabla[0, ]  # Vaciar tabla manteniendo estructura
    
    removeModal()
    showNotification("Todos los resaltados han sido eliminados", type = "warning", duration = 4)
  })
  
  # ========================================
  # Manejo de clics en fragmentos resaltados (modo info)
  # ========================================
  
  observeEvent(input$clickedFragment, {
    # Solo procesar si no estamos en modo deselección
    req(!rv$deselectMode)
    
    fragment_id <- input$clickedFragment
    req(fragment_id)
    
    # Buscar información del fragmento
    fragmento_info <- rv$tabla %>%
      filter(FragmentId == fragment_id) %>%
      arrange(Timestamp)
    
    if (nrow(fragmento_info) > 0) {
      codigos_aplicados <- paste(fragmento_info$Codigo, collapse = ", ")
      texto_fragmento <- fragmento_info$Extracto[1]
      
      showModal(modalDialog(
        title = div(icon("info-circle"), " Información del Fragmento"),
        div(
          div(
            class = "info-panel",
            h4("Texto:"),
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; max-height: 150px; overflow-y: auto; border-left: 4px solid #3498db;",
              texto_fragmento
            )
          ),
          div(
            class = "info-panel", 
            h4("Códigos aplicados:"),
            p(codigos_aplicados, style = "font-weight: bold; color: #3498db; font-size: 16px;"),
            h4("Archivo:"),
            p(fragmento_info$Archivo[1], style = "color: #7f8c8d;")
          ),
          if (nrow(fragmento_info) > 1) {
            div(
              class = "info-panel",
              h5("📈 Historial de codificación:"),
              DTOutput("historialCodificacion")
            )
          }
        ),
        footer = modalButton("Cerrar"),
        size = "m"
      ))
      
      # Mostrar historial si hay múltiples códigos
      if (nrow(fragmento_info) > 1) {
        output$historialCodificacion <- renderDT({
          historial <- fragmento_info %>%
            select(Codigo, Categoria, Timestamp) %>%
            mutate(Timestamp = format(Timestamp, "%d/%m/%Y %H:%M:%S")) %>%
            arrange(desc(Timestamp))
          
          datatable(
            historial,
            options = list(
              pageLength = 5,
              searching = FALSE,
              info = FALSE,
              lengthChange = FALSE,
              dom = 't'
            ),
            colnames = c("Código", "Categoría", "Aplicado")
          )
        }, server = FALSE)
      }
    }
  })
  
  # ========================================
  # Botón de ayuda
  # ========================================
  
  observeEvent(input$ayuda, {
    showModal(modalDialog(
      title = div(icon("question-circle"), " Centro de Ayuda"),
      div(
        div(
          class = "info-panel",
          h4("Modos de Trabajo:"),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            div(
              h5("Modo Seleccionar:", style = "color: #2c3e50;"),
              tags$ul(
                style = "color: #2c3e50;",
                tags$li("Selecciona texto en el visor de documento"),
                tags$li("Aplica códigos automáticamente"),
                tags$li("Resaltado inmediato en tiempo real")
              )
            ),
            div(
              h5("Modo Deseleccionar:", style = "color: #c0392b;"),
              tags$ul(
                style = "color: #2c3e50;",
                tags$li("Clic directo en resaltado"),
                tags$li("Elimina códigos específicos"),
                tags$li("Corrección precisa")
              )
            )
          )
        ),
        
        div(
          class = "info-panel",
          h4("Características Avanzadas:"),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            div(
              h5("Modo Acumulativo:", style = "color: #2c3e50;"),
              p("Permite aplicar múltiples códigos al mismo fragmento de texto", style = "color: #2c3e50;")
            ),
            div(
              h5("Visualización:", style = "color: #2c3e50;"),
              p("Gradientes muestran fragmentos con varios códigos", style = "color: #2c3e50;")
            )
          )
        )
      ),
      footer = modalButton("Entendido"),
      size = "l"
    ))
  })
  
  # ========================================
  # Botón para copiar cita mejorado
  # ========================================
  
  observeEvent(input$copycitation, {
    citation_text <- "Ventura-León, J. (2026). RCualiText (v1.0) [Shiny app]. GitHub. https://github.com/jventural/RCualiText_App"
    
    # Usar JavaScript para copiar al portapapeles
    shinyjs::runjs(paste0("
      navigator.clipboard.writeText('", citation_text, "').then(function() {
        // Éxito
      }, function(err) {
        // Error - fallback para navegadores antiguos
        var textArea = document.createElement('textarea');
        textArea.value = '", citation_text, "';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        try {
          document.execCommand('copy');
        } catch (err) {
          console.error('Error al copiar: ', err);
        }
        document.body.removeChild(textArea);
      });
    "))
    
    showNotification(
      "Cita copiada al portapapeles",
      type = "message",
      duration = 3
    )
  })
  
  # ========================================
  # Tabla de resaltados mejorada
  # ========================================
  
  output$tablaResaltes <- renderDT({
    req(nrow(rv$tabla) > 0)
    
    tabla_mostrar <- rv$tabla %>%
      arrange(desc(Timestamp)) %>%
      select(Extracto, Codigo, Categoria, Archivo, Timestamp) %>%
      mutate(
        Extracto = str_trunc(Extracto, 60),
        Timestamp = format(Timestamp, "%H:%M:%S")
      )
    
    # Crear el datatable
    dt <- datatable(
      tabla_mostrar,
      selection = "single",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        dom = 'frtip',
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ resaltados",
          info = "Mostrando _START_ a _END_ de _TOTAL_ resaltados",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        ),
        columnDefs = list(
          list(targets = 0, width = "250px"),
          list(targets = 1, width = "120px"),
          list(targets = 2, width = "120px"),
          list(targets = 3, width = "180px"),
          list(targets = 4, width = "80px")
        )
      ),
      colnames = c("Extracto", "Código", "Categoría", "Archivo", "Hora")
    )
    
    # Aplicar formatStyle solo si hay códigos disponibles
    if (nrow(rv$codigosDF) > 0 && length(unique(tabla_mostrar$Codigo)) > 0) {
      # Filtrar solo los códigos que aparecen en la tabla
      codigos_en_tabla <- intersect(rv$codigosDF$Codigo, tabla_mostrar$Codigo)
      colores_filtrados <- rv$codigosDF %>%
        filter(Codigo %in% codigos_en_tabla) %>%
        select(Codigo, Color)
      
      if (nrow(colores_filtrados) > 0) {
        dt <- dt %>%
          formatStyle(
            "Codigo",
            backgroundColor = styleEqual(
              colores_filtrados$Codigo,
              colores_filtrados$Color
            ),
            color = "white",
            fontWeight = "bold"
          )
      }
    }
    
    return(dt)
  })
  
  # Eliminar resalte seleccionado desde la tabla
  observeEvent(input$eliminarResalte, {
    sel <- input$tablaResaltes_rows_selected
    req(length(sel) == 1)
    
    # Obtener la fila seleccionada (considerando el orden de la tabla mostrada)
    tabla_ordenada <- rv$tabla %>%
      arrange(desc(Timestamp))
    
    fila_eliminar <- tabla_ordenada[sel, ]
    
    showModal(modalDialog(
      title = div(icon("exclamation-triangle"), " Confirmar Eliminación"),
      div(
        h4("¿Eliminar este resaltado?", style = "color: #c0392b;"),
        div(
          class = "info-panel",
          p(paste("Código:", strong(fila_eliminar$Codigo)), style = "margin: 5px 0;"),
          p(paste("Fragmento:", strong(str_trunc(fila_eliminar$Extracto, 40))), style = "margin: 5px 0;")
        )
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarEliminacion", "Eliminar", class = "btn-default")
      )
    ))
  })
  
  observeEvent(input$confirmarEliminacion, {
    sel <- input$tablaResaltes_rows_selected
    req(length(sel) == 1)
    
    tabla_ordenada <- rv$tabla %>%
      arrange(desc(Timestamp))
    
    fila_eliminar <- tabla_ordenada[sel, ]
    
    # Eliminar la fila específica
    rv$tabla <- rv$tabla %>%
      filter(!(Extracto == fila_eliminar$Extracto & 
                 Codigo == fila_eliminar$Codigo & 
                 Archivo == fila_eliminar$Archivo &
                 Timestamp == fila_eliminar$Timestamp))
    
    removeModal()
    showNotification("Resaltado eliminado", type = "message", duration = 3)
  })
  
  # ========================================
  # Descarga mejorada
  # ========================================
  
  output$descarga <- downloadHandler(
    filename = function() paste0("resaltados_rcualitext_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      
      # Hoja principal con todos los resaltados
      addWorksheet(wb, "Resaltados_Detallados")
      writeData(wb, "Resaltados_Detallados", rv$tabla)
      
      # Hoja con resumen de fragmentos únicos
      resumen_fragmentos <- rv$tabla %>%
        group_by(Extracto, Archivo, FragmentId) %>%
        summarise(
          Codigos_Aplicados = paste(Codigo, collapse = "; "),
          Num_Codigos = n(),
          Primera_Codificacion = min(Timestamp),
          Ultima_Codificacion = max(Timestamp),
          .groups = "drop"
        ) %>%
        arrange(desc(Num_Codigos))
      
      addWorksheet(wb, "Resumen_Fragmentos")
      writeData(wb, "Resumen_Fragmentos", resumen_fragmentos)
      
      # Hoja con estadísticas
      estadisticas <- tibble(
        Metrica = c(
          "Total de Resaltados",
          "Fragmentos Únicos",
          "Fragmentos con Múltiples Códigos",
          "Promedio de Códigos por Fragmento",
          "Documentos Procesados",
          "Códigos Diferentes Utilizados"
        ),
        Valor = c(
          nrow(rv$tabla),
          length(unique(rv$tabla$FragmentId)),
          sum(table(rv$tabla$FragmentId) > 1),
          round(nrow(rv$tabla) / length(unique(rv$tabla$FragmentId)), 2),
          length(unique(rv$tabla$Archivo)),
          length(unique(rv$tabla$Codigo))
        )
      )
      
      addWorksheet(wb, "Estadisticas")
      writeData(wb, "Estadisticas", estadisticas)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification("Datos exportados exitosamente", type = "message", duration = 3)
    }
  )
  
  # ========================================
  # Gráficos de análisis actualizados con mejor diseño
  # ========================================
  
  output$plotCodigos <- renderPlotly({
    # Hacer que dependa tanto de la tabla como de los códigos
    req(nrow(rv$tabla) > 0, nrow(rv$codigosDF) >= 0)

    tryCatch({
      # plot_codigos ahora devuelve un objeto plotly directamente
      plot_codigos(rv$tabla, fill = input$fillToggle, code_colors = get_code_colors())
    }, error = function(e) {
      showNotification(paste("Error al generar gráfico:", e$message), type = "error")
      NULL
    })
  })
  
  output$plotRedCentralidad <- renderPlot({
    # Hacer que dependa tanto de la tabla como de los códigos
    req(nrow(rv$tabla) > 0, nrow(rv$codigosDF) >= 0)
    
    result <- plot_network_and_centrality(rv$tabla, code_colors = get_code_colors())
    result$plot
  }, res = 96, bg = "transparent")
  
  # ========================================
  # Guardar / cargar estado actualizado
  # ========================================
  
  output$saveState <- downloadHandler(
    filename = function() paste0("proyecto_rcualitext_", Sys.Date(), ".rds"),
    content = function(file) {
      estado <- list(
        # Datos básicos
        codigosDF = rv$codigosDF,
        categoriasDF = rv$categoriasDF,
        docs = rv$docs,
        idx = rv$idx,
        texto = rv$texto,
        tabla = rv$tabla,
        deselectMode = rv$deselectMode,
        # Análisis IA
        ia_results = rv$ia_results,
        # Análisis Semántico
        hf_embeddings = rv$hf_embeddings,
        hf_similitud = rv$hf_similitud,
        hf_cache_hash = rv$hf_cache_hash,
        datos_embedding_ref = rv$datos_embedding_ref,
        semantico_clusters = rv$semantico_clusters,
        semantico_validacion = rv$semantico_validacion,
        semantico_coherencia = rv$semantico_coherencia,
        similares_encontrados = rv$similares_encontrados,
        red_semantica = rv$red_semantica,
        visualizacion_2d = rv$visualizacion_2d,
        # Metadatos
        version = "2.5_con_visualizacion",
        metadata = list(
          created = Sys.time(),
          app_version = "RCualiText v2.4 con IA, Análisis Semántico y Red",
          total_codes = nrow(rv$codigosDF),
          total_highlights = nrow(rv$tabla),
          total_docs = length(rv$docs),
          ia_results_count = if(!is.null(rv$ia_results)) nrow(rv$ia_results) else 0,
          embeddings_count = if(!is.null(rv$hf_embeddings)) nrow(rv$hf_embeddings) else 0,
          red_semantica = if(!is.null(rv$red_semantica)) "Sí" else "No",
          visualizacion_2d = if(!is.null(rv$visualizacion_2d)) rv$visualizacion_2d$metodo else "No"
        )
      )
      saveRDS(estado, file)

      showNotification("Proyecto guardado exitosamente (incluye Análisis Semántico y Visualización 2D)", type = "message", duration = 3)
    }
  )
  
  observeEvent(input$loadState, {
    tryCatch({
      est <- readRDS(input$loadState$datapath)
      
      # Verificar compatibilidad de versión
      if (!"FragmentId" %in% names(est$tabla)) {
        # Migrar formato antiguo al nuevo
        est$tabla <- est$tabla %>%
          mutate(
            FragmentId = map_chr(seq_len(nrow(.)), ~crear_fragment_id()),
            Timestamp = Sys.time() + seq_len(nrow(.))
          )
        
        showNotification(
          "Proyecto convertido al nuevo formato con soporte para deselección",
          type = "message",
          duration = 5
        )
      }
      
      # Cargar todos los valores
      for (nm in names(est)) {
        if (nm != "version" && nm != "metadata") {
          rv[[nm]] <- est[[nm]]
        }
      }
      
      # Si no existe ia_results en el archivo antiguo, inicializarlo
      if (is.null(rv$ia_results)) {
        rv$ia_results <- tibble(
          Archivo = character(),
          Categoria = character(),
          Codigo = character(),
          Definicion = character(),
          Extracto = character()
        )
      }

      # Inicializar campos de análisis semántico si no existen (archivos antiguos)
      if (is.null(rv$hf_embeddings)) rv$hf_embeddings <- NULL
      if (is.null(rv$hf_similitud)) rv$hf_similitud <- NULL
      if (is.null(rv$hf_cache_hash)) rv$hf_cache_hash <- NULL
      if (is.null(rv$datos_embedding_ref)) rv$datos_embedding_ref <- NULL
      if (is.null(rv$semantico_clusters)) rv$semantico_clusters <- NULL
      if (is.null(rv$semantico_validacion)) rv$semantico_validacion <- NULL
      if (is.null(rv$semantico_coherencia)) rv$semantico_coherencia <- NULL
      if (is.null(rv$similares_encontrados)) rv$similares_encontrados <- NULL
      if (is.null(rv$red_semantica)) rv$red_semantica <- NULL
      
      # Limpiar categorías vacías en datos existentes
      if (nrow(rv$tabla) > 0) {
        rv$tabla <- rv$tabla %>%
          mutate(Categoria = case_when(
            is.na(Categoria) | Categoria == "" ~ "Sin categoría",
            TRUE ~ Categoria
          ))
      }
      
      # Actualizar interfaz
      updateSelectInput(session, "codigoTexto", choices = rv$codigosDF$Codigo)
      updateSelectizeInput(session, "codigos_for_categoria", choices = rv$codigosDF$Codigo, server = TRUE)
      
      # Restaurar estado del modo
      if (!is.null(rv$deselectMode) && rv$deselectMode) {
        shinyjs::click("modeDeselect")
      } else {
        shinyjs::click("modeSelect")
      }
      
      # Mostrar información del proyecto cargado
      metadata_info <- ""
      if (!is.null(est$metadata)) {
        ia_info <- ""
        if (!is.null(est$metadata$ia_results_count) && est$metadata$ia_results_count > 0) {
          ia_info <- paste0(", ", est$metadata$ia_results_count, " resultados IA")
        }
        metadata_info <- paste0(
          " (", est$metadata$total_codes, " códigos, ",
          est$metadata$total_highlights, " resaltados", ia_info, ")"
        )
      }
      
      showNotification(
        paste0("Proyecto cargado exitosamente", metadata_info),
        type = "message",
        duration = 4
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error al cargar el proyecto:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # ========================================
  # Inicialización de la aplicación mejorada
  # ========================================
  
  # Configurar modo inicial
  observe({
    # Asegurar que el modo seleccionar esté activo al inicio
    if (is.null(rv$deselectMode)) {
      rv$deselectMode <- FALSE
    }
  })
  
  # Renderizar instrucciones iniciales mejoradas
  output$currentModeInfo <- renderUI({
    div(
      class = "info-panel",
      h5(icon("mouse-pointer"), " Modo Seleccionar Activo", style = "color: #2c3e50; margin-bottom: 15px;"),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
        div(
          h6("Acciones disponibles:", style = "color: #2c3e50; margin-bottom: 8px;"),
          tags$ul(
            style = "font-size: 13px; color: #7f8c8d; margin: 0;",
            tags$li("Selecciona texto en el visor de documento"),
            tags$li("Aplica códigos automáticamente")
          )
        ),
        div(
          h6("Características:", style = "color: #2c3e50; margin-bottom: 8px;"),
          tags$ul(
            style = "font-size: 13px; color: #7f8c8d; margin: 0;",
            tags$li("Modo acumulativo disponible"),
            tags$li("Clic en resaltado = info")
          )
        )
      )
    )
  })
  
  # Mensaje de bienvenida mejorado
  observe({
    if (is.null(rv$docs) || length(rv$docs) == 0) {
      showNotification(
        "¡Bienvenido a RCualiText! Carga tus documentos para comenzar.",
        type = "message",
        duration = 5
      )
    }
  })
  
  # ========================================
  # Análisis IA
  # ========================================
  
  # Cargar diccionario para IA
  dict_ia_df <- reactive({
    req(input$dict_ia)
    ext <- tools::file_ext(input$dict_ia$datapath)
    df <- switch(ext,
                 csv  = read.csv(input$dict_ia$datapath, stringsAsFactors = FALSE),
                 xlsx = read_excel(input$dict_ia$datapath),
                 stop("Formato de diccionario no soportado"))
    
    # Validar columnas
    expected_cols <- c("Categoría", "Código", "Definición")
    if (!all(expected_cols %in% names(df))) {
      # Intentar con nombres alternativos
      if (all(c("Categoria", "Codigo", "Definicion") %in% names(df))) {
        names(df)[names(df) == "Categoria"] <- "Categoría"
        names(df)[names(df) == "Codigo"] <- "Código"
        names(df)[names(df) == "Definicion"] <- "Definición"
      } else {
        stop("El diccionario debe tener columnas: Categoría, Código, Definición")
      }
    }
    df
  })
  
  # Ejecutar análisis IA
  observeEvent(input$run_ia_analysis, {
    # Validaciones
    if (is.null(rv$docs) || length(rv$docs) == 0) {
      showNotification("Carga al menos un documento en la pestaña 'Documento'", type = "error", duration = 3)
      return()
    }

    dict <- tryCatch(dict_ia_df(), error = function(e) NULL)
    if (is.null(dict) || nrow(dict) == 0) {
      showNotification("Carga un diccionario de códigos válido", type = "error", duration = 3)
      return()
    }

    # Validar API Key de OpenAI
    api_key <- input$openai_api_key
    if (is.null(api_key) || !nzchar(trimws(api_key))) {
      showNotification("Ingresa tu API Key de OpenAI", type = "error", duration = 3)
      return()
    }

    n_codes <- nrow(dict)
    total <- length(rv$docs) * n_codes

    withProgress(message = "Analizando con GPT-4.1...", value = 0, {
      results_list <- vector("list", total)
      step <- 0

      for (i in seq_along(rv$docs)) {
        doc_name <- rv$docs[[i]]$name
        doc_text <- rv$docs[[i]]$modified

        for (j in seq_len(n_codes)) {
          step <- step + 1
          catg <- dict$Categoría[j]
          code <- dict$Código[j]
          def <- dict$Definición[j]

          incProgress(amount = 1/total,
                      detail = paste(doc_name, "-", code))

          prompt <- paste0(
            "Del texto:\n\n", doc_text,
            "\n\nExtrae fragmentos que correspondan a la siguiente definición:\n\"",
            def, "\"\n\nResponde solo con los fragmentos extraídos, uno por línea."
          )

          tryCatch({
            txt_out <- tryCatch({
              call_openai_api(prompt, api_key)
            }, error = function(e) {
              showNotification(
                paste("Error OpenAI -", code, ":", e$message),
                type = "warning",
                duration = 5
              )
              NULL
            })

            # Procesar resultado
            if (!is.null(txt_out)) {
              exs <- str_split(txt_out, "\n")[[1]]
              exs <- exs[exs != "" & !grepl("^\\s*$", exs)]

              results_list[[step]] <- tibble(
                Archivo = doc_name,
                Categoria = catg,
                Codigo = code,
                Definicion = def,
                Extracto = if(length(exs) > 0) exs else NA_character_
              )
            } else {
              results_list[[step]] <- tibble(
                Archivo = doc_name,
                Categoria = catg,
                Codigo = code,
                Definicion = def,
                Extracto = NA_character_
              )
            }
          }, error = function(e) {
            showNotification(
              paste("Error inesperado -", code, ":", e$message),
              type = "warning",
              duration = 5
            )
            results_list[[step]] <- tibble(
              Archivo = doc_name,
              Categoria = catg,
              Codigo = code,
              Definicion = def,
              Extracto = NA_character_
            )
          })
        }
      }
      
      # Combinar resultados
      all_results <- bind_rows(results_list)
      
      # FORZAR la creación de la columna Archivo si no existe
      if (!"Archivo" %in% names(all_results) && nrow(all_results) > 0) {
        # Recrear la columna Archivo basándose en los nombres de documentos
        doc_names <- sapply(rv$docs, function(d) d$name)
        n_docs <- length(doc_names)
        n_codes <- nrow(dict)
        
        # Crear vector de nombres de archivo que se repite para cada código
        archivo_vec <- rep(doc_names, each = n_codes)
        # Tomar solo las primeras nrow(all_results) entradas
        archivo_vec <- archivo_vec[1:nrow(all_results)]
        
        # Crear nuevo tibble con columna Archivo al inicio
        all_results <- tibble(
          Archivo = archivo_vec,
          Categoria = all_results$Categoria,
          Codigo = all_results$Codigo,
          Definicion = all_results$Definicion,
          Extracto = all_results$Extracto
        )
      }
      
      if (!is.null(all_results) && nrow(all_results) > 0 && "Extracto" %in% names(all_results)) {
        rv$ia_results <- all_results %>%
          filter(!is.na(Extracto) & nchar(trimws(Extracto)) > 0)
      } else {
        rv$ia_results <- tibble(
          Archivo = character(),
          Categoria = character(),
          Codigo = character(),
          Definicion = character(),
          Extracto = character()
        )
      }
      
      # Verificar si se obtuvieron resultados
      if (nrow(rv$ia_results) == 0) {
        showNotification(
          "No se pudieron extraer fragmentos. Verifica tu API Key y conexión.",
          type = "warning",
          duration = 5
        )
      } else {
        showNotification(
          paste("Análisis IA completado:", nrow(rv$ia_results), "extractos encontrados"),
          type = "message",
          duration = 3
        )
      }
    })
  })
  
  # Mostrar resultados IA
  output$tabla_ia_results <- renderDT({
    req(nrow(rv$ia_results) > 0)
    
    datatable(
      rv$ia_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(
            first = "Primero",
            last = "Último",
            `next` = "Siguiente",
            previous = "Anterior"
          )
        )
      ),
      rownames = FALSE
    )
  })
  
  # Gráfico de distribución de códigos IA
  output$plot_ia_distribucion <- renderPlotly({
    req(nrow(rv$ia_results) > 0)
    
    tryCatch({
      # Contar frecuencia de códigos
      freq_data <- rv$ia_results %>%
        count(Codigo, Categoria, name = "Frecuencia") %>%
        arrange(desc(Frecuencia))
      
      p <- ggplot(freq_data, aes(x = reorder(Codigo, Frecuencia), y = Frecuencia, fill = Categoria)) +
        geom_col() +
        coord_flip() +
        labs(x = "Código", y = "Frecuencia", fill = "Categoría") +
        theme_minimal() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9)
        )
      
      plotly::ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        plotly::layout(
          margin = list(l = 100, r = 100, t = 50, b = 50),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }, error = function(e) {
      NULL
    })
  })
  
  # Gráfico de fragmentos por categoría IA
  output$plot_ia_categorias <- renderPlotly({
    req(nrow(rv$ia_results) > 0)
    
    tryCatch({
      # Contar fragmentos por categoría
      cat_data <- rv$ia_results %>%
        count(Categoria, name = "Fragmentos") %>%
        arrange(desc(Fragmentos))
      
      p <- plot_ly(
        cat_data,
        labels = ~Categoria,
        values = ~Fragmentos,
        type = "pie",
        textposition = "inside",
        textinfo = "label+percent",
        hoverinfo = "label+value+percent",
        marker = list(
          line = list(color = "white", width = 2)
        )
      ) %>%
        plotly::layout(
          showlegend = TRUE,
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 0.5
          ),
          margin = list(l = 50, r = 150, t = 50, b = 50),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)
      
      p
    }, error = function(e) {
      NULL
    })
  })
  
  # ========================================
  # Descarga de resultados IA
  # ========================================
  output$download_ia_results <- downloadHandler(
    filename = function() {
      paste0("resultados_ia_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(nrow(rv$ia_results) > 0)
      
      # Crear workbook
      wb <- createWorkbook()
      addWorksheet(wb, "Resultados IA")
      
      # Escribir los datos
      writeData(wb, "Resultados IA", rv$ia_results)
      
      # Aplicar estilos al encabezado
      headerStyle <- createStyle(
        fontSize = 12,
        fontColour = "#FFFFFF",
        halign = "center",
        fgFill = "#4F81BD",
        border = "TopBottom",
        borderColour = "#4F81BD",
        textDecoration = "bold"
      )
      
      addStyle(wb, sheet = "Resultados IA", headerStyle, rows = 1, cols = 1:5, gridExpand = TRUE)
      
      # Ajustar anchos de columna
      setColWidths(wb, sheet = "Resultados IA", cols = 1:5, widths = c(25, 20, 20, 40, 50))

      # Guardar
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # ========================================
  # Descargas Análisis IA - Figuras y Tablas
  # ========================================

  # Descarga tabla IA en Excel
  output$download_tabla_ia_excel <- downloadHandler(
    filename = function() { paste0("tabla_ia_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(rv$ia_results, nrow(rv$ia_results) > 0)
      wb <- createWorkbook()
      addWorksheet(wb, "Análisis IA")
      writeData(wb, "Análisis IA", rv$ia_results)
      headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                                  fgFill = "#27ae60", textDecoration = "bold")
      addStyle(wb, sheet = "Análisis IA", headerStyle, rows = 1, cols = 1:ncol(rv$ia_results), gridExpand = TRUE)
      setColWidths(wb, sheet = "Análisis IA", cols = 1:ncol(rv$ia_results), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tabla IA descargada", type = "message", duration = 3)
    }
  )

  # Descarga figura distribución IA
  output$download_ia_distribucion_png <- downloadHandler(
    filename = function() { paste0("ia_distribucion_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$ia_results, nrow(rv$ia_results) > 0)
      ancho <- if (!is.null(input$sem_plot_width)) input$sem_plot_width else 10
      alto <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else 8
      dpi <- if (!is.null(input$sem_plot_dpi)) input$sem_plot_dpi else 300

      p <- rv$ia_results %>%
        count(Codigo) %>%
        arrange(desc(n)) %>%
        mutate(Codigo = factor(Codigo, levels = Codigo)) %>%
        ggplot(aes(x = Codigo, y = n, fill = Codigo)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(title = "Distribución de Códigos (Análisis IA)", x = "Código", y = "Frecuencia") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(paste0("Figura descargada (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), type = "message", duration = 3)
    }
  )

  # Descarga figura categorías IA
  output$download_ia_categorias_png <- downloadHandler(
    filename = function() { paste0("ia_categorias_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$ia_results, nrow(rv$ia_results) > 0)
      ancho <- if (!is.null(input$sem_plot_width)) input$sem_plot_width else 10
      alto <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else 8
      dpi <- if (!is.null(input$sem_plot_dpi)) input$sem_plot_dpi else 300

      p <- rv$ia_results %>%
        count(Categoria) %>%
        arrange(desc(n)) %>%
        ggplot(aes(x = "", y = n, fill = Categoria)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Fragmentos por Categoría (Análisis IA)", fill = "Categoría") +
        theme_void(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(paste0("Figura descargada (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), type = "message", duration = 3)
    }
  )

  # ========================================
  # Descargas Análisis Semántico - Figuras y Tablas
  # ========================================

  # Descarga tabla clustering
  output$download_clustering_excel <- downloadHandler(
    filename = function() { paste0("clustering_semantico_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(rv$semantico_clusters, rv$datos_embedding_ref)
      datos <- rv$datos_embedding_ref %>%
        mutate(Cluster = rv$semantico_clusters$clusters)
      wb <- createWorkbook()
      addWorksheet(wb, "Clustering")
      writeData(wb, "Clustering", datos)
      headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                                  fgFill = "#3498db", textDecoration = "bold")
      addStyle(wb, sheet = "Clustering", headerStyle, rows = 1, cols = 1:ncol(datos), gridExpand = TRUE)
      setColWidths(wb, sheet = "Clustering", cols = 1:ncol(datos), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tabla de clustering descargada", type = "message", duration = 3)
    }
  )

  # Descarga figura clustering
  output$download_clustering_png <- downloadHandler(
    filename = function() { paste0("clustering_semantico_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$semantico_clusters, rv$hf_embeddings)
      ancho <- if (!is.null(input$sem_plot_width)) input$sem_plot_width else 10
      alto <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else 8
      dpi <- if (!is.null(input$sem_plot_dpi)) input$sem_plot_dpi else 300

      pca_result <- prcomp(rv$hf_embeddings, scale. = TRUE)
      plot_data <- data.frame(
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2],
        Cluster = factor(rv$semantico_clusters$clusters),
        Codigo = rv$datos_embedding_ref$Codigo
      )
      p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "Clustering Semántico", x = "PC1", y = "PC2", color = "Cluster") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(paste0("Figura descargada (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), type = "message", duration = 3)
    }
  )

  # Descarga tabla similitud
  output$download_similitud_excel <- downloadHandler(
    filename = function() { paste0("similitud_semantico_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(rv$similares_encontrados, nrow(rv$similares_encontrados) > 0)
      wb <- createWorkbook()
      addWorksheet(wb, "Similitud")
      writeData(wb, "Similitud", rv$similares_encontrados)
      headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                                  fgFill = "#9b59b6", textDecoration = "bold")
      addStyle(wb, sheet = "Similitud", headerStyle, rows = 1, cols = 1:ncol(rv$similares_encontrados), gridExpand = TRUE)
      setColWidths(wb, sheet = "Similitud", cols = 1:ncol(rv$similares_encontrados), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tabla de similitud descargada", type = "message", duration = 3)
    }
  )

  # Descarga figura visualización 2D
  output$download_visualizacion_png <- downloadHandler(
    filename = function() { paste0("visualizacion_2d_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$visualizacion_2d, rv$datos_embedding_ref)
      ancho <- if (!is.null(input$sem_plot_width)) input$sem_plot_width else 10
      alto <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else 8
      dpi <- if (!is.null(input$sem_plot_dpi)) input$sem_plot_dpi else 300

      coords <- rv$visualizacion_2d$coords
      metodo <- rv$visualizacion_2d$metodo
      plot_data <- coords %>%
        mutate(
          Codigo = rv$datos_embedding_ref$Codigo[1:nrow(coords)],
          Extracto = stringr::str_trunc(rv$datos_embedding_ref$Extracto[1:nrow(coords)], 50)
        )
      p <- ggplot(plot_data, aes(x = X, y = Y, color = Codigo)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Visualización de Embeddings (", toupper(metodo), ")", sep = ""),
          x = paste(toupper(metodo), "Dim 1"), y = paste(toupper(metodo), "Dim 2"), color = "Código"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "right")

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(paste0("Figura descargada (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), type = "message", duration = 3)
    }
  )

  # Descarga tabla coherencia
  output$download_coherencia_excel <- downloadHandler(
    filename = function() { paste0("coherencia_semantico_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(rv$semantico_coherencia)
      wb <- createWorkbook()
      addWorksheet(wb, "Coherencia")
      writeData(wb, "Coherencia", rv$semantico_coherencia)
      headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                                  fgFill = "#e67e22", textDecoration = "bold")
      addStyle(wb, sheet = "Coherencia", headerStyle, rows = 1, cols = 1:ncol(rv$semantico_coherencia), gridExpand = TRUE)
      setColWidths(wb, sheet = "Coherencia", cols = 1:ncol(rv$semantico_coherencia), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tabla de coherencia descargada", type = "message", duration = 3)
    }
  )

  # Descarga figura coherencia (idéntico al renderPlot)
  output$download_coherencia_png <- downloadHandler(
    filename = function() { paste0("coherencia_semantico_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$semantico_coherencia, nrow(rv$semantico_coherencia) > 0)
      ancho <- if (!is.null(input$sem_plot_width)) input$sem_plot_width else 10
      alto <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else 8
      dpi <- if (!is.null(input$sem_plot_dpi)) input$sem_plot_dpi else 300

      datos <- rv$semantico_coherencia %>%
        filter(!is.na(Coherencia_Media)) %>%
        arrange(desc(Coherencia_Media))

      if (nrow(datos) == 0) {
        showNotification("No hay datos de coherencia para graficar", type = "error", duration = 3)
        return(NULL)
      }

      p <- ggplot(datos, aes(x = reorder(Codigo, Coherencia_Media), y = Coherencia_Media, fill = Evaluacion)) +
        geom_col() +
        geom_errorbar(aes(ymin = Coherencia_Min, ymax = Coherencia_Max), width = 0.2, alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(values = c(
          "Excelente" = "#27ae60",
          "Buena" = "#2ecc71",
          "Moderada" = "#f39c12",
          "Baja - revisar" = "#e74c3c"
        )) +
        labs(
          title = "Coherencia Semántica por Código",
          x = "Código",
          y = "Coherencia Media (similitud coseno)",
          fill = "Evaluación"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        geom_hline(yintercept = 0.6, linetype = "dashed", color = "#7f8c8d", alpha = 0.7)

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(paste0("Figura descargada (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), type = "message", duration = 3)
    }
  )

  # Descarga red semántica (idéntico al renderPlot)
  output$download_red_semantica_png <- downloadHandler(
    filename = function() { paste0("red_semantica_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$red_semantica, rv$red_semantica$grafo)
      ancho <- if (!is.null(input$sem_plot_width)) input$sem_plot_width else 12
      alto <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else 10
      dpi <- if (!is.null(input$sem_plot_dpi)) input$sem_plot_dpi else 300

      grafo <- rv$red_semantica$grafo
      color_por <- if (!is.null(input$color_red_semantica)) input$color_red_semantica else "categoria"

      # Detectar comunidades si se seleccionó
      if (color_por == "comunidad") {
        comunidades <- igraph::cluster_louvain(igraph::as.igraph(grafo))
        grafo <- grafo %>%
          tidygraph::activate(nodes) %>%
          tidygraph::mutate(comunidad = factor(comunidades$membership))
        color_var <- "comunidad"
        color_title <- "Comunidad"
      } else {
        color_var <- "categoria"
        color_title <- "Categoría"
      }

      # Crear el gráfico con ggraph (mismo código que renderPlot)
      set.seed(2026)

      p <- ggraph::ggraph(grafo, layout = "fr") +
        ggraph::geom_edge_link(
          ggplot2::aes(width = width, alpha = weight),
          color = "#7f8c8d",
          show.legend = FALSE
        ) +
        ggraph::geom_node_point(
          ggplot2::aes(size = size, color = .data[[color_var]]),
          alpha = 0.8
        ) +
        ggraph::geom_node_text(
          ggplot2::aes(label = name),
          repel = TRUE,
          size = 3.5,
          color = "#2c3e50",
          fontface = "bold"
        ) +
        ggraph::scale_edge_width(range = c(0.5, 3)) +
        ggplot2::scale_size_continuous(range = c(5, 20), guide = "none") +
        ggplot2::labs(
          title = "Red Semántica de Códigos",
          subtitle = paste("Umbral de similitud:", input$umbral_red_semantica),
          color = color_title
        ) +
        ggraph::theme_graph(base_family = "sans") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5, color = "#2c3e50"),
          plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "#7f8c8d"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(face = "bold", size = 10),
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          panel.background = ggplot2::element_rect(fill = "white", color = NA)
        )

      # Paleta de colores
      n_colors <- length(unique(igraph::vertex_attr(igraph::as.igraph(grafo), color_var)))
      if (n_colors <= 8) {
        p <- p + ggplot2::scale_color_brewer(palette = "Set2")
      } else {
        p <- p + ggplot2::scale_color_viridis_d()
      }

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(paste0("Red semántica descargada (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), type = "message", duration = 3)
    }
  )

  # ========================================
  # Análisis Semántico (OpenAI)
  # ========================================

  # Estado de embeddings
  output$estado_embeddings <- renderUI({
    if (is.null(rv$hf_embeddings)) {
      div(
        style = "color: #7f8c8d;",
        icon("circle", class = "text-muted"),
        " Sin embeddings generados"
      )
    } else {
      n_emb <- nrow(rv$hf_embeddings)
      div(
        style = "color: #2c3e50;",
        icon("check-circle", class = "text-success"),
        paste(" Embeddings generados:", n_emb, "fragmentos"),
        br(),
        tags$small(paste("Dimensiones:", ncol(rv$hf_embeddings)), style = "color: #95a5a6;")
      )
    }
  })

  # Generar embeddings
  observeEvent(input$btn_generar_embeddings, {
    # Validar API Key de OpenAI
    api_key <- input$openai_api_key
    if (is.null(api_key) || !nzchar(trimws(api_key))) {
      showNotification("Ingresa tu API Key de OpenAI en la pestaña 'Análisis IA'", type = "error", duration = 4)
      return()
    }

    # Obtener datos del reactive (prioriza IA sobre manual)
    ds <- datos_semantico()

    # Validaciones
    if (ds$n < 2) {
      fuente_msg <- if (ds$fuente == "ninguno") {
        "Necesitas al menos 2 fragmentos. Usa primero 'Análisis IA' o codifica manualmente."
      } else {
        paste("Solo tienes", ds$n, "fragmento(s). Necesitas al menos 2.")
      }
      showNotification(fuente_msg, type = "error", duration = 4)
      return()
    }

    # Verificar si los datos han cambiado (usando hash)
    current_hash <- digest::digest(ds$datos$Extracto)
    if (!is.null(rv$hf_cache_hash) && rv$hf_cache_hash == current_hash && !is.null(rv$hf_embeddings)) {
      showNotification("Los embeddings ya están actualizados", type = "message", duration = 3)
      return()
    }

    # Guardar referencia a los datos usados para embedding
    rv$datos_embedding_ref <- ds$datos

    withProgress(message = "Generando embeddings con OpenAI...", value = 0, {
      tryCatch({
        textos <- ds$datos$Extracto

        fuente_txt <- if (ds$fuente == "ia") "Análisis IA" else "codificación manual"
        incProgress(0.1, detail = paste("Usando datos de", fuente_txt, "..."))

        incProgress(0.2, detail = "Conectando con OpenAI Embeddings...")

        # Usar función de OpenAI para embeddings
        embeddings <- obtener_embeddings_openai(textos, api_key)

        incProgress(0.5, detail = "Embeddings obtenidos via OpenAI")

        incProgress(0.1, detail = "Calculando similitudes...")

        # Calcular matriz de similitud
        similitud <- calcular_similitud_coseno(embeddings)

        # Guardar resultados
        rv$hf_embeddings <- embeddings
        rv$hf_similitud <- similitud
        rv$hf_cache_hash <- current_hash

        incProgress(0.1, detail = "Completado")

        showNotification(
          paste("Embeddings generados:", nrow(embeddings), "fragmentos (OpenAI)"),
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
  })

  # Clustering semántico
  observeEvent(input$btn_clustering, {
    if (is.null(rv$hf_embeddings)) {
      showNotification("Primero genera los embeddings", type = "error", duration = 3)
      return()
    }

    withProgress(message = "Ejecutando clustering...", value = 0.3, {
      tryCatch({
        resultado <- clustering_semantico(
          embeddings_matrix = rv$hf_embeddings,
          n_clusters = input$n_clusters_semantico,
          metodo = "kmeans"
        )

        rv$semantico_clusters <- resultado

        incProgress(0.7, detail = "Completado")

        showNotification(
          paste("Clustering completado:", resultado$n_clusters, "clusters identificados"),
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
  })

  # Tabla de clustering
  output$tabla_clustering_semantico <- renderDT({
    req(rv$semantico_clusters, rv$datos_embedding_ref)

    tabla_clusters <- rv$datos_embedding_ref %>%
      mutate(
        Cluster = rv$semantico_clusters$clusters,
        Extracto_Corto = stringr::str_trunc(Extracto, 60)
      ) %>%
      select(Cluster, Codigo, Categoria, Extracto_Corto, Archivo) %>%
      arrange(Cluster, Codigo)

    datatable(
      tabla_clusters,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          info = "Mostrando _START_ a _END_ de _TOTAL_",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      ),
      rownames = FALSE,
      colnames = c("Cluster", "Código", "Categoría", "Extracto", "Archivo")
    ) %>%
      formatStyle(
        "Cluster",
        backgroundColor = styleInterval(
          seq(1, 10),
          c("#e8f4fd", "#d1e9fc", "#b9defb", "#a2d3fa", "#8bc8f9",
            "#74bdf8", "#5db2f7", "#46a7f6", "#2f9cf5", "#189ff4", "#0093f3")
        ),
        fontWeight = "bold"
      )
  })

  # Gráfico de clustering
  output$plot_clustering_semantico <- renderPlotly({
    req(rv$semantico_clusters, rv$hf_embeddings, rv$datos_embedding_ref)

    # PCA para visualización
    pca_result <- prcomp(rv$hf_embeddings, scale. = TRUE)

    plot_data <- data.frame(
      PC1 = pca_result$x[, 1],
      PC2 = pca_result$x[, 2],
      Cluster = factor(rv$semantico_clusters$clusters),
      Codigo = rv$datos_embedding_ref$Codigo,
      Extracto = stringr::str_trunc(rv$datos_embedding_ref$Extracto, 40)
    )

    p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster, text = paste("Código:", Codigo, "\n", Extracto))) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = "Clusters Semánticos (PCA)", x = "PC1", y = "PC2") +
      theme_minimal() +
      theme(legend.position = "right")

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # Detección de similitud
  observeEvent(input$btn_similitud, {
    if (is.null(rv$hf_similitud) || is.null(rv$datos_embedding_ref)) {
      showNotification("Primero genera los embeddings", type = "error", duration = 3)
      return()
    }

    withProgress(message = "Buscando fragmentos similares...", value = 0.3, {
      tryCatch({
        similares <- detectar_similares_diferente_codigo(
          tabla = rv$datos_embedding_ref,
          similitud_matrix = rv$hf_similitud,
          umbral = input$umbral_similitud
        )

        if (nrow(similares) == 0) {
          showNotification(
            "No se encontraron inconsistencias con el umbral seleccionado",
            type = "message",
            duration = 4
          )
        } else {
          showNotification(
            paste("Se encontraron", nrow(similares), "posibles inconsistencias"),
            type = "warning",
            duration = 4
          )
        }

        # Guardar para mostrar en tabla
        rv$similares_encontrados <- similares

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
  })

  # Tabla de similitud
  output$tabla_similitud_semantico <- renderDT({
    req(rv$similares_encontrados)

    if (nrow(rv$similares_encontrados) == 0) {
      return(
        datatable(
          tibble(Mensaje = "No se encontraron fragmentos similares con diferente código"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }

    tabla_mostrar <- rv$similares_encontrados %>%
      mutate(
        Fragmento1 = stringr::str_trunc(Fragmento1, 50),
        Fragmento2 = stringr::str_trunc(Fragmento2, 50)
      )

    datatable(
      tabla_mostrar,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(search = "Buscar:")
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Similitud",
        background = styleColorBar(c(0, 1), "#3498db"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle(
        "Sugerencia",
        color = styleEqual(
          c("Alta similitud - revisar codificación", "Similitud moderada - considerar unificar"),
          c("#e74c3c", "#f39c12")
        ),
        fontWeight = "bold"
      )
  })

  # Visualización 2D
  observeEvent(input$btn_visualizar, {
    if (is.null(rv$hf_embeddings)) {
      showNotification("Primero genera los embeddings", type = "error", duration = 3)
      return()
    }

    withProgress(message = "Generando visualización 2D...", value = 0.3, {
      tryCatch({
        metodo <- input$metodo_visualizacion
        embeddings_matrix <- rv$hf_embeddings
        n_obs <- nrow(embeddings_matrix)

        # Calcular coordenadas según el método
        if (metodo == "tsne" && requireNamespace("Rtsne", quietly = TRUE)) {
          set.seed(2026)
          perplexity <- min(30, floor((n_obs - 1) / 3))
          perplexity <- max(perplexity, 1)
          tsne_result <- Rtsne::Rtsne(embeddings_matrix, dims = 2, perplexity = perplexity,
                                       verbose = FALSE, max_iter = 500)
          coords <- data.frame(X = tsne_result$Y[, 1], Y = tsne_result$Y[, 2])
        } else if (metodo == "umap" && requireNamespace("umap", quietly = TRUE)) {
          set.seed(2026)
          n_neighbors <- min(15, n_obs - 1)
          umap_result <- umap::umap(embeddings_matrix, n_neighbors = n_neighbors)
          coords <- data.frame(X = umap_result$layout[, 1], Y = umap_result$layout[, 2])
        } else {
          pca_result <- prcomp(embeddings_matrix, scale. = TRUE)
          coords <- data.frame(X = pca_result$x[, 1], Y = pca_result$x[, 2])
          metodo <- "pca"
        }

        # Guardar datos de visualización
        rv$visualizacion_2d <- list(
          coords = coords,
          metodo = metodo,
          timestamp = Sys.time()
        )

        setProgress(value = 1, message = "Visualización completada")
        showNotification("Visualización 2D generada y guardada", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })

  output$plot_embeddings_2d <- renderPlotly({
    # Usar visualización guardada si existe, o requerir que se genere
    req(rv$visualizacion_2d, rv$datos_embedding_ref)

    tryCatch({
      coords <- rv$visualizacion_2d$coords
      metodo <- rv$visualizacion_2d$metodo
      tabla <- rv$datos_embedding_ref

      # Preparar datos para el gráfico
      plot_data <- coords %>%
        mutate(
          Codigo = tabla$Codigo[1:nrow(coords)],
          Extracto = stringr::str_trunc(tabla$Extracto[1:nrow(coords)], 50),
          Categoria = tabla$Categoria[1:nrow(coords)]
        )

      # Crear gráfico
      p <- ggplot(plot_data, aes(x = X, y = Y, color = Codigo, text = Extracto)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Visualización de Embeddings (", toupper(metodo), ")", sep = ""),
          x = paste(toupper(metodo), "Dimensión 1"),
          y = paste(toupper(metodo), "Dimensión 2"),
          color = "Código"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "right",
          plot.title = element_text(face = "bold", hjust = 0.5)
        )

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
  })

  # Análisis de coherencia
  observeEvent(input$btn_coherencia, {
    if (is.null(rv$hf_similitud) || is.null(rv$datos_embedding_ref)) {
      showNotification("Primero genera los embeddings", type = "error", duration = 3)
      return()
    }

    withProgress(message = "Analizando coherencia...", value = 0.3, {
      tryCatch({
        coherencia <- analizar_coherencia_codigos(
          tabla = rv$datos_embedding_ref,
          similitud_matrix = rv$hf_similitud
        )

        rv$semantico_coherencia <- coherencia

        showNotification(
          paste("Análisis de coherencia completado para", nrow(coherencia), "códigos"),
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
  })

  # Tabla de coherencia
  output$tabla_coherencia_semantico <- renderDT({
    req(rv$semantico_coherencia)

    datatable(
      rv$semantico_coherencia,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(search = "Buscar:")
      ),
      rownames = FALSE,
      colnames = c("Código", "N Fragmentos", "Coherencia Media", "Mín", "Máx", "SD", "Evaluación")
    ) %>%
      formatStyle(
        "Evaluacion",
        backgroundColor = styleEqual(
          c("Excelente", "Buena", "Moderada", "Baja - revisar", "Insuficiente (< 2 fragmentos)"),
          c("#27ae60", "#2ecc71", "#f39c12", "#e74c3c", "#95a5a6")
        ),
        color = "white",
        fontWeight = "bold"
      ) %>%
      formatRound(columns = c("Coherencia_Media", "Coherencia_Min", "Coherencia_Max", "Coherencia_SD"), digits = 3)
  })

  # Gráfico de coherencia
  output$plot_coherencia_semantico <- renderPlot({
    req(rv$semantico_coherencia)

    datos <- rv$semantico_coherencia %>%
      filter(!is.na(Coherencia_Media)) %>%
      arrange(desc(Coherencia_Media))

    if (nrow(datos) == 0) {
      return(NULL)
    }

    ggplot(datos, aes(x = reorder(Codigo, Coherencia_Media), y = Coherencia_Media, fill = Evaluacion)) +
      geom_col() +
      geom_errorbar(aes(ymin = Coherencia_Min, ymax = Coherencia_Max), width = 0.2, alpha = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c(
        "Excelente" = "#27ae60",
        "Buena" = "#2ecc71",
        "Moderada" = "#f39c12",
        "Baja - revisar" = "#e74c3c"
      )) +
      labs(
        title = "Coherencia Semántica por Código",
        x = "Código",
        y = "Coherencia Media (similitud coseno)",
        fill = "Evaluación"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5)
      ) +
      geom_hline(yintercept = 0.6, linetype = "dashed", color = "#7f8c8d", alpha = 0.7)
  })

  # ========================================
  # Red Semántica de Códigos
  # ========================================

  # Generar red semántica
  observeEvent(input$btn_generar_red, {
    if (is.null(rv$hf_embeddings) || is.null(rv$datos_embedding_ref)) {
      showNotification("Primero genera los embeddings", type = "error", duration = 3)
      return()
    }

    if (length(unique(rv$datos_embedding_ref$Codigo)) < 2) {
      showNotification("Necesitas al menos 2 códigos diferentes", type = "error", duration = 3)
      return()
    }

    withProgress(message = "Generando red semántica...", value = 0.3, {
      tryCatch({
        red <- calcular_red_semantica_codigos(
          embeddings_matrix = rv$hf_embeddings,
          tabla = rv$datos_embedding_ref,
          umbral_conexion = input$umbral_red_semantica
        )

        rv$red_semantica <- red

        incProgress(0.7, detail = "Completado")

        showNotification(
          paste("Red generada:", red$n_codigos, "códigos,", red$n_conexiones, "conexiones"),
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
  })

  # Info de la red
  output$info_red_semantica <- renderUI({
    red <- rv$red_semantica

    if (is.null(red)) {
      return(
        div(
          style = "text-align: center; padding: 20px; color: #7f8c8d;",
          icon("project-diagram", style = "font-size: 36px; margin-bottom: 10px;"),
          h5("Red Semántica de Códigos"),
          p("Configura el umbral y haz clic en 'Generar Red'")
        )
      )
    }

    div(
      style = "margin-bottom: 10px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
      fluidRow(
        column(4,
          div(style = "text-align: center;",
            h4(red$n_codigos, style = "color: #2c3e50; margin: 0;"),
            tags$small("Códigos", style = "color: #7f8c8d;")
          )
        ),
        column(4,
          div(style = "text-align: center;",
            h4(red$n_conexiones, style = "color: #3498db; margin: 0;"),
            tags$small("Conexiones", style = "color: #7f8c8d;")
          )
        ),
        column(4,
          div(style = "text-align: center;",
            h4(paste0(input$umbral_red_semantica * 100, "%"), style = "color: #27ae60; margin: 0;"),
            tags$small("Umbral", style = "color: #7f8c8d;")
          )
        )
      )
    )
  })

  # Gráfico de red semántica
  output$plot_red_semantica <- renderPlot({
    red <- rv$red_semantica
    req(red)

    grafo <- red$grafo

    # Detectar comunidades si se seleccionó
    if (input$color_red_semantica == "comunidad") {
      # Detectar comunidades con Louvain
      comunidades <- igraph::cluster_louvain(igraph::as.igraph(grafo))
      grafo <- grafo %>%
        tidygraph::activate(nodes) %>%
        tidygraph::mutate(comunidad = factor(comunidades$membership))
      color_var <- "comunidad"
      color_title <- "Comunidad"
    } else {
      color_var <- "categoria"
      color_title <- "Categoría"
    }

    # Crear el gráfico con ggraph
    set.seed(2026)

    p <- ggraph::ggraph(grafo, layout = "fr") +
      ggraph::geom_edge_link(
        ggplot2::aes(width = width, alpha = weight),
        color = "#7f8c8d",
        show.legend = FALSE
      ) +
      ggraph::geom_node_point(
        ggplot2::aes(size = size, color = .data[[color_var]]),
        alpha = 0.8
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(label = name),
        repel = TRUE,
        size = 3.5,
        color = "#2c3e50",
        fontface = "bold"
      ) +
      ggraph::scale_edge_width(range = c(0.5, 3)) +
      ggplot2::scale_size_continuous(range = c(5, 20), guide = "none") +
      ggplot2::labs(
        title = "Red Semántica de Códigos",
        subtitle = paste("Umbral de similitud:", input$umbral_red_semantica),
        color = color_title
      ) +
      ggraph::theme_graph(base_family = "sans") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5, color = "#2c3e50"),
        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "#7f8c8d"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(face = "bold", size = 10),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )

    # Paleta de colores
    n_colors <- length(unique(igraph::vertex_attr(igraph::as.igraph(grafo), color_var)))
    if (n_colors <= 8) {
      p <- p + ggplot2::scale_color_brewer(palette = "Set2")
    } else {
      p <- p + ggplot2::scale_color_viridis_d()
    }

    p
  })

  # Validación con LLM
  observeEvent(input$btn_validacion, {
    # Validar API Key de OpenAI
    api_key <- input$openai_api_key
    if (is.null(api_key) || !nzchar(trimws(api_key))) {
      showNotification("Ingresa tu API Key de OpenAI en la pestaña 'Análisis IA'", type = "error", duration = 4)
      return()
    }

    # Obtener datos del reactive (prioriza IA sobre manual)
    ds <- datos_semantico()

    if (ds$n < 1) {
      showNotification("No hay fragmentos para validar. Usa primero 'Análisis IA' o codifica manualmente.", type = "error", duration = 4)
      return()
    }

    n_validar <- min(input$n_fragmentos_validar, ds$n)

    # Seleccionar muestra aleatoria
    set.seed(Sys.time())
    indices <- sample(1:ds$n, n_validar)
    muestra <- ds$datos[indices, ]

    withProgress(message = "Validando con GPT-4.1...", value = 0.1, {
      tryCatch({
        fuente_txt <- if (ds$fuente == "ia") "Análisis IA" else "codificación manual"
        incProgress(0.2, detail = paste("Usando datos de", fuente_txt, "..."))

        incProgress(0.2, detail = "Conectando con OpenAI...")

        resultado <- validar_codificacion_llm(
          fragmentos = muestra$Extracto,
          codigos = muestra$Codigo,
          api_key = api_key
        )

        rv$semantico_validacion <- resultado

        incProgress(0.5, detail = "Completado")

        showNotification(
          "Validación completada",
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
  })

  # Resultado validación LLM
  output$resultado_validacion_llm <- renderUI({
    if (is.null(rv$semantico_validacion)) {
      return(
        div(
          style = "text-align: center; padding: 40px; color: #7f8c8d;",
          icon("robot", style = "font-size: 48px; margin-bottom: 15px;"),
          h4("Panel de Expertos Virtual"),
          p("Haz clic en 'Validar' para que un LLM evalúe la calidad de tu codificación")
        )
      )
    }

    # Formatear resultado - convertir markdown a HTML con estilos
    resultado <- rv$semantico_validacion

    # Convertir headers principales ### a h3 estilizado
    resultado <- stringr::str_replace_all(
      resultado,
      "###\\s*(.+?)\\n",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; margin-top: 0;'>\\1</h3>\n"
    )

    # Convertir headers de fragmento #### Fragmento N a tarjetas
    resultado <- stringr::str_replace_all(
      resultado,
      "####\\s*(Fragmento\\s*\\d+)\\n",
      "</div><div class='fragment-card' style='background: white; border: 1px solid #e0e0e0; border-radius: 8px; padding: 15px; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.05);'><h4 style='color: #3498db; margin: 0 0 10px 0; font-size: 14px;'><i class='fa fa-file-text-o'></i> \\1</h4>\n"
    )

    # Convertir **Texto:** y otros labels bold
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Texto:\\*\\*\\s*\"(.+?)\"",
      "<div style='background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 5px 0; font-style: italic; border-left: 3px solid #95a5a6;'><strong>Texto:</strong> \"\\1\"</div>"
    )

    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Código Asignado:\\*\\*\\s*(.+?)\\n",
      "<div style='margin: 5px 0;'><strong style='color: #7f8c8d;'>Código:</strong> <span style='background: #3498db; color: white; padding: 2px 8px; border-radius: 12px; font-size: 12px;'>\\1</span></div>\n"
    )

    # Convertir evaluaciones con colores
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Evaluación:\\*\\*\\s*Correcto",
      "<div style='margin: 8px 0;'><strong>Evaluación:</strong> <span style='background: #27ae60; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>✓ Correcto</span></div>"
    )
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Evaluación:\\*\\*\\s*Revisar",
      "<div style='margin: 8px 0;'><strong>Evaluación:</strong> <span style='background: #f39c12; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>⚠ Revisar</span></div>"
    )
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Evaluación:\\*\\*\\s*Incorrecto",
      "<div style='margin: 8px 0;'><strong>Evaluación:</strong> <span style='background: #e74c3c; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>✗ Incorrecto</span></div>"
    )

    # Convertir justificación
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Justificación:\\*\\*\\s*(.+?)\\n",
      "<div style='margin: 5px 0; color: #555;'><strong style='color: #7f8c8d;'>Justificación:</strong> \\1</div>\n"
    )

    # Convertir código alternativo
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*Código Alternativo( Sugerido)?:\\*\\*\\s*(.+?)\\n",
      "<div style='margin: 5px 0;'><strong style='color: #7f8c8d;'>Sugerencia:</strong> <em>\\2</em></div>\n"
    )

    # Limpiar números de lista (1., 2., 3.)
    resultado <- stringr::str_replace_all(resultado, "\\d+\\.\\s*<", "<")

    # Limpiar cualquier **texto** restante
    resultado <- stringr::str_replace_all(resultado, "\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>")

    # Convertir saltos de línea restantes
    resultado <- stringr::str_replace_all(resultado, "\n\n", "<br>")
    resultado <- stringr::str_replace_all(resultado, "\n", " ")

    # Cerrar la última tarjeta y limpiar div inicial vacío
    resultado <- paste0(resultado, "</div>")
    resultado <- stringr::str_replace(resultado, "^</div>", "")

    div(
      h4(icon("gavel"), " Resultado de la Validación", style = "color: #2c3e50; margin-bottom: 15px;"),
      div(
        style = "background: #f0f3f5; padding: 20px; border-radius: 10px; max-height: 500px; overflow-y: auto;",
        HTML(resultado)
      )
    )
  })

  # ========================================
  # Reporte con IA
  # ========================================

  # Variable para almacenar el reporte generado
  rv_reporte <- reactiveVal(NULL)

  # Función para preparar datos del análisis
  preparar_datos_reporte <- function() {
    datos <- list()

    # Datos de codificación
    ds <- datos_semantico()
    if (ds$n > 0) {
      datos$tiene_codificacion <- TRUE
      datos$n_fragmentos <- ds$n
      datos$fuente <- ds$fuente

      # Frecuencia de códigos
      freq_codigos <- ds$datos %>%
        group_by(Codigo) %>%
        summarise(n = n(), .groups = "drop") %>%
        arrange(desc(n))
      datos$freq_codigos <- freq_codigos

      # Categorías si existen
      if ("Categoria" %in% names(ds$datos)) {
        freq_categorias <- ds$datos %>%
          group_by(Categoria) %>%
          summarise(n = n(), .groups = "drop") %>%
          arrange(desc(n))
        datos$freq_categorias <- freq_categorias
      }

      # Ejemplos de fragmentos por código (top 3 más frecuentes)
      top_codigos <- head(freq_codigos$Codigo, 3)
      ejemplos <- list()
      for (cod in top_codigos) {
        frags <- ds$datos %>% filter(Codigo == cod) %>% pull(Extracto) %>% head(2)
        ejemplos[[cod]] <- frags
      }
      datos$ejemplos <- ejemplos
    } else {
      datos$tiene_codificacion <- FALSE
    }

    # Datos de análisis semántico
    if (!is.null(rv$semantico_coherencia)) {
      datos$tiene_coherencia <- TRUE
      datos$coherencia <- rv$semantico_coherencia
    } else {
      datos$tiene_coherencia <- FALSE
    }

    if (!is.null(rv$semantico_clusters)) {
      datos$tiene_clustering <- TRUE
      datos$n_clusters <- rv$semantico_clusters$n_clusters
    } else {
      datos$tiene_clustering <- FALSE
    }

    if (!is.null(rv$red_semantica)) {
      datos$tiene_red <- TRUE
      datos$red_n_codigos <- rv$red_semantica$n_codigos
      datos$red_n_conexiones <- rv$red_semantica$n_conexiones
    } else {
      datos$tiene_red <- FALSE
    }

    # Número de documentos
    datos$n_docs <- length(rv$docs)

    return(datos)
  }

  # Función para construir el prompt
  construir_prompt_reporte <- function(datos, idioma, estilo, secciones) {
    # Instrucciones base según idioma
    if (idioma == "es") {
      instrucciones <- paste0(
        "Eres un experto en análisis cualitativo de datos textuales. ",
        "Genera un reporte interpretativo basado en los siguientes resultados de un análisis cualitativo.\n\n"
      )

      estilo_txt <- switch(estilo,
        "academico" = "Usa un estilo académico formal, apropiado para una tesis o artículo científico. Incluye terminología metodológica precisa.",
        "tecnico" = "Usa un estilo técnico directo, apropiado para un informe de investigación. Sé conciso y preciso.",
        "divulgativo" = "Usa un estilo accesible y claro, apropiado para un público general. Evita jerga técnica excesiva."
      )
    } else {
      instrucciones <- paste0(
        "You are an expert in qualitative textual data analysis. ",
        "Generate an interpretive report based on the following results from a qualitative analysis.\n\n"
      )

      estilo_txt <- switch(estilo,
        "academico" = "Use a formal academic style, appropriate for a thesis or scientific article. Include precise methodological terminology.",
        "tecnico" = "Use a direct technical style, appropriate for a research report. Be concise and precise.",
        "divulgativo" = "Use an accessible and clear style, appropriate for a general audience. Avoid excessive technical jargon."
      )
    }

    # Construir información de datos
    info_datos <- ""

    if (datos$tiene_codificacion) {
      if (idioma == "es") {
        info_datos <- paste0(info_datos,
          "## DATOS DE CODIFICACIÓN\n",
          "- Total de fragmentos codificados: ", datos$n_fragmentos, "\n",
          "- Fuente de datos: ", ifelse(datos$fuente == "ia", "Análisis IA automático", "Codificación manual"), "\n",
          "- Número de documentos analizados: ", datos$n_docs, "\n\n"
        )

        # Frecuencia de códigos
        info_datos <- paste0(info_datos, "## FRECUENCIA DE CÓDIGOS\n")
        for (i in 1:min(nrow(datos$freq_codigos), 10)) {
          info_datos <- paste0(info_datos,
            "- ", datos$freq_codigos$Codigo[i], ": ", datos$freq_codigos$n[i], " fragmentos\n"
          )
        }
        info_datos <- paste0(info_datos, "\n")

        # Ejemplos
        if (length(datos$ejemplos) > 0) {
          info_datos <- paste0(info_datos, "## EJEMPLOS DE FRAGMENTOS\n")
          for (cod in names(datos$ejemplos)) {
            info_datos <- paste0(info_datos, "### Código: ", cod, "\n")
            for (frag in datos$ejemplos[[cod]]) {
              info_datos <- paste0(info_datos, "- \"", stringr::str_trunc(frag, 150), "\"\n")
            }
          }
          info_datos <- paste0(info_datos, "\n")
        }
      } else {
        info_datos <- paste0(info_datos,
          "## CODING DATA\n",
          "- Total coded fragments: ", datos$n_fragmentos, "\n",
          "- Data source: ", ifelse(datos$fuente == "ia", "Automatic AI analysis", "Manual coding"), "\n",
          "- Number of documents analyzed: ", datos$n_docs, "\n\n"
        )

        info_datos <- paste0(info_datos, "## CODE FREQUENCY\n")
        for (i in 1:min(nrow(datos$freq_codigos), 10)) {
          info_datos <- paste0(info_datos,
            "- ", datos$freq_codigos$Codigo[i], ": ", datos$freq_codigos$n[i], " fragments\n"
          )
        }
        info_datos <- paste0(info_datos, "\n")
      }
    }

    # Coherencia
    if (datos$tiene_coherencia && "coherencia" %in% secciones) {
      if (idioma == "es") {
        info_datos <- paste0(info_datos, "## COHERENCIA SEMÁNTICA DE CÓDIGOS\n")
        for (i in 1:min(nrow(datos$coherencia), 8)) {
          info_datos <- paste0(info_datos,
            "- ", datos$coherencia$Codigo[i], ": coherencia = ", datos$coherencia$Coherencia_Media[i],
            " (", datos$coherencia$Evaluacion[i], ")\n"
          )
        }
        info_datos <- paste0(info_datos, "\n")
      } else {
        info_datos <- paste0(info_datos, "## SEMANTIC COHERENCE OF CODES\n")
        for (i in 1:min(nrow(datos$coherencia), 8)) {
          info_datos <- paste0(info_datos,
            "- ", datos$coherencia$Codigo[i], ": coherence = ", datos$coherencia$Coherencia_Media[i],
            " (", datos$coherencia$Evaluacion[i], ")\n"
          )
        }
        info_datos <- paste0(info_datos, "\n")
      }
    }

    # Clustering
    if (datos$tiene_clustering && "clustering" %in% secciones) {
      if (idioma == "es") {
        info_datos <- paste0(info_datos,
          "## CLUSTERING SEMÁNTICO\n",
          "- Se identificaron ", datos$n_clusters, " clusters de fragmentos semánticamente similares.\n\n"
        )
      } else {
        info_datos <- paste0(info_datos,
          "## SEMANTIC CLUSTERING\n",
          "- ", datos$n_clusters, " clusters of semantically similar fragments were identified.\n\n"
        )
      }
    }

    # Red semántica
    if (datos$tiene_red && "red" %in% secciones) {
      if (idioma == "es") {
        info_datos <- paste0(info_datos,
          "## RED SEMÁNTICA DE CÓDIGOS\n",
          "- Códigos en la red: ", datos$red_n_codigos, "\n",
          "- Conexiones detectadas: ", datos$red_n_conexiones, "\n\n"
        )
      } else {
        info_datos <- paste0(info_datos,
          "## SEMANTIC NETWORK OF CODES\n",
          "- Codes in network: ", datos$red_n_codigos, "\n",
          "- Connections detected: ", datos$red_n_conexiones, "\n\n"
        )
      }
    }

    # Instrucciones finales - Formato artículo científico
    if (idioma == "es") {
      tarea <- paste0(
        "---\n\n",
        "## TU TAREA:\n",
        estilo_txt, "\n\n",
        "Genera un reporte con formato de artículo científico que incluya EXACTAMENTE estas dos secciones:\n\n",
        "**Análisis de datos**\n",
        "Escribe 1-2 párrafos describiendo:\n",
        "- El enfoque metodológico del análisis cualitativo realizado\n",
        "- El software utilizado (RCualiText) para el análisis\n",
        "- Los tipos de análisis aplicados: "
      )

      # Agregar análisis realizados
      analisis_list <- c()
      if ("codificacion" %in% secciones) analisis_list <- c(analisis_list, "codificación temática")
      if ("frecuencias" %in% secciones) analisis_list <- c(analisis_list, "análisis de frecuencias")
      if (datos$tiene_clustering && "clustering" %in% secciones) analisis_list <- c(analisis_list, "clustering semántico")
      if (datos$tiene_coherencia && "coherencia" %in% secciones) analisis_list <- c(analisis_list, "análisis de coherencia de códigos")
      if (datos$tiene_red && "red" %in% secciones) analisis_list <- c(analisis_list, "análisis de red semántica")

      tarea <- paste0(tarea, paste(analisis_list, collapse = ", "), ".\n")
      tarea <- paste0(tarea, "- Menciona que se utilizaron embeddings semánticos para análisis avanzados (si aplica)\n\n")

      tarea <- paste0(tarea,
        "**Resultados**\n",
        "Escribe varios párrafos interpretativos que incluyan:\n\n"
      )

      if ("codificacion" %in% secciones || "frecuencias" %in% secciones) {
        tarea <- paste0(tarea,
          "1. **Descripción de la codificación**: Menciona el total de fragmentos codificados, ",
          "los códigos más frecuentes y su significado. Interpreta qué revelan las frecuencias sobre el fenómeno estudiado. ",
          "Al terminar el párrafo, deja una línea en blanco y escribe en una línea aparte SOLO:\n\n",
          "[Insertar Figura de Distribución de Códigos]\n\n"
        )
      }

      if (datos$tiene_clustering && "clustering" %in% secciones) {
        tarea <- paste0(tarea,
          "2. **Patrones semánticos**: Describe los ", datos$n_clusters, " clusters identificados, ",
          "qué códigos se agrupan juntos y qué significa esta agrupación temática. ",
          "Al terminar el párrafo, deja una línea en blanco y escribe en una línea aparte SOLO:\n\n",
          "[Insertar Figura de Clustering Semántico]\n\n"
        )
      }

      if (datos$tiene_coherencia && "coherencia" %in% secciones) {
        tarea <- paste0(tarea,
          "3. **Coherencia interna de códigos**: Discute qué códigos tienen mayor coherencia semántica ",
          "(fragmentos más homogéneos) y cuáles tienen menor coherencia (posiblemente requieren revisión). ",
          "Al terminar el párrafo, deja una línea en blanco y escribe en una línea aparte SOLO:\n\n",
          "[Insertar Figura de Coherencia de Códigos]\n\n"
        )
      }

      if (datos$tiene_red && "red" %in% secciones) {
        tarea <- paste0(tarea,
          "4. **Relaciones entre códigos**: Interpreta la red semántica con ", datos$red_n_codigos,
          " códigos y ", datos$red_n_conexiones, " conexiones. Describe qué códigos están más relacionados, ",
          "qué comunidades temáticas emergen y qué códigos aparecen aislados. ",
          "Al terminar el párrafo, deja una línea en blanco y escribe en una línea aparte SOLO:\n\n",
          "[Insertar Figura de Red Semántica de Códigos]\n\n"
        )
      }

      if ("hallazgos" %in% secciones) {
        tarea <- paste0(tarea,
          "5. **Síntesis de hallazgos**: Resume los descubrimientos más relevantes del análisis, ",
          "integrando los diferentes tipos de evidencia (frecuencias, clusters, coherencia, red).\n\n"
        )
      }

      if ("limitaciones" %in% secciones) {
        tarea <- paste0(tarea,
          "6. **Limitaciones**: Menciona brevemente las limitaciones del análisis (tamaño de muestra, ",
          "naturaleza de los datos, limitaciones del análisis asistido por IA).\n\n"
        )
      }

      tarea <- paste0(tarea,
        "IMPORTANTE:\n",
        "- Escribe de forma fluida y académica, NO como lista de puntos\n",
        "- Integra TODAS las cifras numéricas proporcionadas en los datos\n",
        "- CADA etiqueta [Insertar Figura...] DEBE estar SOLA en su propia línea, separada del texto\n",
        "- NO escribas las etiquetas de figura dentro del párrafo, siempre en línea aparte\n",
        "- Extensión: 500-800 palabras\n"
      )

    } else {
      # English version
      tarea <- paste0(
        "---\n\n",
        "## YOUR TASK:\n",
        estilo_txt, "\n\n",
        "Generate a report in scientific article format including EXACTLY these two sections:\n\n",
        "**Data Analysis**\n",
        "Write 1-2 paragraphs describing:\n",
        "- The methodological approach of the qualitative analysis\n",
        "- The software used (RCualiText) for analysis\n",
        "- Types of analysis applied: "
      )

      analisis_list <- c()
      if ("codificacion" %in% secciones) analisis_list <- c(analisis_list, "thematic coding")
      if ("frecuencias" %in% secciones) analisis_list <- c(analisis_list, "frequency analysis")
      if (datos$tiene_clustering && "clustering" %in% secciones) analisis_list <- c(analisis_list, "semantic clustering")
      if (datos$tiene_coherencia && "coherencia" %in% secciones) analisis_list <- c(analisis_list, "code coherence analysis")
      if (datos$tiene_red && "red" %in% secciones) analisis_list <- c(analisis_list, "semantic network analysis")

      tarea <- paste0(tarea, paste(analisis_list, collapse = ", "), ".\n")
      tarea <- paste0(tarea, "- Mention that semantic embeddings were used for advanced analysis (if applicable)\n\n")

      tarea <- paste0(tarea,
        "**Results**\n",
        "Write several interpretive paragraphs including:\n\n"
      )

      if ("codificacion" %in% secciones || "frecuencias" %in% secciones) {
        tarea <- paste0(tarea,
          "1. **Coding description**: Mention total coded fragments, most frequent codes and their meaning. ",
          "Interpret what frequencies reveal about the studied phenomenon. ",
          "After the paragraph, leave a blank line and write on a separate line ONLY:\n\n",
          "[Insert Code Distribution Figure]\n\n"
        )
      }

      if (datos$tiene_clustering && "clustering" %in% secciones) {
        tarea <- paste0(tarea,
          "2. **Semantic patterns**: Describe the ", datos$n_clusters, " identified clusters, ",
          "which codes group together and what this thematic grouping means. ",
          "After the paragraph, leave a blank line and write on a separate line ONLY:\n\n",
          "[Insert Semantic Clustering Figure]\n\n"
        )
      }

      if (datos$tiene_coherencia && "coherencia" %in% secciones) {
        tarea <- paste0(tarea,
          "3. **Internal code coherence**: Discuss which codes have higher semantic coherence ",
          "(more homogeneous fragments) and which have lower coherence (possibly need review). ",
          "After the paragraph, leave a blank line and write on a separate line ONLY:\n\n",
          "[Insert Code Coherence Figure]\n\n"
        )
      }

      if (datos$tiene_red && "red" %in% secciones) {
        tarea <- paste0(tarea,
          "4. **Relationships between codes**: Interpret the semantic network with ", datos$red_n_codigos,
          " codes and ", datos$red_n_conexiones, " connections. Describe which codes are most related, ",
          "what thematic communities emerge and which codes appear isolated. ",
          "After the paragraph, leave a blank line and write on a separate line ONLY:\n\n",
          "[Insert Semantic Network Figure]\n\n"
        )
      }

      if ("hallazgos" %in% secciones) {
        tarea <- paste0(tarea,
          "5. **Findings synthesis**: Summarize the most relevant discoveries, ",
          "integrating different types of evidence (frequencies, clusters, coherence, network).\n\n"
        )
      }

      if ("limitaciones" %in% secciones) {
        tarea <- paste0(tarea,
          "6. **Limitations**: Briefly mention analysis limitations (sample size, ",
          "data nature, AI-assisted analysis limitations).\n\n"
        )
      }

      tarea <- paste0(tarea,
        "IMPORTANT:\n",
        "- Write fluently and academically, NOT as bullet points\n",
        "- Integrate ALL numerical figures provided in the data\n",
        "- EACH [Insert Figure...] tag MUST be ALONE on its own line, separated from text\n",
        "- DO NOT write figure tags inside the paragraph, always on a separate line\n",
        "- Length: 500-800 words\n"
      )
    }

    prompt_final <- paste0(instrucciones, info_datos, tarea)
    return(prompt_final)
  }

  # Observer para generar reporte
  observeEvent(input$btn_generar_reporte, {
    # Validar API Key de OpenAI
    api_key <- input$openai_api_key
    if (is.null(api_key) || !nzchar(trimws(api_key))) {
      showNotification("Ingresa tu API Key de OpenAI en la pestaña 'Análisis IA'", type = "error", duration = 4)
      return()
    }

    # Validar que hay datos
    ds <- datos_semantico()
    if (ds$n < 1) {
      showNotification("No hay fragmentos codificados. Realiza primero un análisis.", type = "error", duration = 4)
      return()
    }

    # Preparar datos
    datos <- preparar_datos_reporte()

    # Construir prompt
    prompt <- construir_prompt_reporte(
      datos = datos,
      idioma = input$idioma_reporte,
      estilo = input$estilo_reporte,
      secciones = input$secciones_reporte
    )

    withProgress(message = "Generando reporte con GPT-4.1...", value = 0.1, {
      incProgress(0.2, detail = "Conectando con OpenAI...")

      resultado <- tryCatch({
        call_openai_api(
          prompt = prompt,
          api_key = api_key,
          system_prompt = "Eres un experto en análisis cualitativo de datos textuales y redacción académica."
        )
      }, error = function(e) {
        showNotification(
          paste("Error OpenAI:", e$message),
          type = "error",
          duration = 6
        )
        NULL
      })

      incProgress(0.5, detail = "Procesando respuesta...")

      # Verificar resultado
      if (is.null(resultado) || !nzchar(resultado)) {
        showNotification(
          "No se pudo generar el reporte. Verifica tu API Key.",
          type = "error",
          duration = 6
        )
        return()
      }

      # Guardar resultado
      rv_reporte(resultado)

      incProgress(0.2, detail = "Completado")

      showNotification("Reporte generado exitosamente", type = "message", duration = 4)
    })
  })

  # Output del reporte
  output$reporte_ia_output <- renderUI({
    reporte <- rv_reporte()

    if (is.null(reporte)) {
      return(
        div(
          style = "text-align: center; padding: 60px; color: #7f8c8d;",
          icon("file-alt", style = "font-size: 64px; margin-bottom: 20px;"),
          h4("Reporte con IA"),
          p("Configura las opciones y haz clic en 'Generar Reporte'"),
          p("El reporte se generará automáticamente basándose en tus análisis.",
            style = "font-size: 13px;")
        )
      )
    }

    # Formatear el reporte
    reporte_html <- reporte %>%
      stringr::str_replace_all("\n\n", "</p><p>") %>%
      stringr::str_replace_all("\n", "<br>") %>%
      stringr::str_replace_all("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>") %>%
      stringr::str_replace_all("\\*(.+?)\\*", "<em>\\1</em>")

    div(
      div(
        style = "margin-bottom: 15px; padding: 10px; background: #d4edda; border-radius: 5px; border-left: 4px solid #28a745;",
        icon("check-circle", style = "color: #28a745;"),
        tags$span(" Reporte generado exitosamente", style = "color: #155724; font-weight: 500;")
      ),
      div(
        style = "background: #ffffff; padding: 25px; border-radius: 10px; border: 1px solid #e0e4e8; max-height: 600px; overflow-y: auto; line-height: 1.8; text-align: justify;",
        HTML(paste0("<p>", reporte_html, "</p>"))
      )
    )
  })

  # Descarga del reporte en Word
  output$btn_descargar_reporte <- downloadHandler(
    filename = function() {
      paste0("Reporte_Cualitativo_", Sys.Date(), ".docx")
    },
    content = function(file) {
      reporte <- rv_reporte()

      if (!is.null(reporte) && nzchar(reporte)) {
        # Crear documento Word con officer
        doc <- officer::read_docx()

        # Título
        doc <- doc %>%
          officer::body_add_par("Reporte de Análisis Cualitativo", style = "heading 1") %>%
          officer::body_add_par(paste("Generado:", format(Sys.time(), "%d/%m/%Y %H:%M")), style = "Normal") %>%
          officer::body_add_par("", style = "Normal")

        # Procesar el contenido del reporte
        # Primero normalizar saltos de línea
        reporte_limpio <- gsub("\r\n", "\n", reporte)

        # Dividir por líneas individuales para detectar etiquetas de figura
        lineas <- strsplit(reporte_limpio, "\n")[[1]]

        buffer_parrafo <- c()  # Acumular líneas de un mismo párrafo

        procesar_buffer <- function(doc, buffer) {
          if (length(buffer) == 0) return(doc)
          texto <- paste(buffer, collapse = " ")
          texto <- trimws(texto)
          if (nchar(texto) == 0) return(doc)

          # Detectar si es un título (empieza con ** o #)
          if (grepl("^\\*\\*", texto) || grepl("^#", texto)) {
            titulo <- gsub("^\\*\\*|\\*\\*$", "", texto)
            titulo <- gsub("^#+\\s*", "", titulo)
            doc <- doc %>%
              officer::body_add_par(titulo, style = "heading 2")
          } else {
            # Párrafo normal - limpiar markdown
            texto <- gsub("\\*\\*(.+?)\\*\\*", "\\1", texto)
            texto <- gsub("\\*(.+?)\\*", "\\1", texto)
            doc <- doc %>%
              officer::body_add_par(texto, style = "Normal")
          }
          return(doc)
        }

        for (linea in lineas) {
          linea_trim <- trimws(linea)

          # Detectar etiquetas de figura [Insertar Figura...]
          if (grepl("^\\[Insertar\\s+Figura", linea_trim, ignore.case = TRUE)) {
            # Primero procesar el buffer acumulado
            doc <- procesar_buffer(doc, buffer_parrafo)
            buffer_parrafo <- c()

            # Agregar la etiqueta de figura como párrafo separado con estilo especial
            doc <- doc %>%
              officer::body_add_par("", style = "Normal") %>%  # Línea en blanco antes
              officer::body_add_par(linea_trim, style = "Normal") %>%
              officer::body_add_par("", style = "Normal")      # Línea en blanco después

          } else if (nchar(linea_trim) == 0) {
            # Línea vacía = fin de párrafo
            doc <- procesar_buffer(doc, buffer_parrafo)
            buffer_parrafo <- c()

          } else {
            # Acumular línea al buffer del párrafo actual
            buffer_parrafo <- c(buffer_parrafo, linea_trim)
          }
        }

        # Procesar cualquier texto restante en el buffer
        doc <- procesar_buffer(doc, buffer_parrafo)

        # Agregar pie de página
        doc <- doc %>%
          officer::body_add_par("", style = "Normal") %>%
          officer::body_add_par("---", style = "Normal") %>%
          officer::body_add_par("Generado con RCualiText v2.4 - Análisis Cualitativo con IA", style = "Normal")

        # Guardar documento
        print(doc, target = file)

        showNotification("Reporte Word descargado", type = "message", duration = 3)
      } else {
        # Si no hay reporte, crear documento vacío con mensaje
        doc <- officer::read_docx() %>%
          officer::body_add_par("No hay reporte generado", style = "Normal") %>%
          officer::body_add_par("Por favor, genera un reporte primero usando el botón 'Generar Reporte'.", style = "Normal")
        print(doc, target = file)
      }
    }
  )

  # Inicializar variables reactivas para similares encontrados
  observe({
    if (is.null(rv$similares_encontrados)) {
      rv$similares_encontrados <- tibble()
    }
  })
}

# ========================================
# Ejecutar App
# ========================================
shinyApp(ui, server)
