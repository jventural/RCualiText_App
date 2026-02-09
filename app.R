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
library(googlesheets4)
# para análisis semántico
library(digest)  # Para cache hash
options(shiny.maxRequestSize = 50 * 1024^2)

# ========================================
# Configuración de registro de usuarios (Google Sheets)
# ========================================
google_sheets_id <- "16Zz3wYL-OZ7r2y5sfxz-rgA1mF3MT37yavZgnLaZM7Y"
google_credentials_json <- "personal-2025-470111-36843658e199.json"

# Autenticación con service account
tryCatch({
  googlesheets4::gs4_auth(path = google_credentials_json)
  message("Google Sheets autenticado correctamente")
}, error = function(e) {
  message("Error autenticando Google Sheets: ", e[["message"]])
  googlesheets4::gs4_deauth()
})

# Función para guardar registro de usuario en Google Sheets
guardar_registro <- function(nombre, correo, universidad, residencia) {
  nueva_fila <- data.frame(
    fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    nombre = nombre,
    correo = correo,
    universidad = universidad,
    residencia = residencia,
    stringsAsFactors = FALSE
  )
  tryCatch({
    googlesheets4::sheet_append(google_sheets_id, nueva_fila, sheet = 1)
    message("Registro guardado en Google Sheets: ", correo)
    TRUE
  }, error = function(e) {
    message("Error guardando en Google Sheets: ", e[["message"]])
    message("Datos del registro - Nombre: ", nombre, ", Correo: ", correo,
            ", Universidad: ", universidad, ", Residencia: ", residencia)
    FALSE
  })
}

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
    "Format not supported"
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
aplicar_resaltado_multiple <- function(texto_original, fragmentos_df, codes_label = "Codes") {
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
      "title='", codes_label, ": ", codigos_aplicados, "' ",
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
plot_codigos <- function(df, fill = TRUE, code_colors = NULL, labels = list(freq = "Frequency", codes = "Codes", cat = "Category")) {
  # Limpiar datos de categorías vacías o NA
  sin_cat_label <- if (!is.null(labels$sin_cat)) labels$sin_cat else "Uncategorized"
  df <- df %>%
    mutate(Categoria = case_when(
      is.na(Categoria) | Categoria == "" ~ sin_cat_label,
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
      hovertemplate = paste0("<b>%{y}</b><br>", labels$freq, ": %{x}<extra></extra>")
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
        hovertemplate = paste0("<b>%{y}</b><br>", labels$freq, ": %{x}<extra></extra>")
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
        hovertemplate = paste0("<b>%{y}</b><br>", labels$freq, ": %{x}<extra></extra>")
      )
    }
  }

  # Layout común
  p <- p %>%
    plotly::layout(
      xaxis = list(
        title = list(text = labels$freq, font = list(size = 12, family = "sans-serif")),
        tickfont = list(size = 10)
      ),
      yaxis = list(
        title = list(text = labels$codes, font = list(size = 12, family = "sans-serif")),
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
plot_codigos_ggplot <- function(df, fill = TRUE, code_colors = NULL, labels = list(freq = "Frequency", codes = "Codes", cat = "Category", code = "Code")) {
  sin_cat_label <- if (!is.null(labels$sin_cat)) labels$sin_cat else "Uncategorized"
  df <- df %>%
    mutate(Categoria = case_when(
      is.na(Categoria) | Categoria == "" ~ sin_cat_label,
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
      labs(x = labels$codes, y = labels$freq, fill = labels$cat)
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
      labs(x = labels$codes, y = labels$freq, fill = labels$code)
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

plot_network_and_centrality <- function(df, code_colors = NULL, labels = list(code = "Code", centrality = "Centrality (z-score)")) {
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
      scale_fill_manual(name=labels$code, values=code_colors)
      else
        scale_fill_brewer(name=labels$code, palette="Set3") } +
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
    labs(y = labels$centrality, x = labels$code) +
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
detectar_similares_diferente_codigo <- function(tabla, similitud_matrix, umbral = 0.8, labels = list(alta = "High similarity - review coding", moderada = "Moderate similarity - consider merging")) {
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
            Sugerencia = ifelse(sim > 0.9, labels$alta, labels$moderada)
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
          mean(valores) >= 0.8 ~ "excellent",
          mean(valores) >= 0.6 ~ "good",
          mean(valores) >= 0.4 ~ "moderate",
          TRUE ~ "low_review"
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
        Evaluacion = "insufficient"
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
plot_embeddings_semantico <- function(embeddings_matrix, tabla, metodo = "pca", labels = list(title_fn = function(m) paste0("Embedding Visualization (", toupper(m), ")"), dim1_fn = function(m) paste(toupper(m), "Dimension 1"), dim2_fn = function(m) paste(toupper(m), "Dimension 2"), code = "Code")) {
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
      title = labels$title_fn(metodo),
      x = labels$dim1_fn(metodo),
      y = labels$dim2_fn(metodo),
      color = labels$code
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

  return(p)
}

# Función para validar codificación con LLM (OpenAI)
validar_codificacion_llm <- function(fragmentos, codigos, api_key, lang = "en") {
  if (is.null(fragmentos) || length(fragmentos) == 0) {
    stop(if (lang == "es") "No hay fragmentos para validar" else "No fragments to validate")
  }

  if (is.null(api_key) || !nzchar(api_key)) {
    stop(if (lang == "es") "API Key de OpenAI no proporcionada" else "OpenAI API Key not provided")
  }

  # Preparar prompt para validación
  frag_label <- if (lang == "es") "Fragmento" else "Fragment"
  code_label <- if (lang == "es") "Código" else "Code"
  fragmentos_texto <- paste(
    sapply(seq_along(fragmentos), function(i) {
      paste0(frag_label, " ", i, " [", code_label, ": ", codigos[i], "]: \"", fragmentos[i], "\"")
    }),
    collapse = "\n"
  )

  if (lang == "es") {
    prompt <- paste0(
      "Actúa como un panel de 3 expertos en análisis cualitativo. Evalúa si los siguientes fragmentos están correctamente codificados:\n\n",
      fragmentos_texto,
      "\n\nPara cada fragmento, proporciona:\n",
      "1. Evaluación (Correcto/Revisar/Incorrecto)\n",
      "2. Justificación breve\n",
      "3. Código alternativo sugerido (si aplica)\n\n",
      "Responde en formato estructurado."
    )
  } else {
    prompt <- paste0(
      "Act as a panel of 3 qualitative analysis experts. Evaluate whether the following fragments are correctly coded:\n\n",
      fragmentos_texto,
      "\n\nFor each fragment, provide:\n",
      "1. Evaluation (Correct/Review/Incorrect)\n",
      "2. Brief justification\n",
      "3. Suggested alternative code (if applicable)\n\n",
      "Respond in structured format."
    )
  }

  # Llamar a OpenAI
  resultado <- call_openai_api(prompt, api_key)

  if (is.null(resultado) || !nzchar(resultado)) {
    stop(if (lang == "es") "No se recibió respuesta de OpenAI" else "No response received from OpenAI")
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
    system_prompt <- "You are an expert in qualitative textual data analysis and thematic coding."
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
# Internationalization (i18n) - Embedded translations for deployment compatibility
# ========================================
translations <- jsonlite::fromJSON(r"--(
{
  "en": {
    "sidebar": {
      "documento": "Document",
      "codigos": "Codes",
      "categorias": "Categories",
      "extractos": "Extracts",
      "analisis": "Analysis",
      "analisis_ia": "AI Analysis (optional)",
      "analisis_semantico": "Semantic Analysis (experimental)",
      "reporte_ia": "AI Report",
      "proyecto": "Project",
      "citar": "Cite",
      "ayuda": "Help"
    },
    "documento": {
      "panel_control": "Control Panel",
      "cargar_documentos": "Upload Documents",
      "examinar": "Browse...",
      "ningun_archivo": "No file selected",
      "modo_trabajo": "Work Mode",
      "seleccionar": "Select",
      "deseleccionar": "Deselect",
      "codigo_aplicar": "Code to Apply",
      "modo_acumulativo": "Accumulative Mode",
      "modo_acumulativo_help": "Allows applying multiple codes to the same fragment",
      "navegacion": "Navigation",
      "anterior": "Previous",
      "siguiente": "Next",
      "acciones": "Actions",
      "limpiar": "Clear",
      "ayuda_btn": "Help",
      "visor_documento": "Document Viewer",
      "formato_no_soportado": "Format not supported"
    },
    "codigos": {
      "gestion_codigos": "Code Management",
      "nombre_codigo": "Code Name",
      "placeholder_codigo": "E.g.: Positive emotions",
      "color_codigo": "Code Color",
      "guardar": "Save",
      "eliminar": "Delete",
      "lista_codigos": "Code List"
    },
    "categorias": {
      "gestion_categorias": "Category Management",
      "nombre_categoria": "Category Name",
      "placeholder_categoria": "E.g.: Emotional aspects",
      "codigos_asociados": "Associated Codes",
      "placeholder_codigos": "Select codes...",
      "guardar": "Save",
      "eliminar": "Delete",
      "categorias_definidas": "Defined Categories",
      "sin_categoria": "Uncategorized"
    },
    "extractos": {
      "gestion_extractos": "Coded Extract Management",
      "herramientas": "Management Tools",
      "exportar_xlsx": "Export XLSX",
      "eliminar_seleccionado": "Delete Selected",
      "limpiar_todo": "Clear All",
      "guia_resaltados": "Highlight Guide",
      "visualizacion": "Visualization",
      "gradientes_multiples": "Gradients indicate multiple codes",
      "hover_codigos": "Hover shows applied codes",
      "cada_fila": "Each row = one code per fragment",
      "edicion": "Editing",
      "modo_deseleccionar_eliminar": "Deselect mode to remove",
      "seleccion_multiple": "Multiple selection available",
      "exportacion_excel": "Full export to Excel"
    },
    "analisis": {
      "configuracion": "Settings",
      "opciones_visuales": "Visual Options",
      "colorear_categoria": "Color by Category",
      "config_descarga": "Download Settings",
      "ancho_pulg": "Width (in)",
      "alto_pulg": "Height (in)",
      "resolucion_dpi": "Resolution (DPI)",
      "config_aplicada": "Configuration applied to both charts",
      "distribucion_codigos": "Code Distribution",
      "exportar": "Export",
      "distribucion_jpg": "Distribution (JPG)",
      "exportar_red": "Export Network",
      "red_coocurrencia_jpg": "Co-occurrence Network (JPG)",
      "red_coocurrencia_centralidad": "Co-occurrence Network and Centrality Analysis"
    },
    "analisis_ia": {
      "config_analisis_ia": "AI Analysis Settings",
      "config_openai": "OpenAI Settings",
      "api_key_openai": "OpenAI API Key",
      "api_key_help": "Uses the GPT-4.1 model from OpenAI. Get your API Key at platform.openai.com",
      "api_key_requerida": "The analysis requires a valid OpenAI API Key.",
      "diccionario_codigos": "Code Dictionary",
      "cargar_diccionario": "Upload Dictionary",
      "archivo_csv_xlsx": ".csv or .xlsx file",
      "columnas_requeridas": "Must have columns: Category, Code, Definition",
      "ejecutar_analisis": "Run AI Analysis",
      "descargar_resultados": "Download Results (.xlsx)",
      "resultados_help": "Results will be shown below. Download the table in Excel with the button above.",
      "resultados_analisis_ia": "AI Analysis Results",
      "instrucciones": "Instructions",
      "instruccion_1": "1. Make sure you have documents uploaded in the 'Document' tab",
      "instruccion_2": "2. Enter your OpenAI API Key",
      "instruccion_3": "3. Upload a code dictionary with columns: Category, Code, Definition",
      "instruccion_4": "4. Run the analysis and review the results",
      "instruccion_5": "5. If satisfied, integrate the results into your manual analysis",
      "descargar_tabla_excel": "Download Table (Excel)",
      "visualizacion_resultados": "AI Results Visualization",
      "distribucion_codigos_ia": "Code Distribution",
      "fragmentos_categoria": "Fragments by Category"
    },
    "analisis_semantico": {
      "config_analisis_semantico": "Semantic Analysis Settings",
      "configuracion": "Settings",
      "modulo_info": "This module uses the OpenAI API to generate embeddings and semantic analysis.",
      "modelo_embeddings": "Embedding model: text-embedding-3-small",
      "api_key_info": "Enter your OpenAI API Key in the 'AI Analysis' tab",
      "generar_embeddings": "Generate Embeddings",
      "generar_embeddings_help": "Generates vector representations of coded fragments using OpenAI",
      "herramientas_analisis": "Semantic Analysis Tools",
      "requisitos": "Requirements",
      "req_1": "1. Have coded fragments (use 'AI Analysis' or code manually)",
      "req_2": "2. Generate embeddings (uses internal tokens automatically)",
      "req_3": "3. Explore the semantic analysis tools",
      "clustering_semantico": "Semantic Clustering",
      "clustering_desc": "Automatically groups similar fragments",
      "num_clusters": "Number of clusters",
      "ejecutar_clustering": "Run Clustering",
      "deteccion_similitud": "Similarity Detection",
      "similitud_desc": "Finds similar fragments with different codes",
      "umbral_similitud": "Similarity threshold",
      "detectar_similares": "Detect Similar",
      "visualizacion_2d": "2D Visualization",
      "visualizacion_desc": "Visualizes the semantic distribution of fragments",
      "metodo_reduccion": "Reduction method",
      "visualizar": "Visualize",
      "analisis_coherencia": "Coherence Analysis",
      "coherencia_desc": "Evaluates the semantic homogeneity of each code",
      "analizar_coherencia": "Analyze Coherence",
      "validacion_llm": "LLM Validation (Virtual Expert Panel)",
      "validacion_desc": "A language model evaluates the quality of your coding",
      "fragmentos_validar": "Fragments to validate (sample)",
      "validar": "Validate",
      "config_descarga_figuras": "Figure Download Settings",
      "ajusta_dimensiones": "Adjust dimensions before downloading",
      "resultados_analisis_semantico": "Semantic Analysis Results",
      "tab_clustering": "Clustering",
      "tab_similitud": "Similarity",
      "tab_visualizacion": "Visualization",
      "tab_coherencia": "Coherence",
      "tab_red_semantica": "Semantic Network",
      "tab_validacion": "LLM Validation",
      "tabla_excel": "Table Excel",
      "figura_png": "Figure PNG",
      "config_red": "Settings",
      "umbral_conexion": "Connection threshold (similarity)",
      "umbral_conexion_help": "Codes with similarity above the threshold are connected",
      "colorear_por": "Color by",
      "categoria_opt": "Category",
      "comunidad_opt": "Community (detected)",
      "generar_red": "Generate Network",
      "descargar_png": "Download PNG",
      "sin_embeddings": "No embeddings generated",
      "embeddings_generados": "Embeddings generated: {n} fragments",
      "dimensiones": "Dimensions: {d}"
    },
    "reporte": {
      "config_reporte": "Report Settings",
      "informacion": "Information",
      "info_desc": "Generates an automatic interpretive report based on the analyses performed.",
      "info_modelo": "Uses the GPT-4.1 model from OpenAI to generate interpretive reports.",
      "idioma_reporte": "Report language",
      "estilo_redaccion": "Writing style",
      "academico": "Academic (thesis/article)",
      "tecnico": "Technical (report)",
      "divulgativo": "Popular (general)",
      "secciones_incluir": "Sections to include",
      "sec_codificacion": "Coding summary",
      "sec_frecuencias": "Frequency analysis",
      "sec_clustering": "Semantic clustering",
      "sec_coherencia": "Code coherence",
      "sec_red": "Semantic network",
      "sec_hallazgos": "Key findings",
      "sec_limitaciones": "Limitations",
      "requisitos": "Requirements",
      "req_fragmentos": "Have coded fragments (manual or AI)",
      "req_embeddings": "For semantic analysis: have generated embeddings",
      "generar_reporte": "Generate Report",
      "descargar_docx": "Download (.docx)",
      "reporte_generado": "Generated Report",
      "reporte_ia_titulo": "AI Report",
      "reporte_ia_desc": "Configure the options and click 'Generate Report'",
      "reporte_ia_auto": "The report will be generated automatically based on your analyses."
    },
    "proyecto": {
      "guardar_proyecto": "Save Project",
      "respaldo_datos": "Data Backup",
      "guardar_desc": "Saves all your work including codes, categories and highlights.",
      "descargar_estado": "Download State (.rds)",
      "cargar_proyecto": "Load Project",
      "restaurar_datos": "Restore Data",
      "cargar_desc": "Load a previously saved project to continue working.",
      "seleccionar_archivo": "Select File",
      "buscar": "Browse...",
      "archivo_no_seleccionado": ".rds file not selected"
    },
    "citar": {
      "como_citar": "How to Cite RCualiText",
      "reconocimiento": "Academic Recognition",
      "cita_apa": "APA 7th Edition Citation",
      "copiar_cita": "Copy Citation",
      "info_software": "Software Information",
      "autor": "Author: ",
      "anio": "Year: ",
      "version": "Version: ",
      "tipo": "Type: ",
      "tipo_valor": "Shiny app for qualitative analysis",
      "repositorio": "Repository: ",
      "importante": "Important",
      "importante_desc": "If you use RCualiText in your research or academic work, we appreciate you including this citation to acknowledge the author's work and allow other researchers to access this tool."
    },
    "info": {
      "acerca_de": "About RCualiText",
      "analisis_avanzado": "Advanced Qualitative Analysis",
      "descripcion_1": "RCualiText is an advanced application for qualitative text coding that allows you to load documents (.txt and .docx), define codes and categories, highlight extracts of interest, and visualize code frequencies and co-occurrence networks.",
      "descripcion_2": "With RCualiText you can interactively manage your code list, group them into categories, export your highlights to Excel, and graphically analyze your qualitative data through modern visualizations and network analysis.",
      "resaltado_inteligente": "Smart Highlighting",
      "modo_seleccionar_desc": "Apply codes to selected fragments",
      "modo_deseleccionar_desc": "Remove specific codes with one click",
      "modo_acumulativo_desc": "Multiple codes per fragment",
      "gradientes_desc": "Multiple codes with visual effects",
      "tooltips_desc": "Information on mouse hover",
      "exportacion_desc": "Detailed data to Excel",
      "guia_deseleccion": "Deselection Guide",
      "guia_paso_1": "Activate 'Deselect' mode in the control panel",
      "guia_paso_2": "Click directly on the highlighted text",
      "guia_paso_3": "Select which specific code to remove",
      "guia_paso_4": "Return to 'Select' mode to continue"
    },
    "modes": {
      "seleccionar_activo": "Select Mode Active",
      "acciones_disponibles": "Available actions:",
      "selecciona_texto": "Select text in the document viewer",
      "aplica_codigos": "Apply codes automatically",
      "caracteristicas": "Features:",
      "acumulativo_disponible": "Accumulative mode available",
      "clic_info": "Click on highlight = info",
      "deseleccionar_activo": "Deselect Mode Active",
      "instrucciones_deseleccion": "Instructions:",
      "clic_resaltado": "Click directly on highlighted text",
      "elige_codigo": "Choose specific code to remove",
      "importante": "Important:",
      "cursor_cambia": "Cursor changes on highlight",
      "vuelve_seleccionar": "Return to 'Select' afterwards"
    },
    "notifications": {
      "bienvenido": "Welcome to RCualiText! Load your documents to get started.",
      "modo_seleccionar": "Select mode activated",
      "modo_deseleccionar": "Deselect mode activated - Click on highlighted text in viewer",
      "codigo_actualizado": "Code {code} updated",
      "codigo_anadido": "Code {code} added",
      "codigo_eliminado": "Code {code} deleted",
      "codigo_eliminado_con_resaltados": "Code {code} deleted along with {n} highlight(s)",
      "categoria_actualizada": "Category {cat} updated",
      "categoria_anadida": "Category {cat} added",
      "categoria_eliminada": "Category {cat} deleted",
      "documentos_cargados": "{n} document(s) loaded",
      "codigo_anadido_fragmento": "Code {code} added to fragment",
      "codigo_ya_aplicado": "Code {code} is already applied",
      "fragmento_recodificado": "Fragment recoded with {code}",
      "fragmento_codificado": "Fragment coded with {code}",
      "resaltados_eliminados": "Highlights removed from {file}",
      "todos_eliminados": "All highlights have been deleted",
      "resaltado_eliminado": "Highlight deleted",
      "no_codigos_fragmento": "No codes found for this fragment",
      "codigo_eliminado_fragmento": "Code {code} removed from fragment",
      "datos_exportados": "Data exported successfully",
      "error_grafico": "Error generating chart: {msg}",
      "no_datos_descargar": "No data to download",
      "grafico_distribucion_descargado": "Distribution chart downloaded ({w}x{h} in, {dpi} DPI)",
      "grafico_red_descargado": "Network chart downloaded ({w}x{h} in, {dpi} DPI)",
      "proyecto_guardado": "Project saved successfully (includes Semantic Analysis and 2D Visualization)",
      "proyecto_convertido": "Project converted to new format with deselection support",
      "proyecto_cargado": "Project loaded successfully{info}",
      "error_cargar_proyecto": "Error loading project: {msg}",
      "cita_copiada": "Citation copied to clipboard",
      "tabla_ia_descargada": "AI Table downloaded",
      "figura_descargada": "Figure downloaded ({w}x{h} in, {dpi} DPI)",
      "tabla_clustering_descargada": "Clustering table downloaded",
      "tabla_similitud_descargada": "Similarity table downloaded",
      "tabla_coherencia_descargada": "Coherence table downloaded",
      "red_semantica_descargada": "Semantic network downloaded ({w}x{h} in, {dpi} DPI)",
      "carga_documentos": "Load at least one document in the 'Document' tab",
      "carga_diccionario": "Upload a valid code dictionary",
      "ingresa_api_key": "Enter your OpenAI API Key",
      "ingresa_api_key_pestana": "Enter your OpenAI API Key in the 'AI Analysis' tab",
      "analisis_ia_completado": "AI Analysis completed: {n} extracts found",
      "no_extraer_fragmentos": "Could not extract fragments. Check your API Key and connection.",
      "error_openai": "Error OpenAI - {code}: {msg}",
      "error_inesperado": "Unexpected error - {code}: {msg}",
      "formato_diccionario_error": "Dictionary format not supported",
      "columnas_diccionario_error": "Dictionary must have columns: Category, Code, Definition",
      "embeddings_actualizados": "Embeddings are already up to date",
      "embeddings_generados": "Embeddings generated: {n} fragments (OpenAI)",
      "primero_genera_embeddings": "First generate the embeddings",
      "clustering_completado": "Clustering completed: {n} clusters identified",
      "no_inconsistencias": "No inconsistencies found with the selected threshold",
      "inconsistencias_encontradas": "{n} possible inconsistencies found",
      "necesitas_fragmentos": "You need at least 2 fragments. Use 'AI Analysis' first or code manually.",
      "solo_fragmentos": "You only have {n} fragment(s). You need at least 2.",
      "necesitas_2_codigos": "You need at least 2 different codes",
      "red_generada": "Network generated: {n_cod} codes, {n_con} connections",
      "coherencia_completada": "Coherence analysis completed for {n} codes",
      "no_fragmentos_validar": "No fragments to validate. Use 'AI Analysis' first or code manually.",
      "validacion_completada": "Validation completed",
      "visualizacion_generada": "2D Visualization generated and saved",
      "no_datos_coherencia": "No coherence data to plot",
      "no_hay_documentos": "No documents loaded",
      "doc_info": "Document {idx} of {total}: {name}",
      "reporte_generado": "Report generated successfully",
      "no_generar_reporte": "Could not generate report. Check your API Key.",
      "no_fragmentos_codificados": "No coded fragments. Perform an analysis first.",
      "reporte_word_descargado": "Word report downloaded",
      "este_codigo_no_resaltados": "This code has no associated highlights.",
      "eliminar_resaltados_asociados": "This will also delete {n} highlight(s) associated with this code.",
      "no_hay_reporte": "No report generated",
      "genera_reporte_primero": "Please generate a report first using the 'Generate Report' button.",
      "meta_codigos": "codes",
      "meta_resaltados": "highlights",
      "meta_resultados_ia": "AI results"
    },
    "modals": {
      "confirmar_eliminacion": "Confirm Deletion",
      "eliminar_resaltado": "Delete this highlight?",
      "texto": "Text: ",
      "codigo_label": "Code: ",
      "archivo_label": "File: ",
      "cancelar": "Cancel",
      "eliminar": "Delete",
      "seleccionar_codigo_eliminar": "Select Code to Delete",
      "multiples_codigos": "This fragment has multiple codes:",
      "codigos_aplicados": "Applied codes:",
      "eliminar_seleccionado": "Delete Selected",
      "confirmar_eliminacion_codigo": "Confirm Code Deletion",
      "eliminar_codigo_pregunta": "Delete code: {code}?",
      "confirmar_limpieza": "Confirm Cleanup",
      "estas_seguro": "Are you sure?",
      "eliminar_resaltados_doc": "All highlights will be removed from document: {file}",
      "si_limpiar": "Yes, clear",
      "confirmacion": "Confirmation",
      "eliminar_todos_resaltados": "Delete ALL highlights?",
      "eliminar_todos_desc": "This action will remove all codes applied in all documents.",
      "no_deshacer": "This action cannot be undone.",
      "si_eliminar_todo": "Yes, delete all",
      "info_fragmento": "Fragment Information",
      "texto_label": "Text:",
      "codigos_aplicados_label": "Applied codes:",
      "archivo_info_label": "File:",
      "historial_codificacion": "Coding history:",
      "cerrar": "Close",
      "confirmar_eliminacion_extracto": "Confirm Deletion",
      "eliminar_resaltado_pregunta": "Delete this highlight?",
      "codigo_modal": "Code: ",
      "fragmento_modal": "Fragment: "
    },
    "help": {
      "centro_ayuda": "Help Center",
      "modos_trabajo": "Work Modes:",
      "modo_seleccionar": "Select Mode:",
      "selecciona_texto_visor": "Select text in document viewer",
      "aplica_codigos_auto": "Apply codes automatically",
      "resaltado_tiempo_real": "Real-time highlighting",
      "modo_deseleccionar": "Deselect Mode:",
      "clic_resaltado": "Direct click on highlight",
      "elimina_codigos": "Remove specific codes",
      "correccion_precisa": "Precise correction",
      "caracteristicas_avanzadas": "Advanced Features:",
      "modo_acumulativo": "Accumulative Mode:",
      "acumulativo_desc": "Allows applying multiple codes to the same text fragment",
      "visualizacion": "Visualization:",
      "gradientes_desc": "Gradients show fragments with multiple codes",
      "entendido": "Got it"
    },
    "datatable": {
      "search": "Search:",
      "lengthMenu": "Show _MENU_ entries",
      "info": "Showing _START_ to _END_ of _TOTAL_ entries",
      "info_codigos": "Showing _START_ to _END_ of _TOTAL_ codes",
      "info_categorias": "Showing _START_ to _END_ of _TOTAL_ categories",
      "info_resaltados": "Showing _START_ to _END_ of _TOTAL_ highlights",
      "info_registros": "Showing _START_ to _END_ of _TOTAL_ records",
      "lengthMenu_codigos": "Show _MENU_ codes",
      "lengthMenu_categorias": "Show _MENU_ categories",
      "lengthMenu_resaltados": "Show _MENU_ highlights",
      "lengthMenu_registros": "Show _MENU_ records",
      "paginate_previous": "Previous",
      "paginate_next": "Next",
      "paginate_first": "First",
      "paginate_last": "Last",
      "no_similares": "No similar fragments found with different code"
    },
    "colnames": {
      "codigo": "Code",
      "color": "Color",
      "categoria": "Category",
      "codigos_asociados": "Associated Codes",
      "extracto": "Extract",
      "archivo": "File",
      "hora": "Time",
      "cluster": "Cluster",
      "n_fragmentos": "N Fragments",
      "coherencia_media": "Mean Coherence",
      "min": "Min",
      "max": "Max",
      "sd": "SD",
      "evaluacion": "Evaluation",
      "aplicado": "Applied"
    },
    "evaluacion": {
      "excelente": "Excellent",
      "buena": "Good",
      "moderada": "Moderate",
      "baja_revisar": "Low - review",
      "insuficiente": "Insufficient (< 2 fragments)"
    },
    "plots": {
      "frecuencia": "Frequency",
      "codigos_x": "Codes",
      "codigo_label": "Code",
      "categoria_fill": "Category",
      "centralidad_zscore": "Centrality (z-score)",
      "codigo_legend": "Code",
      "distribucion_codigos_ia": "Code Distribution (AI Analysis)",
      "fragmentos_categoria_ia": "Fragments by Category (AI Analysis)",
      "clustering_semantico": "Semantic Clustering",
      "coherencia_semantica": "Semantic Coherence by Code",
      "coherencia_media_label": "Mean Coherence (cosine similarity)",
      "evaluacion_fill": "Evaluation",
      "red_semantica_codigos": "Semantic Code Network",
      "umbral_similitud_subtitle": "Similarity threshold: {val}",
      "comunidad": "Community",
      "visualizacion_embeddings": "Embedding Visualization ({method})",
      "dimension_1": "{method} Dimension 1",
      "dimension_2": "{method} Dimension 2",
      "clusters_semanticos_pca": "Semantic Clusters (PCA)",
      "sin_categoria": "Uncategorized"
    },
    "excel": {
      "resaltados_detallados": "Detailed_Highlights",
      "resumen_fragmentos": "Fragment_Summary",
      "estadisticas": "Statistics",
      "total_resaltados": "Total Highlights",
      "fragmentos_unicos": "Unique Fragments",
      "fragmentos_multiples_codigos": "Fragments with Multiple Codes",
      "promedio_codigos": "Average Codes per Fragment",
      "documentos_procesados": "Documents Processed",
      "codigos_utilizados": "Different Codes Used",
      "codigos_aplicados_col": "Applied_Codes",
      "num_codigos": "Num_Codes",
      "primera_codificacion": "First_Coding",
      "ultima_codificacion": "Last_Coding",
      "metrica": "Metric",
      "valor": "Value"
    },
    "validacion": {
      "panel_expertos": "Virtual Expert Panel",
      "haz_clic_validar": "Click 'Validate' to have an LLM evaluate the quality of your coding",
      "resultado_validacion": "Validation Result",
      "texto_label": "Text:",
      "codigo_label": "Code:",
      "evaluacion_label": "Evaluation:",
      "correcto": "Correct",
      "revisar": "Review",
      "incorrecto": "Incorrect",
      "justificacion_label": "Justification:",
      "sugerencia_label": "Suggestion:",
      "no_fragmentos": "No fragments to validate",
      "api_key_no_proporcionada": "OpenAI API Key not provided",
      "no_respuesta_openai": "No response received from OpenAI",
      "alta_similitud": "High similarity - review coding",
      "similitud_moderada": "Moderate similarity - consider merging"
    },
    "red_semantica": {
      "titulo_red": "Semantic Code Network",
      "configura_umbral": "Set the threshold and click 'Generate Network'",
      "codigos_label": "Codes",
      "conexiones_label": "Connections",
      "umbral_label": "Threshold"
    },
    "progress": {
      "analizando_gpt": "Analyzing with GPT-4.1...",
      "generando_embeddings": "Generating embeddings with OpenAI...",
      "conectando_openai": "Connecting with OpenAI...",
      "usando_datos": "Using data from {source}...",
      "embeddings_obtenidos": "Embeddings obtained via OpenAI",
      "calculando_similitudes": "Calculating similarities...",
      "completado": "Completed",
      "ejecutando_clustering": "Running clustering...",
      "buscando_similares": "Searching for similar fragments...",
      "generando_visualizacion": "Generating 2D visualization...",
      "analizando_coherencia": "Analyzing coherence...",
      "generando_red": "Generating semantic network...",
      "generando_reporte": "Generating report with GPT-4.1...",
      "procesando_respuesta": "Processing response...",
      "visualizacion_completada": "Visualization completed",
      "validando_gpt": "Validating with GPT-4.1..."
    },
    "fuente": {
      "ia": "AI Analysis",
      "manual": "manual coding"
    },
    "login": {
      "titulo": "Welcome to RCualiText",
      "subtitulo": "Qualitative Text Analysis Tool",
      "descripcion": "Please enter your details to continue",
      "nombre": "Full Name",
      "correo": "Email",
      "universidad": "University / Institution",
      "residencia": "Place of Residence",
      "boton": "Enter Application",
      "error_nombre": "Please enter your full name",
      "error_correo": "Please enter a valid email",
      "error_universidad": "Please enter your university or institution",
      "error_residencia": "Please enter your place of residence"
    }
  },
  "es": {
    "sidebar": {
      "documento": "Documento",
      "codigos": "C\u00f3digos",
      "categorias": "Categor\u00edas",
      "extractos": "Extractos",
      "analisis": "An\u00e1lisis",
      "analisis_ia": "An\u00e1lisis IA (opcional)",
      "analisis_semantico": "An\u00e1lisis Sem\u00e1ntico (experimental)",
      "reporte_ia": "Reporte con IA",
      "proyecto": "Proyecto",
      "citar": "Citar",
      "ayuda": "Ayuda"
    },
    "documento": {
      "panel_control": "Panel de Control",
      "cargar_documentos": "Cargar Documentos",
      "examinar": "Examinar...",
      "ningun_archivo": "Ning\u00fan archivo seleccionado",
      "modo_trabajo": "Modo de Trabajo",
      "seleccionar": "Seleccionar",
      "deseleccionar": "Deseleccionar",
      "codigo_aplicar": "C\u00f3digo a Aplicar",
      "modo_acumulativo": "Modo Acumulativo",
      "modo_acumulativo_help": "Permite aplicar m\u00faltiples c\u00f3digos al mismo fragmento",
      "navegacion": "Navegaci\u00f3n",
      "anterior": "Anterior",
      "siguiente": "Siguiente",
      "acciones": "Acciones",
      "limpiar": "Limpiar",
      "ayuda_btn": "Ayuda",
      "visor_documento": "Visor de Documento",
      "formato_no_soportado": "Formato no soportado"
    },
    "codigos": {
      "gestion_codigos": "Gesti\u00f3n de C\u00f3digos",
      "nombre_codigo": "Nombre del C\u00f3digo",
      "placeholder_codigo": "Ej: Emociones positivas",
      "color_codigo": "Color del C\u00f3digo",
      "guardar": "Guardar",
      "eliminar": "Eliminar",
      "lista_codigos": "Lista de C\u00f3digos"
    },
    "categorias": {
      "gestion_categorias": "Gesti\u00f3n de Categor\u00edas",
      "nombre_categoria": "Nombre de Categor\u00eda",
      "placeholder_categoria": "Ej: Aspectos emocionales",
      "codigos_asociados": "C\u00f3digos Asociados",
      "placeholder_codigos": "Selecciona c\u00f3digos...",
      "guardar": "Guardar",
      "eliminar": "Eliminar",
      "categorias_definidas": "Categor\u00edas Definidas",
      "sin_categoria": "Sin categor\u00eda"
    },
    "extractos": {
      "gestion_extractos": "Gesti\u00f3n de Extractos Codificados",
      "herramientas": "Herramientas de Gesti\u00f3n",
      "exportar_xlsx": "Exportar XLSX",
      "eliminar_seleccionado": "Eliminar Seleccionado",
      "limpiar_todo": "Limpiar Todo",
      "guia_resaltados": "Gu\u00eda de Resaltados",
      "visualizacion": "Visualizaci\u00f3n",
      "gradientes_multiples": "Gradientes indican m\u00faltiples c\u00f3digos",
      "hover_codigos": "Hover muestra c\u00f3digos aplicados",
      "cada_fila": "Cada fila = un c\u00f3digo por fragmento",
      "edicion": "Edici\u00f3n",
      "modo_deseleccionar_eliminar": "Modo deseleccionar para eliminar",
      "seleccion_multiple": "Selecci\u00f3n m\u00faltiple disponible",
      "exportacion_excel": "Exportaci\u00f3n completa a Excel"
    },
    "analisis": {
      "configuracion": "Configuraci\u00f3n",
      "opciones_visuales": "Opciones Visuales",
      "colorear_categoria": "Colorear por Categor\u00eda",
      "config_descarga": "Configuraci\u00f3n de Descarga",
      "ancho_pulg": "Ancho (pulg)",
      "alto_pulg": "Alto (pulg)",
      "resolucion_dpi": "Resoluci\u00f3n (DPI)",
      "config_aplicada": "Configuraci\u00f3n aplicada a ambos gr\u00e1ficos",
      "distribucion_codigos": "Distribuci\u00f3n de C\u00f3digos",
      "exportar": "Exportar",
      "distribucion_jpg": "Distribuci\u00f3n (JPG)",
      "exportar_red": "Exportar Red",
      "red_coocurrencia_jpg": "Red de Coocurrencia (JPG)",
      "red_coocurrencia_centralidad": "Red de Coocurrencia y An\u00e1lisis de Centralidad"
    },
    "analisis_ia": {
      "config_analisis_ia": "Configuraci\u00f3n del An\u00e1lisis IA",
      "config_openai": "Configuraci\u00f3n de OpenAI",
      "api_key_openai": "API Key de OpenAI",
      "api_key_help": "Usa el modelo GPT-4.1 de OpenAI. Obt\u00e9n tu API Key en platform.openai.com",
      "api_key_requerida": "El an\u00e1lisis requiere una API Key v\u00e1lida de OpenAI.",
      "diccionario_codigos": "Diccionario de C\u00f3digos",
      "cargar_diccionario": "Cargar Diccionario",
      "archivo_csv_xlsx": "Archivo .csv o .xlsx",
      "columnas_requeridas": "Debe tener columnas: Categor\u00eda, C\u00f3digo, Definici\u00f3n",
      "ejecutar_analisis": "Ejecutar An\u00e1lisis IA",
      "descargar_resultados": "Descargar Resultados (.xlsx)",
      "resultados_help": "Los resultados se mostrar\u00e1n abajo. Descarga la tabla en Excel con el bot\u00f3n de arriba.",
      "resultados_analisis_ia": "Resultados del An\u00e1lisis IA",
      "instrucciones": "Instrucciones",
      "instruccion_1": "1. Aseg\u00farate de tener documentos cargados en la pesta\u00f1a 'Documento'",
      "instruccion_2": "2. Ingresa tu API Key de OpenAI",
      "instruccion_3": "3. Carga un diccionario de c\u00f3digos con las columnas: Categor\u00eda, C\u00f3digo, Definici\u00f3n",
      "instruccion_4": "4. Ejecuta el an\u00e1lisis y revisa los resultados",
      "instruccion_5": "5. Si est\u00e1s satisfecho, integra los resultados al an\u00e1lisis manual",
      "descargar_tabla_excel": "Descargar Tabla (Excel)",
      "visualizacion_resultados": "Visualizaci\u00f3n de Resultados IA",
      "distribucion_codigos_ia": "Distribuci\u00f3n de C\u00f3digos",
      "fragmentos_categoria": "Fragmentos por Categor\u00eda"
    },
    "analisis_semantico": {
      "config_analisis_semantico": "Configuraci\u00f3n del An\u00e1lisis Sem\u00e1ntico",
      "configuracion": "Configuraci\u00f3n",
      "modulo_info": "Este m\u00f3dulo utiliza la API de OpenAI para generar embeddings y an\u00e1lisis sem\u00e1ntico.",
      "modelo_embeddings": "Modelo de embeddings: text-embedding-3-small",
      "api_key_info": "Ingresa tu API Key de OpenAI en la pesta\u00f1a 'An\u00e1lisis IA'",
      "generar_embeddings": "Generar Embeddings",
      "generar_embeddings_help": "Genera representaciones vectoriales de los fragmentos codificados usando OpenAI",
      "herramientas_analisis": "Herramientas de An\u00e1lisis Sem\u00e1ntico",
      "requisitos": "Requisitos",
      "req_1": "1. Ten fragmentos codificados (usa 'An\u00e1lisis IA' o codifica manualmente)",
      "req_2": "2. Genera los embeddings (usa tokens internos autom\u00e1ticamente)",
      "req_3": "3. Explora las herramientas de an\u00e1lisis sem\u00e1ntico",
      "clustering_semantico": "Clustering Sem\u00e1ntico",
      "clustering_desc": "Agrupa fragmentos similares autom\u00e1ticamente",
      "num_clusters": "N\u00famero de clusters",
      "ejecutar_clustering": "Ejecutar Clustering",
      "deteccion_similitud": "Detecci\u00f3n de Similitud",
      "similitud_desc": "Encuentra fragmentos similares con diferente c\u00f3digo",
      "umbral_similitud": "Umbral de similitud",
      "detectar_similares": "Detectar Similares",
      "visualizacion_2d": "Visualizaci\u00f3n 2D",
      "visualizacion_desc": "Visualiza la distribuci\u00f3n sem\u00e1ntica de fragmentos",
      "metodo_reduccion": "M\u00e9todo de reducci\u00f3n",
      "visualizar": "Visualizar",
      "analisis_coherencia": "An\u00e1lisis de Coherencia",
      "coherencia_desc": "Eval\u00faa la homogeneidad sem\u00e1ntica de cada c\u00f3digo",
      "analizar_coherencia": "Analizar Coherencia",
      "validacion_llm": "Validaci\u00f3n con LLM (Panel de Expertos Virtual)",
      "validacion_desc": "Un modelo de lenguaje eval\u00faa la calidad de tu codificaci\u00f3n",
      "fragmentos_validar": "Fragmentos a validar (muestra)",
      "validar": "Validar",
      "config_descarga_figuras": "Configuraci\u00f3n de Descarga de Figuras",
      "ajusta_dimensiones": "Ajusta las dimensiones antes de descargar",
      "resultados_analisis_semantico": "Resultados del An\u00e1lisis Sem\u00e1ntico",
      "tab_clustering": "Clustering",
      "tab_similitud": "Similitud",
      "tab_visualizacion": "Visualizaci\u00f3n",
      "tab_coherencia": "Coherencia",
      "tab_red_semantica": "Red Sem\u00e1ntica",
      "tab_validacion": "Validaci\u00f3n LLM",
      "tabla_excel": "Tabla Excel",
      "figura_png": "Figura PNG",
      "config_red": "Configuraci\u00f3n",
      "umbral_conexion": "Umbral de conexi\u00f3n (similitud)",
      "umbral_conexion_help": "C\u00f3digos con similitud mayor al umbral se conectan",
      "colorear_por": "Colorear por",
      "categoria_opt": "Categor\u00eda",
      "comunidad_opt": "Comunidad (detectada)",
      "generar_red": "Generar Red",
      "descargar_png": "Descargar PNG",
      "sin_embeddings": "Sin embeddings generados",
      "embeddings_generados": "Embeddings generados: {n} fragmentos",
      "dimensiones": "Dimensiones: {d}"
    },
    "reporte": {
      "config_reporte": "Configuraci\u00f3n del Reporte",
      "informacion": "Informaci\u00f3n",
      "info_desc": "Genera un reporte interpretativo autom\u00e1tico basado en los an\u00e1lisis realizados.",
      "info_modelo": "Usa el modelo GPT-4.1 de OpenAI para generar reportes interpretativos.",
      "idioma_reporte": "Idioma del reporte",
      "estilo_redaccion": "Estilo de redacci\u00f3n",
      "academico": "Acad\u00e9mico (tesis/art\u00edculo)",
      "tecnico": "T\u00e9cnico (informe)",
      "divulgativo": "Divulgativo (general)",
      "secciones_incluir": "Secciones a incluir",
      "sec_codificacion": "Resumen de codificaci\u00f3n",
      "sec_frecuencias": "An\u00e1lisis de frecuencias",
      "sec_clustering": "Clustering sem\u00e1ntico",
      "sec_coherencia": "Coherencia de c\u00f3digos",
      "sec_red": "Red sem\u00e1ntica",
      "sec_hallazgos": "Hallazgos principales",
      "sec_limitaciones": "Limitaciones",
      "requisitos": "Requisitos",
      "req_fragmentos": "Tener fragmentos codificados (manual o IA)",
      "req_embeddings": "Para an\u00e1lisis sem\u00e1ntico: haber generado embeddings",
      "generar_reporte": "Generar Reporte",
      "descargar_docx": "Descargar (.docx)",
      "reporte_generado": "Reporte Generado",
      "reporte_ia_titulo": "Reporte con IA",
      "reporte_ia_desc": "Configura las opciones y haz clic en 'Generar Reporte'",
      "reporte_ia_auto": "El reporte se generar\u00e1 autom\u00e1ticamente bas\u00e1ndose en tus an\u00e1lisis."
    },
    "proyecto": {
      "guardar_proyecto": "Guardar Proyecto",
      "respaldo_datos": "Respaldo de Datos",
      "guardar_desc": "Guarda todo tu trabajo incluyendo c\u00f3digos, categor\u00edas y resaltados.",
      "descargar_estado": "Descargar Estado (.rds)",
      "cargar_proyecto": "Cargar Proyecto",
      "restaurar_datos": "Restaurar Datos",
      "cargar_desc": "Carga un proyecto previamente guardado para continuar trabajando.",
      "seleccionar_archivo": "Seleccionar Archivo",
      "buscar": "Buscar...",
      "archivo_no_seleccionado": "Archivo .rds no seleccionado"
    },
    "citar": {
      "como_citar": "C\u00f3mo Citar RCualiText",
      "reconocimiento": "Reconocimiento Acad\u00e9mico",
      "cita_apa": "Cita en formato APA 7\u00aa edici\u00f3n",
      "copiar_cita": "Copiar Cita",
      "info_software": "Informaci\u00f3n del Software",
      "autor": "Autor: ",
      "anio": "A\u00f1o: ",
      "version": "Versi\u00f3n: ",
      "tipo": "Tipo: ",
      "tipo_valor": "Aplicaci\u00f3n Shiny para an\u00e1lisis cualitativo",
      "repositorio": "Repositorio: ",
      "importante": "Importante",
      "importante_desc": "Si utilizas RCualiText en tu investigaci\u00f3n o trabajo acad\u00e9mico, te agradecemos que incluyas esta cita para reconocer el trabajo del autor y permitir que otros investigadores puedan acceder a esta herramienta."
    },
    "info": {
      "acerca_de": "Acerca de RCualiText",
      "analisis_avanzado": "An\u00e1lisis Cualitativo Avanzado",
      "descripcion_1": "RCualiText es una aplicaci\u00f3n avanzada para la codificaci\u00f3n cualitativa de textos que permite cargar documentos (.txt y .docx), definir c\u00f3digos y categor\u00edas, resaltar extractos de inter\u00e9s y visualizar frecuencias y redes de coocurrencia de c\u00f3digos.",
      "descripcion_2": "Con RCualiText puedes gestionar de manera interactiva tu lista de c\u00f3digos, agruparlos en categor\u00edas, exportar tus resaltados a Excel y analizar gr\u00e1ficamente tus datos cualitativos mediante visualizaciones modernas y an\u00e1lisis de redes.",
      "resaltado_inteligente": "Resaltado Inteligente",
      "modo_seleccionar_desc": "Aplica c\u00f3digos a fragmentos seleccionados",
      "modo_deseleccionar_desc": "Elimina c\u00f3digos espec\u00edficos con un clic",
      "modo_acumulativo_desc": "M\u00faltiples c\u00f3digos por fragmento",
      "gradientes_desc": "C\u00f3digos m\u00faltiples con efectos visuales",
      "tooltips_desc": "Informaci\u00f3n al pasar el mouse",
      "exportacion_desc": "Datos detallados a Excel",
      "guia_deseleccion": "Gu\u00eda de Deselecci\u00f3n",
      "guia_paso_1": "Activa el modo 'Deseleccionar' en el panel de controles",
      "guia_paso_2": "Haz clic directamente sobre el texto resaltado",
      "guia_paso_3": "Selecciona qu\u00e9 c\u00f3digo espec\u00edfico eliminar",
      "guia_paso_4": "Vuelve al modo 'Seleccionar' para continuar"
    },
    "modes": {
      "seleccionar_activo": "Modo Seleccionar Activo",
      "acciones_disponibles": "Acciones disponibles:",
      "selecciona_texto": "Selecciona texto en el visor de documento",
      "aplica_codigos": "Aplica c\u00f3digos autom\u00e1ticamente",
      "caracteristicas": "Caracter\u00edsticas:",
      "acumulativo_disponible": "Modo acumulativo disponible",
      "clic_info": "Clic en resaltado = info",
      "deseleccionar_activo": "Modo Deseleccionar Activo",
      "instrucciones_deseleccion": "Instrucciones:",
      "clic_resaltado": "Clic directo en texto resaltado",
      "elige_codigo": "Elige c\u00f3digo espec\u00edfico a eliminar",
      "importante": "Importante:",
      "cursor_cambia": "Cursor cambia en resaltado",
      "vuelve_seleccionar": "Vuelve a 'Seleccionar' despu\u00e9s"
    },
    "notifications": {
      "bienvenido": "\u00a1Bienvenido a RCualiText! Carga tus documentos para comenzar.",
      "modo_seleccionar": "Modo Seleccionar activado",
      "modo_deseleccionar": "Modo Deseleccionar activado - Haz clic en texto resaltado del visor",
      "codigo_actualizado": "C\u00f3digo {code} actualizado",
      "codigo_anadido": "C\u00f3digo {code} a\u00f1adido",
      "codigo_eliminado": "C\u00f3digo {code} eliminado",
      "codigo_eliminado_con_resaltados": "C\u00f3digo {code} eliminado junto con {n} resaltado(s)",
      "categoria_actualizada": "Categor\u00eda {cat} actualizada",
      "categoria_anadida": "Categor\u00eda {cat} a\u00f1adida",
      "categoria_eliminada": "Categor\u00eda {cat} eliminada",
      "documentos_cargados": "{n} documento(s) cargado(s)",
      "codigo_anadido_fragmento": "C\u00f3digo {code} a\u00f1adido al fragmento",
      "codigo_ya_aplicado": "El c\u00f3digo {code} ya est\u00e1 aplicado",
      "fragmento_recodificado": "Fragmento recodificado con {code}",
      "fragmento_codificado": "Fragmento codificado con {code}",
      "resaltados_eliminados": "Resaltados eliminados de {file}",
      "todos_eliminados": "Todos los resaltados han sido eliminados",
      "resaltado_eliminado": "Resaltado eliminado",
      "no_codigos_fragmento": "No se encontraron c\u00f3digos para este fragmento",
      "codigo_eliminado_fragmento": "C\u00f3digo {code} eliminado del fragmento",
      "datos_exportados": "Datos exportados exitosamente",
      "error_grafico": "Error al generar gr\u00e1fico: {msg}",
      "no_datos_descargar": "No hay datos para descargar",
      "grafico_distribucion_descargado": "Gr\u00e1fico de distribuci\u00f3n descargado ({w}\u00d7{h} pulg, {dpi} DPI)",
      "grafico_red_descargado": "Gr\u00e1fico de red descargado ({w}\u00d7{h} pulg, {dpi} DPI)",
      "proyecto_guardado": "Proyecto guardado exitosamente (incluye An\u00e1lisis Sem\u00e1ntico y Visualizaci\u00f3n 2D)",
      "proyecto_convertido": "Proyecto convertido al nuevo formato con soporte para deselecci\u00f3n",
      "proyecto_cargado": "Proyecto cargado exitosamente{info}",
      "error_cargar_proyecto": "Error al cargar el proyecto: {msg}",
      "cita_copiada": "Cita copiada al portapapeles",
      "tabla_ia_descargada": "Tabla IA descargada",
      "figura_descargada": "Figura descargada ({w}\u00d7{h} pulg, {dpi} DPI)",
      "tabla_clustering_descargada": "Tabla de clustering descargada",
      "tabla_similitud_descargada": "Tabla de similitud descargada",
      "tabla_coherencia_descargada": "Tabla de coherencia descargada",
      "red_semantica_descargada": "Red sem\u00e1ntica descargada ({w}\u00d7{h} pulg, {dpi} DPI)",
      "carga_documentos": "Carga al menos un documento en la pesta\u00f1a 'Documento'",
      "carga_diccionario": "Carga un diccionario de c\u00f3digos v\u00e1lido",
      "ingresa_api_key": "Ingresa tu API Key de OpenAI",
      "ingresa_api_key_pestana": "Ingresa tu API Key de OpenAI en la pesta\u00f1a 'An\u00e1lisis IA'",
      "analisis_ia_completado": "An\u00e1lisis IA completado: {n} extractos encontrados",
      "no_extraer_fragmentos": "No se pudieron extraer fragmentos. Verifica tu API Key y conexi\u00f3n.",
      "error_openai": "Error OpenAI - {code}: {msg}",
      "error_inesperado": "Error inesperado - {code}: {msg}",
      "formato_diccionario_error": "Formato de diccionario no soportado",
      "columnas_diccionario_error": "El diccionario debe tener columnas: Categor\u00eda, C\u00f3digo, Definici\u00f3n",
      "embeddings_actualizados": "Los embeddings ya est\u00e1n actualizados",
      "embeddings_generados": "Embeddings generados: {n} fragmentos (OpenAI)",
      "primero_genera_embeddings": "Primero genera los embeddings",
      "clustering_completado": "Clustering completado: {n} clusters identificados",
      "no_inconsistencias": "No se encontraron inconsistencias con el umbral seleccionado",
      "inconsistencias_encontradas": "Se encontraron {n} posibles inconsistencias",
      "necesitas_fragmentos": "Necesitas al menos 2 fragmentos. Usa primero 'An\u00e1lisis IA' o codifica manualmente.",
      "solo_fragmentos": "Solo tienes {n} fragmento(s). Necesitas al menos 2.",
      "necesitas_2_codigos": "Necesitas al menos 2 c\u00f3digos diferentes",
      "red_generada": "Red generada: {n_cod} c\u00f3digos, {n_con} conexiones",
      "coherencia_completada": "An\u00e1lisis de coherencia completado para {n} c\u00f3digos",
      "no_fragmentos_validar": "No hay fragmentos para validar. Usa primero 'An\u00e1lisis IA' o codifica manualmente.",
      "validacion_completada": "Validaci\u00f3n completada",
      "visualizacion_generada": "Visualizaci\u00f3n 2D generada y guardada",
      "no_datos_coherencia": "No hay datos de coherencia para graficar",
      "no_hay_documentos": "No hay documentos cargados",
      "doc_info": "Documento {idx} de {total}: {name}",
      "reporte_generado": "Reporte generado exitosamente",
      "no_generar_reporte": "No se pudo generar el reporte. Verifica tu API Key.",
      "no_fragmentos_codificados": "No hay fragmentos codificados. Realiza primero un an\u00e1lisis.",
      "reporte_word_descargado": "Reporte Word descargado",
      "este_codigo_no_resaltados": "Este c\u00f3digo no tiene resaltados asociados.",
      "eliminar_resaltados_asociados": "Esto tambi\u00e9n eliminar\u00e1 {n} resaltado(s) asociado(s) a este c\u00f3digo.",
      "no_hay_reporte": "No hay reporte generado",
      "genera_reporte_primero": "Por favor, genera un reporte primero usando el bot\u00f3n 'Generar Reporte'.",
      "meta_codigos": "c\u00f3digos",
      "meta_resaltados": "resaltados",
      "meta_resultados_ia": "resultados IA"
    },
    "modals": {
      "confirmar_eliminacion": "Confirmar Eliminaci\u00f3n",
      "eliminar_resaltado": "\u00bfEliminar este resaltado?",
      "texto": "Texto: ",
      "codigo_label": "C\u00f3digo: ",
      "archivo_label": "Archivo: ",
      "cancelar": "Cancelar",
      "eliminar": "Eliminar",
      "seleccionar_codigo_eliminar": "Seleccionar C\u00f3digo a Eliminar",
      "multiples_codigos": "Este fragmento tiene m\u00faltiples c\u00f3digos:",
      "codigos_aplicados": "C\u00f3digos aplicados:",
      "eliminar_seleccionado": "Eliminar Seleccionado",
      "confirmar_eliminacion_codigo": "Confirmar Eliminaci\u00f3n de C\u00f3digo",
      "eliminar_codigo_pregunta": "\u00bfEliminar el c\u00f3digo: {code}?",
      "confirmar_limpieza": "Confirmar Limpieza",
      "estas_seguro": "\u00bfEst\u00e1s seguro?",
      "eliminar_resaltados_doc": "Se eliminar\u00e1n todos los resaltados del documento: {file}",
      "si_limpiar": "S\u00ed, limpiar",
      "confirmacion": "Confirmaci\u00f3n",
      "eliminar_todos_resaltados": "\u00bfEliminar TODOS los resaltados?",
      "eliminar_todos_desc": "Esta acci\u00f3n eliminar\u00e1 todos los c\u00f3digos aplicados en todos los documentos.",
      "no_deshacer": "Esta acci\u00f3n no se puede deshacer.",
      "si_eliminar_todo": "S\u00ed, eliminar todo",
      "info_fragmento": "Informaci\u00f3n del Fragmento",
      "texto_label": "Texto:",
      "codigos_aplicados_label": "C\u00f3digos aplicados:",
      "archivo_info_label": "Archivo:",
      "historial_codificacion": "Historial de codificaci\u00f3n:",
      "cerrar": "Cerrar",
      "confirmar_eliminacion_extracto": "Confirmar Eliminaci\u00f3n",
      "eliminar_resaltado_pregunta": "\u00bfEliminar este resaltado?",
      "codigo_modal": "C\u00f3digo: ",
      "fragmento_modal": "Fragmento: "
    },
    "help": {
      "centro_ayuda": "Centro de Ayuda",
      "modos_trabajo": "Modos de Trabajo:",
      "modo_seleccionar": "Modo Seleccionar:",
      "selecciona_texto_visor": "Selecciona texto en el visor de documento",
      "aplica_codigos_auto": "Aplica c\u00f3digos autom\u00e1ticamente",
      "resaltado_tiempo_real": "Resaltado inmediato en tiempo real",
      "modo_deseleccionar": "Modo Deseleccionar:",
      "clic_resaltado": "Clic directo en resaltado",
      "elimina_codigos": "Elimina c\u00f3digos espec\u00edficos",
      "correccion_precisa": "Correcci\u00f3n precisa",
      "caracteristicas_avanzadas": "Caracter\u00edsticas Avanzadas:",
      "modo_acumulativo": "Modo Acumulativo:",
      "acumulativo_desc": "Permite aplicar m\u00faltiples c\u00f3digos al mismo fragmento de texto",
      "visualizacion": "Visualizaci\u00f3n:",
      "gradientes_desc": "Gradientes muestran fragmentos con varios c\u00f3digos",
      "entendido": "Entendido"
    },
    "datatable": {
      "search": "Buscar:",
      "lengthMenu": "Mostrar _MENU_ registros",
      "info": "Mostrando _START_ a _END_ de _TOTAL_ registros",
      "info_codigos": "Mostrando _START_ a _END_ de _TOTAL_ c\u00f3digos",
      "info_categorias": "Mostrando _START_ a _END_ de _TOTAL_ categor\u00edas",
      "info_resaltados": "Mostrando _START_ a _END_ de _TOTAL_ resaltados",
      "info_registros": "Mostrando _START_ a _END_ de _TOTAL_ registros",
      "lengthMenu_codigos": "Mostrar _MENU_ c\u00f3digos",
      "lengthMenu_categorias": "Mostrar _MENU_ categor\u00edas",
      "lengthMenu_resaltados": "Mostrar _MENU_ resaltados",
      "lengthMenu_registros": "Mostrar _MENU_ registros",
      "paginate_previous": "Anterior",
      "paginate_next": "Siguiente",
      "paginate_first": "Primero",
      "paginate_last": "\u00daltimo",
      "no_similares": "No se encontraron fragmentos similares con diferente c\u00f3digo"
    },
    "colnames": {
      "codigo": "C\u00f3digo",
      "color": "Color",
      "categoria": "Categor\u00eda",
      "codigos_asociados": "C\u00f3digos Asociados",
      "extracto": "Extracto",
      "archivo": "Archivo",
      "hora": "Hora",
      "cluster": "Cluster",
      "n_fragmentos": "N Fragmentos",
      "coherencia_media": "Coherencia Media",
      "min": "M\u00edn",
      "max": "M\u00e1x",
      "sd": "SD",
      "evaluacion": "Evaluaci\u00f3n",
      "aplicado": "Aplicado"
    },
    "evaluacion": {
      "excelente": "Excelente",
      "buena": "Buena",
      "moderada": "Moderada",
      "baja_revisar": "Baja - revisar",
      "insuficiente": "Insuficiente (< 2 fragmentos)"
    },
    "plots": {
      "frecuencia": "Frecuencia",
      "codigos_x": "C\u00f3digos",
      "codigo_label": "C\u00f3digo",
      "categoria_fill": "Categor\u00eda",
      "centralidad_zscore": "Centralidad (z-score)",
      "codigo_legend": "C\u00f3digo",
      "distribucion_codigos_ia": "Distribuci\u00f3n de C\u00f3digos (An\u00e1lisis IA)",
      "fragmentos_categoria_ia": "Fragmentos por Categor\u00eda (An\u00e1lisis IA)",
      "clustering_semantico": "Clustering Sem\u00e1ntico",
      "coherencia_semantica": "Coherencia Sem\u00e1ntica por C\u00f3digo",
      "coherencia_media_label": "Coherencia Media (similitud coseno)",
      "evaluacion_fill": "Evaluaci\u00f3n",
      "red_semantica_codigos": "Red Sem\u00e1ntica de C\u00f3digos",
      "umbral_similitud_subtitle": "Umbral de similitud: {val}",
      "comunidad": "Comunidad",
      "visualizacion_embeddings": "Visualizaci\u00f3n de Embeddings ({method})",
      "dimension_1": "{method} Dimensi\u00f3n 1",
      "dimension_2": "{method} Dimensi\u00f3n 2",
      "clusters_semanticos_pca": "Clusters Sem\u00e1nticos (PCA)",
      "sin_categoria": "Sin categor\u00eda"
    },
    "excel": {
      "resaltados_detallados": "Resaltados_Detallados",
      "resumen_fragmentos": "Resumen_Fragmentos",
      "estadisticas": "Estadisticas",
      "total_resaltados": "Total de Resaltados",
      "fragmentos_unicos": "Fragmentos \u00danicos",
      "fragmentos_multiples_codigos": "Fragmentos con M\u00faltiples C\u00f3digos",
      "promedio_codigos": "Promedio de C\u00f3digos por Fragmento",
      "documentos_procesados": "Documentos Procesados",
      "codigos_utilizados": "C\u00f3digos Diferentes Utilizados",
      "codigos_aplicados_col": "Codigos_Aplicados",
      "num_codigos": "Num_Codigos",
      "primera_codificacion": "Primera_Codificacion",
      "ultima_codificacion": "Ultima_Codificacion",
      "metrica": "Metrica",
      "valor": "Valor"
    },
    "validacion": {
      "panel_expertos": "Panel de Expertos Virtual",
      "haz_clic_validar": "Haz clic en 'Validar' para que un LLM eval\u00fae la calidad de tu codificaci\u00f3n",
      "resultado_validacion": "Resultado de la Validaci\u00f3n",
      "texto_label": "Texto:",
      "codigo_label": "C\u00f3digo:",
      "evaluacion_label": "Evaluaci\u00f3n:",
      "correcto": "Correcto",
      "revisar": "Revisar",
      "incorrecto": "Incorrecto",
      "justificacion_label": "Justificaci\u00f3n:",
      "sugerencia_label": "Sugerencia:",
      "no_fragmentos": "No hay fragmentos para validar",
      "api_key_no_proporcionada": "API Key de OpenAI no proporcionada",
      "no_respuesta_openai": "No se recibi\u00f3 respuesta de OpenAI",
      "alta_similitud": "Alta similitud - revisar codificaci\u00f3n",
      "similitud_moderada": "Similitud moderada - considerar unificar"
    },
    "red_semantica": {
      "titulo_red": "Red Sem\u00e1ntica de C\u00f3digos",
      "configura_umbral": "Configura el umbral y haz clic en 'Generar Red'",
      "codigos_label": "C\u00f3digos",
      "conexiones_label": "Conexiones",
      "umbral_label": "Umbral"
    },
    "progress": {
      "analizando_gpt": "Analizando con GPT-4.1...",
      "generando_embeddings": "Generando embeddings con OpenAI...",
      "conectando_openai": "Conectando con OpenAI...",
      "usando_datos": "Usando datos de {source}...",
      "embeddings_obtenidos": "Embeddings obtenidos via OpenAI",
      "calculando_similitudes": "Calculando similitudes...",
      "completado": "Completado",
      "ejecutando_clustering": "Ejecutando clustering...",
      "buscando_similares": "Buscando fragmentos similares...",
      "generando_visualizacion": "Generando visualizaci\u00f3n 2D...",
      "analizando_coherencia": "Analizando coherencia...",
      "generando_red": "Generando red sem\u00e1ntica...",
      "generando_reporte": "Generando reporte con GPT-4.1...",
      "procesando_respuesta": "Procesando respuesta...",
      "visualizacion_completada": "Visualizaci\u00f3n completada",
      "validando_gpt": "Validando con GPT-4.1..."
    },
    "fuente": {
      "ia": "An\u00e1lisis IA",
      "manual": "codificaci\u00f3n manual"
    },
    "login": {
      "titulo": "Bienvenido a RCualiText",
      "subtitulo": "Herramienta de An\u00e1lisis Cualitativo de Texto",
      "descripcion": "Por favor, ingrese sus datos para continuar",
      "nombre": "Nombre Completo",
      "correo": "Correo Electr\u00f3nico",
      "universidad": "Universidad / Instituci\u00f3n",
      "residencia": "Lugar de Residencia",
      "boton": "Ingresar a la Aplicaci\u00f3n",
      "error_nombre": "Por favor ingrese su nombre completo",
      "error_correo": "Por favor ingrese un correo v\u00e1lido",
      "error_universidad": "Por favor ingrese su universidad o instituci\u00f3n",
      "error_residencia": "Por favor ingrese su lugar de residencia"
    }
  }
}
)--", simplifyDataFrame = FALSE)

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
    titleWidth = 280,
    tags$li(class = "dropdown",
      div(style = "padding: 10px 15px; display: flex; align-items: center; gap: 8px;",
        materialSwitch(inputId = "lang_toggle", label = "EN/ES", value = FALSE, status = "primary")
      )
    )
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenuOutput("dynamic_sidebar")
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
    # ===== Login overlay (covers entire viewport until registration) =====
    div(
      id = "login_overlay",
      style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 9999; background: linear-gradient(135deg, #2c3e50 0%, #34495e 50%, #2c3e50 100%); display: flex; align-items: center; justify-content: center; font-family: 'Source Sans Pro', sans-serif; padding: 20px;",
      uiOutput("login_form_ui")
    ),
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&family=Source+Serif+Pro:wght@400;600;700&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        /* Language toggle fix */
        .navbar-custom-menu .form-group.shiny-input-container {
          margin-bottom: 0 !important;
          display: inline-flex;
          align-items: center;
          gap: 12px;
        }
        .navbar-custom-menu label,
        .navbar-custom-menu .form-group label,
        .navbar-custom-menu .shiny-input-container label {
          color: #fff !important;
          font-weight: 600 !important;
          font-size: 14px !important;
          margin: 0 0 0 10px !important;
          padding: 0 !important;
        }

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
                  title = span(id = "box_panel_control", "Control Panel"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  fileInput("archivo",
                            div(icon("upload"), span(id = "lbl_cargar_docs", " Upload Documents")),
                            multiple = TRUE,
                            accept = c(".txt", ".docx"),
                            buttonLabel = "Browse...",
                            placeholder = "No file selected"),

                  # Panel de modos con diseño moderno
                  div(
                    class = "info-panel",
                    h5(icon("cog"), span(id = "h_modo_trabajo", " Work Mode"), style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    div(
                      style = "display: flex; flex-wrap: wrap; gap: 8px; justify-content: center;",
                      actionButton("modeSelect",
                                   div(icon("mouse-pointer"), span(id = "btn_mode_select", " Select")),
                                   class = "btn-primary btn-sm mode-button active"),
                      actionButton("modeDeselect",
                                   div(icon("eraser"), span(id = "btn_mode_deselect", " Deselect")),
                                   class = "btn-default btn-sm mode-button")
                    )
                  ),

                  # Controles de codificación mejorados
                  conditionalPanel(
                    condition = "input.modeSelect",
                    div(
                      style = "margin: 20px 0;",
                      selectInput("codigoTexto",
                                  div(icon("tag"), span(id = "lbl_codigo_aplicar", " Code to Apply")),
                                  choices = NULL),
                      div(
                        class = "info-panel",
                        checkboxInput("modoAcumulativo",
                                      div(icon("layer-group"), span(id = "lbl_modo_acumulativo", " Accumulative Mode")),
                                      value = TRUE),
                        helpText(span(id = "help_modo_acumulativo", "Allows applying multiple codes to the same fragment"),
                                 style = "color: #7f8c8d; font-size: 12px;")
                      )
                    )
                  ),

                  # Controles de navegación mejorados
                  div(
                    style = "margin: 20px 0;",
                    h5(icon("arrows-alt-h"), span(id = "h_navegacion", " Navigation"), style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    fluidRow(
                      column(6, actionButton("prev_doc",
                                             div(icon("chevron-left"), span(id = "btn_anterior", " Previous")),
                                             class = "btn-default btn-sm btn-block")),
                      column(6, actionButton("next_doc",
                                             div(icon("chevron-right"), span(id = "btn_siguiente", " Next")),
                                             class = "btn-default btn-sm btn-block"))
                    )
                  ),

                  # Botones de acción mejorados
                  div(
                    style = "margin: 20px 0;",
                    h5(icon("tools"), span(id = "h_acciones", " Actions"), style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    div(
                      style = "display: flex; flex-wrap: wrap; gap: 8px;",
                      actionButton("limpiarResaltados",
                                   div(icon("broom"), span(id = "btn_limpiar", " Clear")),
                                   class = "btn-default btn-sm"),
                      actionButton("ayuda",
                                   div(icon("question-circle"), span(id = "btn_ayuda", " Help")),
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
                  title = span(id = "box_visor_documento", "Document Viewer"),
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
                  title = span(id = "box_gestion_codigos", "Code Management"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    style = "space-y: 20px;",
                    textInput("new_codigo",
                              div(icon("tag"), span(id = "lbl_nombre_codigo", " Code Name")),
                              value = "",
                              placeholder = "E.g.: Positive emotions"),

                    div(
                      style = "margin: 20px 0;",
                      h5(icon("palette"), span(id = "h_color_codigo", " Code Color"), style = "color: #2c3e50; margin-bottom: 10px;"),
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
                                   div(icon("save"), span(id = "btn_guardar_codigo", " Save")),
                                   class = "btn-primary btn-sm"),
                      actionButton("deleteCodigo",
                                   div(icon("trash"), span(id = "btn_eliminar_codigo", " Delete")),
                                   class = "btn-default btn-sm")
                    )
                  )
                ),
                box(
                  width = 8,
                  title = span(id = "box_lista_codigos", "Code List"),
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
                  title = span(id = "box_gestion_categorias", "Category Management"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    textInput("new_categoria",
                              div(icon("folder"), span(id = "lbl_nombre_categoria", " Category Name")),
                              value = "",
                              placeholder = "E.g.: Emotional aspects"),

                    div(
                      style = "margin: 20px 0;",
                      selectizeInput("codigos_for_categoria",
                                     div(icon("tags"), span(id = "lbl_codigos_asociados", " Associated Codes")),
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(placeholder = "Select codes..."))
                    ),

                    div(
                      style = "display: flex; gap: 10px; margin-top: 25px;",
                      actionButton("addOrUpdateCategoria",
                                   div(icon("save"), span(id = "btn_guardar_cat", " Save")),
                                   class = "btn-primary btn-sm"),
                      actionButton("deleteCategoria",
                                   div(icon("trash"), span(id = "btn_eliminar_cat", " Delete")),
                                   class = "btn-default btn-sm")
                    )
                  )
                ),
                box(
                  width = 8,
                  height = 600,
                  title = span(id = "box_categorias_definidas", "Defined Categories"),
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
                  title = span(id = "box_gestion_extractos", "Coded Extract Management"),
                  status = "primary",
                  solidHeader = TRUE,

                  # Panel de controles mejorado
                  div(
                    class = "info-panel",
                    h5(icon("cogs"), span(id = "h_herramientas", " Management Tools"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    fluidRow(
                      column(4,
                             downloadButton("descarga",
                                            div(icon("download"), span(id = "btn_exportar_xlsx", " Export XLSX")),
                                            class = "btn-primary btn-sm btn-block")),
                      column(4,
                             actionButton("eliminarResalte",
                                          div(icon("minus-circle"), span(id = "btn_eliminar_seleccionado", " Delete Selected")),
                                          class = "btn-default btn-sm btn-block")),
                      column(4,
                             actionButton("eliminarTodosResaltes",
                                          div(icon("trash-alt"), span(id = "btn_limpiar_todo", " Clear All")),
                                          class = "btn-default btn-sm btn-block"))
                    )
                  ),

                  # Tabla de resaltados
                  DTOutput("tablaResaltes") %>% withSpinner(type = 4, color = "#5a6c7d"),

                  # Panel informativo mejorado
                  div(
                    class = "info-panel",
                    style = "margin-top: 20px;",
                    h5(icon("info-circle"), span(id = "h_guia_resaltados", " Highlight Guide"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
                      div(
                        h6(icon("paint-brush"), span(id = "h_visualizacion_ext", " Visualization"), style = "color: #2c3e50; margin-bottom: 8px;"),
                        tags$ul(
                          style = "font-size: 13px; color: #7f8c8d;",
                          tags$li(span(id = "li_gradientes", "Gradients indicate multiple codes")),
                          tags$li(span(id = "li_hover", "Hover shows applied codes")),
                          tags$li(span(id = "li_cada_fila", "Each row = one code per fragment"))
                        )
                      ),
                      div(
                        h6(icon("edit"), span(id = "h_edicion_ext", " Editing"), style = "color: #2c3e50; margin-bottom: 8px;"),
                        tags$ul(
                          style = "font-size: 13px; color: #7f8c8d;",
                          tags$li(span(id = "li_modo_desel", "Deselect mode to remove")),
                          tags$li(span(id = "li_seleccion_mult", "Multiple selection available")),
                          tags$li(span(id = "li_exportacion", "Full export to Excel"))
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
                  title = span(id = "box_configuracion_analisis", "Settings"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("chart-bar"), span(id = "h_opciones_visuales", " Visual Options"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    prettySwitch("fillToggle",
                                 span(id = "lbl_colorear_categoria", "Color by Category"),
                                 value = TRUE,
                                 status = "primary",
                                 fill = TRUE)
                  ),

                  # Controles de descarga personalizados
                  div(
                    class = "download-controls-container",
                    h5(icon("cogs"), span(id = "h_config_descarga", " Download Settings"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    
                    fluidRow(
                      column(6,
                             numericInput("plot_width",
                                          div(icon("arrows-alt-h"), span(id = "lbl_ancho", " Width (in)")),
                                          value = 12,
                                          min = 5,
                                          max = 20,
                                          step = 0.5)
                      ),
                      column(6,
                             numericInput("plot_height",
                                          div(icon("arrows-alt-v"), span(id = "lbl_alto", " Height (in)")),
                                          value = 8,
                                          min = 4,
                                          max = 16,
                                          step = 0.5)
                      )
                    ),

                    numericInput("plot_dpi",
                                 div(icon("expand"), span(id = "lbl_dpi", " Resolution (DPI)")),
                                 value = 600,
                                 min = 150,
                                 max = 1200,
                                 step = 50),

                    helpText(span(id = "help_config_aplicada", "Configuration applied to both charts"),
                             style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;")
                  )
                ),
                box(
                  width = 9,
                  height = 650,
                  title = span(id = "box_distribucion_codigos", "Code Distribution"),
                  status = "primary",
                  solidHeader = TRUE,
                  
                  plotlyOutput("plotCodigos", height = "580px") %>%
                    withSpinner(type = 6, color = "#5a6c7d")
                ),
                box(
                  width = 3,
                  title = span(id = "box_exportar_analisis", "Export"),
                  status = "primary",
                  solidHeader = TRUE,
                  downloadButton("download_distribucion_jpg",
                                 div(icon("download"), span(id = "btn_dist_jpg", " Distribution (JPG)")),
                                 class = "btn-primary btn-sm btn-block")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = span(id = "box_exportar_red", "Export Network"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  downloadButton("download_red_jpg",
                                 div(icon("download"), span(id = "btn_red_jpg", " Co-occurrence Network (JPG)")),
                                 class = "btn-primary btn-sm")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  height = 750,
                  title = span(id = "box_red_coocurrencia", "Co-occurrence Network and Centrality Analysis"),
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
                  title = span(id = "box_config_ia", "AI Analysis Settings"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("key"), span(id = "h_config_openai", " OpenAI Settings"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    passwordInput("openai_api_key",
                                  div(icon("lock"), span(id = "lbl_api_key", " OpenAI API Key")),
                                  placeholder = "sk-..."),
                    helpText(span(id = "help_api_key", "Uses the GPT-4.1 model from OpenAI. Get your API Key at platform.openai.com"),
                             style = "color: #7f8c8d; font-size: 12px;"),
                    div(
                      style = "margin-top: 10px; padding: 10px; background: #e8f4f8; border-radius: 5px;",
                      tags$small(
                        icon("info-circle"),
                        span(id = "info_api_key_req", " The analysis requires a valid OpenAI API Key."),
                        style = "color: #2980b9;"
                      )
                    )
                  ),

                  div(
                    class = "info-panel",
                    style = "margin-top: 20px;",
                    h5(icon("book"), span(id = "h_diccionario", " Code Dictionary"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    fileInput("dict_ia",
                              div(icon("upload"), span(id = "lbl_cargar_dict", " Upload Dictionary")),
                              accept = c(".csv", ".xlsx"),
                              buttonLabel = "Browse...",
                              placeholder = ".csv or .xlsx file"),
                    helpText(span(id = "help_columnas_req", "Must have columns: Category, Code, Definition"),
                             style = "color: #7f8c8d; font-size: 12px;")
                  ),

                  div(
                    style = "margin-top: 25px;",
                    actionButton("run_ia_analysis",
                                 div(icon("play"), span(id = "btn_ejecutar_ia", " Run AI Analysis")),
                                 class = "btn-primary btn-block", style = "margin-bottom: 10px;"),
                    downloadButton("download_ia_results",
                                   div(icon("download"), span(id = "btn_descargar_resultados", " Download Results (.xlsx)")),
                                   class = "btn-default btn-block"),
                    helpText(span(id = "help_resultados", "Results will be shown below. Download the table in Excel with the button above."),
                             style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;")
                  )
                ),
                box(
                  width = 8,
                  title = span(id = "box_resultados_ia", "AI Analysis Results"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), span(id = "h_instrucciones_ia", " Instructions"), style = "color: #2c3e50;"),
                    p(span(id = "p_instruccion_1", "1. Make sure you have documents uploaded in the 'Document' tab"),
                      style = "color: #7f8c8d;"),
                    p(span(id = "p_instruccion_2", "2. Enter your OpenAI API Key"),
                      style = "color: #7f8c8d;"),
                    p(span(id = "p_instruccion_3", "3. Upload a code dictionary with columns: Category, Code, Definition"),
                      style = "color: #7f8c8d;"),
                    p(span(id = "p_instruccion_4", "4. Run the analysis and review the results"),
                      style = "color: #7f8c8d;"),
                    p(span(id = "p_instruccion_5", "5. If satisfied, integrate the results into your manual analysis"),
                      style = "color: #7f8c8d;")
                  ),
                  DTOutput("tabla_ia_results") %>% withSpinner(type = 6, color = "#5a6c7d"),
                  div(
                    style = "margin-top: 15px;",
                    downloadButton("download_tabla_ia_excel",
                                   div(icon("file-excel"), span(id = "btn_descargar_tabla_excel", " Download Table (Excel)")),
                                   class = "btn-success btn-sm")
                  )
                )
              ),

              # Análisis visual de resultados IA
              fluidRow(
                box(
                  width = 12,
                  title = span(id = "box_visualizacion_ia", "AI Results Visualization"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  fluidRow(
                    column(6,
                           h5(icon("chart-bar"), span(id = "h_dist_codigos_ia", " Code Distribution"), style = "color: #2c3e50; margin-bottom: 15px;"),
                           plotlyOutput("plot_ia_distribucion", height = "400px") %>%
                             withSpinner(type = 6, color = "#5a6c7d"),
                           div(style = "margin-top: 10px;",
                               downloadButton("download_ia_distribucion_png",
                                              div(icon("image"), " PNG"),
                                              class = "btn-primary btn-sm"))
                    ),
                    column(6,
                           h5(icon("chart-pie"), span(id = "h_frag_categoria_ia", " Fragments by Category"), style = "color: #2c3e50; margin-bottom: 15px;"),
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
                  title = span(id = "box_config_semantico", "Semantic Analysis Settings"),
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    class = "info-panel",
                    h5(icon("key"), span(id = "h_config_sem", " Settings"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    p(span(id = "p_modulo_info", "This module uses the OpenAI API to generate embeddings and semantic analysis."),
                      style = "color: #7f8c8d; font-size: 12px;"),
                    tags$small(
                      icon("info-circle"),
                      span(id = "info_modelo_emb", " Embedding model: text-embedding-3-small"),
                      style = "color: #2980b9; display: block; margin-top: 10px;"
                    ),
                    tags$small(
                      icon("info-circle"),
                      span(id = "info_api_key_sem", " Enter your OpenAI API Key in the 'AI Analysis' tab"),
                      style = "color: #2980b9; display: block; margin-top: 5px;"
                    )
                  ),

                  div(
                    style = "margin-top: 20px;",
                    actionButton("btn_generar_embeddings",
                                 div(icon("brain"), span(id = "btn_gen_emb", " Generate Embeddings")),
                                 class = "btn-primary btn-block"),
                    helpText(span(id = "help_gen_emb", "Generates vector representations of coded fragments using OpenAI"),
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
                  title = span(id = "box_herramientas_sem", "Semantic Analysis Tools"),
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), span(id = "h_requisitos_sem", " Requirements"), style = "color: #2c3e50; margin-bottom: 10px;"),
                    p(span(id = "p_req_1", "1. Have coded fragments (use 'AI Analysis' or code manually)"),
                      style = "color: #7f8c8d; margin: 3px 0;"),
                    p(span(id = "p_req_2", "2. Generate embeddings (uses internal tokens automatically)"),
                      style = "color: #7f8c8d; margin: 3px 0;"),
                    p(span(id = "p_req_3", "3. Explore the semantic analysis tools"),
                      style = "color: #7f8c8d; margin: 3px 0;")
                  ),

                  # Grid de botones de funcionalidades
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 20px;",

                    # Clustering
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("object-group"), span(id = "h_clustering", " Semantic Clustering"), style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p(span(id = "p_clustering_desc", "Automatically groups similar fragments"), style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      numericInput("n_clusters_semantico", span(id = "lbl_num_clusters", "Number of clusters"), value = 3, min = 2, max = 20, step = 1),
                      actionButton("btn_clustering", div(icon("sitemap"), span(id = "btn_ejecutar_clustering", " Run Clustering")),
                                   class = "btn-primary btn-sm btn-block")
                    ),

                    # Similitud
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("exchange-alt"), span(id = "h_similitud", " Similarity Detection"), style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p(span(id = "p_similitud_desc", "Finds similar fragments with different codes"), style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      sliderInput("umbral_similitud", span(id = "lbl_umbral_sim", "Similarity threshold"), min = 0.5, max = 0.95, value = 0.8, step = 0.05),
                      actionButton("btn_similitud", div(icon("search"), span(id = "btn_detectar_sim", " Detect Similar")),
                                   class = "btn-primary btn-sm btn-block")
                    ),

                    # Visualización
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("project-diagram"), span(id = "h_vis_2d", " 2D Visualization"), style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p(span(id = "p_vis_desc", "Visualizes the semantic distribution of fragments"), style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      selectInput("metodo_visualizacion", span(id = "lbl_metodo_red", "Reduction method"),
                                  choices = c("PCA" = "pca", "t-SNE" = "tsne", "UMAP" = "umap"),
                                  selected = "pca"),
                      actionButton("btn_visualizar", div(icon("chart-area"), span(id = "btn_visualizar_lbl", " Visualize")),
                                   class = "btn-primary btn-sm btn-block")
                    ),

                    # Coherencia
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("check-double"), span(id = "h_coherencia", " Coherence Analysis"), style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p(span(id = "p_coherencia_desc", "Evaluates the semantic homogeneity of each code"), style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      br(),
                      actionButton("btn_coherencia", div(icon("tasks"), span(id = "btn_analizar_coh", " Analyze Coherence")),
                                   class = "btn-primary btn-sm btn-block")
                    )
                  ),

                  # Validación LLM (fila completa)
                  div(
                    style = "margin-top: 15px;",
                    div(
                      class = "semantico-card",
                      style = "background: #ffffff; padding: 15px; border-radius: 6px; border: 1px solid #e0e4e8;",
                      h5(icon("user-check"), span(id = "h_validacion_llm", " LLM Validation (Virtual Expert Panel)"), style = "color: #2c3e50; margin-bottom: 10px; font-weight: 600;"),
                      p(span(id = "p_validacion_desc", "A language model evaluates the quality of your coding"), style = "color: #7f8c8d; font-size: 12px; margin-bottom: 10px;"),
                      fluidRow(
                        column(8,
                               numericInput("n_fragmentos_validar", span(id = "lbl_frag_validar", "Fragments to validate (sample)"), value = 10, min = 1, max = 50, step = 1)
                        ),
                        column(4,
                               actionButton("btn_validacion", div(icon("gavel"), span(id = "btn_validar", " Validate")),
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
                  title = div(icon("download"), span(id = "box_config_descarga_fig", " Figure Download Settings")),
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
                        helpText(icon("info-circle"), span(id = "help_ajusta_dim", " Adjust dimensions before downloading"),
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
                  title = span(id = "box_resultados_sem", "Semantic Analysis Results"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  tabsetPanel(
                    id = "tabs_resultados_semantico",
                    type = "pills",

                    tabPanel(
                      title = div(icon("object-group"), span(id = "tab_clustering_lbl", " Clustering")),
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
                      title = div(icon("exchange-alt"), span(id = "tab_similitud_lbl", " Similarity")),
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
                      title = div(icon("project-diagram"), span(id = "tab_vis_lbl", " Visualization")),
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
                      title = div(icon("check-double"), span(id = "tab_coherencia_lbl", " Coherence")),
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
                      title = div(icon("project-diagram"), span(id = "tab_red_sem_lbl", " Semantic Network")),
                      value = "tab_red_semantica",
                      div(
                        style = "padding: 15px;",
                        fluidRow(
                          column(4,
                            div(
                              class = "info-panel",
                              style = "padding: 15px; margin-bottom: 15px;",
                              h5(icon("sliders-h"), span(id = "h_config_red", " Settings"), style = "color: #2c3e50; margin-bottom: 15px;"),
                              sliderInput("umbral_red_semantica",
                                          span(id = "lbl_umbral_conexion", "Connection threshold (similarity)"),
                                          min = 0.2, max = 0.9, value = 0.4, step = 0.05),
                              helpText(span(id = "help_umbral_conexion", "Codes with similarity above the threshold are connected"),
                                       style = "color: #7f8c8d; font-size: 11px;"),
                              selectInput("color_red_semantica",
                                          span(id = "lbl_colorear_por", "Color by"),
                                          choices = c("Category" = "categoria", "Community (detected)" = "comunidad"),
                                          selected = "categoria"),
                              actionButton("btn_generar_red",
                                           div(icon("project-diagram"), span(id = "btn_gen_red", " Generate Network")),
                                           class = "btn-primary btn-block",
                                           style = "margin-top: 15px;"),
                              div(style = "margin-top: 15px;",
                                  downloadButton("download_red_semantica_png",
                                                 div(icon("image"), span(id = "btn_descargar_png", " Download PNG")),
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
                      title = div(icon("user-check"), span(id = "tab_validacion_lbl", " LLM Validation")),
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
                  title = span(id = "box_config_reporte", "Report Settings"),
                  status = "primary",
                  solidHeader = TRUE,

                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), span(id = "h_info_reporte", " Information"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    p(span(id = "p_info_reporte_desc", "Generates an automatic interpretive report based on the analyses performed."),
                      style = "color: #7f8c8d; margin-bottom: 10px;"),
                    p(span(id = "p_info_reporte_modelo", "Uses the GPT-4.1 model from OpenAI to generate interpretive reports."),
                      style = "color: #7f8c8d; font-size: 12px;")
                  ),

                  hr(),

                  selectInput("idioma_reporte",
                              div(icon("language"), span(id = "lbl_idioma_reporte", " Report language")),
                              choices = c("English" = "en", "Español" = "es"),
                              selected = "en"),

                  selectInput("estilo_reporte",
                              div(icon("file-alt"), span(id = "lbl_estilo", " Writing style")),
                              choices = c(
                                "Academic (thesis/article)" = "academico",
                                "Technical (report)" = "tecnico",
                                "Popular (general)" = "divulgativo"
                              ),
                              selected = "academico"),

                  checkboxGroupInput("secciones_reporte",
                                     div(icon("list-check"), span(id = "lbl_secciones", " Sections to include")),
                                     choices = c(
                                       "Coding summary" = "codificacion",
                                       "Frequency analysis" = "frecuencias",
                                       "Semantic clustering" = "clustering",
                                       "Code coherence" = "coherencia",
                                       "Semantic network" = "red",
                                       "Key findings" = "hallazgos",
                                       "Limitations" = "limitaciones"
                                     ),
                                     selected = c("codificacion", "frecuencias", "hallazgos")),

                  hr(),

                  div(
                    class = "info-panel",
                    style = "background: #fff3cd; border-color: #ffc107;",
                    h5(icon("exclamation-triangle"), span(id = "h_requisitos_rep", " Requirements"), style = "color: #856404; margin-bottom: 10px;"),
                    tags$ul(
                      style = "color: #856404; font-size: 12px; padding-left: 20px;",
                      tags$li(span(id = "li_req_frag", "Have coded fragments (manual or AI)")),
                      tags$li(span(id = "li_req_emb", "For semantic analysis: have generated embeddings"))
                    )
                  ),

                  hr(),

                  actionButton("btn_generar_reporte",
                               div(icon("magic"), span(id = "btn_gen_reporte", " Generate Report")),
                               class = "btn-primary btn-block btn-lg"),

                  div(
                    style = "margin-top: 15px;",
                    downloadButton("btn_descargar_reporte",
                                   div(icon("file-word"), span(id = "btn_desc_docx", " Download (.docx)")),
                                   class = "btn-success btn-block")
                  )
                ),

                box(
                  width = 8,
                  title = span(id = "box_reporte_generado", "Generated Report"),
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
                  title = span(id = "box_guardar_proyecto", "Save Project"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("save"), span(id = "h_respaldo", " Data Backup"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    p(span(id = "p_guardar_desc", "Saves all your work including codes, categories and highlights."),
                      style = "color: #7f8c8d; margin-bottom: 20px;"),
                    downloadButton("saveState",
                                   div(icon("download"), span(id = "btn_descargar_estado", " Download State (.rds)")),
                                   class = "btn-primary")
                  )
                ),
                box(
                  width = 6,
                  title = span(id = "box_cargar_proyecto", "Load Project"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("upload"), span(id = "h_restaurar", " Restore Data"), style = "color: #2c3e50; margin-bottom: 15px;"),
                    p(span(id = "p_cargar_desc", "Load a previously saved project to continue working."),
                      style = "color: #7f8c8d; margin-bottom: 20px;"),
                    fileInput("loadState",
                              div(icon("folder-open"), span(id = "lbl_seleccionar_archivo", " Select File")),
                              accept = ".rds",
                              buttonLabel = "Browse...",
                              placeholder = ".rds file not selected")
                  )
                )
              )
      ),
      
      # ---- Cómo citar (diseño mejorado) ----
      tabItem("citar",
              fluidRow(
                box(
                  width = 12,
                  title = span(id = "box_como_citar", "How to Cite RCualiText"),
                  status = "primary",
                  solidHeader = TRUE,

                  # Header visual mejorado
                  div(
                    style = "text-align: center; padding: 30px; background: #2c3e50; margin: -25px -25px 25px -25px; color: white;",
                    div(
                      style = "font-size: 64px; margin-bottom: 15px;",
                      icon("quote-right")
                    ),
                    h2(span(id = "h_reconocimiento", "Academic Recognition"), style = "margin: 0; font-weight: 600;")
                  ),

                  # Cita principal
                  div(
                    class = "info-panel",
                    h3(icon("graduation-cap"), span(id = "h_cita_apa", " APA 7th Edition Citation"),
                       style = "color: #2c3e50; margin-bottom: 20px; font-weight: 600;"),
                    
                    div(
                      style = "background: #f4f6f9; padding: 25px; border-left: 5px solid #2c3e50; margin: 20px 0; font-family: 'Georgia', serif; font-size: 16px; line-height: 2; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.06);",
                      HTML("Ventura-León, J. (2026). <em>RCualiText</em> (v1.0) [Shiny app]. GitHub. https://github.com/jventural/RCualiText_App")
                    ),
                    
                    div(
                      style = "text-align: center; margin: 25px 0;",
                      actionButton("copycitation",
                                   div(icon("copy"), span(id = "btn_copiar_cita", " Copy Citation")),
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
                      h4(icon("info-circle"), span(id = "h_info_software", " Software Information"), style = "color: #2c3e50; margin-bottom: 15px;"),
                      div(
                        style = "space-y: 10px;",
                        div(strong(span(id = "lbl_autor", "Author: ")), "Dr. José Ventura-León"),
                        div(strong(span(id = "lbl_anio", "Year: ")), "2026"),
                        div(strong(span(id = "lbl_version", "Version: ")), "2.0"),
                        div(strong(span(id = "lbl_tipo", "Type: ")), span(id = "val_tipo", "Shiny app for qualitative analysis")),
                        div(strong(span(id = "lbl_repositorio", "Repository: ")),
                            tags$a("GitHub",
                                   href = "https://github.com/jventural/RCualiText_App",
                                   target = "_blank",
                                   style = "color: #2c3e50; text-decoration: none; font-weight: 500;"))
                      )
                    ),

                    # Nota importante
                    div(
                      class = "danger-panel",
                      h4(icon("exclamation-triangle"), span(id = "h_importante", " Important"), style = "color: #c0392b; margin-bottom: 15px;"),
                      p(span(id = "p_importante_desc", "If you use RCualiText in your research or academic work, we appreciate you including this citation to acknowledge the author's work and allow other researchers to access this tool."),
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
                  title = span(id = "box_acerca_de", "About RCualiText"),
                  status = "primary",
                  solidHeader = TRUE,

                  # Header mejorado
                  div(
                    style = "text-align: center; padding: 30px; background: #2c3e50; margin: -25px -25px 25px -25px; color: white;",
                    div(
                      style = "font-size: 64px; margin-bottom: 15px;",
                      icon("microscope")
                    ),
                    h2(span(id = "h_analisis_avanzado", "Advanced Qualitative Analysis"), style = "margin: 0; font-weight: 600;")
                  ),

                  # Descripción principal
                  div(
                    class = "info-panel",
                    p(span(id = "p_descripcion_1", "RCualiText is an advanced application for qualitative text coding that allows you to load documents (.txt and .docx), define codes and categories, highlight extracts of interest, and visualize code frequencies and co-occurrence networks."),
                      style = "font-size: 16px; line-height: 1.8; color: #2c3e50;"),
                    p(span(id = "p_descripcion_2", "With RCualiText you can interactively manage your code list, group them into categories, export your highlights to Excel, and graphically analyze your qualitative data through modern visualizations and network analysis."),
                      style = "font-size: 16px; line-height: 1.8; color: #2c3e50;")
                  ),

                  # Funcionalidades en grid
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 25px; margin: 30px 0;",

                    # Funcionalidades de Resaltado
                    div(
                      class = "info-panel",
                      h4(icon("highlighter"), span(id = "h_resaltado_inteligente", " Smart Highlighting"), style = "color: #2c3e50; margin-bottom: 15px;"),
                      tags$ul(
                        style = "line-height: 1.8; color: #2c3e50;",
                        tags$li(strong("Select Mode:"), span(id = "li_info_sel", " Apply codes to selected fragments")),
                        tags$li(strong("Deselect Mode:"), span(id = "li_info_desel", " Remove specific codes with one click")),
                        tags$li(strong("Accumulative Mode:"), span(id = "li_info_acum", " Multiple codes per fragment")),
                        tags$li(strong("Visual Gradients:"), span(id = "li_info_grad", " Multiple codes with visual effects")),
                        tags$li(strong("Tooltips:"), span(id = "li_info_tips", " Information on mouse hover")),
                        tags$li(strong("Export:"), span(id = "li_info_export", " Detailed data to Excel"))
                      )
                    ),

                    # Guía de uso
                    div(
                      class = "info-panel",
                      h4(icon("user-graduate"), span(id = "h_guia_deseleccion", " Deselection Guide"), style = "color: #2c3e50; margin-bottom: 15px;"),
                      tags$ol(
                        style = "line-height: 1.8; color: #2c3e50;",
                        tags$li(span(id = "li_guia_1", "Activate 'Deselect' mode in the control panel")),
                        tags$li(span(id = "li_guia_2", "Click directly on the highlighted text")),
                        tags$li(span(id = "li_guia_3", "Select which specific code to remove")),
                        tags$li(span(id = "li_guia_4", "Return to 'Select' mode to continue"))
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
  # ========================================
  # i18n - Language system
  # ========================================
  current_lang <- reactiveVal("en")

  observeEvent(input$lang_toggle, {
    current_lang(if (input$lang_toggle) "es" else "en")
  })

  # Also handle login language toggle
  observeEvent(input$login_lang_toggle, {
    current_lang(if (input$login_lang_toggle) "es" else "en")
  })

  tr <- function(key, ...) {
    lang <- current_lang()
    parts <- strsplit(key, "\\.")[[1]]
    val <- translations[[lang]]
    for (p in parts) { val <- val[[p]]; if (is.null(val)) return(key) }
    replacements <- list(...)
    for (nm in names(replacements)) {
      val <- gsub(paste0("\\{", nm, "\\}"), as.character(replacements[[nm]]), val)
    }
    val
  }

  # ========================================
  # Registration / Login system
  # ========================================
  error_registro <- reactiveVal("")

  output$login_form_ui <- renderUI({
    current_lang()  # Trigger re-render on language change
    tagList(
      # Language toggle in top-right corner
      div(
        style = "position: fixed; top: 20px; right: 20px; z-index: 10000; background: rgba(255,255,255,0.15); padding: 8px 16px; border-radius: 20px; backdrop-filter: blur(10px);",
        materialSwitch(inputId = "login_lang_toggle", label = "EN/ES", value = (current_lang() == "es"), status = "primary"),
        tags$style(HTML("
          #login_lang_toggle ~ .bootstrap-switch .bootstrap-switch-label { background: rgba(255,255,255,0.3) !important; }
        "))
      ),

      # Main card
      div(
        style = "background: #ffffff; border-radius: 16px; box-shadow: 0 20px 60px rgba(0,0,0,0.3); max-width: 480px; width: 100%; padding: 0; overflow: hidden;",

        # Header
        div(
          style = "background: linear-gradient(135deg, #2980b9, #3498db); padding: 35px 40px; text-align: center;",
          tags$i(class = "fa fa-microscope", style = "font-size: 48px; color: #fff; margin-bottom: 12px; display: block;"),
          h2(tr("login.titulo"), style = "color: #fff; margin: 0 0 8px 0; font-weight: 700; font-size: 26px;"),
          p(tr("login.subtitulo"), style = "color: rgba(255,255,255,0.85); margin: 0 0 4px 0; font-size: 14px; font-weight: 300;"),
          p(tr("login.descripcion"), style = "color: rgba(255,255,255,0.7); margin: 0; font-size: 13px; font-weight: 300;")
        ),

        # Form body
        div(
          style = "padding: 35px 40px;",

          tags$style(HTML("
            .login-field .form-group { margin-bottom: 20px; }
            .login-field .form-control {
              border: 2px solid #e0e6ed;
              border-radius: 8px;
              padding: 12px 15px;
              font-size: 14px;
              transition: border-color 0.3s, box-shadow 0.3s;
              background: #f8f9fa;
            }
            .login-field .form-control:focus {
              border-color: #2980b9;
              box-shadow: 0 0 0 3px rgba(41,128,185,0.15);
              background: #fff;
            }
            .login-field label {
              font-weight: 600;
              color: #2c3e50;
              font-size: 13px;
              margin-bottom: 6px;
            }
            .btn-registro {
              background: linear-gradient(135deg, #2980b9, #3498db);
              border: none;
              color: #fff;
              padding: 14px;
              font-size: 16px;
              font-weight: 600;
              border-radius: 8px;
              width: 100%;
              cursor: pointer;
              transition: transform 0.2s, box-shadow 0.2s;
              margin-top: 10px;
            }
            .btn-registro:hover {
              transform: translateY(-2px);
              box-shadow: 0 8px 25px rgba(41,128,185,0.4);
            }
            .error-msg {
              color: #c0392b;
              background: #fdeaea;
              padding: 10px 15px;
              border-radius: 6px;
              font-size: 13px;
              margin-bottom: 15px;
              border-left: 3px solid #c0392b;
            }
          ")),

          # Error message
          conditionalPanel(
            condition = "output.show_registro_error",
            div(class = "error-msg", textOutput("registro_error_text"))
          ),

          div(class = "login-field",
            textInput("reg_nombre", tr("login.nombre"), placeholder = tr("login.nombre")),
            textInput("reg_correo", tr("login.correo"), placeholder = "example@university.edu"),
            textInput("reg_universidad", tr("login.universidad"), placeholder = tr("login.universidad")),
            textInput("reg_residencia", tr("login.residencia"), placeholder = tr("login.residencia"))
          ),

          actionButton("btn_registro", tr("login.boton"), class = "btn-registro",
                       icon = icon("sign-in-alt"))
        ),

        # Footer
        div(
          style = "padding: 15px 40px; background: #f8f9fa; text-align: center; border-top: 1px solid #e0e6ed;",
          p(style = "margin: 0; color: #95a5a6; font-size: 12px;",
            "Dr. Jos\u00e9 Ventura-Le\u00f3n | RCualiText v1.0")
        )
      )
    )
  })

  # Error display helpers
  output$show_registro_error <- reactive({ nchar(error_registro()) > 0 })
  outputOptions(output, "show_registro_error", suspendWhenHidden = FALSE)
  output$registro_error_text <- renderText({ error_registro() })

  # Registration button handler
  observeEvent(input$btn_registro, {
    nombre <- trimws(input$reg_nombre %||% "")
    correo <- trimws(input$reg_correo %||% "")
    universidad <- trimws(input$reg_universidad %||% "")
    residencia <- trimws(input$reg_residencia %||% "")

    # Validations
    if (nchar(nombre) == 0) {
      error_registro(tr("login.error_nombre"))
      return()
    }
    if (!grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", correo)) {
      error_registro(tr("login.error_correo"))
      return()
    }
    if (nchar(universidad) == 0) {
      error_registro(tr("login.error_universidad"))
      return()
    }
    if (nchar(residencia) == 0) {
      error_registro(tr("login.error_residencia"))
      return()
    }

    # Save registration
    guardar_registro(nombre, correo, universidad, residencia)

    # Clear error and hide login overlay
    error_registro("")
    shinyjs::hide("login_overlay")
  })

  dt_language <- function() {
    list(
      search = tr("datatable.search"),
      lengthMenu = tr("datatable.lengthMenu"),
      info = tr("datatable.info"),
      paginate = list(
        previous = tr("datatable.paginate_previous"),
        `next` = tr("datatable.paginate_next")
      )
    )
  }

  # Dynamic sidebar
  output$dynamic_sidebar <- renderMenu({
    current_lang()
    sidebarMenu(id = "sidebar",
      menuItem(tr("sidebar.documento"), tabName = "texto", icon = icon("file-text")),
      menuItem(tr("sidebar.codigos"), tabName = "codigos", icon = icon("tags")),
      menuItem(tr("sidebar.categorias"), tabName = "categorias", icon = icon("folder-open")),
      menuItem(tr("sidebar.extractos"), tabName = "resaltes", icon = icon("highlighter")),
      menuItem(tr("sidebar.analisis"), tabName = "analisis", icon = icon("chart-bar")),
      menuItem(tr("sidebar.analisis_ia"), tabName = "analisis_ia", icon = icon("robot")),
      menuItem(tr("sidebar.analisis_semantico"), tabName = "analisis_semantico", icon = icon("brain")),
      menuItem(tr("sidebar.reporte_ia"), tabName = "reporte_ia", icon = icon("file-alt")),
      menuItem(tr("sidebar.proyecto"), tabName = "estado", icon = icon("save")),
      menuItem(tr("sidebar.citar"), tabName = "citar", icon = icon("quote-right")),
      menuItem(tr("sidebar.ayuda"), tabName = "info", icon = icon("info-circle"))
    )
  })

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
  # Master language observer - updates all UI text
  # ========================================
  observeEvent(current_lang(), {
    lang <- current_lang()
    # --- Document tab ---
    shinyjs::html("box_panel_control", tr("documento.panel_control"))
    shinyjs::html("lbl_cargar_docs", paste0(" ", tr("documento.cargar_documentos")))
    shinyjs::html("h_modo_trabajo", paste0(" ", tr("documento.modo_trabajo")))
    shinyjs::html("btn_mode_select", paste0(" ", tr("documento.seleccionar")))
    shinyjs::html("btn_mode_deselect", paste0(" ", tr("documento.deseleccionar")))
    shinyjs::html("lbl_codigo_aplicar", paste0(" ", tr("documento.codigo_aplicar")))
    shinyjs::html("lbl_modo_acumulativo", paste0(" ", tr("documento.modo_acumulativo")))
    shinyjs::html("help_modo_acumulativo", tr("documento.modo_acumulativo_help"))
    shinyjs::html("h_navegacion", paste0(" ", tr("documento.navegacion")))
    shinyjs::html("btn_anterior", paste0(" ", tr("documento.anterior")))
    shinyjs::html("btn_siguiente", paste0(" ", tr("documento.siguiente")))
    shinyjs::html("h_acciones", paste0(" ", tr("documento.acciones")))
    shinyjs::html("btn_limpiar", paste0(" ", tr("documento.limpiar")))
    shinyjs::html("btn_ayuda", paste0(" ", tr("documento.ayuda_btn")))
    shinyjs::html("box_visor_documento", tr("documento.visor_documento"))
    # --- Codes tab ---
    shinyjs::html("box_gestion_codigos", tr("codigos.gestion_codigos"))
    shinyjs::html("lbl_nombre_codigo", paste0(" ", tr("codigos.nombre_codigo")))
    shinyjs::html("h_color_codigo", paste0(" ", tr("codigos.color_codigo")))
    shinyjs::html("btn_guardar_codigo", paste0(" ", tr("codigos.guardar")))
    shinyjs::html("btn_eliminar_codigo", paste0(" ", tr("codigos.eliminar")))
    shinyjs::html("box_lista_codigos", tr("codigos.lista_codigos"))
    # --- Categories tab ---
    shinyjs::html("box_gestion_categorias", tr("categorias.gestion_categorias"))
    shinyjs::html("lbl_nombre_categoria", paste0(" ", tr("categorias.nombre_categoria")))
    shinyjs::html("lbl_codigos_asociados", paste0(" ", tr("categorias.codigos_asociados")))
    shinyjs::html("btn_guardar_cat", paste0(" ", tr("categorias.guardar")))
    shinyjs::html("btn_eliminar_cat", paste0(" ", tr("categorias.eliminar")))
    shinyjs::html("box_categorias_definidas", tr("categorias.categorias_definidas"))
    # --- Extracts tab ---
    shinyjs::html("box_gestion_extractos", tr("extractos.gestion_extractos"))
    shinyjs::html("h_herramientas", paste0(" ", tr("extractos.herramientas")))
    shinyjs::html("btn_exportar_xlsx", paste0(" ", tr("extractos.exportar_xlsx")))
    shinyjs::html("btn_eliminar_seleccionado", paste0(" ", tr("extractos.eliminar_seleccionado")))
    shinyjs::html("btn_limpiar_todo", paste0(" ", tr("extractos.limpiar_todo")))
    shinyjs::html("h_guia_resaltados", paste0(" ", tr("extractos.guia_resaltados")))
    shinyjs::html("h_visualizacion_ext", paste0(" ", tr("extractos.visualizacion")))
    shinyjs::html("li_gradientes", tr("extractos.gradientes_multiples"))
    shinyjs::html("li_hover", tr("extractos.hover_codigos"))
    shinyjs::html("li_cada_fila", tr("extractos.cada_fila"))
    shinyjs::html("h_edicion_ext", paste0(" ", tr("extractos.edicion")))
    shinyjs::html("li_modo_desel", tr("extractos.modo_deseleccionar_eliminar"))
    shinyjs::html("li_seleccion_mult", tr("extractos.seleccion_multiple"))
    shinyjs::html("li_exportacion", tr("extractos.exportacion_excel"))
    # --- Analysis tab ---
    shinyjs::html("box_configuracion_analisis", tr("analisis.configuracion"))
    shinyjs::html("h_opciones_visuales", paste0(" ", tr("analisis.opciones_visuales")))
    shinyjs::html("lbl_colorear_categoria", tr("analisis.colorear_categoria"))
    shinyjs::html("h_config_descarga", paste0(" ", tr("analisis.config_descarga")))
    shinyjs::html("lbl_ancho", paste0(" ", tr("analisis.ancho_pulg")))
    shinyjs::html("lbl_alto", paste0(" ", tr("analisis.alto_pulg")))
    shinyjs::html("lbl_dpi", paste0(" ", tr("analisis.resolucion_dpi")))
    shinyjs::html("help_config_aplicada", tr("analisis.config_aplicada"))
    shinyjs::html("box_distribucion_codigos", tr("analisis.distribucion_codigos"))
    shinyjs::html("box_exportar_analisis", tr("analisis.exportar"))
    shinyjs::html("btn_dist_jpg", paste0(" ", tr("analisis.distribucion_jpg")))
    shinyjs::html("box_exportar_red", tr("analisis.exportar_red"))
    shinyjs::html("btn_red_jpg", paste0(" ", tr("analisis.red_coocurrencia_jpg")))
    shinyjs::html("box_red_coocurrencia", tr("analisis.red_coocurrencia_centralidad"))
    # --- AI Analysis tab ---
    shinyjs::html("box_config_ia", tr("analisis_ia.config_analisis_ia"))
    shinyjs::html("h_config_openai", paste0(" ", tr("analisis_ia.config_openai")))
    shinyjs::html("lbl_api_key", paste0(" ", tr("analisis_ia.api_key_openai")))
    shinyjs::html("help_api_key", tr("analisis_ia.api_key_help"))
    shinyjs::html("info_api_key_req", paste0(" ", tr("analisis_ia.api_key_requerida")))
    shinyjs::html("h_diccionario", paste0(" ", tr("analisis_ia.diccionario_codigos")))
    shinyjs::html("lbl_cargar_dict", paste0(" ", tr("analisis_ia.cargar_diccionario")))
    shinyjs::html("help_columnas_req", tr("analisis_ia.columnas_requeridas"))
    shinyjs::html("btn_ejecutar_ia", paste0(" ", tr("analisis_ia.ejecutar_analisis")))
    shinyjs::html("btn_descargar_resultados", paste0(" ", tr("analisis_ia.descargar_resultados")))
    shinyjs::html("help_resultados", tr("analisis_ia.resultados_help"))
    shinyjs::html("box_resultados_ia", tr("analisis_ia.resultados_analisis_ia"))
    shinyjs::html("h_instrucciones_ia", paste0(" ", tr("analisis_ia.instrucciones")))
    shinyjs::html("p_instruccion_1", tr("analisis_ia.instruccion_1"))
    shinyjs::html("p_instruccion_2", tr("analisis_ia.instruccion_2"))
    shinyjs::html("p_instruccion_3", tr("analisis_ia.instruccion_3"))
    shinyjs::html("p_instruccion_4", tr("analisis_ia.instruccion_4"))
    shinyjs::html("p_instruccion_5", tr("analisis_ia.instruccion_5"))
    shinyjs::html("btn_descargar_tabla_excel", paste0(" ", tr("analisis_ia.descargar_tabla_excel")))
    shinyjs::html("box_visualizacion_ia", tr("analisis_ia.visualizacion_resultados"))
    shinyjs::html("h_dist_codigos_ia", paste0(" ", tr("analisis_ia.distribucion_codigos_ia")))
    shinyjs::html("h_frag_categoria_ia", paste0(" ", tr("analisis_ia.fragmentos_categoria")))
    # --- Semantic Analysis tab ---
    shinyjs::html("box_config_semantico", tr("analisis_semantico.config_analisis_semantico"))
    shinyjs::html("h_config_sem", paste0(" ", tr("analisis_semantico.configuracion")))
    shinyjs::html("p_modulo_info", tr("analisis_semantico.modulo_info"))
    shinyjs::html("info_modelo_emb", paste0(" ", tr("analisis_semantico.modelo_embeddings")))
    shinyjs::html("info_api_key_sem", paste0(" ", tr("analisis_semantico.api_key_info")))
    shinyjs::html("btn_gen_emb", paste0(" ", tr("analisis_semantico.generar_embeddings")))
    shinyjs::html("help_gen_emb", tr("analisis_semantico.generar_embeddings_help"))
    shinyjs::html("box_herramientas_sem", tr("analisis_semantico.herramientas_analisis"))
    shinyjs::html("h_requisitos_sem", paste0(" ", tr("analisis_semantico.requisitos")))
    shinyjs::html("p_req_1", tr("analisis_semantico.req_1"))
    shinyjs::html("p_req_2", tr("analisis_semantico.req_2"))
    shinyjs::html("p_req_3", tr("analisis_semantico.req_3"))
    shinyjs::html("h_clustering", paste0(" ", tr("analisis_semantico.clustering_semantico")))
    shinyjs::html("p_clustering_desc", tr("analisis_semantico.clustering_desc"))
    shinyjs::html("lbl_num_clusters", tr("analisis_semantico.num_clusters"))
    shinyjs::html("btn_ejecutar_clustering", paste0(" ", tr("analisis_semantico.ejecutar_clustering")))
    shinyjs::html("h_similitud", paste0(" ", tr("analisis_semantico.deteccion_similitud")))
    shinyjs::html("p_similitud_desc", tr("analisis_semantico.similitud_desc"))
    shinyjs::html("lbl_umbral_sim", tr("analisis_semantico.umbral_similitud"))
    shinyjs::html("btn_detectar_sim", paste0(" ", tr("analisis_semantico.detectar_similares")))
    shinyjs::html("h_vis_2d", paste0(" ", tr("analisis_semantico.visualizacion_2d")))
    shinyjs::html("p_vis_desc", tr("analisis_semantico.visualizacion_desc"))
    shinyjs::html("lbl_metodo_red", tr("analisis_semantico.metodo_reduccion"))
    shinyjs::html("btn_visualizar_lbl", paste0(" ", tr("analisis_semantico.visualizar")))
    shinyjs::html("h_coherencia", paste0(" ", tr("analisis_semantico.analisis_coherencia")))
    shinyjs::html("p_coherencia_desc", tr("analisis_semantico.coherencia_desc"))
    shinyjs::html("btn_analizar_coh", paste0(" ", tr("analisis_semantico.analizar_coherencia")))
    shinyjs::html("h_validacion_llm", paste0(" ", tr("analisis_semantico.validacion_llm")))
    shinyjs::html("p_validacion_desc", tr("analisis_semantico.validacion_desc"))
    shinyjs::html("lbl_frag_validar", tr("analisis_semantico.fragmentos_validar"))
    shinyjs::html("btn_validar", paste0(" ", tr("analisis_semantico.validar")))
    shinyjs::html("box_config_descarga_fig", paste0(" ", tr("analisis_semantico.config_descarga_figuras")))
    shinyjs::html("help_ajusta_dim", paste0(" ", tr("analisis_semantico.ajusta_dimensiones")))
    shinyjs::html("box_resultados_sem", tr("analisis_semantico.resultados_analisis_semantico"))
    shinyjs::html("tab_clustering_lbl", paste0(" ", tr("analisis_semantico.tab_clustering")))
    shinyjs::html("tab_similitud_lbl", paste0(" ", tr("analisis_semantico.tab_similitud")))
    shinyjs::html("tab_vis_lbl", paste0(" ", tr("analisis_semantico.tab_visualizacion")))
    shinyjs::html("tab_coherencia_lbl", paste0(" ", tr("analisis_semantico.tab_coherencia")))
    shinyjs::html("tab_red_sem_lbl", paste0(" ", tr("analisis_semantico.tab_red_semantica")))
    shinyjs::html("tab_validacion_lbl", paste0(" ", tr("analisis_semantico.tab_validacion")))
    shinyjs::html("h_config_red", paste0(" ", tr("analisis_semantico.config_red")))
    shinyjs::html("lbl_umbral_conexion", tr("analisis_semantico.umbral_conexion"))
    shinyjs::html("help_umbral_conexion", tr("analisis_semantico.umbral_conexion_help"))
    shinyjs::html("lbl_colorear_por", tr("analisis_semantico.colorear_por"))
    shinyjs::html("btn_gen_red", paste0(" ", tr("analisis_semantico.generar_red")))
    shinyjs::html("btn_descargar_png", paste0(" ", tr("analisis_semantico.descargar_png")))
    # --- Report tab ---
    shinyjs::html("box_config_reporte", tr("reporte.config_reporte"))
    shinyjs::html("h_info_reporte", paste0(" ", tr("reporte.informacion")))
    shinyjs::html("p_info_reporte_desc", tr("reporte.info_desc"))
    shinyjs::html("p_info_reporte_modelo", tr("reporte.info_modelo"))
    shinyjs::html("lbl_idioma_reporte", paste0(" ", tr("reporte.idioma_reporte")))
    shinyjs::html("lbl_estilo", paste0(" ", tr("reporte.estilo_redaccion")))
    shinyjs::html("lbl_secciones", paste0(" ", tr("reporte.secciones_incluir")))
    shinyjs::html("h_requisitos_rep", paste0(" ", tr("reporte.requisitos")))
    shinyjs::html("li_req_frag", tr("reporte.req_fragmentos"))
    shinyjs::html("li_req_emb", tr("reporte.req_embeddings"))
    shinyjs::html("btn_gen_reporte", paste0(" ", tr("reporte.generar_reporte")))
    shinyjs::html("btn_desc_docx", paste0(" ", tr("reporte.descargar_docx")))
    shinyjs::html("box_reporte_generado", tr("reporte.reporte_generado"))
    # --- Project tab ---
    shinyjs::html("box_guardar_proyecto", tr("proyecto.guardar_proyecto"))
    shinyjs::html("h_respaldo", paste0(" ", tr("proyecto.respaldo_datos")))
    shinyjs::html("p_guardar_desc", tr("proyecto.guardar_desc"))
    shinyjs::html("btn_descargar_estado", paste0(" ", tr("proyecto.descargar_estado")))
    shinyjs::html("box_cargar_proyecto", tr("proyecto.cargar_proyecto"))
    shinyjs::html("h_restaurar", paste0(" ", tr("proyecto.restaurar_datos")))
    shinyjs::html("p_cargar_desc", tr("proyecto.cargar_desc"))
    shinyjs::html("lbl_seleccionar_archivo", paste0(" ", tr("proyecto.seleccionar_archivo")))
    # --- Cite tab ---
    shinyjs::html("box_como_citar", tr("citar.como_citar"))
    shinyjs::html("h_reconocimiento", tr("citar.reconocimiento"))
    shinyjs::html("h_cita_apa", paste0(" ", tr("citar.cita_apa")))
    shinyjs::html("btn_copiar_cita", paste0(" ", tr("citar.copiar_cita")))
    shinyjs::html("h_info_software", paste0(" ", tr("citar.info_software")))
    shinyjs::html("lbl_autor", tr("citar.autor"))
    shinyjs::html("lbl_anio", tr("citar.anio"))
    shinyjs::html("lbl_version", tr("citar.version"))
    shinyjs::html("lbl_tipo", tr("citar.tipo"))
    shinyjs::html("val_tipo", tr("citar.tipo_valor"))
    shinyjs::html("lbl_repositorio", tr("citar.repositorio"))
    shinyjs::html("h_importante", paste0(" ", tr("citar.importante")))
    shinyjs::html("p_importante_desc", tr("citar.importante_desc"))
    # --- Help tab ---
    shinyjs::html("box_acerca_de", tr("info.acerca_de"))
    shinyjs::html("h_analisis_avanzado", tr("info.analisis_avanzado"))
    shinyjs::html("p_descripcion_1", tr("info.descripcion_1"))
    shinyjs::html("p_descripcion_2", tr("info.descripcion_2"))
    shinyjs::html("h_resaltado_inteligente", paste0(" ", tr("info.resaltado_inteligente")))
    shinyjs::html("li_info_sel", paste0(" ", tr("info.modo_seleccionar_desc")))
    shinyjs::html("li_info_desel", paste0(" ", tr("info.modo_deseleccionar_desc")))
    shinyjs::html("li_info_acum", paste0(" ", tr("info.modo_acumulativo_desc")))
    shinyjs::html("li_info_grad", paste0(" ", tr("info.gradientes_desc")))
    shinyjs::html("li_info_tips", paste0(" ", tr("info.tooltips_desc")))
    shinyjs::html("li_info_export", paste0(" ", tr("info.exportacion_desc")))
    shinyjs::html("h_guia_deseleccion", paste0(" ", tr("info.guia_deseleccion")))
    shinyjs::html("li_guia_1", tr("info.guia_paso_1"))
    shinyjs::html("li_guia_2", tr("info.guia_paso_2"))
    shinyjs::html("li_guia_3", tr("info.guia_paso_3"))
    shinyjs::html("li_guia_4", tr("info.guia_paso_4"))

    # --- Update select inputs with translated choices ---
    updateSelectInput(session, "estilo_reporte",
      choices = stats::setNames(
        c("academico", "tecnico", "divulgativo"),
        c(tr("reporte.academico"), tr("reporte.tecnico"), tr("reporte.divulgativo"))
      ),
      selected = input$estilo_reporte)
    updateCheckboxGroupInput(session, "secciones_reporte",
      choices = stats::setNames(
        c("codificacion", "frecuencias", "clustering", "coherencia", "red", "hallazgos", "limitaciones"),
        c(tr("reporte.sec_codificacion"), tr("reporte.sec_frecuencias"), tr("reporte.sec_clustering"),
          tr("reporte.sec_coherencia"), tr("reporte.sec_red"), tr("reporte.sec_hallazgos"), tr("reporte.sec_limitaciones"))
      ),
      selected = input$secciones_reporte)
    updateSelectInput(session, "color_red_semantica",
      choices = stats::setNames(
        c("categoria", "comunidad"),
        c(tr("analisis_semantico.categoria_opt"), tr("analisis_semantico.comunidad_opt"))
      ),
      selected = input$color_red_semantica)
  })

  # Sync idioma_reporte with global toggle (user can override manually)
  idioma_reporte_synced <- reactiveVal(TRUE)

  observeEvent(input$idioma_reporte, {
    if (!identical(input$idioma_reporte, current_lang())) {
      idioma_reporte_synced(FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(current_lang(), {
    if (idioma_reporte_synced()) {
      updateSelectInput(session, "idioma_reporte", selected = current_lang())
    }
    idioma_reporte_synced(TRUE)
  }, priority = -1)

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
                                  code_colors = code_colors,
                                  labels = list(freq = tr("plots.frecuencia"), codes = tr("plots.codigos_x"),
                                                cat = tr("plots.categoria_fill"), code = tr("plots.codigo_label"),
                                                sin_cat = tr("plots.sin_categoria")))

        # Guardar como JPG con parámetros personalizados
        ggsave(file, plot = p, device = "jpeg",
               width = ancho, height = alto, dpi = dpi, bg = "white")

        showNotification(
          tr("notifications.grafico_distribucion_descargado", w = ancho, h = alto, dpi = dpi),
          type = "message", duration = 4
        )
      } else {
        showNotification(tr("notifications.no_datos_descargar"), type = "error", duration = 3)
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
          tr("notifications.grafico_red_descargado", w = ancho, h = alto, dpi = dpi),
          type = "message", duration = 4
        )
      } else {
        showNotification(tr("notifications.no_datos_descargar"), type = "error", duration = 3)
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
        h5(icon("mouse-pointer"), paste0(" ", tr("modes.seleccionar_activo")), style = "color: #2c3e50; margin-bottom: 15px;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            h6(tr("modes.acciones_disponibles"), style = "color: #2c3e50; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #7f8c8d; margin: 0;",
              tags$li(tr("modes.selecciona_texto")),
              tags$li(tr("modes.aplica_codigos"))
            )
          ),
          div(
            h6(tr("modes.caracteristicas"), style = "color: #2c3e50; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #7f8c8d; margin: 0;",
              tags$li(tr("modes.acumulativo_disponible")),
              tags$li(tr("modes.clic_info"))
            )
          )
        )
      )
    })

    showNotification(tr("notifications.modo_seleccionar"), type = "message", duration = 2)
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
        h5(icon("eraser"), paste0(" ", tr("modes.deseleccionar_activo")), style = "color: #c0392b; margin-bottom: 15px;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            h6(tr("modes.instrucciones_deseleccion"), style = "color: #c0392b; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #a93226; margin: 0;",
              tags$li(tr("modes.clic_resaltado")),
              tags$li(tr("modes.elige_codigo"))
            )
          ),
          div(
            h6(tr("modes.importante"), style = "color: #c0392b; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #a93226; margin: 0;",
              tags$li(tr("modes.cursor_cambia")),
              tags$li(tr("modes.vuelve_seleccionar"))
            )
          )
        )
      )
    })
    
    showNotification(tr("notifications.modo_deseleccionar"),
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
      showNotification(tr("notifications.no_codigos_fragmento"), type = "error", duration = 3)
      return()
    }
    
    if (nrow(fragmentos_asociados) == 1) {
      # Solo un código - eliminar directamente
      codigo_eliminar <- fragmentos_asociados$Codigo[1]
      
      showModal(modalDialog(
        title = div(icon("exclamation-triangle"), paste0(" ", tr("modals.confirmar_eliminacion"))),
        div(
          h4(tr("modals.eliminar_resaltado"), style = "color: #c0392b;"),
          div(
            class = "info-panel",
            strong(tr("modals.texto")), str_trunc(fragment_text, 50), br(),
            strong(tr("modals.codigo_label")), span(codigo_eliminar, style = paste0("background:", fragmentos_asociados$Color[1], "; padding: 4px 8px; border-radius: 4px; color: white;")), br(),
            strong(tr("modals.archivo_label")), fragmentos_asociados$Archivo[1]
          )
        ),
        footer = tagList(
          modalButton(tr("modals.cancelar")),
          actionButton("confirmarEliminacionUnica", tr("modals.eliminar"), class = "btn-default")
        )
      ))
      
      # Guardar el fragmento para eliminación
      rv$fragmento_a_eliminar <- fragmentos_asociados[1, ]
      
    } else {
      # Múltiples códigos - mostrar opciones
      showModal(modalDialog(
        title = div(icon("list"), paste0(" ", tr("modals.seleccionar_codigo_eliminar"))),
        div(
          h4(tr("modals.multiples_codigos"), style = "color: #2c3e50;"),
          div(
            class = "info-panel",
            strong(tr("modals.texto")), str_trunc(fragment_text, 50)
          ),
          h5(tr("modals.codigos_aplicados")),
          DTOutput("tablaCodigosFragmento")
        ),
        footer = tagList(
          modalButton(tr("modals.cancelar")),
          actionButton("eliminarCodigoSeleccionado", tr("modals.eliminar_seleccionado"), class = "btn-default")
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
          colnames = c(tr("colnames.codigo"), tr("colnames.categoria"), tr("colnames.aplicado"))
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
    showNotification(tr("notifications.codigo_eliminado_fragmento", code = frag$Codigo),
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
    showNotification(tr("notifications.codigo_eliminado_fragmento", code = frag_eliminar$Codigo),
                     type = "message", duration = 3)

    # Limpiar variables temporales
    rv$fragmentos_multiples <- NULL
  })
  
  # ========================================
  # CRUD códigos
  # ========================================
  
  output$tablaCodigos <- renderDT({
    current_lang()
    datatable(
      rv$codigosDF,
      selection = "single",
      options = list(
        pageLength = 8,
        dom = 'frtip',
        language = list(
          search = tr("datatable.search"),
          lengthMenu = tr("datatable.lengthMenu_codigos"),
          info = tr("datatable.info_codigos"),
          paginate = list(previous = tr("datatable.paginate_previous"), `next` = tr("datatable.paginate_next"))
        )
      ),
      colnames = c(tr("colnames.codigo"), tr("colnames.color"))
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
      showNotification(tr("notifications.codigo_actualizado", code = input$new_codigo), type = "message", duration = 2)
    } else {
      df <- bind_rows(df, tibble(Codigo=input$new_codigo, Color=input$new_color))
      showNotification(tr("notifications.codigo_anadido", code = input$new_codigo), type = "message", duration = 2)
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
      title = div(icon("exclamation-triangle"), paste0(" ", tr("modals.confirmar_eliminacion_codigo"))),
      div(
        h4(tr("modals.eliminar_codigo_pregunta", code = codigo_eliminar), style = "color: #c0392b;"),
        if(resaltados_afectados > 0) {
          div(
            class = "danger-panel",
            p(tr("notifications.eliminar_resaltados_asociados", n = resaltados_afectados),
              style = "color: #c0392b; font-weight: bold;")
          )
        } else {
          div(
            class = "info-panel",
            p(tr("notifications.este_codigo_no_resaltados"), style = "color: #7f8c8d;")
          )
        }
      ),
      footer = tagList(
        modalButton(tr("modals.cancelar")),
        actionButton("confirmarEliminarCodigo", tr("modals.eliminar"), class = "btn-default")
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
        tr("notifications.codigo_eliminado_con_resaltados", code = codigo_eliminar, n = resaltados_eliminados),
        type = "warning",
        duration = 4
      )
    } else {
      showNotification(
        tr("notifications.codigo_eliminado", code = codigo_eliminar),
        type = "warning",
        duration = 2
      )
    }
  })
  
  # ========================================
  # CRUD categorías
  # ========================================
  
  output$tablaCategorias <- renderDT({
    current_lang()
    datatable(
      rv$categoriasDF,
      selection = "single",
      options = list(
        pageLength = 8,
        dom = 'frtip',
        language = list(
          search = tr("datatable.search"),
          lengthMenu = tr("datatable.lengthMenu_categorias"),
          info = tr("datatable.info_categorias"),
          paginate = list(previous = tr("datatable.paginate_previous"), `next` = tr("datatable.paginate_next"))
        )
      ),
      colnames = c(tr("colnames.categoria"), tr("colnames.codigos_asociados"))
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
      showNotification(tr("notifications.categoria_actualizada", cat = input$new_categoria), type = "message", duration = 2)
    } else {
      df <- bind_rows(df, nueva)
      showNotification(tr("notifications.categoria_anadida", cat = input$new_categoria), type = "message", duration = 2)
    }
    rv$categoriasDF <- df
  })
  
  observeEvent(input$deleteCategoria, {
    sel <- input$tablaCategorias_rows_selected; req(length(sel)==1)
    categoria_eliminar <- rv$categoriasDF$Categoria[sel]
    df <- rv$categoriasDF[-sel, ]
    rv$categoriasDF <- df
    showNotification(tr("notifications.categoria_eliminada", cat = categoria_eliminar), type = "warning", duration = 2)
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
    showNotification(tr("notifications.documentos_cargados", n = nrow(files)), type = "message", duration = 3)
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
      rv$texto <- aplicar_resaltado_multiple(texto_original, fragmentos_archivo, codes_label = tr("colnames.codigo"))
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
    current_lang()
    if (!is.null(rv$docs) && rv$idx>0) {
      tr("notifications.doc_info", idx = rv$idx, total = length(rv$docs), name = rv$docs[[rv$idx]]$name)
    } else tr("notifications.no_hay_documentos")
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
    
    # Si no hay categoría asignada, usar tr("categorias.sin_categoria")
    if (is.null(cat_sel) || is.na(cat_sel) || cat_sel == "") {
      cat_sel <- tr("categorias.sin_categoria")
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
          tr("notifications.codigo_anadido_fragmento", code = code),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          tr("notifications.codigo_ya_aplicado", code = code),
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
          tr("notifications.fragmento_recodificado", code = code),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          tr("notifications.fragmento_codificado", code = code),
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
      title = div(icon("exclamation-triangle"), paste0(" ", tr("modals.confirmar_limpieza"))),
      div(
        h4(tr("modals.estas_seguro"), style = "color: #2c3e50;"),
        div(
          class = "info-panel",
          p(tr("modals.eliminar_resaltados_doc", file = archivo_actual),
            style = "color: #e67e22; margin: 0;")
        )
      ),
      footer = tagList(
        modalButton(tr("modals.cancelar")),
        actionButton("confirmarLimpieza", tr("modals.si_limpiar"), class = "btn-default")
      )
    ))
  })
  
  observeEvent(input$confirmarLimpieza, {
    archivo_actual <- rv$docs[[rv$idx]]$name
    rv$tabla <- rv$tabla %>%
      filter(Archivo != archivo_actual)
    
    removeModal()
    showNotification(
      tr("notifications.resaltados_eliminados", file = archivo_actual),
      type = "message",
      duration = 3
    )
  })
  
  # Función para eliminar todos los resaltados
  observeEvent(input$eliminarTodosResaltes, {
    showModal(modalDialog(
      title = div(icon("exclamation-triangle"), paste0(" ", tr("modals.confirmacion"))),
      div(
        class = "danger-panel",
        h4(tr("modals.eliminar_todos_resaltados"), style = "color: #c0392b;"),
        p(tr("modals.eliminar_todos_desc"), style = "color: #a93226;"),
        p(strong(tr("modals.no_deshacer")), style = "color: #8b1538; font-size: 16px;")
      ),
      footer = tagList(
        modalButton(tr("modals.cancelar")),
        actionButton("confirmarEliminacionTotal", tr("modals.si_eliminar_todo"), class = "btn-default")
      )
    ))
  })
  
  observeEvent(input$confirmarEliminacionTotal, {
    rv$tabla <- rv$tabla[0, ]  # Vaciar tabla manteniendo estructura
    
    removeModal()
    showNotification(tr("notifications.todos_eliminados"), type = "warning", duration = 4)
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
        title = div(icon("info-circle"), paste0(" ", tr("modals.info_fragmento"))),
        div(
          div(
            class = "info-panel",
            h4(tr("modals.texto_label")),
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; max-height: 150px; overflow-y: auto; border-left: 4px solid #3498db;",
              texto_fragmento
            )
          ),
          div(
            class = "info-panel",
            h4(tr("modals.codigos_aplicados_label")),
            p(codigos_aplicados, style = "font-weight: bold; color: #3498db; font-size: 16px;"),
            h4(tr("modals.archivo_info_label")),
            p(fragmento_info$Archivo[1], style = "color: #7f8c8d;")
          ),
          if (nrow(fragmento_info) > 1) {
            div(
              class = "info-panel",
              h5(tr("modals.historial_codificacion")),
              DTOutput("historialCodificacion")
            )
          }
        ),
        footer = modalButton(tr("modals.cerrar")),
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
            colnames = c(tr("colnames.codigo"), tr("colnames.categoria"), tr("colnames.aplicado"))
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
      title = div(icon("question-circle"), paste0(" ", tr("help.centro_ayuda"))),
      div(
        div(
          class = "info-panel",
          h4(tr("help.modos_trabajo")),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            div(
              h5(tr("help.modo_seleccionar"), style = "color: #2c3e50;"),
              tags$ul(
                style = "color: #2c3e50;",
                tags$li(tr("help.selecciona_texto_visor")),
                tags$li(tr("help.aplica_codigos_auto")),
                tags$li(tr("help.resaltado_tiempo_real"))
              )
            ),
            div(
              h5(tr("help.modo_deseleccionar"), style = "color: #c0392b;"),
              tags$ul(
                style = "color: #2c3e50;",
                tags$li(tr("help.clic_resaltado")),
                tags$li(tr("help.elimina_codigos")),
                tags$li(tr("help.correccion_precisa"))
              )
            )
          )
        ),

        div(
          class = "info-panel",
          h4(tr("help.caracteristicas_avanzadas")),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            div(
              h5(tr("help.modo_acumulativo"), style = "color: #2c3e50;"),
              p(tr("help.acumulativo_desc"), style = "color: #2c3e50;")
            ),
            div(
              h5(tr("help.visualizacion"), style = "color: #2c3e50;"),
              p(tr("help.gradientes_desc"), style = "color: #2c3e50;")
            )
          )
        )
      ),
      footer = modalButton(tr("help.entendido")),
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
      tr("notifications.cita_copiada"),
      type = "message",
      duration = 3
    )
  })
  
  # ========================================
  # Tabla de resaltados mejorada
  # ========================================
  
  output$tablaResaltes <- renderDT({
    current_lang()
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
          search = tr("datatable.search"),
          lengthMenu = tr("datatable.lengthMenu_resaltados"),
          info = tr("datatable.info_resaltados"),
          paginate = list(previous = tr("datatable.paginate_previous"), `next` = tr("datatable.paginate_next"))
        ),
        columnDefs = list(
          list(targets = 0, width = "250px"),
          list(targets = 1, width = "120px"),
          list(targets = 2, width = "120px"),
          list(targets = 3, width = "180px"),
          list(targets = 4, width = "80px")
        )
      ),
      colnames = c(tr("colnames.extracto"), tr("colnames.codigo"), tr("colnames.categoria"), tr("colnames.archivo"), tr("colnames.hora"))
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
      title = div(icon("exclamation-triangle"), paste0(" ", tr("modals.confirmar_eliminacion_extracto"))),
      div(
        h4(tr("modals.eliminar_resaltado_pregunta"), style = "color: #c0392b;"),
        div(
          class = "info-panel",
          p(paste0(tr("modals.codigo_modal"), strong(fila_eliminar$Codigo)), style = "margin: 5px 0;"),
          p(paste0(tr("modals.fragmento_modal"), strong(str_trunc(fila_eliminar$Extracto, 40))), style = "margin: 5px 0;")
        )
      ),
      footer = tagList(
        modalButton(tr("modals.cancelar")),
        actionButton("confirmarEliminacion", tr("modals.eliminar"), class = "btn-default")
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
    showNotification(tr("notifications.resaltado_eliminado"), type = "message", duration = 3)
  })
  
  # ========================================
  # Descarga mejorada
  # ========================================
  
  output$descarga <- downloadHandler(
    filename = function() paste0(if (current_lang() == "es") "resaltados_rcualitext_" else "highlights_rcualitext_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      
      # Hoja principal con todos los resaltados
      sheet1_name <- tr("excel.resaltados_detallados")
      addWorksheet(wb, sheet1_name)
      writeData(wb, sheet1_name, rv$tabla)
      
      # Hoja con resumen de fragmentos únicos
      resumen_fragmentos <- rv$tabla %>%
        group_by(Extracto, Archivo, FragmentId) %>%
        summarise(
          !!tr("excel.codigos_aplicados_col") := paste(Codigo, collapse = "; "),
          !!tr("excel.num_codigos") := n(),
          !!tr("excel.primera_codificacion") := min(Timestamp),
          !!tr("excel.ultima_codificacion") := max(Timestamp),
          .groups = "drop"
        ) %>%
        arrange(desc(.data[[tr("excel.num_codigos")]]))

      sheet2_name <- tr("excel.resumen_fragmentos")
      addWorksheet(wb, sheet2_name)
      writeData(wb, sheet2_name, resumen_fragmentos)
      
      # Hoja con estadísticas
      estadisticas <- tibble(
        !!tr("excel.metrica") := c(
          tr("excel.total_resaltados"),
          tr("excel.fragmentos_unicos"),
          tr("excel.fragmentos_multiples_codigos"),
          tr("excel.promedio_codigos"),
          tr("excel.documentos_procesados"),
          tr("excel.codigos_utilizados")
        ),
        !!tr("excel.valor") := c(
          nrow(rv$tabla),
          length(unique(rv$tabla$FragmentId)),
          sum(table(rv$tabla$FragmentId) > 1),
          round(nrow(rv$tabla) / length(unique(rv$tabla$FragmentId)), 2),
          length(unique(rv$tabla$Archivo)),
          length(unique(rv$tabla$Codigo))
        )
      )
      
      sheet3_name <- tr("excel.estadisticas")
      addWorksheet(wb, sheet3_name)
      writeData(wb, sheet3_name, estadisticas)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification(tr("notifications.datos_exportados"), type = "message", duration = 3)
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
      plot_codigos(rv$tabla, fill = input$fillToggle, code_colors = get_code_colors(),
                   labels = list(freq = tr("plots.frecuencia"), codes = tr("plots.codigos_x"), cat = tr("plots.categoria_fill"), sin_cat = tr("plots.sin_categoria")))
    }, error = function(e) {
      showNotification(tr("notifications.error_grafico", msg = e$message), type = "error")
      NULL
    })
  })
  
  output$plotRedCentralidad <- renderPlot({
    # Hacer que dependa tanto de la tabla como de los códigos
    req(nrow(rv$tabla) > 0, nrow(rv$codigosDF) >= 0)
    
    result <- plot_network_and_centrality(rv$tabla, code_colors = get_code_colors(),
                                          labels = list(code = tr("plots.codigo_label"), centrality = tr("plots.centralidad_zscore")))
    result$plot
  }, res = 96, bg = "transparent")
  
  # ========================================
  # Guardar / cargar estado actualizado
  # ========================================
  
  output$saveState <- downloadHandler(
    filename = function() paste0(if (current_lang() == "es") "proyecto_rcualitext_" else "project_rcualitext_", Sys.Date(), ".rds"),
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
          red_semantica = if(!is.null(rv$red_semantica)) "Yes" else "No",
          visualizacion_2d = if(!is.null(rv$visualizacion_2d)) rv$visualizacion_2d$metodo else "No"
        )
      )
      saveRDS(estado, file)

      showNotification(tr("notifications.proyecto_guardado"), type = "message", duration = 3)
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
          tr("notifications.proyecto_convertido"),
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
            is.na(Categoria) | Categoria == "" ~ tr("categorias.sin_categoria"),
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
          ia_info <- paste0(", ", est$metadata$ia_results_count, " ", tr("notifications.meta_resultados_ia"))
        }
        metadata_info <- paste0(
          " (", est$metadata$total_codes, " ", tr("notifications.meta_codigos"), ", ",
          est$metadata$total_highlights, " ", tr("notifications.meta_resaltados"), ia_info, ")"
        )
      }
      
      showNotification(
        tr("notifications.proyecto_cargado", info = metadata_info),
        type = "message",
        duration = 4
      )

    }, error = function(e) {
      showNotification(
        tr("notifications.error_cargar_proyecto", msg = e$message),
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
      h5(icon("mouse-pointer"), paste0(" ", tr("modes.seleccionar_activo")), style = "color: #2c3e50; margin-bottom: 15px;"),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
        div(
          h6(tr("modes.acciones_disponibles"), style = "color: #2c3e50; margin-bottom: 8px;"),
          tags$ul(
            style = "font-size: 13px; color: #7f8c8d; margin: 0;",
            tags$li(tr("modes.selecciona_texto")),
            tags$li(tr("modes.aplica_codigos"))
          )
        ),
        div(
          h6(tr("modes.caracteristicas"), style = "color: #2c3e50; margin-bottom: 8px;"),
          tags$ul(
            style = "font-size: 13px; color: #7f8c8d; margin: 0;",
            tags$li(tr("modes.acumulativo_disponible")),
            tags$li(tr("modes.clic_info"))
          )
        )
      )
    )
  })
  
  # Mensaje de bienvenida mejorado
  observe({
    if (is.null(rv$docs) || length(rv$docs) == 0) {
      showNotification(
        tr("notifications.bienvenido"),
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
                 stop(tr("notifications.formato_diccionario_error")))
    
    # Validar columnas (accept Spanish or English column names)
    expected_cols <- c("Categoría", "Código", "Definición")
    if (!all(expected_cols %in% names(df))) {
      # Intentar con nombres alternativos en español sin tildes
      if (all(c("Categoria", "Codigo", "Definicion") %in% names(df))) {
        names(df)[names(df) == "Categoria"] <- "Categoría"
        names(df)[names(df) == "Codigo"] <- "Código"
        names(df)[names(df) == "Definicion"] <- "Definición"
      } else if (all(c("Category", "Code", "Definition") %in% names(df))) {
        names(df)[names(df) == "Category"] <- "Categoría"
        names(df)[names(df) == "Code"] <- "Código"
        names(df)[names(df) == "Definition"] <- "Definición"
      } else {
        stop(tr("notifications.columnas_diccionario_error"))
      }
    }
    df
  })
  
  # Ejecutar análisis IA
  observeEvent(input$run_ia_analysis, {
    # Validaciones
    if (is.null(rv$docs) || length(rv$docs) == 0) {
      showNotification(tr("notifications.carga_documentos"), type = "error", duration = 3)
      return()
    }

    dict <- tryCatch(dict_ia_df(), error = function(e) NULL)
    if (is.null(dict) || nrow(dict) == 0) {
      showNotification(tr("notifications.carga_diccionario"), type = "error", duration = 3)
      return()
    }

    # Validar API Key de OpenAI
    api_key <- input$openai_api_key
    if (is.null(api_key) || !nzchar(trimws(api_key))) {
      showNotification(tr("notifications.ingresa_api_key"), type = "error", duration = 3)
      return()
    }

    n_codes <- nrow(dict)
    total <- length(rv$docs) * n_codes

    withProgress(message = tr("progress.analizando_gpt"), value = 0, {
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

          prompt <- if (current_lang() == "es") {
            paste0(
              "Del texto:\n\n", doc_text,
              "\n\nExtrae fragmentos que correspondan a la siguiente definición:\n\"",
              def, "\"\n\nResponde solo con los fragmentos extraídos, uno por línea."
            )
          } else {
            paste0(
              "From the text:\n\n", doc_text,
              "\n\nExtract fragments that match the following definition:\n\"",
              def, "\"\n\nRespond only with the extracted fragments, one per line."
            )
          }

          tryCatch({
            txt_out <- tryCatch({
              call_openai_api(prompt, api_key)
            }, error = function(e) {
              showNotification(
                tr("notifications.error_openai", code = code, msg = e$message),
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
              tr("notifications.error_inesperado", code = code, msg = e$message),
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
          tr("notifications.no_extraer_fragmentos"),
          type = "warning",
          duration = 5
        )
      } else {
        showNotification(
          tr("notifications.analisis_ia_completado", n = nrow(rv$ia_results)),
          type = "message",
          duration = 3
        )
      }
    })
  })
  
  # Mostrar resultados IA
  output$tabla_ia_results <- renderDT({
    current_lang()
    req(nrow(rv$ia_results) > 0)

    datatable(
      rv$ia_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = tr("datatable.search"),
          lengthMenu = tr("datatable.lengthMenu_registros"),
          info = tr("datatable.info_registros"),
          paginate = list(
            first = tr("datatable.paginate_first"),
            last = tr("datatable.paginate_last"),
            `next` = tr("datatable.paginate_next"),
            previous = tr("datatable.paginate_previous")
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
        labs(x = tr("plots.codigo_label"), y = tr("plots.frecuencia"), fill = tr("plots.categoria_fill")) +
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
      ia_sheet <- tr("fuente.ia")
      addWorksheet(wb, ia_sheet)

      # Escribir los datos
      writeData(wb, ia_sheet, rv$ia_results)

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

      addStyle(wb, sheet = ia_sheet, headerStyle, rows = 1, cols = 1:5, gridExpand = TRUE)

      # Ajustar anchos de columna
      setColWidths(wb, sheet = ia_sheet, cols = 1:5, widths = c(25, 20, 20, 40, 50))

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
      ia_sheet2 <- tr("fuente.ia")
      addWorksheet(wb, ia_sheet2)
      writeData(wb, ia_sheet2, rv$ia_results)
      headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                                  fgFill = "#27ae60", textDecoration = "bold")
      addStyle(wb, sheet = ia_sheet2, headerStyle, rows = 1, cols = 1:ncol(rv$ia_results), gridExpand = TRUE)
      setColWidths(wb, sheet = ia_sheet2, cols = 1:ncol(rv$ia_results), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification(tr("notifications.tabla_ia_descargada"), type = "message", duration = 3)
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
        labs(title = tr("plots.distribucion_codigos_ia"), x = tr("plots.codigo_label"), y = tr("plots.frecuencia")) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(tr("notifications.figura_descargada", w = ancho, h = alto, dpi = dpi), type = "message", duration = 3)
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
        labs(title = tr("plots.fragmentos_categoria_ia"), fill = tr("plots.categoria_fill")) +
        theme_void(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(tr("notifications.figura_descargada", w = ancho, h = alto, dpi = dpi), type = "message", duration = 3)
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
      showNotification(tr("notifications.tabla_clustering_descargada"), type = "message", duration = 3)
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
        labs(title = tr("plots.clustering_semantico"), x = "PC1", y = "PC2", color = "Cluster") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(tr("notifications.figura_descargada", w = ancho, h = alto, dpi = dpi), type = "message", duration = 3)
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
      showNotification(tr("notifications.tabla_similitud_descargada"), type = "message", duration = 3)
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
          title = tr("plots.visualizacion_embeddings", method = toupper(metodo)),
          x = tr("plots.dimension_1", method = toupper(metodo)),
          y = tr("plots.dimension_2", method = toupper(metodo)),
          color = tr("plots.codigo_label")
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "right")

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(tr("notifications.figura_descargada", w = ancho, h = alto, dpi = dpi), type = "message", duration = 3)
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
      showNotification(tr("notifications.tabla_coherencia_descargada"), type = "message", duration = 3)
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
        showNotification(tr("notifications.no_datos_coherencia"), type = "error", duration = 3)
        return(NULL)
      }

      # Translate evaluation labels for display
      eval_labels <- c("excellent" = tr("evaluacion.excelente"), "good" = tr("evaluacion.buena"),
                        "moderate" = tr("evaluacion.moderada"), "low_review" = tr("evaluacion.baja_revisar"))
      datos <- datos %>% mutate(Evaluacion = ifelse(Evaluacion %in% names(eval_labels), eval_labels[Evaluacion], Evaluacion))
      eval_colors <- stats::setNames(c("#27ae60", "#2ecc71", "#f39c12", "#e74c3c"),
                                     c(tr("evaluacion.excelente"), tr("evaluacion.buena"), tr("evaluacion.moderada"), tr("evaluacion.baja_revisar")))

      p <- ggplot(datos, aes(x = reorder(Codigo, Coherencia_Media), y = Coherencia_Media, fill = Evaluacion)) +
        geom_col() +
        geom_errorbar(aes(ymin = Coherencia_Min, ymax = Coherencia_Max), width = 0.2, alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(values = eval_colors) +
        labs(
          title = tr("plots.coherencia_semantica"),
          x = tr("plots.codigo_label"),
          y = tr("plots.coherencia_media_label"),
          fill = tr("plots.evaluacion_fill")
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        geom_hline(yintercept = 0.6, linetype = "dashed", color = "#7f8c8d", alpha = 0.7)

      ggsave(file, plot = p, width = ancho, height = alto, dpi = dpi, bg = "white")
      showNotification(tr("notifications.figura_descargada", w = ancho, h = alto, dpi = dpi), type = "message", duration = 3)
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
        color_title <- tr("plots.comunidad")
      } else {
        color_var <- "categoria"
        color_title <- tr("plots.categoria_fill")
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
          title = tr("plots.red_semantica_codigos"),
          subtitle = tr("plots.umbral_similitud_subtitle", val = input$umbral_red_semantica),
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
      showNotification(tr("notifications.red_semantica_descargada", w = ancho, h = alto, dpi = dpi), type = "message", duration = 3)
    }
  )

  # ========================================
  # Análisis Semántico (OpenAI)
  # ========================================

  # Estado de embeddings
  output$estado_embeddings <- renderUI({
    current_lang()
    if (is.null(rv$hf_embeddings)) {
      div(
        style = "color: #7f8c8d;",
        icon("circle", class = "text-muted"),
        paste0(" ", tr("analisis_semantico.sin_embeddings"))
      )
    } else {
      n_emb <- nrow(rv$hf_embeddings)
      div(
        style = "color: #2c3e50;",
        icon("check-circle", class = "text-success"),
        paste0(" ", tr("analisis_semantico.embeddings_generados", n = n_emb)),
        br(),
        tags$small(tr("analisis_semantico.dimensiones", d = ncol(rv$hf_embeddings)), style = "color: #95a5a6;")
      )
    }
  })

  # Generar embeddings
  observeEvent(input$btn_generar_embeddings, {
    # Validar API Key de OpenAI
    api_key <- input$openai_api_key
    if (is.null(api_key) || !nzchar(trimws(api_key))) {
      showNotification(tr("notifications.ingresa_api_key_pestana"), type = "error", duration = 4)
      return()
    }

    # Obtener datos del reactive (prioriza IA sobre manual)
    ds <- datos_semantico()

    # Validaciones
    if (ds$n < 2) {
      fuente_msg <- if (ds$fuente == "ninguno") {
        tr("notifications.necesitas_fragmentos")
      } else {
        tr("notifications.solo_fragmentos", n = ds$n)
      }
      showNotification(fuente_msg, type = "error", duration = 4)
      return()
    }

    # Verificar si los datos han cambiado (usando hash)
    current_hash <- digest::digest(ds$datos$Extracto)
    if (!is.null(rv$hf_cache_hash) && rv$hf_cache_hash == current_hash && !is.null(rv$hf_embeddings)) {
      showNotification(tr("notifications.embeddings_actualizados"), type = "message", duration = 3)
      return()
    }

    # Guardar referencia a los datos usados para embedding
    rv$datos_embedding_ref <- ds$datos

    withProgress(message = tr("progress.generando_embeddings"), value = 0, {
      tryCatch({
        textos <- ds$datos$Extracto

        fuente_txt <- if (ds$fuente == "ia") tr("fuente.ia") else tr("fuente.manual")
        incProgress(0.1, detail = tr("progress.usando_datos", source = fuente_txt))

        incProgress(0.2, detail = tr("progress.conectando_openai"))

        # Usar función de OpenAI para embeddings
        embeddings <- obtener_embeddings_openai(textos, api_key)

        incProgress(0.5, detail = tr("progress.embeddings_obtenidos"))

        incProgress(0.1, detail = tr("progress.calculando_similitudes"))

        # Calcular matriz de similitud
        similitud <- calcular_similitud_coseno(embeddings)

        # Guardar resultados
        rv$hf_embeddings <- embeddings
        rv$hf_similitud <- similitud
        rv$hf_cache_hash <- current_hash

        incProgress(0.1, detail = tr("progress.completado"))

        showNotification(
          tr("notifications.embeddings_generados", n = nrow(embeddings)),
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
      showNotification(tr("notifications.primero_genera_embeddings"), type = "error", duration = 3)
      return()
    }

    withProgress(message = tr("progress.ejecutando_clustering"), value = 0.3, {
      tryCatch({
        resultado <- clustering_semantico(
          embeddings_matrix = rv$hf_embeddings,
          n_clusters = input$n_clusters_semantico,
          metodo = "kmeans"
        )

        rv$semantico_clusters <- resultado

        incProgress(0.7, detail = tr("progress.completado"))

        showNotification(
          tr("notifications.clustering_completado", n = resultado$n_clusters),
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
          search = tr("datatable.search"),
          info = tr("datatable.info"),
          paginate = list(previous = tr("datatable.paginate_previous"), `next` = tr("datatable.paginate_next"))
        )
      ),
      rownames = FALSE,
      colnames = c(tr("colnames.cluster"), tr("colnames.codigo"), tr("colnames.categoria"), tr("colnames.extracto"), tr("colnames.archivo"))
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

    p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster, text = paste0(tr("plots.codigo_label"), ": ", Codigo, "\n", Extracto))) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = tr("plots.clusters_semanticos_pca"), x = "PC1", y = "PC2") +
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
      showNotification(tr("notifications.primero_genera_embeddings"), type = "error", duration = 3)
      return()
    }

    withProgress(message = tr("progress.buscando_similares"), value = 0.3, {
      tryCatch({
        similares <- detectar_similares_diferente_codigo(
          tabla = rv$datos_embedding_ref,
          similitud_matrix = rv$hf_similitud,
          umbral = input$umbral_similitud,
          labels = list(
            alta = tr("validacion.alta_similitud"),
            moderada = tr("validacion.similitud_moderada")
          )
        )

        if (nrow(similares) == 0) {
          showNotification(
            tr("notifications.no_inconsistencias"),
            type = "message",
            duration = 4
          )
        } else {
          showNotification(
            tr("notifications.inconsistencias_encontradas", n = nrow(similares)),
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
          tibble(Mensaje = tr("datatable.no_similares")),
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
        language = list(search = tr("datatable.search"))
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
          c(tr("validacion.alta_similitud"), tr("validacion.similitud_moderada")),
          c("#e74c3c", "#f39c12")
        ),
        fontWeight = "bold"
      )
  })

  # Visualización 2D
  observeEvent(input$btn_visualizar, {
    if (is.null(rv$hf_embeddings)) {
      showNotification(tr("notifications.primero_genera_embeddings"), type = "error", duration = 3)
      return()
    }

    withProgress(message = tr("progress.generando_visualizacion"), value = 0.3, {
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

        setProgress(value = 1, message = tr("progress.visualizacion_completada"))
        showNotification(tr("notifications.visualizacion_generada"), type = "message", duration = 3)

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
          title = tr("plots.visualizacion_embeddings", method = toupper(metodo)),
          x = tr("plots.dimension_1", method = toupper(metodo)),
          y = tr("plots.dimension_2", method = toupper(metodo)),
          color = tr("plots.codigo_label")
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
      showNotification(tr("notifications.primero_genera_embeddings"), type = "error", duration = 3)
      return()
    }

    withProgress(message = tr("progress.analizando_coherencia"), value = 0.3, {
      tryCatch({
        coherencia <- analizar_coherencia_codigos(
          tabla = rv$datos_embedding_ref,
          similitud_matrix = rv$hf_similitud
        )

        rv$semantico_coherencia <- coherencia

        showNotification(
          tr("notifications.coherencia_completada", n = nrow(coherencia)),
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
    current_lang()
    req(rv$semantico_coherencia)

    # Translate evaluation labels for display
    eval_labels <- c("excellent" = tr("evaluacion.excelente"), "good" = tr("evaluacion.buena"),
                      "moderate" = tr("evaluacion.moderada"), "low_review" = tr("evaluacion.baja_revisar"),
                      "insufficient" = tr("evaluacion.insuficiente"))
    display_data <- rv$semantico_coherencia %>%
      mutate(Evaluacion = ifelse(Evaluacion %in% names(eval_labels), eval_labels[Evaluacion], Evaluacion))

    datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(search = tr("datatable.search"))
      ),
      rownames = FALSE,
      colnames = c(tr("colnames.codigo"), tr("colnames.n_fragmentos"), tr("colnames.coherencia_media"), tr("colnames.min"), tr("colnames.max"), tr("colnames.sd"), tr("colnames.evaluacion"))
    ) %>%
      formatStyle(
        "Evaluacion",
        backgroundColor = styleEqual(
          c(tr("evaluacion.excelente"), tr("evaluacion.buena"), tr("evaluacion.moderada"), tr("evaluacion.baja_revisar"), tr("evaluacion.insuficiente")),
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

    # Translate evaluation labels for display
    eval_labels <- c("excellent" = tr("evaluacion.excelente"), "good" = tr("evaluacion.buena"),
                      "moderate" = tr("evaluacion.moderada"), "low_review" = tr("evaluacion.baja_revisar"))
    datos <- datos %>% mutate(Evaluacion = ifelse(Evaluacion %in% names(eval_labels), eval_labels[Evaluacion], Evaluacion))
    eval_colors <- stats::setNames(c("#27ae60", "#2ecc71", "#f39c12", "#e74c3c"),
                                   c(tr("evaluacion.excelente"), tr("evaluacion.buena"), tr("evaluacion.moderada"), tr("evaluacion.baja_revisar")))

    ggplot(datos, aes(x = reorder(Codigo, Coherencia_Media), y = Coherencia_Media, fill = Evaluacion)) +
      geom_col() +
      geom_errorbar(aes(ymin = Coherencia_Min, ymax = Coherencia_Max), width = 0.2, alpha = 0.7) +
      coord_flip() +
      scale_fill_manual(values = eval_colors) +
      labs(
        title = tr("plots.coherencia_semantica"),
        x = tr("plots.codigo_label"),
        y = tr("plots.coherencia_media_label"),
        fill = tr("plots.evaluacion_fill")
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
      showNotification(tr("notifications.primero_genera_embeddings"), type = "error", duration = 3)
      return()
    }

    if (length(unique(rv$datos_embedding_ref$Codigo)) < 2) {
      showNotification(tr("notifications.necesitas_2_codigos"), type = "error", duration = 3)
      return()
    }

    withProgress(message = tr("progress.generando_red"), value = 0.3, {
      tryCatch({
        red <- calcular_red_semantica_codigos(
          embeddings_matrix = rv$hf_embeddings,
          tabla = rv$datos_embedding_ref,
          umbral_conexion = input$umbral_red_semantica
        )

        rv$red_semantica <- red

        incProgress(0.7, detail = tr("progress.completado"))

        showNotification(
          tr("notifications.red_generada", n_cod = red$n_codigos, n_con = red$n_conexiones),
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
    current_lang()
    red <- rv$red_semantica

    if (is.null(red)) {
      return(
        div(
          style = "text-align: center; padding: 20px; color: #7f8c8d;",
          icon("project-diagram", style = "font-size: 36px; margin-bottom: 10px;"),
          h5(tr("red_semantica.titulo_red")),
          p(tr("red_semantica.configura_umbral"))
        )
      )
    }

    div(
      style = "margin-bottom: 10px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
      fluidRow(
        column(4,
          div(style = "text-align: center;",
            h4(red$n_codigos, style = "color: #2c3e50; margin: 0;"),
            tags$small(tr("red_semantica.codigos_label"), style = "color: #7f8c8d;")
          )
        ),
        column(4,
          div(style = "text-align: center;",
            h4(red$n_conexiones, style = "color: #3498db; margin: 0;"),
            tags$small(tr("red_semantica.conexiones_label"), style = "color: #7f8c8d;")
          )
        ),
        column(4,
          div(style = "text-align: center;",
            h4(paste0(input$umbral_red_semantica * 100, "%"), style = "color: #27ae60; margin: 0;"),
            tags$small(tr("red_semantica.umbral_label"), style = "color: #7f8c8d;")
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
      color_title <- tr("plots.comunidad")
    } else {
      color_var <- "categoria"
      color_title <- tr("plots.categoria_fill")
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
        title = tr("plots.red_semantica_codigos"),
        subtitle = tr("plots.umbral_similitud_subtitle", val = input$umbral_red_semantica),
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
      showNotification(tr("notifications.ingresa_api_key_pestana"), type = "error", duration = 4)
      return()
    }

    # Obtener datos del reactive (prioriza IA sobre manual)
    ds <- datos_semantico()

    if (ds$n < 1) {
      showNotification(tr("notifications.no_fragmentos_validar"), type = "error", duration = 4)
      return()
    }

    n_validar <- min(input$n_fragmentos_validar, ds$n)

    # Seleccionar muestra aleatoria
    set.seed(Sys.time())
    indices <- sample(1:ds$n, n_validar)
    muestra <- ds$datos[indices, ]

    withProgress(message = tr("progress.validando_gpt"), value = 0.1, {
      tryCatch({
        fuente_txt <- if (ds$fuente == "ia") tr("fuente.ia") else tr("fuente.manual")
        incProgress(0.2, detail = tr("progress.usando_datos", source = fuente_txt))

        incProgress(0.2, detail = tr("progress.conectando_openai"))

        resultado <- validar_codificacion_llm(
          fragmentos = muestra$Extracto,
          codigos = muestra$Codigo,
          api_key = api_key,
          lang = current_lang()
        )

        rv$semantico_validacion <- resultado

        incProgress(0.5, detail = tr("progress.completado"))

        showNotification(
          tr("notifications.validacion_completada"),
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
    current_lang()
    if (is.null(rv$semantico_validacion)) {
      return(
        div(
          style = "text-align: center; padding: 40px; color: #7f8c8d;",
          icon("robot", style = "font-size: 48px; margin-bottom: 15px;"),
          h4(tr("validacion.panel_expertos")),
          p(tr("validacion.haz_clic_validar"))
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

    # Convertir headers de fragmento #### Fragmento/Fragment N a tarjetas
    resultado <- stringr::str_replace_all(
      resultado,
      "####\\s*((Fragmento|Fragment)\\s*\\d+)\\n",
      "</div><div class='fragment-card' style='background: white; border: 1px solid #e0e0e0; border-radius: 8px; padding: 15px; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.05);'><h4 style='color: #3498db; margin: 0 0 10px 0; font-size: 14px;'><i class='fa fa-file-text-o'></i> \\1</h4>\n"
    )

    # Convertir **Texto:**/**Text:** y otros labels bold
    texto_label <- tr("validacion.texto_label")
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Texto|Text):\\*\\*\\s*\"(.+?)\"",
      paste0("<div style='background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 5px 0; font-style: italic; border-left: 3px solid #95a5a6;'><strong>", texto_label, "</strong> \"\\2\"</div>")
    )

    codigo_display <- tr("validacion.codigo_label")
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Código Asignado|Assigned Code|Code):\\*\\*\\s*(.+?)\\n",
      paste0("<div style='margin: 5px 0;'><strong style='color: #7f8c8d;'>", codigo_display, "</strong> <span style='background: #3498db; color: white; padding: 2px 8px; border-radius: 12px; font-size: 12px;'>\\2</span></div>\n")
    )

    # Convertir evaluaciones con colores
    eval_label <- tr("validacion.evaluacion_label")
    correcto_label <- tr("validacion.correcto")
    revisar_label <- tr("validacion.revisar")
    incorrecto_label <- tr("validacion.incorrecto")

    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Evaluación|Evaluation):\\*\\*\\s*(Correcto|Correct)",
      paste0("<div style='margin: 8px 0;'><strong>", eval_label, "</strong> <span style='background: #27ae60; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>&#10003; ", correcto_label, "</span></div>")
    )
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Evaluación|Evaluation):\\*\\*\\s*(Revisar|Review)",
      paste0("<div style='margin: 8px 0;'><strong>", eval_label, "</strong> <span style='background: #f39c12; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>&#9888; ", revisar_label, "</span></div>")
    )
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Evaluación|Evaluation):\\*\\*\\s*(Incorrecto|Incorrect)",
      paste0("<div style='margin: 8px 0;'><strong>", eval_label, "</strong> <span style='background: #e74c3c; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>&#10007; ", incorrecto_label, "</span></div>")
    )

    # Convertir justificación
    justif_label <- tr("validacion.justificacion_label")
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Justificación|Justification):\\*\\*\\s*(.+?)\\n",
      paste0("<div style='margin: 5px 0; color: #555;'><strong style='color: #7f8c8d;'>", justif_label, "</strong> \\2</div>\n")
    )

    # Convertir código alternativo
    sugerencia_label <- tr("validacion.sugerencia_label")
    resultado <- stringr::str_replace_all(
      resultado,
      "\\*\\*(Código Alternativo( Sugerido)?|Suggested Alternative Code|Alternative Code):\\*\\*\\s*(.+?)\\n",
      paste0("<div style='margin: 5px 0;'><strong style='color: #7f8c8d;'>", sugerencia_label, "</strong> <em>\\3</em></div>\n")
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
      h4(icon("gavel"), paste0(" ", tr("validacion.resultado_validacion")), style = "color: #2c3e50; margin-bottom: 15px;"),
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
      showNotification(tr("notifications.ingresa_api_key_pestana"), type = "error", duration = 4)
      return()
    }

    # Validar que hay datos
    ds <- datos_semantico()
    if (ds$n < 1) {
      showNotification(tr("notifications.no_fragmentos_codificados"), type = "error", duration = 4)
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

    withProgress(message = tr("progress.generando_reporte"), value = 0.1, {
      incProgress(0.2, detail = tr("progress.conectando_openai"))

      resultado <- tryCatch({
        call_openai_api(
          prompt = prompt,
          api_key = api_key,
          system_prompt = if(input$idioma_reporte == "es") "Eres un experto en an\u00e1lisis cualitativo de datos textuales y redacci\u00f3n acad\u00e9mica." else "You are an expert in qualitative textual data analysis and academic writing."
        )
      }, error = function(e) {
        showNotification(
          paste0("Error OpenAI: ", e$message),
          type = "error",
          duration = 6
        )
        NULL
      })

      incProgress(0.5, detail = tr("progress.procesando_respuesta"))

      # Verificar resultado
      if (is.null(resultado) || !nzchar(resultado)) {
        showNotification(
          tr("notifications.no_generar_reporte"),
          type = "error",
          duration = 6
        )
        return()
      }

      # Guardar resultado
      rv_reporte(resultado)

      incProgress(0.2, detail = tr("progress.completado"))

      showNotification(tr("notifications.reporte_generado"), type = "message", duration = 4)
    })
  })

  # Output del reporte
  output$reporte_ia_output <- renderUI({
    current_lang()
    reporte <- rv_reporte()

    if (is.null(reporte)) {
      return(
        div(
          style = "text-align: center; padding: 60px; color: #7f8c8d;",
          icon("file-alt", style = "font-size: 64px; margin-bottom: 20px;"),
          h4(tr("reporte.reporte_ia_titulo")),
          p(tr("reporte.reporte_ia_desc")),
          p(tr("reporte.reporte_ia_auto"),
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
        tags$span(paste0(" ", tr("notifications.reporte_generado")), style = "color: #155724; font-weight: 500;")
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
      paste0(if (current_lang() == "es") "Reporte_Cualitativo_" else "Qualitative_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      reporte <- rv_reporte()

      if (!is.null(reporte) && nzchar(reporte)) {
        # Crear documento Word con officer
        doc <- officer::read_docx()

        # Título
        doc <- doc %>%
          officer::body_add_par(if(current_lang() == "es") "Reporte de An\u00e1lisis Cualitativo" else "Qualitative Analysis Report", style = "heading 1") %>%
          officer::body_add_par(paste(if(current_lang() == "es") "Generado:" else "Generated:", format(Sys.time(), "%d/%m/%Y %H:%M")), style = "Normal") %>%
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
          if (grepl("^\\[(Insertar\\s+Figura|Insert\\s+.*Figure)", linea_trim, ignore.case = TRUE)) {
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
          officer::body_add_par(if(current_lang() == "es") "Generado con RCualiText v2.4 - An\u00e1lisis Cualitativo con IA" else "Generated with RCualiText v2.4 - Qualitative Analysis with AI", style = "Normal")

        # Guardar documento
        print(doc, target = file)

        showNotification(tr("notifications.reporte_word_descargado"), type = "message", duration = 3)
      } else {
        # Si no hay reporte, crear documento vacío con mensaje
        doc <- officer::read_docx() %>%
          officer::body_add_par(tr("notifications.no_hay_reporte"), style = "Normal") %>%
          officer::body_add_par(tr("notifications.genera_reporte_primero"), style = "Normal")
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
