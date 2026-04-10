# R/plots.R
# Visualization functions for RCualiText App
# Includes: code distribution, network, embedding plots

#' @title Interactive code frequency bar chart (plotly)
#' @description Builds a horizontal stacked bar chart of code frequencies per file using plotly, optionally colored by category or by a user-provided code-color mapping.
#' @param df Data frame of fragments with columns `Archivo`, `Codigo`, and `Categoria`.
#' @param fill Logical. If TRUE, bars are filled/stacked by category (default TRUE).
#' @param code_colors Named character vector mapping code names to hex colors, used when `fill = FALSE`.
#' @param labels Named list with plot labels (`freq`, `codes`, `cat`, optional `sin_cat`).
#' @return A plotly htmlwidget with the interactive bar chart.
plot_codigos <- function(df, fill = TRUE, code_colors = NULL,
    labels = list(freq = "Frequency", codes = "Codes", cat = "Category")) {
  sin_cat_label <- if (!is.null(labels$sin_cat)) labels$sin_cat else "Uncategorized"
  df_counts <- prepare_code_counts(df, fill, sin_cat_label)

  if (fill && "Categoria" %in% names(df_counts)) {
    p <- plotly::plot_ly(
      data = df_counts, y = ~Codigo, x = ~Frecuencia, color = ~Categoria,
      type = "bar", orientation = "h", text = ~Frecuencia, textposition = "outside",
      textfont = list(size = 12, color = "#2c3e50", family = "Arial Black"),
      hovertemplate = paste0("<b>%{y}</b><br>", labels$freq, ": %{x}<extra></extra>")
    )
  } else {
    if (!is.null(code_colors)) {
      df_counts <- df_counts %>% dplyr::mutate(Color = code_colors[as.character(Codigo)])
      p <- plotly::plot_ly(
        data = df_counts, y = ~Codigo, x = ~Frecuencia, type = "bar", orientation = "h",
        text = ~Frecuencia, textposition = "outside",
        textfont = list(size = 12, color = "#2c3e50", family = "Arial Black"),
        marker = list(color = ~Color),
        hovertemplate = paste0("<b>%{y}</b><br>", labels$freq, ": %{x}<extra></extra>")
      )
    } else {
      p <- plotly::plot_ly(
        data = df_counts, y = ~Codigo, x = ~Frecuencia, color = ~Codigo,
        type = "bar", orientation = "h", text = ~Frecuencia, textposition = "outside",
        textfont = list(size = 12, color = "#2c3e50", family = "Arial Black"),
        hovertemplate = paste0("<b>%{y}</b><br>", labels$freq, ": %{x}<extra></extra>")
      )
    }
  }

  p %>% plotly::layout(
    xaxis = list(title = list(text = labels$freq, font = list(size = 12, family = "sans-serif")), tickfont = list(size = 10)),
    yaxis = list(title = list(text = labels$codes, font = list(size = 12, family = "sans-serif")), tickfont = list(size = 10), categoryorder = "total ascending"),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, font = list(size = 10)),
    margin = list(l = 120, r = 80, t = 40, b = 80),
    barmode = "stack", plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
  ) %>% plotly::config(displayModeBar = FALSE)
}

#' @title Static code frequency bar chart (ggplot2)
#' @description Builds a faceted horizontal bar chart of code frequencies per file using ggplot2, suitable for high-quality static export.
#' @param df Data frame of fragments with columns `Archivo`, `Codigo`, and `Categoria`.
#' @param fill Logical. If TRUE, bars are filled by category (default TRUE).
#' @param code_colors Named character vector mapping code names to hex colors, used when `fill = FALSE`.
#' @param labels Named list with plot labels (`freq`, `codes`, `cat`, `code`, optional `sin_cat`).
#' @return A ggplot object ready to print or save with `ggsave`.
plot_codigos_ggplot <- function(df, fill = TRUE, code_colors = NULL,
    labels = list(freq = "Frequency", codes = "Codes", cat = "Category", code = "Code")) {
  sin_cat_label <- if (!is.null(labels$sin_cat)) labels$sin_cat else "Uncategorized"
  df_counts <- prepare_code_counts(df, fill, sin_cat_label)

  if (fill && "Categoria" %in% names(df_counts)) {
    p <- ggplot2::ggplot(df_counts, ggplot2::aes(x = Codigo, y = Frecuencia, fill = Categoria)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = Frecuencia), hjust = -0.3, size = 4, fontface = "bold", color = "#2c3e50") +
      ggplot2::facet_wrap(~ Archivo, scales = "free_y") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = labels$codes, y = labels$freq, fill = labels$cat)
  } else {
    p <- ggplot2::ggplot(df_counts, ggplot2::aes(x = Codigo, y = Frecuencia, fill = Codigo)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = Frecuencia), hjust = -0.3, size = 4, fontface = "bold", color = "#2c3e50") +
      ggplot2::facet_wrap(~ Archivo, scales = "free_y") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = labels$codes, y = labels$freq, fill = labels$code)
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11, face = "bold")
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  if (!fill && !is.null(code_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = code_colors)
  }
  p
}

#' @title Code co-occurrence network and centrality plot
#' @description Builds a co-occurrence network of codes (based on how frequently they appear together in files) and returns a combined network plus strength-centrality plot along with the raw centrality table.
#' @param df Data frame of fragments with columns `Archivo` and `Codigo`.
#' @param code_colors Named character vector mapping code names to hex colors; falls back to Set3 palette.
#' @param labels Named list with labels (`code`, `centrality`).
#' @return A list with elements `plot` (a patchwork ggplot combining network and centrality chart) and `table` (a tibble of centrality values and z-scores).
plot_network_and_centrality <- function(df, code_colors = NULL,
    labels = list(code = "Code", centrality = "Centrality (z-score)")) {
  dtm <- df %>%
    dplyr::count(Archivo, Codigo, name = "freq") %>%
    tidyr::pivot_wider(names_from = Codigo, values_from = freq, values_fill = 0)
  mat <- dtm %>% dplyr::select(-Archivo) %>% as.matrix() %>% crossprod()

  graph_tbl <- mat %>%
    igraph::graph_from_adjacency_matrix(weighted = TRUE) %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(
      full_name   = name,
      label_abbr  = substr(name, 1, 3),
      strength    = tidygraph::centrality_degree(weights = weight),
      closeness   = tidygraph::centrality_closeness(),
      betweenness = tidygraph::centrality_betweenness()
    )

  set.seed(2026)

  net_plot <- ggraph::ggraph(graph_tbl, layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(width = weight), color = "gray80", alpha = 0.6) +
    ggraph::scale_edge_width(range = c(0.5, 3), guide = "none") +
    ggraph::geom_node_point(ggplot2::aes(fill = full_name), shape = 21, size = 12, color = "white") +
    { if (!is.null(code_colors))
      ggplot2::scale_fill_manual(name = labels$code, values = code_colors)
    else
      ggplot2::scale_fill_brewer(name = labels$code, palette = "Set3") } +
    ggraph::geom_node_text(ggplot2::aes(label = label_abbr), size = 3.5) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.text = ggplot2::element_text(size = 9)
    )

  cents <- graph_tbl %>%
    tibble::as_tibble() %>%
    dplyr::select(full_name, strength) %>%
    tidyr::pivot_longer(cols = -full_name, names_to = "metric", values_to = "value") %>%
    dplyr::group_by(metric) %>%
    dplyr::mutate(zscore = round((value - mean(value)) / sd(value), 2)) %>%
    dplyr::ungroup()

  cent_plot <- cents %>%
    dplyr::filter(metric == "strength") %>%
    ggplot2::ggplot(ggplot2::aes(full_name, zscore, group = metric)) +
    ggplot2::geom_line(color = "gray40", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(fill = full_name), shape = 21, size = 5, color = "white", stroke = 1.5) +
    { if (!is.null(code_colors))
      ggplot2::scale_fill_manual(values = code_colors, guide = "none")
    else
      ggplot2::scale_fill_brewer(palette = "Set3", guide = "none") } +
    ggplot2::coord_flip() +
    ggplot2::labs(y = labels$centrality, x = labels$code) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 10, face = "bold")
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7)

  combined <- net_plot + cent_plot +
    patchwork::plot_layout(ncol = 2, widths = c(2.5, 1), guides = "collect")

  combined <- combined +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        legend.position = "bottom", legend.direction = "horizontal",
        legend.justification = "center", legend.box = "horizontal",
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 8),
        legend.margin = ggplot2::margin(t = 8, b = 5),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 15, l = 5)
      )
    )

  list(plot = combined, table = cents)
}

#' @title 2D semantic embedding scatter plot
#' @description Projects high-dimensional fragment embeddings into 2D using PCA, t-SNE, or UMAP, and returns a ggplot scatter plot colored by code with tooltip-ready text.
#' @param embeddings_matrix Numeric matrix. Rows correspond to fragments.
#' @param tabla Data frame of fragments with columns `Codigo`, `Extracto`, and `Categoria`.
#' @param metodo Character. Dimensionality reduction method: "pca", "tsne", or "umap" (default "pca"). Falls back to PCA if the required package is not installed.
#' @param labels Named list with label helpers (`title_fn`, `dim1_fn`, `dim2_fn`, `code`).
#' @return A ggplot object showing the 2D scatter, or NULL if the input has fewer than 3 observations.
plot_embeddings_semantico <- function(embeddings_matrix, tabla, metodo = "pca",
    labels = list(
      title_fn = function(m) paste0("Embedding Visualization (", toupper(m), ")"),
      dim1_fn = function(m) paste(toupper(m), "Dimension 1"),
      dim2_fn = function(m) paste(toupper(m), "Dimension 2"),
      code = "Code"
    )) {
  if (is.null(embeddings_matrix) || nrow(embeddings_matrix) < 3) return(NULL)
  n_obs <- nrow(embeddings_matrix)
  if (metodo == "tsne" && requireNamespace("Rtsne", quietly = TRUE)) {
    set.seed(2026)
    perplexity <- min(30, floor((n_obs - 1) / 3))
    perplexity <- max(perplexity, 1)
    tsne_result <- Rtsne::Rtsne(embeddings_matrix, dims = 2, perplexity = perplexity, verbose = FALSE, max_iter = 500)
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
  plot_data <- coords %>%
    dplyr::mutate(
      Codigo = tabla$Codigo[1:nrow(coords)],
      Extracto = stringr::str_trunc(tabla$Extracto[1:nrow(coords)], 50),
      Categoria = tabla$Categoria[1:nrow(coords)]
    )
  ggplot2::ggplot(plot_data, ggplot2::aes(x = X, y = Y, color = Codigo, text = Extracto)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::labs(title = labels$title_fn(metodo), x = labels$dim1_fn(metodo), y = labels$dim2_fn(metodo), color = labels$code) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right", plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
}
