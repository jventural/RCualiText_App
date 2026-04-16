# ========================================
# R/viz_extra.R
# Sankey, heatmap, dendrogram, timeline, project map
# ========================================

viz_extra_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "viz_extra",
    tabsetPanel(
      tabPanel("Sankey",
               br(),
               helpText("Flow between document groups -> categories -> codes."),
               actionButton("viz_sankey_run", "Render Sankey",
                            icon = icon("project-diagram"), class = "btn-primary"),
               plotly::plotlyOutput("viz_sankey", height = "600px")
      ),
      tabPanel("Heatmap",
               br(),
               helpText("Co-occurrence heatmap of codes."),
               actionButton("viz_hm_run", "Render heatmap",
                            icon = icon("th"), class = "btn-primary"),
               plotly::plotlyOutput("viz_hm", height = "600px")
      ),
      tabPanel("Dendrogram",
               br(),
               helpText("Hierarchical clustering of codes based on co-occurrence."),
               numericInput("viz_dendro_k", "Clusters (k)", 4, min = 2, max = 15),
               actionButton("viz_dendro_run", "Compute dendrogram",
                            icon = icon("sitemap"), class = "btn-primary"),
               plotOutput("viz_dendro", height = "600px")
      ),
      tabPanel("Timeline",
               br(),
               helpText("Coding activity over time (timestamp axis)."),
               actionButton("viz_tl_run", "Render timeline",
                            icon = icon("clock"), class = "btn-primary"),
               plotly::plotlyOutput("viz_timeline", height = "500px")
      ),
      tabPanel("Project Map",
               br(),
               helpText("Free-form draggable network of codes, categories, memos."),
               actionButton("viz_map_run", "Build project map",
                            icon = icon("map"), class = "btn-primary"),
               visNetwork::visNetworkOutput("viz_map", height = "700px")
      )
    )
  )
}

setup_viz_extra_server <- function(input, output, session, rv) {
  # ---- Sankey ----
  observeEvent(input$viz_sankey_run, {
    output$viz_sankey <- plotly::renderPlotly({
      req(rv$tabla); if (nrow(rv$tabla) == 0) return(NULL)
      df <- rv$tabla
      # Nodes: Archivo, Categoria, Codigo
      df$Categoria <- ifelse(is.na(df$Categoria) | df$Categoria == "",
                             "(uncategorized)", df$Categoria)
      nodes <- unique(c(paste0("D:", df$Archivo),
                        paste0("C:", df$Categoria),
                        paste0("K:", df$Codigo)))
      idx <- setNames(seq_along(nodes) - 1, nodes)
      e1 <- df %>% dplyr::count(Archivo, Categoria) %>%
        dplyr::transmute(source = idx[paste0("D:", Archivo)],
                         target = idx[paste0("C:", Categoria)], value = n)
      e2 <- df %>% dplyr::count(Categoria, Codigo) %>%
        dplyr::transmute(source = idx[paste0("C:", Categoria)],
                         target = idx[paste0("K:", Codigo)], value = n)
      edges <- dplyr::bind_rows(e1, e2)
      plotly::plot_ly(
        type = "sankey",
        node = list(label = sub("^[DCK]:", "", nodes), pad = 15, thickness = 20),
        link = list(source = edges$source, target = edges$target,
                    value = edges$value)
      )
    })
  })

  # ---- Heatmap ----
  observeEvent(input$viz_hm_run, {
    output$viz_hm <- plotly::renderPlotly({
      req(rv$tabla); if (nrow(rv$tabla) == 0) return(NULL)
      df <- rv$tabla
      # Co-occurrence: doc x code matrix -> crossprod
      mat <- table(df$Archivo, df$Codigo)
      co <- t(mat) %*% mat
      diag(co) <- 0
      plotly::plot_ly(x = colnames(co), y = rownames(co),
                      z = as.matrix(co), type = "heatmap",
                      colors = "YlOrRd")
    })
  })

  # ---- Dendrogram ----
  observeEvent(input$viz_dendro_run, {
    output$viz_dendro <- renderPlot({
      req(rv$tabla)
      mat <- table(rv$tabla$Archivo, rv$tabla$Codigo)
      if (ncol(mat) < 3) return(plot.new())
      co <- t(mat) %*% mat; diag(co) <- 0
      d <- as.dist(1 - co / (max(co) + 1e-9))
      hc <- hclust(d, method = "average")
      k <- max(2, min(input$viz_dendro_k %||% 4, length(hc$labels) - 1))
      plot(hc, main = "Code similarity dendrogram", xlab = "", sub = "")
      rect.hclust(hc, k = k, border = 2:(k + 1))
    })
  })

  # ---- Timeline ----
  observeEvent(input$viz_tl_run, {
    output$viz_timeline <- plotly::renderPlotly({
      req(rv$tabla); if (!"Timestamp" %in% names(rv$tabla)) return(NULL)
      df <- rv$tabla[!is.na(rv$tabla$Timestamp), ]
      if (nrow(df) == 0) return(NULL)
      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x = Timestamp, y = Codigo,
                                        color = Archivo,
                                        text = substr(Extracto, 1, 80))) +
        ggplot2::geom_point(size = 2, alpha = 0.7) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Coding timeline")
      plotly::ggplotly(p, tooltip = c("x", "y", "color", "text"))
    })
  })

  # ---- Project map ----
  observeEvent(input$viz_map_run, {
    output$viz_map <- visNetwork::renderVisNetwork({
      req(rv$codigosDF)
      codes <- rv$codigosDF$Codigo
      cats <- unique(rv$categoriasDF$Categoria)
      memo_titles <- if (!is.null(rv$memos)) rv$memos$titulo else character()
      nodes <- tibble::tibble(
        id = c(codes, cats, memo_titles),
        label = c(codes, cats, memo_titles),
        group = c(rep("Code", length(codes)),
                  rep("Category", length(cats)),
                  rep("Memo", length(memo_titles))),
        color = c(ifelse(is.na(rv$codigosDF$Color), "#3498db", rv$codigosDF$Color),
                  rep("#e67e22", length(cats)),
                  rep("#f1c40f", length(memo_titles)))
      )
      edges <- tibble::tibble(from = character(), to = character())
      if (!is.null(rv$categoriasDF) && nrow(rv$categoriasDF) > 0) {
        for (i in seq_len(nrow(rv$categoriasDF))) {
          cc <- strsplit(rv$categoriasDF$Codigos[i], ",\\s*")[[1]]
          edges <- dplyr::bind_rows(edges,
                                    tibble::tibble(from = rv$categoriasDF$Categoria[i],
                                                   to = cc))
        }
      }
      if (!is.null(rv$memos) && nrow(rv$memos) > 0) {
        mv <- rv$memos[rv$memos$vinculo_tipo == "codigo", ]
        if (nrow(mv) > 0) {
          edges <- dplyr::bind_rows(edges,
                                    tibble::tibble(from = mv$titulo,
                                                   to = mv$vinculo_id))
        }
      }
      visNetwork::visNetwork(nodes, edges, width = "100%") %>%
        visNetwork::visPhysics(stabilization = FALSE) %>%
        visNetwork::visOptions(highlightNearest = TRUE,
                               nodesIdSelection = TRUE) %>%
        visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE,
                                   zoomView = TRUE) %>%
        visNetwork::visLegend()
    })
  })
}
