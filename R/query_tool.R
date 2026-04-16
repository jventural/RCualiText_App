# ========================================
# R/query_tool.R
# NVivo-like boolean query builder + matrix queries
# ========================================

query_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "query",
    fluidRow(
      box(
        width = 12, title = "Boolean Query Builder", status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput("query_codes_a", "Codes (A)", choices = NULL,
                                   multiple = TRUE)),
          column(2, selectInput("query_op", "Operator",
                                choices = c("AND", "OR", "NOT", "NEAR"))),
          column(3, selectizeInput("query_codes_b", "Codes (B)", choices = NULL,
                                   multiple = TRUE)),
          column(2, numericInput("query_near_window",
                                 "NEAR window (chars)", 150, min = 10, max = 2000)),
          column(2, actionButton("query_run", "Run query",
                                 icon = icon("play"), class = "btn-primary"))
        ),
        hr(),
        h4("Descriptor filter (optional)"),
        uiOutput("query_desc_filters"),
        hr(),
        h4("Results"),
        verbatimTextOutput("query_summary"),
        DT::DTOutput("query_results_tbl"),
        downloadButton("query_download", "Download results (xlsx)")
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Matrix Query (descriptor x code)", status = "success",
        solidHeader = TRUE,
        fluidRow(
          column(4, selectInput("query_mat_desc", "Descriptor variable",
                                choices = NULL)),
          column(4, selectizeInput("query_mat_codes", "Codes (columns)",
                                   choices = NULL, multiple = TRUE)),
          column(4, actionButton("query_mat_run", "Compute matrix",
                                 icon = icon("th"), class = "btn-primary"))
        ),
        DT::DTOutput("query_matrix_tbl"),
        helpText("Click any cell to see underlying excerpts"),
        DT::DTOutput("query_matrix_excerpts")
      )
    )
  )
}

# Evaluate boolean query over rv$tabla
query_evaluate <- function(tabla, codes_a, op, codes_b, near_window = 150) {
  if (is.null(tabla) || nrow(tabla) == 0) return(tabla[0, ])
  a <- tabla[tabla$Codigo %in% codes_a, ]
  b <- tabla[tabla$Codigo %in% codes_b, ]
  if (op == "AND") {
    # Same fragment or same document with both
    common_docs <- intersect(a$Archivo, b$Archivo)
    return(tabla[tabla$Archivo %in% common_docs &
                   tabla$Codigo %in% c(codes_a, codes_b), ])
  }
  if (op == "OR") {
    return(tabla[tabla$Codigo %in% c(codes_a, codes_b), ])
  }
  if (op == "NOT") {
    docs_b <- unique(b$Archivo)
    return(a[!a$Archivo %in% docs_b, ])
  }
  if (op == "NEAR") {
    # Rough NEAR: within 'near_window' characters of both codes in same doc
    if (!"char_start" %in% names(a) || !"char_start" %in% names(b)) {
      # Fallback: same document
      common_docs <- intersect(a$Archivo, b$Archivo)
      return(tabla[tabla$Archivo %in% common_docs &
                     tabla$Codigo %in% c(codes_a, codes_b), ])
    }
    hits <- list()
    for (d in intersect(a$Archivo, b$Archivo)) {
      aa <- a[a$Archivo == d, ]; bb <- b[b$Archivo == d, ]
      for (i in seq_len(nrow(aa))) {
        for (j in seq_len(nrow(bb))) {
          if (abs(aa$char_start[i] - bb$char_start[j]) <= near_window) {
            hits[[length(hits) + 1]] <- aa[i, ]
            hits[[length(hits) + 1]] <- bb[j, ]
          }
        }
      }
    }
    if (length(hits) == 0) return(tabla[0, ])
    return(dplyr::distinct(dplyr::bind_rows(hits)))
  }
  tabla[0, ]
}

setup_query_server <- function(input, output, session, rv) {
  observe({
    codes <- if (!is.null(rv$codigosDF)) rv$codigosDF$Codigo else character()
    updateSelectizeInput(session, "query_codes_a", choices = codes, server = TRUE)
    updateSelectizeInput(session, "query_codes_b", choices = codes, server = TRUE)
    updateSelectizeInput(session, "query_mat_codes", choices = codes, server = TRUE)
    vars <- if (!is.null(rv$descriptores))
      setdiff(names(rv$descriptores), "Archivo") else character()
    updateSelectInput(session, "query_mat_desc", choices = vars)
  })

  output$query_desc_filters <- renderUI({
    if (is.null(rv$descriptores) || ncol(rv$descriptores) <= 1) {
      return(tags$em("No descriptors defined"))
    }
    vars <- setdiff(names(rv$descriptores), "Archivo")
    tagList(lapply(vars, function(v) {
      vals <- unique(rv$descriptores[[v]])
      vals <- vals[!is.na(vals) & vals != ""]
      selectInput(paste0("query_desc_f_", v), v,
                  choices = c("All" = "", vals), multiple = TRUE)
    }))
  })

  # Filter tabla by descriptor filters
  filtered_tabla <- reactive({
    df <- rv$tabla
    if (!is.null(rv$descriptores) && ncol(rv$descriptores) > 1) {
      vars <- setdiff(names(rv$descriptores), "Archivo")
      keep_docs <- rv$descriptores$Archivo
      for (v in vars) {
        val <- input[[paste0("query_desc_f_", v)]]
        if (!is.null(val) && length(val) > 0 && !all(val == "")) {
          keep_docs <- intersect(keep_docs,
                                 rv$descriptores$Archivo[rv$descriptores[[v]] %in% val])
        }
      }
      df <- df[df$Archivo %in% keep_docs, ]
    }
    df
  })

  query_res <- reactiveVal(NULL)
  observeEvent(input$query_run, {
    df <- filtered_tabla()
    res <- query_evaluate(df, input$query_codes_a, input$query_op,
                          input$query_codes_b, input$query_near_window)
    query_res(res)
  })

  output$query_summary <- renderPrint({
    req(query_res())
    cat("Matching fragments:", nrow(query_res()), "\n")
    cat("Across documents:", length(unique(query_res()$Archivo)), "\n")
    cat("Codes involved:", paste(unique(query_res()$Codigo), collapse = ", "), "\n")
  })
  output$query_results_tbl <- DT::renderDT({
    req(query_res())
    DT::datatable(query_res()[, intersect(c("Archivo", "Codigo", "Extracto",
                                            "Coder", "FragmentId"),
                                          names(query_res()))],
                  rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$query_download <- downloadHandler(
    filename = function() paste0("query_results_",
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      req(query_res()); openxlsx::write.xlsx(query_res(), file)
    }
  )

  # Matrix queries
  query_matrix <- reactiveVal(NULL)
  observeEvent(input$query_mat_run, {
    req(rv$tabla, rv$descriptores, input$query_mat_desc)
    codes <- input$query_mat_codes
    if (length(codes) == 0) {
      showNotification("Pick codes first", type = "warning"); return()
    }
    df <- dplyr::left_join(rv$tabla, rv$descriptores, by = "Archivo")
    var <- input$query_mat_desc
    mat <- df %>%
      dplyr::filter(Codigo %in% codes) %>%
      dplyr::count(!!rlang::sym(var), Codigo) %>%
      tidyr::pivot_wider(names_from = Codigo, values_from = n, values_fill = 0)
    query_matrix(list(mat = mat, df = df, var = var, codes = codes))
  })
  output$query_matrix_tbl <- DT::renderDT({
    req(query_matrix())
    DT::datatable(query_matrix()$mat, rownames = FALSE,
                  selection = list(mode = "single", target = "cell"),
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  output$query_matrix_excerpts <- DT::renderDT({
    req(query_matrix())
    cell <- input$query_matrix_tbl_cells_selected
    if (is.null(cell) || length(cell) == 0 || nrow(cell) == 0) {
      return(DT::datatable(data.frame(Info = "Click a cell above to see excerpts"),
                           rownames = FALSE, options = list(dom = "t")))
    }
    row <- cell[1, 1] + 1; col <- cell[1, 2] + 1
    mat <- query_matrix()$mat
    var <- query_matrix()$var
    # Column 1 is the descriptor value; others are codes
    if (col == 1) return(NULL)
    val <- mat[[var]][row]
    code <- names(mat)[col]
    df <- query_matrix()$df
    sub <- df[df[[var]] == val & df$Codigo == code, ]
    DT::datatable(sub[, intersect(c("Archivo", "Extracto", "Codigo"),
                                  names(sub))],
                  rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })
}
