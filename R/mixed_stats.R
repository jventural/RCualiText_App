# ========================================
# R/mixed_stats.R
# Chi-square descriptor x code + QCA export
# ========================================

mixed_stats_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "mixed_stats",
    fluidRow(
      box(
        width = 6, title = "Chi-square (descriptor x code)",
        status = "primary", solidHeader = TRUE,
        selectInput("mx_desc_var", "Descriptor", choices = NULL),
        selectizeInput("mx_codes", "Codes", choices = NULL, multiple = TRUE),
        actionButton("mx_run_chi", "Run test", icon = icon("calculator"),
                     class = "btn-primary"),
        hr(),
        verbatimTextOutput("mx_chi_output"),
        plotOutput("mx_chi_plot", height = "400px")
      ),
      box(
        width = 6, title = "QCA (Qualitative Comparative Analysis) export",
        status = "success", solidHeader = TRUE,
        helpText("Exports a truth-table-ready CSV: cases x codes (0/1)."),
        selectInput("mx_qca_case", "Case variable",
                    choices = NULL),
        selectizeInput("mx_qca_codes", "Outcome/conditions (codes)",
                       choices = NULL, multiple = TRUE),
        numericInput("mx_qca_threshold",
                     "Presence threshold (min code count per case)",
                     value = 1, min = 1),
        downloadButton("mx_qca_download", "Download truth table (csv)"),
        hr(),
        actionButton("mx_qca_run", "Run QCA (needs 'QCA' package)",
                     icon = icon("play"), class = "btn-primary"),
        verbatimTextOutput("mx_qca_output")
      )
    )
  )
}

setup_mixed_stats_server <- function(input, output, session, rv) {
  observe({
    vars <- if (!is.null(rv$descriptores))
      setdiff(names(rv$descriptores), "Archivo") else character()
    updateSelectInput(session, "mx_desc_var", choices = vars)
    updateSelectInput(session, "mx_qca_case",
                      choices = c("Archivo", vars))
    codes <- if (!is.null(rv$codigosDF)) rv$codigosDF$Codigo else character()
    updateSelectizeInput(session, "mx_codes", choices = codes, server = TRUE)
    updateSelectizeInput(session, "mx_qca_codes", choices = codes, server = TRUE)
  })

  mx_chi <- reactiveVal(NULL)
  observeEvent(input$mx_run_chi, {
    req(rv$tabla, rv$descriptores, input$mx_desc_var, input$mx_codes)
    df <- dplyr::left_join(rv$tabla, rv$descriptores, by = "Archivo")
    df <- df[df$Codigo %in% input$mx_codes, ]
    if (nrow(df) == 0) {
      mx_chi(list(err = "No rows"))
      return()
    }
    tbl <- table(df[[input$mx_desc_var]], df$Codigo)
    chi <- tryCatch(suppressWarnings(chisq.test(tbl)),
                    error = function(e) NULL)
    mx_chi(list(tbl = tbl, test = chi))
  })
  output$mx_chi_output <- renderPrint({
    res <- mx_chi(); req(res)
    if (!is.null(res$err)) { cat(res$err); return() }
    cat("Contingency table:\n"); print(res$tbl)
    cat("\n\nChi-square test:\n")
    if (is.null(res$test)) cat("Test not computable") else print(res$test)
  })
  output$mx_chi_plot <- renderPlot({
    res <- mx_chi(); req(res, res$tbl)
    df <- as.data.frame(res$tbl)
    names(df) <- c("Descriptor", "Code", "Freq")
    ggplot2::ggplot(df, ggplot2::aes(x = Descriptor, y = Freq, fill = Code)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Descriptor x Code frequencies")
  })

  # QCA truth table
  mx_qca_table <- reactive({
    req(rv$tabla, input$mx_qca_case, input$mx_qca_codes)
    case_var <- input$mx_qca_case
    df <- if (case_var == "Archivo") rv$tabla else
      dplyr::left_join(rv$tabla, rv$descriptores, by = "Archivo")
    cnt <- df %>%
      dplyr::filter(Codigo %in% input$mx_qca_codes) %>%
      dplyr::count(!!rlang::sym(case_var), Codigo) %>%
      tidyr::pivot_wider(names_from = Codigo, values_from = n, values_fill = 0)
    # Dichotomize
    th <- input$mx_qca_threshold %||% 1
    ids <- cnt[[case_var]]
    mat <- as.data.frame(lapply(cnt[, -1, drop = FALSE],
                                function(x) as.integer(x >= th)))
    cbind(setNames(data.frame(ids), case_var), mat)
  })

  output$mx_qca_download <- downloadHandler(
    filename = function() paste0("qca_truth_table_",
                                 format(Sys.time(), "%Y%m%d"), ".csv"),
    content = function(file) {
      write.csv(mx_qca_table(), file, row.names = FALSE)
    }
  )

  observeEvent(input$mx_qca_run, {
    if (!requireNamespace("QCA", quietly = TRUE)) {
      showNotification("Install the 'QCA' package to run", type = "warning")
      return()
    }
    tt <- mx_qca_table()
    if (ncol(tt) < 3) {
      showNotification("Need at least 2 codes + case variable",
                       type = "warning"); return()
    }
    # Treat last selected code as outcome, rest as conditions
    outcome <- tail(input$mx_qca_codes, 1)
    conds <- setdiff(input$mx_qca_codes, outcome)
    tryCatch({
      # Sanitize column names
      nm <- make.names(names(tt)); names(tt) <- nm
      outcome_sn <- make.names(outcome); conds_sn <- make.names(conds)
      out <- QCA::truthTable(tt, outcome = outcome_sn,
                             conditions = conds_sn, n.cut = 1,
                             incl.cut = 0.8, show.cases = TRUE)
      output$mx_qca_output <- renderPrint({ print(out) })
    }, error = function(e) {
      output$mx_qca_output <- renderPrint(cat("QCA error:",
                                              conditionMessage(e)))
    })
  })
}
