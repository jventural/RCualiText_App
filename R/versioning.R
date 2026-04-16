# ========================================
# R/versioning.R
# Auto-save with rotation + codebook diff between versions
# ========================================

versioning_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "versioning",
    fluidRow(
      box(
        width = 4, title = "Auto-save", status = "primary", solidHeader = TRUE,
        checkboxInput("ver_autosave_on", "Enable auto-save", value = TRUE),
        numericInput("ver_interval", "Interval (minutes)", value = 5,
                     min = 1, max = 60),
        numericInput("ver_keep", "Keep latest N snapshots",
                     value = 10, min = 2, max = 50),
        textInput("ver_dir", "Directory",
                  value = file.path(tempdir(), "rcualitext_versions")),
        actionButton("ver_save_now", "Save snapshot now",
                     icon = icon("save"), class = "btn-primary"),
        hr(),
        verbatimTextOutput("ver_status")
      ),
      box(
        width = 8, title = "Snapshots", status = "success", solidHeader = TRUE,
        DT::DTOutput("ver_snapshots"),
        hr(),
        actionButton("ver_restore", "Restore selected",
                     icon = icon("undo"), class = "btn-warning"),
        actionButton("ver_delete", "Delete selected",
                     icon = icon("trash"), class = "btn-danger")
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Codebook diff (between two snapshots)",
        status = "info", solidHeader = TRUE,
        fluidRow(
          column(4, selectInput("ver_diff_a", "Snapshot A (older)",
                                choices = NULL)),
          column(4, selectInput("ver_diff_b", "Snapshot B (newer)",
                                choices = NULL)),
          column(4, actionButton("ver_diff_run", "Compute diff",
                                 icon = icon("code-branch"),
                                 class = "btn-primary"))
        ),
        hr(),
        h4("Added codes"), verbatimTextOutput("ver_diff_added"),
        h4("Removed codes"), verbatimTextOutput("ver_diff_removed"),
        h4("Renamed / color-changed"), DT::DTOutput("ver_diff_changed")
      )
    )
  )
}

ver_snapshot_list <- function(dir) {
  if (!dir.exists(dir)) return(tibble::tibble(File = character(),
                                              Modified = as.POSIXct(character()),
                                              SizeKB = numeric()))
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(tibble::tibble(File = character(),
                                                Modified = as.POSIXct(character()),
                                                SizeKB = numeric()))
  info <- file.info(files)
  tibble::tibble(File = basename(files),
                 Modified = info$mtime,
                 SizeKB = round(info$size / 1024, 1)) %>%
    dplyr::arrange(dplyr::desc(Modified))
}

ver_save_snapshot <- function(rv, dir, keep = 10) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  path <- file.path(dir, paste0("snap_", ts, ".rds"))
  # Read rv inside isolate to avoid creating reactive deps that re-fire the calling observer
  estado <- shiny::isolate({
    list(
      codigosDF = rv$codigosDF, categoriasDF = rv$categoriasDF,
      docs = rv$docs, tabla = rv$tabla, memos = rv$memos,
      descriptores = rv$descriptores,
      code_groups = rv$code_groups, doc_groups = rv$doc_groups,
      research_questions = rv$research_questions,
      bookmarks = rv$bookmarks, audit_log = rv$audit_log,
      hyperlinks = rv$hyperlinks, coders = rv$coders,
      saved_at = Sys.time()
    )
  })
  saveRDS(estado, path)
  # Rotation
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) > keep) {
    info <- file.info(files)
    ord <- order(info$mtime)
    to_delete <- files[ord[seq_len(length(files) - keep)]]
    file.remove(to_delete)
  }
  path
}

setup_versioning_server <- function(input, output, session, rv) {
  rv_status <- reactiveValues(last = "never", n = 0)

  # Auto-save observer
  # IMPORTANT: rv reads must be isolated to avoid creating reactive deps that
  # retrigger the observer in a hot loop (which floods the WS message queue
  # and blocks initial output values from being flushed to the client).
  observe({
    # Require inputs to be initialized before running (avoids spurious fire on init)
    req(input$ver_autosave_on, input$ver_dir, input$ver_interval, input$ver_keep)
    wait_ms <- max(60, (input$ver_interval %||% 5) * 60) * 1000
    invalidateLater(wait_ms, session)
    isolate({
      tryCatch({
        p <- ver_save_snapshot(rv, input$ver_dir, input$ver_keep)
        rv_status$last <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        rv_status$n <- rv_status$n + 1
      }, error = function(e) NULL)
    })
  }, priority = -100)

  observeEvent(input$ver_save_now, {
    tryCatch({
      p <- ver_save_snapshot(rv, input$ver_dir, input$ver_keep)
      showNotification(paste("Saved:", basename(p)), type = "message")
      rv_status$last <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }, error = function(e) {
      showNotification(paste("Save failed:", conditionMessage(e)),
                       type = "error")
    })
  })

  output$ver_status <- renderText({
    paste0("Last snapshot: ", rv_status$last,
           "\nSnapshots this session: ", rv_status$n)
  })

  snapshots_r <- reactive({
    req(input$ver_dir)
    invalidateLater(5000, session)
    ver_snapshot_list(input$ver_dir %||% file.path(tempdir(), "rcualitext_versions"))
  })

  output$ver_snapshots <- DT::renderDT({
    DT::datatable(snapshots_r(), rownames = FALSE, selection = "single",
                  options = list(pageLength = 10, order = list(list(1, "desc"))))
  })

  observe({
    f <- snapshots_r()$File
    updateSelectInput(session, "ver_diff_a", choices = f)
    updateSelectInput(session, "ver_diff_b", choices = f)
  })

  observeEvent(input$ver_restore, {
    sel <- input$ver_snapshots_rows_selected
    if (length(sel) == 0) {
      showNotification("Select a snapshot first", type = "warning"); return()
    }
    fn <- snapshots_r()$File[sel]
    full <- file.path(input$ver_dir, fn)
    estado <- readRDS(full)
    for (k in names(estado)) {
      rv[[k]] <- estado[[k]]
    }
    showNotification(paste("Restored", fn), type = "message")
  })

  observeEvent(input$ver_delete, {
    sel <- input$ver_snapshots_rows_selected
    if (length(sel) == 0) return()
    fn <- snapshots_r()$File[sel]
    file.remove(file.path(input$ver_dir, fn))
  })

  # Codebook diff
  diff_res <- reactiveVal(NULL)
  observeEvent(input$ver_diff_run, {
    req(input$ver_diff_a, input$ver_diff_b)
    a <- readRDS(file.path(input$ver_dir, input$ver_diff_a))
    b <- readRDS(file.path(input$ver_dir, input$ver_diff_b))
    codes_a <- a$codigosDF$Codigo; codes_b <- b$codigosDF$Codigo
    added <- setdiff(codes_b, codes_a)
    removed <- setdiff(codes_a, codes_b)
    # Color/renames on common
    common <- intersect(codes_a, codes_b)
    ca <- a$codigosDF[match(common, a$codigosDF$Codigo), ]
    cb <- b$codigosDF[match(common, b$codigosDF$Codigo), ]
    changed <- tibble::tibble(
      Codigo = common,
      Color_A = ca$Color, Color_B = cb$Color,
      Parent_A = ca$Parent %||% NA, Parent_B = cb$Parent %||% NA
    )
    changed <- changed[changed$Color_A != changed$Color_B |
                         (!is.na(changed$Parent_A) &
                            changed$Parent_A != changed$Parent_B), ]
    diff_res(list(added = added, removed = removed, changed = changed))
  })
  output$ver_diff_added <- renderPrint({
    req(diff_res()); cat(paste(diff_res()$added, collapse = ", "))
  })
  output$ver_diff_removed <- renderPrint({
    req(diff_res()); cat(paste(diff_res()$removed, collapse = ", "))
  })
  output$ver_diff_changed <- DT::renderDT({
    req(diff_res()); DT::datatable(diff_res()$changed, rownames = FALSE,
                                   options = list(pageLength = 10))
  })
}
