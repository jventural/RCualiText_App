# ========================================
# R/collaboration.R
# Multi-coder support: users, document lock, side-by-side compare, reconciliation
# ========================================

collab_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "collab",
    fluidRow(
      box(
        width = 4, title = "Coders", status = "primary", solidHeader = TRUE,
        textInput("collab_new_coder", "Add coder name"),
        textInput("collab_new_role", "Role (e.g., primary, secondary)"),
        actionButton("collab_add_coder", "Add coder", icon = icon("user-plus")),
        hr(),
        DT::DTOutput("collab_coders_tbl"),
        hr(),
        selectInput("collab_active_coder", "Active coder (this session)",
                    choices = NULL),
        helpText("All codings this session are stamped with the active coder.")
      ),
      box(
        width = 4, title = "Document Lock", status = "warning", solidHeader = TRUE,
        uiOutput("collab_lock_status"),
        actionButton("collab_lock_doc", "Lock current doc", icon = icon("lock")),
        actionButton("collab_unlock_doc", "Release lock", icon = icon("unlock")),
        hr(),
        h5("Active locks"),
        DT::DTOutput("collab_locks_tbl")
      ),
      box(
        width = 4, title = "Assignments", status = "info", solidHeader = TRUE,
        selectInput("collab_assign_doc", "Document", choices = NULL),
        selectInput("collab_assign_coder", "Assigned to", choices = NULL),
        actionButton("collab_assign", "Assign", icon = icon("tasks")),
        hr(),
        DT::DTOutput("collab_assignments_tbl")
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Side-by-side comparison / reconciliation",
        status = "success", solidHeader = TRUE,
        fluidRow(
          column(4, selectInput("collab_cmp_coder_a", "Coder A", choices = NULL)),
          column(4, selectInput("collab_cmp_coder_b", "Coder B", choices = NULL)),
          column(4, selectInput("collab_cmp_doc", "Document", choices = NULL))
        ),
        actionButton("collab_cmp_compute", "Compare", icon = icon("columns"),
                     class = "btn-primary"),
        hr(),
        fluidRow(
          column(6, h4("Coder A"), DT::DTOutput("collab_cmp_a")),
          column(6, h4("Coder B"), DT::DTOutput("collab_cmp_b"))
        ),
        hr(),
        h4("Agreement summary"),
        verbatimTextOutput("collab_cmp_summary"),
        h4("Conflicts (same fragment, different codes)"),
        DT::DTOutput("collab_conflicts"),
        hr(),
        h4("Resolve conflict"),
        fluidRow(
          column(4, selectInput("collab_resolve_which", "Keep",
                                choices = c("Coder A", "Coder B", "Both",
                                            "Delete both"))),
          column(4, actionButton("collab_resolve_apply", "Apply",
                                 icon = icon("gavel"), class = "btn-warning"))
        )
      )
    )
  )
}

setup_collab_server <- function(input, output, session, rv) {
  # --- reactive state (isolate for non-reactive init) ---
  isolate({
    if (is.null(rv$coders)) rv$coders <- tibble::tibble(
      Coder = character(), Role = character(),
      JoinedAt = as.POSIXct(character()))
    if (is.null(rv$locks)) rv$locks <- tibble::tibble(
      Archivo = character(), Coder = character(),
      LockedAt = as.POSIXct(character()))
    if (is.null(rv$assignments)) rv$assignments <- tibble::tibble(
      Archivo = character(), Coder = character())
    if (is.null(rv$active_coder)) rv$active_coder <- NA_character_
    # ensure tabla has Coder column (one-time init)
    if (!"Coder" %in% names(rv$tabla)) {
      rv$tabla$Coder <- NA_character_
    }
  })

  observeEvent(input$collab_add_coder, {
    nm <- trimws(input$collab_new_coder %||% "")
    if (nchar(nm) == 0) return()
    if (nm %in% rv$coders$Coder) {
      showNotification("Coder already exists", type = "warning"); return()
    }
    rv$coders <- dplyr::bind_rows(rv$coders, tibble::tibble(
      Coder = nm, Role = input$collab_new_role %||% "",
      JoinedAt = Sys.time()))
    updateSelectInput(session, "collab_active_coder",
                      choices = rv$coders$Coder)
    updateSelectInput(session, "collab_assign_coder",
                      choices = rv$coders$Coder)
    updateSelectInput(session, "collab_cmp_coder_a", choices = rv$coders$Coder)
    updateSelectInput(session, "collab_cmp_coder_b", choices = rv$coders$Coder)
  })

  output$collab_coders_tbl <- DT::renderDT({
    DT::datatable(rv$coders, rownames = FALSE,
                  options = list(pageLength = 5, dom = "t"))
  })

  observeEvent(input$collab_active_coder, {
    rv$active_coder <- input$collab_active_coder
  })

  # Lock / unlock
  observeEvent(input$collab_lock_doc, {
    req(rv$docs, rv$idx)
    coder <- rv$active_coder
    if (is.na(coder) || nchar(coder) == 0) {
      showNotification("Set an active coder first", type = "error"); return()
    }
    doc <- rv$docs[[rv$idx]]$name
    if (doc %in% rv$locks$Archivo) {
      showNotification("Document already locked", type = "warning"); return()
    }
    rv$locks <- dplyr::bind_rows(rv$locks, tibble::tibble(
      Archivo = doc, Coder = coder, LockedAt = Sys.time()))
  })
  observeEvent(input$collab_unlock_doc, {
    req(rv$docs, rv$idx)
    doc <- rv$docs[[rv$idx]]$name
    rv$locks <- rv$locks[rv$locks$Archivo != doc, ]
  })
  output$collab_lock_status <- renderUI({
    if (is.null(rv$docs) || rv$idx == 0) return(tags$em("No active document"))
    doc <- rv$docs[[rv$idx]]$name
    row <- rv$locks[rv$locks$Archivo == doc, ]
    if (nrow(row) == 0) {
      tags$span(icon("unlock"), " ", doc, " is unlocked",
                style = "color: #27ae60;")
    } else {
      tags$span(icon("lock"), " ", doc, " locked by ", row$Coder[1],
                style = "color: #e74c3c;")
    }
  })
  output$collab_locks_tbl <- DT::renderDT({
    DT::datatable(rv$locks, rownames = FALSE,
                  options = list(pageLength = 5, dom = "t"))
  })

  # Assignments
  observe({
    docs_n <- if (!is.null(rv$docs)) sapply(rv$docs, function(d) d$name) else character()
    updateSelectInput(session, "collab_assign_doc", choices = docs_n)
    updateSelectInput(session, "collab_cmp_doc", choices = docs_n)
  })
  observeEvent(input$collab_assign, {
    req(input$collab_assign_doc, input$collab_assign_coder)
    rv$assignments <- dplyr::bind_rows(
      rv$assignments[rv$assignments$Archivo != input$collab_assign_doc, ],
      tibble::tibble(Archivo = input$collab_assign_doc,
                     Coder = input$collab_assign_coder))
  })
  output$collab_assignments_tbl <- DT::renderDT({
    DT::datatable(rv$assignments, rownames = FALSE,
                  options = list(pageLength = 5, dom = "t"))
  })

  # Compare two coders
  collab_cmp <- reactiveVal(NULL)
  observeEvent(input$collab_cmp_compute, {
    req(rv$tabla, "Coder" %in% names(rv$tabla))
    a <- input$collab_cmp_coder_a; b <- input$collab_cmp_coder_b
    doc <- input$collab_cmp_doc
    df <- rv$tabla
    if (!is.null(doc) && nchar(doc) > 0) df <- df[df$Archivo == doc, ]
    dfA <- df[df$Coder == a, ]; dfB <- df[df$Coder == b, ]
    # Conflicts: same FragmentId, different Codigo
    join <- dplyr::inner_join(dfA[, c("FragmentId", "Extracto", "Codigo")],
                              dfB[, c("FragmentId", "Codigo")],
                              by = "FragmentId",
                              suffix = c("_A", "_B"))
    conflicts <- join[join$Codigo_A != join$Codigo_B, ]
    # Agreement
    common <- intersect(dfA$FragmentId, dfB$FragmentId)
    agree <- sum(dfA$Codigo[match(common, dfA$FragmentId)] ==
                   dfB$Codigo[match(common, dfB$FragmentId)])
    total <- length(common)
    pct <- if (total > 0) round(100 * agree / total, 2) else NA
    collab_cmp(list(dfA = dfA, dfB = dfB, conflicts = conflicts,
                    agree = agree, total = total, pct = pct))
  })
  output$collab_cmp_a <- DT::renderDT({
    req(collab_cmp())
    DT::datatable(collab_cmp()$dfA[, c("FragmentId", "Extracto", "Codigo")],
                  rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$collab_cmp_b <- DT::renderDT({
    req(collab_cmp())
    DT::datatable(collab_cmp()$dfB[, c("FragmentId", "Extracto", "Codigo")],
                  rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$collab_cmp_summary <- renderPrint({
    req(collab_cmp())
    x <- collab_cmp()
    cat("Common fragments:", x$total, "\n")
    cat("Agreements:", x$agree, "\n")
    cat("Agreement %:", x$pct, "%\n")
    cat("Conflicts:", nrow(x$conflicts), "\n")
  })
  output$collab_conflicts <- DT::renderDT({
    req(collab_cmp())
    DT::datatable(collab_cmp()$conflicts, rownames = FALSE,
                  selection = "single",
                  options = list(pageLength = 5, scrollX = TRUE))
  })

  observeEvent(input$collab_resolve_apply, {
    req(collab_cmp())
    sel <- input$collab_conflicts_rows_selected
    if (length(sel) == 0) {
      showNotification("Select a conflict row first", type = "error"); return()
    }
    conflict <- collab_cmp()$conflicts[sel, ]
    which <- input$collab_resolve_which
    fid <- conflict$FragmentId
    a <- input$collab_cmp_coder_a; b <- input$collab_cmp_coder_b
    if (which == "Coder A") {
      rv$tabla <- rv$tabla[!(rv$tabla$FragmentId == fid & rv$tabla$Coder == b), ]
    } else if (which == "Coder B") {
      rv$tabla <- rv$tabla[!(rv$tabla$FragmentId == fid & rv$tabla$Coder == a), ]
    } else if (which == "Delete both") {
      rv$tabla <- rv$tabla[rv$tabla$FragmentId != fid, ]
    }
    showNotification("Conflict resolved", type = "message")
  })
}
