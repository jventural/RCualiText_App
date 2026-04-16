# ========================================
# R/templates.R
# Analysis templates (Braun & Clarke, Grounded Theory, Content Analysis)
# ========================================

templates_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "templates",
    fluidRow(
      box(
        width = 12, title = "Analysis Templates", status = "primary",
        solidHeader = TRUE,
        helpText("Pick a template to pre-populate a guided flow. This will add example categories and codes (merged, not overwritten)."),
        fluidRow(
          column(4,
                 div(class = "info-panel",
                     h4("Thematic analysis (Braun & Clarke, 2006)"),
                     tags$ul(
                       tags$li("Familiarization"),
                       tags$li("Initial codes"),
                       tags$li("Theme search"),
                       tags$li("Theme review"),
                       tags$li("Define / name"),
                       tags$li("Report")
                     ),
                     actionButton("tpl_apply_braun", "Apply template",
                                  icon = icon("check"), class = "btn-primary"))),
          column(4,
                 div(class = "info-panel",
                     h4("Grounded Theory (Strauss & Corbin)"),
                     tags$ul(
                       tags$li("Open coding"),
                       tags$li("Axial coding"),
                       tags$li("Selective coding"),
                       tags$li("Theoretical saturation")
                     ),
                     actionButton("tpl_apply_gt", "Apply template",
                                  icon = icon("check"), class = "btn-primary"))),
          column(4,
                 div(class = "info-panel",
                     h4("Content Analysis (Krippendorff)"),
                     tags$ul(
                       tags$li("Unitizing"),
                       tags$li("Sampling"),
                       tags$li("Coding"),
                       tags$li("Reducing"),
                       tags$li("Inferring / Narrating")
                     ),
                     actionButton("tpl_apply_content", "Apply template",
                                  icon = icon("check"), class = "btn-primary")))
        ),
        hr(),
        h4("Guided checklist (current template)"),
        verbatimTextOutput("tpl_current"),
        DT::DTOutput("tpl_checklist"),
        actionButton("tpl_toggle_step", "Toggle completed",
                     icon = icon("check-square"))
      )
    )
  )
}

tpl_braun <- list(
  name = "Braun & Clarke",
  categories = c("Phase 1: Familiarization", "Phase 2: Initial codes",
                 "Phase 3: Themes", "Phase 4: Review", "Phase 5: Define",
                 "Phase 6: Report"),
  codes = c("first-impression", "data-familiarization", "initial-code",
            "candidate-theme", "theme-review", "defined-theme",
            "subtheme", "narrative-example"),
  steps = c("Read all documents end to end",
            "Note initial impressions as memos",
            "Generate codes systematically",
            "Group codes into candidate themes",
            "Review themes against data",
            "Define and name themes",
            "Produce the report / manuscript")
)

tpl_gt <- list(
  name = "Grounded Theory",
  categories = c("Open codes", "Axial codes", "Selective codes",
                 "Theoretical codes"),
  codes = c("in-vivo", "descriptive", "process", "condition",
            "action", "consequence", "core-category"),
  steps = c("Open coding: line-by-line",
            "Constant comparison between incidents",
            "Axial coding: relate categories",
            "Selective coding: core category",
            "Theoretical sampling",
            "Check theoretical saturation",
            "Write integrative memos")
)

tpl_content <- list(
  name = "Content Analysis",
  categories = c("Manifest content", "Latent content",
                 "Categories", "Sub-categories"),
  codes = c("unit-of-analysis", "manifest", "latent",
            "category-main", "category-sub", "inference"),
  steps = c("Define unit of analysis",
            "Define sampling frame",
            "Build coding scheme",
            "Pilot with subset (inter-coder agreement)",
            "Code full corpus",
            "Reduce / abstract",
            "Report frequencies and inferences")
)

tpl_apply_template <- function(rv, tpl) {
  # categories
  for (c in tpl$categories) {
    if (!c %in% rv$categoriasDF$Categoria) {
      rv$categoriasDF <- dplyr::bind_rows(
        rv$categoriasDF,
        tibble::tibble(Categoria = c, Codigos = ""))
    }
  }
  # codes (random color)
  pal <- c("#3498db", "#e67e22", "#27ae60", "#9b59b6", "#c0392b",
           "#16a085", "#f39c12", "#2c3e50")
  for (i in seq_along(tpl$codes)) {
    co <- tpl$codes[i]
    if (!co %in% rv$codigosDF$Codigo) {
      rv$codigosDF <- dplyr::bind_rows(
        rv$codigosDF,
        tibble::tibble(Codigo = co,
                       Color = pal[(i - 1) %% length(pal) + 1],
                       Parent = NA_character_))
    }
  }
}

setup_templates_server <- function(input, output, session, rv) {
  isolate({
    if (is.null(rv$active_template)) rv$active_template <- NULL
    if (is.null(rv$template_progress)) rv$template_progress <- character()
  })

  apply_and_set <- function(tpl) {
    tpl_apply_template(rv, tpl)
    rv$active_template <- tpl
    rv$template_progress <- character()
    showNotification(paste("Applied template:", tpl$name), type = "message")
  }

  observeEvent(input$tpl_apply_braun, apply_and_set(tpl_braun))
  observeEvent(input$tpl_apply_gt, apply_and_set(tpl_gt))
  observeEvent(input$tpl_apply_content, apply_and_set(tpl_content))

  output$tpl_current <- renderPrint({
    if (is.null(rv$active_template)) cat("No active template")
    else cat("Active template:", rv$active_template$name)
  })

  output$tpl_checklist <- DT::renderDT({
    if (is.null(rv$active_template)) {
      return(DT::datatable(data.frame(Step = "No template active"),
                           rownames = FALSE, options = list(dom = "t")))
    }
    tpl <- rv$active_template
    df <- tibble::tibble(
      Step = tpl$steps,
      Done = ifelse(tpl$steps %in% rv$template_progress, "\u2713", "")
    )
    DT::datatable(df, rownames = FALSE, selection = "single",
                  options = list(pageLength = 10, dom = "t"))
  })

  observeEvent(input$tpl_toggle_step, {
    req(rv$active_template)
    sel <- input$tpl_checklist_rows_selected
    if (length(sel) == 0) return()
    step <- rv$active_template$steps[sel]
    if (step %in% rv$template_progress) {
      rv$template_progress <- setdiff(rv$template_progress, step)
    } else {
      rv$template_progress <- c(rv$template_progress, step)
    }
  })
}
