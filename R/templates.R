# ========================================
# R/templates.R
# Analysis templates (Braun & Clarke, Grounded Theory, Content Analysis)
# - Vinculacion categoria <-> codigos
# - i18n (EN/ES) segun current_lang()
# - Persistencia: rv$active_template y rv$template_progress se guardan en .rds
# ========================================

# ---- Definiciones de plantillas ----
# Estructura: list(
#   name      = caracter,
#   category_codes = list("Categoria" = c("codigo1","codigo2"), ...),
#   steps     = caracter[]
# )

# ---- ENGLISH templates ----
tpl_braun_en <- list(
  name = "Thematic analysis (Braun & Clarke, 2006)",
  category_codes = list(
    "Phase 1: Familiarization" = c("first-impression", "data-familiarization"),
    "Phase 2: Initial codes"   = c("initial-code"),
    "Phase 3: Themes"          = c("candidate-theme", "subtheme"),
    "Phase 4: Review"          = c("theme-review"),
    "Phase 5: Define"          = c("defined-theme"),
    "Phase 6: Report"          = c("narrative-example")
  ),
  steps = c(
    "Read all documents end to end",
    "Note initial impressions as memos",
    "Generate codes systematically",
    "Group codes into candidate themes",
    "Review themes against data",
    "Define and name themes",
    "Produce the report / manuscript"
  )
)

tpl_gt_en <- list(
  name = "Grounded Theory (Strauss & Corbin)",
  category_codes = list(
    "Open codes"        = c("in-vivo", "descriptive"),
    "Axial codes"       = c("process", "condition", "action", "consequence"),
    "Selective codes"   = c("core-category"),
    "Theoretical codes" = c("theoretical-saturation")
  ),
  steps = c(
    "Open coding: line-by-line",
    "Constant comparison between incidents",
    "Axial coding: relate categories",
    "Selective coding: core category",
    "Theoretical sampling",
    "Check theoretical saturation",
    "Write integrative memos"
  )
)

tpl_content_en <- list(
  name = "Content Analysis (Krippendorff)",
  category_codes = list(
    "Manifest content" = c("unit-of-analysis", "manifest"),
    "Latent content"   = c("latent", "inference"),
    "Categories"       = c("category-main"),
    "Sub-categories"   = c("category-sub")
  ),
  steps = c(
    "Define unit of analysis",
    "Define sampling frame",
    "Build coding scheme",
    "Pilot with subset (inter-coder agreement)",
    "Code full corpus",
    "Reduce / abstract",
    "Report frequencies and inferences"
  )
)

# ---- SPANISH templates ----
tpl_braun_es <- list(
  name = "An\u00e1lisis tem\u00e1tico (Braun y Clarke, 2006)",
  category_codes = list(
    "Fase 1: Familiarizaci\u00f3n"      = c("primera-impresion", "familiarizacion-datos"),
    "Fase 2: C\u00f3digos iniciales"    = c("codigo-inicial"),
    "Fase 3: B\u00fasqueda de temas"    = c("tema-candidato", "subtema"),
    "Fase 4: Revisi\u00f3n de temas"    = c("revision-tema"),
    "Fase 5: Definici\u00f3n y nombres" = c("tema-definido"),
    "Fase 6: Reporte"                   = c("ejemplo-narrativo")
  ),
  steps = c(
    "Leer todos los documentos de principio a fin",
    "Anotar impresiones iniciales como memos",
    "Generar c\u00f3digos sistem\u00e1ticamente",
    "Agrupar c\u00f3digos en temas candidatos",
    "Revisar los temas contra los datos",
    "Definir y nombrar los temas",
    "Producir el reporte / manuscrito"
  )
)

tpl_gt_es <- list(
  name = "Teor\u00eda Fundamentada (Strauss y Corbin)",
  category_codes = list(
    "C\u00f3digos abiertos"     = c("in-vivo", "descriptivo"),
    "C\u00f3digos axiales"      = c("proceso", "condicion", "accion", "consecuencia"),
    "C\u00f3digos selectivos"   = c("categoria-central"),
    "C\u00f3digos te\u00f3ricos" = c("saturacion-teorica")
  ),
  steps = c(
    "Codificaci\u00f3n abierta: l\u00ednea por l\u00ednea",
    "Comparaci\u00f3n constante entre incidentes",
    "Codificaci\u00f3n axial: relacionar categor\u00edas",
    "Codificaci\u00f3n selectiva: categor\u00eda central",
    "Muestreo te\u00f3rico",
    "Verificar saturaci\u00f3n te\u00f3rica",
    "Escribir memos integrativos"
  )
)

tpl_content_es <- list(
  name = "An\u00e1lisis de Contenido (Krippendorff)",
  category_codes = list(
    "Contenido manifiesto" = c("unidad-de-analisis", "manifiesto"),
    "Contenido latente"    = c("latente", "inferencia"),
    "Categor\u00edas"      = c("categoria-principal"),
    "Subcategor\u00edas"   = c("categoria-secundaria")
  ),
  steps = c(
    "Definir unidad de an\u00e1lisis",
    "Definir el marco de muestreo",
    "Construir el esquema de codificaci\u00f3n",
    "Pilotear con subconjunto (acuerdo inter-codificador)",
    "Codificar el corpus completo",
    "Reducir / abstraer",
    "Reportar frecuencias e inferencias"
  )
)

# Helper: obtener templates segun idioma
tpl_get_templates <- function(lang = "es") {
  if (identical(lang, "es")) {
    list(braun = tpl_braun_es, gt = tpl_gt_es, content = tpl_content_es)
  } else {
    list(braun = tpl_braun_en, gt = tpl_gt_en, content = tpl_content_en)
  }
}

# ---- UI ----
templates_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "templates",
    fluidRow(
      box(
        width = 12, title = textOutput("tpl_box_title", inline = TRUE),
        status = "primary", solidHeader = TRUE,
        uiOutput("tpl_help_text"),
        # Las 3 cajas se renderizan dinamicamente segun current_lang()
        uiOutput("tpl_options_ui"),
        hr(),
        h4(textOutput("tpl_checklist_title", inline = TRUE)),
        verbatimTextOutput("tpl_current"),
        DT::DTOutput("tpl_checklist"),
        actionButton("tpl_toggle_step",
                     textOutput("tpl_toggle_label", inline = TRUE),
                     icon = icon("check-square")),
        actionButton("tpl_clear_active",
                     textOutput("tpl_clear_label", inline = TRUE),
                     icon = icon("times-circle"),
                     class = "btn-warning",
                     style = "margin-left: 10px;")
      )
    )
  )
}

# ---- Aplicar plantilla (con vinculacion categoria <-> codigos) ----
tpl_apply_template <- function(rv, tpl) {
  pal <- c("#3498db", "#e67e22", "#27ae60", "#9b59b6", "#c0392b",
           "#16a085", "#f39c12", "#2c3e50", "#1abc9c", "#e74c3c")
  i <- 0
  for (cat_name in names(tpl$category_codes)) {
    codes_for_cat <- tpl$category_codes[[cat_name]]
    # Crear codigos (asignando color rotativo de la paleta)
    for (co in codes_for_cat) {
      i <- i + 1
      if (!co %in% rv$codigosDF$Codigo) {
        rv$codigosDF <- dplyr::bind_rows(
          rv$codigosDF,
          tibble::tibble(Codigo = co,
                         Color = pal[(i - 1) %% length(pal) + 1],
                         Parent = NA_character_))
      }
    }
    # Crear categoria con codigos asignados (o hacer merge si ya existe)
    codes_str_new <- paste(codes_for_cat, collapse = ";")
    if (!cat_name %in% rv$categoriasDF$Categoria) {
      rv$categoriasDF <- dplyr::bind_rows(
        rv$categoriasDF,
        tibble::tibble(Categoria = cat_name, Codigos = codes_str_new))
    } else {
      idx <- which(rv$categoriasDF$Categoria == cat_name)
      existing <- strsplit(rv$categoriasDF$Codigos[idx], ";")[[1]]
      existing <- existing[nchar(existing) > 0]
      merged <- unique(c(existing, codes_for_cat))
      rv$categoriasDF$Codigos[idx] <- paste(merged, collapse = ";")
    }
  }
}

# ---- Helpers de localizacion para el UI ----
.tpl_t <- function(lang, key) {
  d <- list(
    es = list(
      box_title       = "Plantillas de an\u00e1lisis",
      help_text       = "Selecciona una plantilla para pre-poblar un flujo guiado. Esto agregar\u00e1 categor\u00edas y c\u00f3digos de ejemplo (suma, no sobreescribe). Cada c\u00f3digo queda vinculado a su categor\u00eda.",
      btn_apply       = "Aplicar plantilla",
      checklist_title = "Lista de pasos (plantilla activa)",
      toggle_label    = "Marcar / desmarcar paso",
      clear_label     = "Quitar plantilla activa",
      no_active       = "(sin plantilla activa)",
      active_template = "Plantilla activa:",
      no_template     = "No hay plantilla activa",
      applied_msg     = "Plantilla aplicada:",
      cleared_msg     = "Plantilla quitada (los c\u00f3digos y categor\u00edas se conservan)"
    ),
    en = list(
      box_title       = "Analysis Templates",
      help_text       = "Pick a template to pre-populate a guided flow. This adds example categories and codes (merged, not overwritten). Each code is linked to its category.",
      btn_apply       = "Apply template",
      checklist_title = "Step checklist (active template)",
      toggle_label    = "Toggle completed",
      clear_label     = "Clear active template",
      no_active       = "(no active template)",
      active_template = "Active template:",
      no_template     = "No active template",
      applied_msg     = "Template applied:",
      cleared_msg     = "Template cleared (codes and categories kept)"
    )
  )
  d[[lang]][[key]] %||% key
}

# ---- Server ----
setup_templates_server <- function(input, output, session, rv) {
  isolate({
    if (is.null(rv$active_template)) rv$active_template <- NULL
    if (is.null(rv$template_progress)) rv$template_progress <- character()
  })

  # current_lang() viene del scope del server de app.R
  cl <- function() {
    if (exists("current_lang", inherits = TRUE)) tryCatch(current_lang(), error = function(e) "es")
    else "es"
  }

  # Etiquetas reactivas
  output$tpl_box_title       <- renderText(.tpl_t(cl(), "box_title"))
  output$tpl_checklist_title <- renderText(.tpl_t(cl(), "checklist_title"))
  output$tpl_toggle_label    <- renderText(.tpl_t(cl(), "toggle_label"))
  output$tpl_clear_label     <- renderText(.tpl_t(cl(), "clear_label"))
  output$tpl_help_text       <- renderUI(helpText(.tpl_t(cl(), "help_text")))

  # Render dinamico de las 3 cajas (cambia con el toggle EN/ES)
  output$tpl_options_ui <- renderUI({
    lang <- cl()
    tpls <- tpl_get_templates(lang)
    btn_label <- .tpl_t(lang, "btn_apply")
    fluidRow(
      column(4, div(class = "info-panel",
        h4(tpls$braun$name),
        tags$ul(lapply(names(tpls$braun$category_codes), function(c) tags$li(c))),
        actionButton("tpl_apply_braun", btn_label,
                     icon = icon("check"), class = "btn-primary"))),
      column(4, div(class = "info-panel",
        h4(tpls$gt$name),
        tags$ul(lapply(names(tpls$gt$category_codes), function(c) tags$li(c))),
        actionButton("tpl_apply_gt", btn_label,
                     icon = icon("check"), class = "btn-primary"))),
      column(4, div(class = "info-panel",
        h4(tpls$content$name),
        tags$ul(lapply(names(tpls$content$category_codes), function(c) tags$li(c))),
        actionButton("tpl_apply_content", btn_label,
                     icon = icon("check"), class = "btn-primary")))
    )
  })

  apply_and_set <- function(tpl) {
    tpl_apply_template(rv, tpl)
    rv$active_template <- tpl
    rv$template_progress <- character()
    showNotification(paste(.tpl_t(cl(), "applied_msg"), tpl$name),
                     type = "message", duration = 4)
  }

  observeEvent(input$tpl_apply_braun, {
    apply_and_set(tpl_get_templates(cl())$braun)
  })
  observeEvent(input$tpl_apply_gt, {
    apply_and_set(tpl_get_templates(cl())$gt)
  })
  observeEvent(input$tpl_apply_content, {
    apply_and_set(tpl_get_templates(cl())$content)
  })

  observeEvent(input$tpl_clear_active, {
    rv$active_template <- NULL
    rv$template_progress <- character()
    showNotification(.tpl_t(cl(), "cleared_msg"),
                     type = "warning", duration = 4)
  })

  output$tpl_current <- renderPrint({
    lang <- cl()
    if (is.null(rv$active_template)) cat(.tpl_t(lang, "no_template"))
    else cat(.tpl_t(lang, "active_template"), rv$active_template$name)
  })

  output$tpl_checklist <- DT::renderDT({
    lang <- cl()
    if (is.null(rv$active_template)) {
      return(DT::datatable(
        data.frame(Step = .tpl_t(lang, "no_active")),
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
