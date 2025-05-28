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

# para análisis y visualización
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(patchwork)
library(plotly)

# ========================================
# Función para leer archivos (.txt, .docx)
# ========================================
leer_archivo <- function(archivo) {
  ext <- tools::file_ext(archivo$datapath)
  if (ext == "txt") {
    lineas <- readLines(archivo$datapath, encoding = "UTF-8")
    paste(lineas[lineas != ""], collapse = "\n")
  } else if (ext %in% c("docx","doc")) {
    doc <- read_docx(archivo$datapath)
    df  <- docx_summary(doc)
    df  <- df[!is.na(df$text) & df$text != "", ]
    df  <- df[!duplicated(df$text), ]
    paste(df$text, collapse = "\n")
  } else {
    "Formato no soportado"
  }
}

# ========================================
# Funciones de análisis
# ========================================
plot_codigos <- function(df, fill = TRUE, code_colors = NULL) {
  reorder_size <- function(x) factor(x, levels = names(sort(table(x))))
  mapping    <- if (fill && "Categoria" %in% names(df)) {
    aes(x = reorder_size(Codigo), fill = Categoria)
  } else {
    aes(x = reorder_size(Codigo), fill = Codigo)
  }
  legend_pos <- if (fill && "Categoria" %in% names(df)) "bottom" else "none"
  
  p <- df %>%
    ggplot(mapping) +
    geom_bar() +
    geom_text(aes(label = ..count..), stat="count", nudge_y=-0.6, size=3) +
    facet_wrap(~ Archivo, scales="free_y") +
    coord_flip() +
    labs(x="Códigos", y="Frecuencia") +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      legend.position = legend_pos,
      strip.text      = element_text(size=12, face="bold"),
      plot.title      = element_text(size=14, face="bold")
    )
  
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
  
  # 1) red
  net_plot <- ggraph(graph_tbl, layout="fr") +
    geom_edge_link(aes(width=weight), color="gray80", alpha=0.6) +
    scale_edge_width(range = c(0.5, 3), guide = "none") +
    geom_node_point(aes(fill=full_name), shape=21, size=12, color="white") +
    {
      if (!is.null(code_colors)) {
        scale_fill_manual(name="Código", values=code_colors)
      } else {
        scale_fill_brewer("Código", palette="Set3")
      }
    } +
    geom_node_text(aes(label=label_abbr), size=3) +
    theme_void() +
    theme(legend.position="right")
  
  # 2) centralidad
  cents <- graph_tbl %>%
    as_tibble() %>%
    select(full_name, strength) %>%
    pivot_longer(cols = -full_name, names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(zscore = round((value - mean(value)) / sd(value), 2)) %>%
    ungroup()
  
  cent_plot <- cents %>%
    filter(metric == "strength") %>%
    ggplot(aes(full_name, zscore, group = metric)) +
    geom_line() + geom_point(size = 3) +
    coord_flip() +
    labs(y = "Centralidad (z-score)", x = "Código") +
    theme_bw()
  
  # 3) combinar
  combined <- net_plot + cent_plot +
    plot_layout(ncol=2, widths=c(3,1), guides="collect") &
    theme(legend.position="bottom")
  
  list(plot = combined, table = cents)
}

# ========================================
# UI
# ========================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "RCualiText", titleWidth = 260),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Texto",      tabName = "texto",      icon = icon("file-alt")),
      menuItem("Códigos",    tabName = "codigos",    icon = icon("tags")),
      menuItem("Categorías", tabName = "categorias", icon = icon("folder-open")),
      menuItem("Resaltes",   tabName = "resaltes",   icon = icon("highlighter")),
      menuItem("Análisis",   tabName = "analisis",   icon = icon("project-diagram")),
      menuItem("Estado",     tabName = "estado",     icon = icon("save")),
      menuItem("Info",       tabName = "info",       icon = icon("info-circle"))
    ), 
    width = 260
  ),
  dashboardBody(
    theme = bs_theme(bootswatch = "flatly", base_font = font_google("Segoe UI")),
    useShinyjs(),
    tags$head(tags$style(HTML("
      .box { border-radius: 8px; }
      .content-wrapper { background: #ecf0f5; }
    "))),
    tabItems(
      # Texto
      tabItem("texto",
              fluidRow(
                box(width = 4, title = "Cargar Documento", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fileInput("archivo", "Sube .txt/.docx", multiple = TRUE, accept = c(".txt", ".docx")),
                    selectInput("codigoTexto", "Selecciona Código", choices = NULL),
                    fluidRow(
                      column(6, actionBttn("prev_doc", "Anterior", style = "jelly", color = "primary")),
                      column(6, actionBttn("next_doc", "Siguiente", style = "jelly", color = "primary"))
                    ),
                    textOutput("doc_info")
                ),
                box(width = 8, title = "Visor de Texto", status = "info", solidHeader = TRUE,
                    withSpinner(uiOutput("contenido"), type = 6)
                )
              )
      ),
      # Códigos
      tabItem("codigos",
              fluidRow(
                box(width = 4, title = "Nuevo / Editar Código", status = "warning", solidHeader = TRUE,
                    textInput("new_codigo", "Código", value = ""),
                    colourInput("new_color", "Color", value = "#FFFF00"),
                    actionBttn("addOrUpdateCodigo", "Guardar", style = "bordered", color = "success"),
                    actionBttn("deleteCodigo", "Borrar", style = "bordered", color = "danger")
                ),
                box(width = 8, title = "Lista de Códigos", status = "warning", solidHeader = TRUE,
                    DTOutput("tablaCodigos")
                )
              )
      ),
      # Categorías
      tabItem("categorias",
              fluidRow(
                box(width = 4, title = "Nueva / Editar Categoría", status = "info", solidHeader = TRUE,
                    textInput("new_categoria", "Categoría", value = ""),
                    selectizeInput("codigos_for_categoria", "Códigos asociados", choices = NULL, multiple = TRUE),
                    actionBttn("addOrUpdateCategoria", "Guardar", style = "bordered", color = "success"),
                    actionBttn("deleteCategoria", "Borrar", style = "bordered", color = "danger")
                ),
                box(width = 8, title = "Lista de Categorías", status = "info", solidHeader = TRUE,
                    DTOutput("tablaCategorias")
                )
              )
      ),
      # Resaltes
      tabItem("resaltes",
              fluidRow(
                box(width = 12, title = "Tabla de Resaltes", status = "danger", solidHeader = TRUE,
                    downloadBttn("descarga", "Descargar XLSX", style = "gradient", color = "primary"),
                    DTOutput("tablaResaltes") %>% withSpinner(type = 4)
                )
              )
      ),
      # Análisis
      tabItem("analisis",
              fluidRow(
                box(width = 3, title = "Opciones de Gráficos", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    prettySwitch("fillToggle", "Fill por Categoría", value = TRUE, status = "info")
                ),
                box(width = 9, height = 600, title = "Frecuencias de Códigos", status = "primary", solidHeader = TRUE,
                    plotlyOutput("plotCodigos") %>% withSpinner()
                )
              ),
              fluidRow(
                box(width = 12, height = 600, title = "Red de Códigos y Centralidad", status = "primary", solidHeader = TRUE,
                    plotOutput("plotRedCentralidad") %>% withSpinner()
                )
              )
      ),
      # Estado
      tabItem("estado",
              fluidRow(
                box(width = 6, title = "Guardar Estado", status = "info", solidHeader = TRUE,
                    downloadBttn("saveState", "Guardar .rds", style = "stretch", color = "warning")
                ),
                box(width = 6, title = "Cargar Estado", status = "info", solidHeader = TRUE,
                    fileInput("loadState", "Selecciona .rds", accept = ".rds")
                )
              )
      ),
      # Info
      tabItem("info",
              fluidRow(
                box(width = 12, title = "Acerca del programa", status = "primary", solidHeader = TRUE,
                    p("RCualiText es una aplicación para la codificación cualitativa de textos que permite cargar documentos (.txt y .docx), definir códigos y categorías, resaltar extractos de interés y visualizar frecuencias y redes de coocurrencia de códigos."),
                    p("Con RCualiText puedes gestionar de manera interactiva tu lista de códigos, agruparlos en categorías, exportar tus resaltados a Excel y analizar gráficamente tus datos cualitativos mediante gráficos de barras y redes de centralidad."),
                    p(strong("Autor: Dr. José Ventura-León"))
                    
                )
              )
      )
    )
  )
)


# ========================================
# Servidor
# ========================================
server <- function(input, output, session) {
  rv <- reactiveValues(
    codigosDF    = tibble(Codigo=character(), Color=character()),
    categoriasDF = tibble(Categoria=character(), Codigos=character()),
    docs         = NULL, idx=0, texto="",
    tabla        = tibble(Extracto=character(), Codigo=character(), Categoria=character(), Color=character(), Archivo=character())
  )
  
  # helper colors
  get_code_colors <- reactive(set_names(rv$codigosDF$Color, rv$codigosDF$Codigo))
  
  # -- Códigos CRUD --
  proxyCod <- dataTableProxy("tablaCodigos")
  observeEvent(input$tablaCodigos_rows_selected, {
    sel <- input$tablaCodigos_rows_selected; req(length(sel)==1)
    updateTextInput(session, "new_codigo", value=rv$codigosDF$Codigo[sel])
    updateColourInput(session, "new_color", value=rv$codigosDF$Color[sel])
  })
  observeEvent(input$addOrUpdateCodigo, {
    req(input$new_codigo)
    df <- rv$codigosDF
    if (input$new_codigo %in% df$Codigo) {
      df <- df %>% mutate(Color = if_else(Codigo==input$new_codigo, input$new_color, Color))
    } else {
      df <- bind_rows(df, tibble(Codigo=input$new_codigo, Color=input$new_color))
    }
    rv$codigosDF <- df
    updateSelectInput(session, "codigoTexto", choices=df$Codigo)
    updateSelectizeInput(session, "codigos_for_categoria", choices=df$Codigo, server=TRUE)
    replaceData(proxyCod, df, resetPaging=FALSE)
  })
  observeEvent(input$deleteCodigo, {
    sel <- input$tablaCodigos_rows_selected; req(length(sel)==1)
    df <- rv$codigosDF[-sel,]; rv$codigosDF <- df
    updateSelectInput(session, "codigoTexto", choices=df$Codigo)
    updateSelectizeInput(session, "codigos_for_categoria", choices=df$Codigo, server=TRUE)
    replaceData(proxyCod, df, resetPaging=FALSE)
  })
  output$tablaCodigos <- renderDT(rv$codigosDF, selection="single", options=list(pageLength=5))
  
  # -- Categorías CRUD --
  proxyCat <- dataTableProxy("tablaCategorias")
  observeEvent(input$tablaCategorias_rows_selected, {
    sel <- input$tablaCategorias_rows_selected; req(length(sel)==1)
    cats <- str_split(rv$categoriasDF$Codigos[sel], ",\\s*")[[1]]
    updateTextInput(session, "new_categoria", value=rv$categoriasDF$Categoria[sel])
    updateSelectizeInput(session, "codigos_for_categoria", selected=cats)
  })
  observeEvent(input$addOrUpdateCategoria, {
    req(input$new_categoria)
    df <- rv$categoriasDF
    line <- tibble(Categoria=input$new_categoria,
                   Codigos=paste(input$codigos_for_categoria, collapse=", "))
    if (input$new_categoria %in% df$Categoria) {
      df <- df %>% filter(Categoria!=input$new_categoria) %>% bind_rows(line)
    } else {
      df <- bind_rows(df, line)
    }
    rv$categoriasDF <- df
    replaceData(proxyCat, df, resetPaging=FALSE)
  })
  observeEvent(input$deleteCategoria, {
    sel <- input$tablaCategorias_rows_selected; req(length(sel)==1)
    df <- rv$categoriasDF[-sel,]; rv$categoriasDF <- df
    replaceData(proxyCat, df, resetPaging=FALSE)
  })
  output$tablaCategorias <- renderDT(rv$categoriasDF, selection="single", options=list(pageLength=5))
  
  # -- Documentos & Resaltado --
  observeEvent(input$archivo, {
    files <- input$archivo
    docs <- map(seq_len(nrow(files)), ~{
      txt <- leer_archivo(files[.,]); list(name=files$name[.], original=txt, modified=txt)
    })
    rv$docs <- docs; rv$idx <- 1; rv$texto <- docs[[1]]$modified
    output$contenido <- renderUI(HTML(rv$texto))
  })
  observeEvent(input$prev_doc, {
    req(rv$idx>1)
    rv$idx   <- rv$idx - 1
    rv$texto <- rv$docs[[rv$idx]]$modified
    output$contenido <- renderUI(HTML(rv$texto))
  })
  observeEvent(input$next_doc, {
    req(rv$idx<length(rv$docs))
    rv$idx   <- rv$idx + 1
    rv$texto <- rv$docs[[rv$idx]]$modified
    output$contenido <- renderUI(HTML(rv$texto))
  })
  output$doc_info <- renderText({
    if (!is.null(rv$docs) && rv$idx > 0) {
      paste0("Documento ", rv$idx, " de ", length(rv$docs), ": ", rv$docs[[rv$idx]]$name)
    } else ""
  })
  observeEvent(input$selectedText, {
    txt <- str_trim(input$selectedText)
    req(txt != "", input$codigoTexto != "")
    code    <- input$codigoTexto
    col     <- get_code_colors()[code]
    cat_sel <- rv$categoriasDF %>% 
      filter(str_detect(Codigos, fixed(code))) %>% 
      pull(Categoria) %>% first() %||% ""
    newrow <- tibble(Extracto=txt, Codigo=code, Categoria=cat_sel, Color=col, Archivo=rv$docs[[rv$idx]]$name)
    rv$tabla <- bind_rows(rv$tabla, newrow)
    span     <- paste0("<span style='background:", col, ";'>", txt, "</span>")
    rv$texto <- sub(txt, span, rv$texto, fixed=TRUE)
    rv$docs[[rv$idx]]$modified <- rv$texto
    output$contenido <- renderUI(HTML(rv$texto))
    runjs("window.getSelection().removeAllRanges();")
  })
  
  # -- Resaltes --
  output$tablaResaltes <- renderDT(rv$tabla, options=list(pageLength=5))
  output$descarga <- downloadHandler(
    filename = function() paste0("resaltes-", Sys.Date(), ".xlsx"),
    content  = function(file) {
      wb <- createWorkbook(); addWorksheet(wb, "Resaltes")
      writeData(wb, "Resaltes", rv$tabla); saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
  
  # -- Análisis --
  output$plotCodigos <- renderPlotly({
    req(nrow(rv$tabla)>0)
    p <- plot_codigos(rv$tabla, fill=input$fillToggle, code_colors=get_code_colors())
    ggplotly(p) %>% layout(legend=list(orientation="h", x=0.4, y=-0.2))
  })
  output$plotRedCentralidad <- renderPlot({
    req(nrow(rv$tabla)>0)
    plot_network_and_centrality(rv$tabla, code_colors=get_code_colors())$plot
  }, res=96)
  
  # -- Estado --
  output$saveState <- downloadHandler(
    filename = function() paste0("estado_", Sys.Date(), ".rds"),
    content  = function(file) saveRDS(reactiveValuesToList(rv), file)
  )
  observeEvent(input$loadState, {
    est <- readRDS(input$loadState$datapath)
    for (nm in names(est)) rv[[nm]] <- est[[nm]]
    updateSelectInput(session, "codigoTexto", choices=rv$codigosDF$Codigo)
    updateSelectizeInput(session, "codigos_for_categoria", choices=rv$codigosDF$Codigo, server=TRUE)
    output$contenido <- renderUI(HTML(rv$texto))
    replaceData(proxyCod, rv$codigosDF,     resetPaging=FALSE)
    replaceData(proxyCat, rv$categoriasDF, resetPaging=FALSE)
  })
}

# ========================================
# Ejecutar App
# ========================================
shinyApp(ui, server)
