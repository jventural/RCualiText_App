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
options(shiny.maxRequestSize = 50 * 1024^2)

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
    "Formato no soportado"
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
aplicar_resaltado_multiple <- function(texto_original, fragmentos_df) {
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
      "title='Códigos: ", codigos_aplicados, "' ",
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
plot_codigos <- function(df, fill = TRUE, code_colors = NULL) {
  # Limpiar datos de categorías vacías o NA
  df <- df %>%
    mutate(Categoria = case_when(
      is.na(Categoria) | Categoria == "" ~ "Sin categoría",
      TRUE ~ Categoria
    ))
  
  reorder_size <- function(x) factor(x, levels = names(sort(table(x))))
  
  # Configurar aesthetic y nombre de leyenda
  if (fill && "Categoria" %in% names(df)) {
    mapping <- aes(x = reorder_size(Codigo), fill = Categoria)
    legend_title <- "Categoría"
  } else {
    mapping <- aes(x = reorder_size(Codigo), fill = Codigo)
    legend_title <- "Código"
  }
  
  p <- df %>%
    ggplot(mapping) +
    geom_bar() +
    geom_text(aes(label = ..count..), stat="count", nudge_y=-0.6, size=3) +
    facet_wrap(~ Archivo, scales="free_y") +
    coord_flip() +
    labs(x="Códigos", y="Frecuencia", fill = legend_title) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size=12, face="bold"),
      plot.title = element_text(size=14, face="bold")
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
  
  # Establecer semilla aleatoria fija para layout consistente
  set.seed(2025)
  
  # Configurar plot de red
  net_plot <- ggraph(graph_tbl, layout="fr") +
    geom_edge_link(aes(width=weight), color="gray80", alpha=0.6) +
    scale_edge_width(range = c(0.3, 2), guide = "none") +
    geom_node_point(aes(fill=full_name), shape=21, size=10, color="white") +
    { if (!is.null(code_colors))
      scale_fill_manual(name="Código", values=code_colors)
      else
        scale_fill_brewer(name="Código", palette="Set3") } +
    geom_node_text(aes(label=label_abbr), size=2.5) +
    guides(fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0.5
    )) +
    theme_void()
  
  cents <- graph_tbl %>%
    as_tibble() %>%
    select(full_name, strength) %>%
    pivot_longer(cols = -full_name, names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(zscore = round((value - mean(value)) / sd(value), 2)) %>%
    ungroup()
  
  # Plot de centralidad
  cent_plot <- cents %>%
    filter(metric == "strength") %>%
    ggplot(aes(full_name, zscore, group = metric)) +
    geom_line() +
    geom_point(size = 2) +
    coord_flip() +
    labs(y = "Centralidad (z-score)", x = "Código") +
    theme_bw(base_size = 10)
  
  # Combinar plots con leyenda compartida en la parte inferior centrada
  combined <- net_plot + cent_plot +
    plot_layout(ncol=2, widths=c(3,1), guides="collect")
  
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
# Función para llamar a la API de OpenAI (Análisis con IA)
# ========================================
call_openai_api <- function(prompt, api_key, model = "gpt-4.1") {
  # Validaciones defensivas
  if (is.null(prompt) || length(prompt) == 0 || is.na(prompt[1])) {
    stop("Prompt no válido")
  }
  
  # Forzar prompt a ser un string simple
  prompt <- as.character(prompt)[1]
  
  if (nchar(prompt) == 0) {
    stop("Prompt vacío")
  }
  
  resp <- NULL
  intento <- 1
  last_error <- NULL
  
  while (intento <= 3) {
    resp <- tryCatch({
      httr::POST(
        "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type`  = "application/json"
        ),
        httr::timeout(160),
        body = jsonlite::toJSON(list(
          model       = model,
          messages    = list(
            list(role = "system",
                 content = "Eres un experto en análisis cualitativo de datos textuales y codificación temática."),
            list(role = "user", content = as.character(prompt)[1])
          ),
          temperature = 0.2
        ), auto_unbox = TRUE)
      )
    }, error = function(e) {
      last_error <<- e$message
      NULL
    })
    
    if (!is.null(resp)) {
      if (httr::status_code(resp) == 200) {
        return(httr::content(resp)$choices[[1]]$message$content)
      } else {
        # Capturar mensaje de error de la API
        error_content <- tryCatch(httr::content(resp), error = function(e) NULL)
        if (!is.null(error_content$error$message)) {
          last_error <- error_content$error$message
        } else {
          last_error <- paste("HTTP Status:", httr::status_code(resp))
        }
      }
    }
    
    intento <- intento + 1
    Sys.sleep(2)
  }
  
  # Si llegamos aquí, falló después de 3 intentos
  error_msg <- if (!is.null(last_error)) {
    paste0("Error OpenAI: ", last_error)
  } else {
    "No se pudo conectar con OpenAI"
  }
  stop(error_msg)
}

# ========================================
# UI (actualizado con controles de descarga personalizados)
# ========================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      style = "font-weight: bold; font-size: 18px; color: #fff; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);",
      icon("microscope", style = "margin-right: 8px;"),
      "RCualiText"
    ), 
    titleWidth = 280
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("📄 Documento", tabName = "texto", icon = icon("file-text"), 
               badgeLabel = "Nuevo", badgeColor = "green"),
      menuItem("🏷️ Códigos", tabName = "codigos", icon = icon("tags")),
      menuItem("📁 Categorías", tabName = "categorias", icon = icon("folder-open")),
      menuItem("🎨 Resaltados", tabName = "resaltes", icon = icon("highlighter")),
      menuItem("📊 Análisis", tabName = "analisis", icon = icon("project-diagram")),
      menuItem("🤖 Análisis IA (opcional)", tabName = "analisis_ia", icon = icon("robot"),
               badgeLabel = "IA", badgeColor = "purple"),
      menuItem("💾 Estado", tabName = "estado", icon = icon("save")),
      menuItem("📚 Citar", tabName = "citar", icon = icon("quote-right")),
      menuItem("ℹ️ Ayuda", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    theme = bs_theme(
      bootswatch = "flatly", 
      base_font = font_google("Inter"),
      primary = "#3498db",
      secondary = "#95a5a6",
      success = "#2ecc71",
      warning = "#f39c12",
      danger = "#e74c3c",
      info = "#17a2b8"
    ),
    useShinyjs(),
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        /* Estilos generales mejorados */
        body {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }
        
        .content-wrapper {
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          min-height: 100vh;
        }
        
        .main-sidebar {
          background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
          box-shadow: 4px 0 15px rgba(0,0,0,0.1);
        }
        
        .main-header .navbar {
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        /* Cajas mejoradas */
        .box {
          border-radius: 15px;
          box-shadow: 0 8px 25px rgba(0,0,0,0.1);
          border: none;
          overflow: hidden;
          transition: all 0.3s ease;
          background: rgba(255,255,255,0.95);
          backdrop-filter: blur(10px);
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 15px 35px rgba(0,0,0,0.15);
        }
        
        .box-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 15px 15px 0 0;
          padding: 20px;
          border-bottom: none;
        }
        
        .box-header.with-border {
          border-bottom: none;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 16px;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
        }
        
        .box-body {
          padding: 25px;
        }
        
        /* Estilos para resaltado múltiple mejorados */
        .highlight-multiple {
          position: relative;
          padding: 6px 12px;
          border-radius: 8px;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          cursor: pointer;
          display: inline-block;
          margin: 2px;
          transform: translateZ(0);
        }
        
        .highlight-multiple:hover {
          transform: translateY(-2px) scale(1.05);
          box-shadow: 0 8px 25px rgba(0,0,0,0.3) !important;
          z-index: 10;
        }
        
        /* Indicador visual para modo deselección */
        #document-viewer.deselect-mode .highlight-multiple:hover {
          box-shadow: 0 0 0 3px #e74c3c, 0 8px 25px rgba(231,76,60,0.4) !important;
          transform: scale(1.08);
        }
        
        /* Tooltip mejorado */
        .highlight-multiple[title]:hover::after {
          content: attr(title);
          position: absolute;
          top: -45px;
          left: 50%;
          transform: translateX(-50%);
          background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
          color: white;
          padding: 10px 15px;
          border-radius: 8px;
          font-size: 12px;
          white-space: nowrap;
          z-index: 1000;
          box-shadow: 0 5px 20px rgba(0,0,0,0.3);
          animation: fadeIn 0.3s ease;
        }
        
        .highlight-multiple[title]:hover::before {
          content: '';
          position: absolute;
          top: -8px;
          left: 50%;
          transform: translateX(-50%);
          border-left: 8px solid transparent;
          border-right: 8px solid transparent;
          border-top: 8px solid #2c3e50;
          z-index: 1001;
        }
        
        @keyframes fadeIn {
          from { opacity: 0; transform: translateX(-50%) translateY(-5px); }
          to { opacity: 1; transform: translateX(-50%) translateY(0); }
        }
        
        /* Botones de modo mejorados */
        .mode-button {
          margin: 8px 4px;
          border-radius: 25px;
          padding: 12px 24px;
          font-weight: 600;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          border: 2px solid transparent;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          position: relative;
          overflow: hidden;
        }
        
        .mode-button:before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%;
          width: 100%;
          height: 100%;
          background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
          transition: all 0.5s;
        }
        
        .mode-button:hover:before {
          left: 100%;
        }
        
        .mode-button.active {
          box-shadow: 0 0 20px rgba(52,152,219,0.6);
          transform: scale(1.05);
        }
        
        .deselect-active {
          background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%) !important;
          border-color: #e74c3c !important;
          color: white !important;
          box-shadow: 0 0 20px rgba(231,76,60,0.6) !important;
        }
        
        /* Visor de texto mejorado */
        #document-viewer.content {
          background: rgba(255,255,255,0.98);
          padding: 15px 25px 25px 25px;
          border-radius: 12px;
          box-shadow: inset 0 2px 10px rgba(0,0,0,0.05);
          border: 1px solid rgba(255,255,255,0.2);
          backdrop-filter: blur(10px);
        }
        
        /* Eliminar márgenes del contenido del visor */
        #contenido {
          margin: 0 !important;
          padding: 0 !important;
        }
        
        /* Asegurar que el contenido del visor inicie sin espacios */
        #document-viewer .content > div {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }
        
        /* Eliminar márgenes del spinner y contenido interno */
        #document-viewer .sk-folding-cube {
          margin-top: 10px !important;
        }
        
        /* Eliminar márgenes iniciales del texto renderizado */
        #contenido > * {
          margin-top: 0 !important;
        }
        
        #contenido > *:first-child {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }
        
        /* Botones mejorados */
        .btn {
          border-radius: 25px;
          font-weight: 500;
          transition: all 0.3s ease;
          border: none;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(0,0,0,0.15);
        }
        
        /* Inputs mejorados */
        .form-control {
          border-radius: 10px;
          border: 2px solid #e9ecef;
          transition: all 0.3s ease;
          padding: 12px 15px;
        }
        
        .form-control:focus {
          border-color: #3498db;
          box-shadow: 0 0 0 0.2rem rgba(52,152,219,0.25);
        }
        
        /* Tablas mejoradas */
        .dataTables_wrapper {
          border-radius: 12px;
          overflow: hidden;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        table.dataTable thead th {
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
          color: white;
          border: none;
          font-weight: 600;
        }
        
        table.dataTable tbody tr:hover {
          background: rgba(52,152,219,0.1);
        }
        
        /* Info panels mejorados */
        .info-panel {
          background: linear-gradient(135deg, rgba(52,152,219,0.1) 0%, rgba(155,89,182,0.1) 100%);
          border: 2px solid rgba(52,152,219,0.2);
          border-radius: 15px;
          padding: 20px;
          margin: 15px 0;
          backdrop-filter: blur(5px);
        }
        
        .danger-panel {
          background: linear-gradient(135deg, rgba(231,76,60,0.1) 0%, rgba(192,57,43,0.1) 100%);
          border: 2px solid rgba(231,76,60,0.2);
        }
        
        /* Controles de descarga mejorados */
        .download-controls-container {
          background: linear-gradient(135deg, rgba(46,204,113,0.1) 0%, rgba(39,174,96,0.1) 100%);
          border: 2px solid rgba(46,204,113,0.2);
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
        }
        
        .download-controls-grid {
          display: grid;
          grid-template-columns: 1fr 1fr 1fr auto;
          gap: 15px;
          align-items: end;
        }
        
        /* Spinner mejorado */
        .sk-folding-cube {
          margin: 20px auto;
          width: 40px;
          height: 40px;
          position: relative;
          -webkit-transform: rotateZ(45deg);
          transform: rotateZ(45deg);
        }
        
        /* Sidebar mejorado */
        .sidebar-menu > li > a {
          color: rgba(255,255,255,0.9);
          transition: all 0.3s ease;
          border-radius: 8px;
          margin: 4px 8px;
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background: rgba(255,255,255,0.1);
          color: white;
          transform: translateX(5px);
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
                  title = "🎯 Centro de Control", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  fileInput("archivo", 
                            div(icon("upload"), " Cargar Documentos"), 
                            multiple = TRUE, 
                            accept = c(".txt", ".docx"),
                            buttonLabel = "Examinar...",
                            placeholder = "Ningún archivo seleccionado"),
                  
                  # Panel de modos con diseño moderno
                  div(
                    class = "info-panel",
                    h5(icon("cog"), " Modo de Trabajo", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    div(
                      style = "display: flex; flex-wrap: wrap; gap: 8px; justify-content: center;",
                      actionBttn("modeSelect", 
                                 label = div(icon("mouse-pointer"), " Seleccionar"), 
                                 style = "gradient", color = "royal", size = "sm", 
                                 class = "mode-button active"),
                      actionBttn("modeDeselect", 
                                 label = div(icon("eraser"), " Deseleccionar"), 
                                 style = "gradient", color = "danger", size = "sm", 
                                 class = "mode-button")
                    )
                  ),
                  
                  # Controles de codificación mejorados
                  conditionalPanel(
                    condition = "input.modeSelect",
                    div(
                      style = "margin: 20px 0;",
                      selectInput("codigoTexto", 
                                  div(icon("tag"), " Código a Aplicar"), 
                                  choices = NULL),
                      div(
                        class = "info-panel",
                        checkboxInput("modoAcumulativo", 
                                      div(icon("layer-group"), " Modo Acumulativo"), 
                                      value = TRUE),
                        helpText("Permite aplicar múltiples códigos al mismo fragmento", 
                                 style = "color: #7f8c8d; font-size: 12px;")
                      )
                    )
                  ),
                  
                  # Controles de navegación mejorados
                  div(
                    style = "margin: 20px 0;",
                    h5(icon("arrows-alt-h"), " Navegación", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    fluidRow(
                      column(6, actionBttn("prev_doc", 
                                           div(icon("chevron-left"), " Anterior"), 
                                           style = "jelly", color = "royal", size = "sm")),
                      column(6, actionBttn("next_doc", 
                                           div(icon("chevron-right"), " Siguiente"), 
                                           style = "jelly", color = "royal", size = "sm"))
                    )
                  ),
                  
                  # Botones de acción mejorados
                  div(
                    style = "margin: 20px 0;",
                    h5(icon("tools"), " Acciones", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    div(
                      style = "display: flex; flex-wrap: wrap; gap: 8px;",
                      actionBttn("limpiarResaltados", 
                                 div(icon("broom"), " Limpiar"), 
                                 style = "bordered", color = "warning", size = "sm"),
                      actionBttn("ayuda", 
                                 div(icon("question-circle"), " Ayuda"), 
                                 style = "bordered", color = "royal", size = "sm")
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
                  title = "📖 Visor de Documento", 
                  status = "info", 
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
                  title = "🏷️ Gestión de Códigos", 
                  status = "warning", 
                  solidHeader = TRUE,
                  div(
                    style = "space-y: 20px;",
                    textInput("new_codigo", 
                              div(icon("tag"), " Nombre del Código"), 
                              value = "",
                              placeholder = "Ej: Emociones positivas"),
                    
                    div(
                      style = "margin: 20px 0;",
                      h5(icon("palette"), " Color del Código", style = "color: #2c3e50; margin-bottom: 10px;"),
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
                      actionBttn("addOrUpdateCodigo", 
                                 div(icon("save"), " Guardar"), 
                                 style = "gradient", color = "success", size = "sm"),
                      actionBttn("deleteCodigo", 
                                 div(icon("trash"), " Eliminar"), 
                                 style = "gradient", color = "danger", size = "sm")
                    )
                  )
                ),
                box(
                  width = 8, 
                  title = "📋 Lista de Códigos Activos", 
                  status = "warning", 
                  solidHeader = TRUE,
                  DTOutput("tablaCodigos") %>% withSpinner(type = 4, color = "#f39c12")
                )
              )
      ),
      
      # ---- Categorías (diseño mejorado) ----
      tabItem("categorias",
              fluidRow(
                box(
                  width = 4, 
                  height = 600,
                  title = "📁 Gestión de Categorías", 
                  status = "info", 
                  solidHeader = TRUE,
                  div(
                    textInput("new_categoria", 
                              div(icon("folder"), " Nombre de Categoría"), 
                              value = "",
                              placeholder = "Ej: Aspectos emocionales"),
                    
                    div(
                      style = "margin: 20px 0;",
                      selectizeInput("codigos_for_categoria", 
                                     div(icon("tags"), " Códigos Asociados"), 
                                     choices = NULL, 
                                     multiple = TRUE,
                                     options = list(placeholder = "Selecciona códigos..."))
                    ),
                    
                    div(
                      style = "display: flex; gap: 10px; margin-top: 25px;",
                      actionBttn("addOrUpdateCategoria", 
                                 div(icon("save"), " Guardar"), 
                                 style = "gradient", color = "success", size = "sm"),
                      actionBttn("deleteCategoria", 
                                 div(icon("trash"), " Eliminar"), 
                                 style = "gradient", color = "danger", size = "sm")
                    )
                  )
                ),
                box(
                  width = 8, 
                  height = 600,
                  title = "🗂️ Categorías Definidas", 
                  status = "info", 
                  solidHeader = TRUE,
                  DTOutput("tablaCategorias") %>% withSpinner(type = 4, color = "#17a2b8")
                )
              )
      ),
      
      # ---- Resaltes (diseño mejorado) ----
      tabItem("resaltes",
              fluidRow(
                box(
                  width = 12, 
                  title = "🎨 Gestión de Resaltados", 
                  status = "danger", 
                  solidHeader = TRUE,
                  
                  # Panel de controles mejorado
                  div(
                    class = "info-panel",
                    h5(icon("cogs"), " Herramientas de Gestión", style = "color: #2c3e50; margin-bottom: 15px;"),
                    fluidRow(
                      column(4, 
                             downloadBttn("descarga", 
                                          div(icon("download"), " Exportar XLSX"), 
                                          style = "gradient", color = "royal", size = "sm")),
                      column(4, 
                             actionBttn("eliminarResalte", 
                                        div(icon("minus-circle"), " Eliminar Seleccionado"), 
                                        style = "gradient", color = "danger", size = "sm")),
                      column(4, 
                             actionBttn("eliminarTodosResaltes", 
                                        div(icon("trash-alt"), " Limpiar Todo"), 
                                        style = "gradient", color = "warning", size = "sm"))
                    )
                  ),
                  
                  # Tabla de resaltados
                  DTOutput("tablaResaltes") %>% withSpinner(type = 4, color = "#e74c3c"),
                  
                  # Panel informativo mejorado
                  div(
                    class = "info-panel",
                    style = "margin-top: 20px;",
                    h5(icon("info-circle"), " Guía de Resaltados", style = "color: #3498db; margin-bottom: 15px;"),
                    div(
                      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
                      div(
                        h6(icon("paint-brush"), " Visualización", style = "color: #2c3e50; margin-bottom: 8px;"),
                        tags$ul(
                          style = "font-size: 13px; color: #7f8c8d;",
                          tags$li("Gradientes indican múltiples códigos"),
                          tags$li("Hover muestra códigos aplicados"),
                          tags$li("Cada fila = un código por fragmento")
                        )
                      ),
                      div(
                        h6(icon("edit"), " Edición", style = "color: #2c3e50; margin-bottom: 8px;"),
                        tags$ul(
                          style = "font-size: 13px; color: #7f8c8d;",
                          tags$li("Modo deseleccionar para eliminar"),
                          tags$li("Selección múltiple disponible"),
                          tags$li("Exportación completa a Excel")
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
                  title = "⚙️ Configuración de Análisis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("chart-bar"), " Opciones Visuales", style = "color: #2c3e50; margin-bottom: 15px;"),
                    prettySwitch("fillToggle", 
                                 "Colorear por Categoría", 
                                 value = TRUE, 
                                 status = "info",
                                 fill = TRUE)
                  ),
                  
                  # Controles de descarga personalizados
                  div(
                    class = "download-controls-container",
                    h5(icon("cogs"), " Configuración de Descarga", style = "color: #27ae60; margin-bottom: 15px;"),
                    
                    fluidRow(
                      column(6,
                             numericInput("plot_width", 
                                          div(icon("arrows-alt-h"), " Ancho (pulg)"), 
                                          value = 12, 
                                          min = 5, 
                                          max = 20, 
                                          step = 0.5)
                      ),
                      column(6,
                             numericInput("plot_height", 
                                          div(icon("arrows-alt-v"), " Alto (pulg)"), 
                                          value = 8, 
                                          min = 4, 
                                          max = 16, 
                                          step = 0.5)
                      )
                    ),
                    
                    numericInput("plot_dpi", 
                                 div(icon("expand"), " Resolución (DPI)"), 
                                 value = 600, 
                                 min = 150, 
                                 max = 1200, 
                                 step = 50),
                    
                    helpText("Configuración aplicada a ambos gráficos", 
                             style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;")
                  )
                ),
                box(
                  width = 9,
                  height = 650,
                  title = "📊 Distribución de Códigos",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  plotlyOutput("plotCodigos", height = "580px") %>%
                    withSpinner(type = 6, color = "#3498db")
                ),
                box(
                  width = 3,
                  title = "💾 Descargas",
                  status = "success",
                  solidHeader = TRUE,
                  downloadBttn("download_distribucion_jpg",
                               div(icon("download"), " Distribución (JPG)"),
                               style = "gradient", color = "success", size = "sm", block = TRUE)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "💾 Descarga Red",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  downloadBttn("download_red_jpg",
                               div(icon("download"), " Red de Coocurrencia (JPG)"),
                               style = "gradient", color = "success", size = "sm")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  height = 750,
                  title = "🕸️ Red de Coocurrencia y Análisis de Centralidad",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  plotOutput("plotRedCentralidad", height = "680px") %>%
                    withSpinner(type = 6, color = "#3498db")
                )
              )
      ),
      
      # ---- Análisis IA (nuevo) ----
      tabItem("analisis_ia",
              fluidRow(
                box(
                  width = 4,
                  title = "🤖 Configuración del Análisis IA",
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("key"), " API de OpenAI", style = "color: #2c3e50; margin-bottom: 15px;"),
                    passwordInput("openai_api_key",
                                  div(icon("lock"), " API Key"),
                                  placeholder = "sk-..."),
                    helpText("Tu API Key de OpenAI (gpt-4)",
                             style = "color: #7f8c8d; font-size: 12px;")
                  ),
                  
                  div(
                    class = "info-panel",
                    style = "margin-top: 20px;",
                    h5(icon("book"), " Diccionario de Códigos", style = "color: #2c3e50; margin-bottom: 15px;"),
                    fileInput("dict_ia",
                              div(icon("upload"), " Cargar Diccionario"),
                              accept = c(".csv", ".xlsx"),
                              buttonLabel = "Examinar...",
                              placeholder = "Archivo .csv o .xlsx"),
                    helpText("Debe tener columnas: Categoría, Código, Definición",
                             style = "color: #7f8c8d; font-size: 12px;")
                  ),
                  
                  div(
                    style = "margin-top: 25px;",
                    actionBttn("run_ia_analysis",
                               div(icon("play"), " Ejecutar Análisis IA"),
                               style = "gradient", color = "royal", size = "md", block = TRUE),
                    downloadBttn("download_ia_results",
                                 label = "Descargar Resultados (.xlsx)",
                                 style = "gradient", color = "success", size = "md", block = TRUE),
                    helpText("Los resultados se mostrarán abajo. Descarga la tabla en Excel con el botón de arriba.",
                             style = "color: #7f8c8d; font-size: 12px; margin-top: 10px;")
                  )
                ),
                box(
                  width = 8,
                  title = "📊 Resultados del Análisis IA",
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("info-circle"), " Instrucciones", style = "color: #2c3e50;"),
                    p("1. Asegúrate de tener documentos cargados en la pestaña 'Documento'",
                      style = "color: #7f8c8d;"),
                    p("2. Ingresa tu API Key de OpenAI",
                      style = "color: #7f8c8d;"),
                    p("3. Carga un diccionario de códigos con las columnas: Categoría, Código, Definición",
                      style = "color: #7f8c8d;"),
                    p("4. Ejecuta el análisis y revisa los resultados",
                      style = "color: #7f8c8d;"),
                    p("5. Si estás satisfecho, integra los resultados al análisis manual",
                      style = "color: #7f8c8d;")
                  ),
                  DTOutput("tabla_ia_results") %>% withSpinner(type = 6, color = "#3498db")
                )
              ),
              
              # Análisis visual de resultados IA
              fluidRow(
                box(
                  width = 12,
                  title = "📊 Análisis Visual de Resultados IA",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  fluidRow(
                    column(6,
                           h5(icon("chart-bar"), " Distribución de Códigos", style = "color: #2c3e50; margin-bottom: 15px;"),
                           plotlyOutput("plot_ia_distribucion", height = "400px") %>%
                             withSpinner(type = 6, color = "#17a2b8")
                    ),
                    column(6,
                           h5(icon("chart-pie"), " Fragmentos por Categoría", style = "color: #2c3e50; margin-bottom: 15px;"),
                           plotlyOutput("plot_ia_categorias", height = "400px") %>%
                             withSpinner(type = 6, color = "#17a2b8")
                    )
                  )
                )
              )
      ),
      
      # ---- Estado (diseño mejorado) ----
      tabItem("estado",
              fluidRow(
                box(
                  width = 6, 
                  title = "💾 Guardar Proyecto", 
                  status = "info", 
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("save"), " Respaldo de Datos", style = "color: #2c3e50; margin-bottom: 15px;"),
                    p("Guarda todo tu trabajo incluyendo códigos, categorías y resaltados.",
                      style = "color: #7f8c8d; margin-bottom: 20px;"),
                    downloadBttn("saveState", 
                                 div(icon("download"), " Descargar Estado (.rds)"), 
                                 style = "gradient", color = "warning", size = "md")
                  )
                ),
                box(
                  width = 6, 
                  title = "📂 Cargar Proyecto", 
                  status = "info", 
                  solidHeader = TRUE,
                  div(
                    class = "info-panel",
                    h5(icon("upload"), " Restaurar Datos", style = "color: #2c3e50; margin-bottom: 15px;"),
                    p("Carga un proyecto previamente guardado para continuar trabajando.",
                      style = "color: #7f8c8d; margin-bottom: 20px;"),
                    fileInput("loadState", 
                              div(icon("folder-open"), " Seleccionar Archivo"), 
                              accept = ".rds",
                              buttonLabel = "Buscar...",
                              placeholder = "Archivo .rds no seleccionado")
                  )
                )
              )
      ),
      
      # ---- Cómo citar (diseño mejorado) ----
      tabItem("citar",
              fluidRow(
                box(
                  width = 12, 
                  title = "📚 Cómo Citar RCualiText", 
                  status = "success", 
                  solidHeader = TRUE,
                  
                  # Header visual mejorado
                  div(
                    style = "text-align: center; padding: 30px; background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%); margin: -25px -25px 25px -25px; color: white;",
                    div(
                      style = "font-size: 64px; margin-bottom: 15px;",
                      icon("quote-right")
                    ),
                    h2("Reconocimiento Académico", style = "margin: 0; font-weight: 600; text-shadow: 1px 1px 2px rgba(0,0,0,0.2);")
                  ),
                  
                  # Cita principal
                  div(
                    class = "info-panel",
                    h3(icon("graduation-cap"), " Cita en formato APA 7ª edición", 
                       style = "color: #27ae60; margin-bottom: 20px; font-weight: 600;"),
                    
                    div(
                      style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 25px; border-left: 5px solid #27ae60; margin: 20px 0; font-family: 'Georgia', serif; font-size: 16px; line-height: 2; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                      HTML("Ventura-León, J. (2025). <em>RCualiText</em> (v1.0) [Shiny app]. GitHub. https://github.com/jventural/RCualiText_App")
                    ),
                    
                    div(
                      style = "text-align: center; margin: 25px 0;",
                      actionBttn("copycitation", 
                                 div(icon("copy"), " Copiar Cita"), 
                                 style = "gradient", color = "success", size = "md",
                                 class = "btn-lg")
                    )
                  ),
                  
                  hr(),
                  
                  # Información adicional mejorada
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 25px; margin-top: 30px;",
                    
                    # Información del software
                    div(
                      class = "info-panel",
                      h4(icon("info-circle"), " Información del Software", style = "color: #3498db; margin-bottom: 15px;"),
                      div(
                        style = "space-y: 10px;",
                        div(strong("👨‍💻 Autor: "), "Dr. José Ventura-León"),
                        div(strong("📅 Año: "), "2025"),
                        div(strong("🔢 Versión: "), "2.0"),
                        div(strong("⚙️ Tipo: "), "Aplicación Shiny para análisis cualitativo"),
                        div(strong("🌐 Repositorio: "), 
                            tags$a("GitHub", 
                                   href = "https://github.com/jventural/RCualiText_App", 
                                   target = "_blank", 
                                   style = "color: #3498db; text-decoration: none; font-weight: 500;"))
                      )
                    ),
                    
                    # Nota importante
                    div(
                      class = "danger-panel",
                      h4(icon("exclamation-triangle"), " Importante", style = "color: #e74c3c; margin-bottom: 15px;"),
                      p("Si utilizas RCualiText en tu investigación o trabajo académico, te agradecemos que incluyas esta cita para reconocer el trabajo del autor y permitir que otros investigadores puedan acceder a esta herramienta.", 
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
                  title = "ℹ️ Acerca de RCualiText", 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  # Header mejorado
                  div(
                    style = "text-align: center; padding: 30px; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); margin: -25px -25px 25px -25px; color: white;",
                    div(
                      style = "font-size: 64px; margin-bottom: 15px;",
                      icon("microscope")
                    ),
                    h2("Análisis Cualitativo Avanzado", style = "margin: 0; font-weight: 600; text-shadow: 1px 1px 2px rgba(0,0,0,0.2);")
                  ),
                  
                  # Descripción principal
                  div(
                    class = "info-panel",
                    p("RCualiText es una aplicación avanzada para la codificación cualitativa de textos que permite cargar documentos (.txt y .docx), definir códigos y categorías, resaltar extractos de interés y visualizar frecuencias y redes de coocurrencia de códigos.", 
                      style = "font-size: 16px; line-height: 1.8; color: #2c3e50;"),
                    p("Con RCualiText puedes gestionar de manera interactiva tu lista de códigos, agruparlos en categorías, exportar tus resaltados a Excel y analizar gráficamente tus datos cualitativos mediante visualizaciones modernas y análisis de redes.", 
                      style = "font-size: 16px; line-height: 1.8; color: #2c3e50;")
                  ),
                  
                  # Funcionalidades en grid
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 25px; margin: 30px 0;",
                    
                    # Funcionalidades de Resaltado
                    div(
                      class = "info-panel",
                      h4(icon("highlighter"), " Resaltado Inteligente", style = "color: #e74c3c; margin-bottom: 15px;"),
                      tags$ul(
                        style = "line-height: 1.8; color: #2c3e50;",
                        tags$li(strong("🎯 Modo Seleccionar:"), " Aplica códigos a fragmentos seleccionados"),
                        tags$li(strong("🗑️ Modo Deseleccionar:"), " Elimina códigos específicos con un clic"),
                        tags$li(strong("📚 Modo Acumulativo:"), " Múltiples códigos por fragmento"),
                        tags$li(strong("🌈 Gradientes Visuales:"), " Códigos múltiples con efectos visuales"),
                        tags$li(strong("💡 Tooltips Informativos:"), " Información al pasar el mouse"),
                        tags$li(strong("📊 Exportación Completa:"), " Datos detallados a Excel")
                      )
                    ),
                    
                    # Guía de uso
                    div(
                      class = "info-panel",
                      h4(icon("user-graduate"), " Guía de Deselección", style = "color: #9b59b6; margin-bottom: 15px;"),
                      tags$ol(
                        style = "line-height: 1.8; color: #2c3e50;",
                        tags$li("Activa el modo 'Deseleccionar' en el panel de controles"),
                        tags$li("Haz clic directamente sobre el texto resaltado"),
                        tags$li("Selecciona qué código específico eliminar"),
                        tags$li("Vuelve al modo 'Seleccionar' para continuar")
                      )
                    )
                  ),
                  
                  # Autor y versión
                  div(
                    class = "info-panel",
                    style = "text-align: center; margin-top: 30px;",
                    h4("👨‍🔬 Dr. José Ventura-León", style = "color: #2c3e50; margin-bottom: 10px;")
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
    )
  )
  
  get_code_colors <- reactive({
    set_names(rv$codigosDF$Color, rv$codigosDF$Codigo)
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
        
        p <- plot_codigos(rv$tabla, 
                          fill = input$fillToggle, 
                          code_colors = code_colors)
        
        # Guardar como JPG con parámetros personalizados
        ggsave(file, plot = p, device = "jpeg", 
               width = ancho, height = alto, dpi = dpi, bg = "white")
        
        showNotification(
          paste0("📊 Gráfico de distribución descargado (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), 
          type = "message", duration = 4
        )
      } else {
        showNotification("❌ No hay datos para descargar", type = "error", duration = 3)
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
          paste0("🕸️ Gráfico de red descargado (", ancho, "×", alto, " pulg, ", dpi, " DPI)"), 
          type = "message", duration = 4
        )
      } else {
        showNotification("❌ No hay datos para descargar", type = "error", duration = 3)
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
        h5(icon("mouse-pointer"), " Modo Seleccionar Activo", style = "color: #3498db; margin-bottom: 15px;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            h6("✅ Acciones disponibles:", style = "color: #2c3e50; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #7f8c8d; margin: 0;",
              tags$li("Selecciona texto en el visor de documento"),
              tags$li("Aplica códigos automáticamente")
            )
          ),
          div(
            h6("⚡ Características:", style = "color: #2c3e50; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #7f8c8d; margin: 0;",
              tags$li("Modo acumulativo disponible"),
              tags$li("Clic en resaltado = info")
            )
          )
        )
      )
    })
    
    showNotification("🎯 Modo Seleccionar activado", type = "message", duration = 2)
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
        h5(icon("eraser"), " Modo Deseleccionar Activo", style = "color: #e74c3c; margin-bottom: 15px;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            h6("🎯 Instrucciones:", style = "color: #c0392b; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #a93226; margin: 0;",
              tags$li("Clic directo en texto resaltado"),
              tags$li("Elige código específico a eliminar")
            )
          ),
          div(
            h6("⚠️ Importante:", style = "color: #c0392b; margin-bottom: 8px;"),
            tags$ul(
              style = "font-size: 13px; color: #a93226; margin: 0;",
              tags$li("Cursor cambia en resaltado"),
              tags$li("Vuelve a 'Seleccionar' después")
            )
          )
        )
      )
    })
    
    showNotification("🗑️ Modo Deseleccionar activado - Haz clic en texto resaltado del visor", 
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
      showNotification("❌ No se encontraron códigos para este fragmento", type = "error", duration = 3)
      return()
    }
    
    if (nrow(fragmentos_asociados) == 1) {
      # Solo un código - eliminar directamente
      codigo_eliminar <- fragmentos_asociados$Codigo[1]
      
      showModal(modalDialog(
        title = div(icon("exclamation-triangle"), " Confirmar Eliminación"),
        div(
          h4("¿Eliminar este resaltado?", style = "color: #e74c3c;"),
          div(
            class = "info-panel",
            strong("📝 Texto: "), str_trunc(fragment_text, 50), br(),
            strong("🏷️ Código: "), span(codigo_eliminar, style = paste0("background:", fragmentos_asociados$Color[1], "; padding: 4px 8px; border-radius: 4px; color: white;")), br(),
            strong("📄 Archivo: "), fragmentos_asociados$Archivo[1]
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirmarEliminacionUnica", "🗑️ Eliminar", class = "btn-danger")
        )
      ))
      
      # Guardar el fragmento para eliminación
      rv$fragmento_a_eliminar <- fragmentos_asociados[1, ]
      
    } else {
      # Múltiples códigos - mostrar opciones
      showModal(modalDialog(
        title = div(icon("list"), " Seleccionar Código a Eliminar"),
        div(
          h4("Este fragmento tiene múltiples códigos:", style = "color: #3498db;"),
          div(
            class = "info-panel",
            strong("📝 Texto: "), str_trunc(fragment_text, 50)
          ),
          h5("🏷️ Códigos aplicados:"),
          DTOutput("tablaCodigosFragmento")
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("eliminarCodigoSeleccionado", "🗑️ Eliminar Seleccionado", class = "btn-danger")
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
          colnames = c("Código", "Categoría", "Aplicado")
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
    showNotification(paste("✅ Código", frag$Codigo, "eliminado del fragmento"), 
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
    showNotification(paste("✅ Código", frag_eliminar$Codigo, "eliminado del fragmento"), 
                     type = "message", duration = 3)
    
    # Limpiar variables temporales
    rv$fragmentos_multiples <- NULL
  })
  
  # ========================================
  # CRUD códigos
  # ========================================
  
  output$tablaCodigos <- renderDT({
    datatable(
      rv$codigosDF, 
      selection = "single", 
      options = list(
        pageLength = 8,
        dom = 'frtip',
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ códigos",
          info = "Mostrando _START_ a _END_ de _TOTAL_ códigos",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      ),
      colnames = c("🏷️ Código", "🎨 Color")
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
      showNotification(paste("✅ Código", input$new_codigo, "actualizado"), type = "message", duration = 2)
    } else {
      df <- bind_rows(df, tibble(Codigo=input$new_codigo, Color=input$new_color))
      showNotification(paste("➕ Código", input$new_codigo, "añadido"), type = "message", duration = 2)
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
      title = div(icon("exclamation-triangle"), " Confirmar Eliminación de Código"),
      div(
        h4(paste("¿Eliminar el código:", codigo_eliminar, "?"), style = "color: #e74c3c;"),
        if(resaltados_afectados > 0) {
          div(
            class = "danger-panel",
            p(paste("⚠️ Esto también eliminará", resaltados_afectados, "resaltado(s) asociado(s) a este código."),
              style = "color: #c0392b; font-weight: bold;")
          )
        } else {
          div(
            class = "info-panel",
            p("Este código no tiene resaltados asociados.", style = "color: #7f8c8d;")
          )
        }
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarEliminarCodigo", "🗑️ Eliminar", class = "btn-danger")
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
        paste("🗑️ Código", codigo_eliminar, "eliminado junto con", resaltados_eliminados, "resaltado(s)"), 
        type = "warning", 
        duration = 4
      )
    } else {
      showNotification(
        paste("🗑️ Código", codigo_eliminar, "eliminado"), 
        type = "warning", 
        duration = 2
      )
    }
  })
  
  # ========================================
  # CRUD categorías
  # ========================================
  
  output$tablaCategorias <- renderDT({
    datatable(
      rv$categoriasDF, 
      selection = "single", 
      options = list(
        pageLength = 8,
        dom = 'frtip',
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ categorías",
          info = "Mostrando _START_ a _END_ de _TOTAL_ categorías",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      ),
      colnames = c("📁 Categoría", "🏷️ Códigos Asociados")
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
      showNotification(paste("✅ Categoría", input$new_categoria, "actualizada"), type = "message", duration = 2)
    } else {
      df <- bind_rows(df, nueva)
      showNotification(paste("➕ Categoría", input$new_categoria, "añadida"), type = "message", duration = 2)
    }
    rv$categoriasDF <- df
  })
  
  observeEvent(input$deleteCategoria, {
    sel <- input$tablaCategorias_rows_selected; req(length(sel)==1)
    categoria_eliminar <- rv$categoriasDF$Categoria[sel]
    df <- rv$categoriasDF[-sel, ]
    rv$categoriasDF <- df
    showNotification(paste("🗑️ Categoría", categoria_eliminar, "eliminada"), type = "warning", duration = 2)
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
    showNotification(paste("📄", nrow(files), "documento(s) cargado(s)"), type = "message", duration = 3)
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
      rv$texto <- aplicar_resaltado_multiple(texto_original, fragmentos_archivo)
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
    if (!is.null(rv$docs) && rv$idx>0) {
      paste0("📄 Documento ", rv$idx, " de ", length(rv$docs), ": ", rv$docs[[rv$idx]]$name)
    } else "📂 No hay documentos cargados"
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
    
    # Si no hay categoría asignada, usar "Sin categoría"
    if (is.null(cat_sel) || is.na(cat_sel) || cat_sel == "") {
      cat_sel <- "Sin categoría"
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
          paste("➕ Código", code, "añadido al fragmento"),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste("⚠️ El código", code, "ya está aplicado"),
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
          paste("🔄 Fragmento recodificado con", code),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste("✅ Fragmento codificado con", code),
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
      title = div(icon("exclamation-triangle"), " Confirmar Limpieza"),
      div(
        h4("¿Estás seguro?", style = "color: #f39c12;"),
        div(
          class = "info-panel",
          p(paste("Se eliminarán todos los resaltados del documento:", strong(archivo_actual)),
            style = "color: #e67e22; margin: 0;")
        )
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarLimpieza", "🧹 Sí, limpiar", class = "btn-warning")
      )
    ))
  })
  
  observeEvent(input$confirmarLimpieza, {
    archivo_actual <- rv$docs[[rv$idx]]$name
    rv$tabla <- rv$tabla %>%
      filter(Archivo != archivo_actual)
    
    removeModal()
    showNotification(
      paste("🧹 Resaltados eliminados de", archivo_actual),
      type = "message",
      duration = 3
    )
  })
  
  # Función para eliminar todos los resaltados
  observeEvent(input$eliminarTodosResaltes, {
    showModal(modalDialog(
      title = div(icon("exclamation-triangle"), " ⚠️ Confirmación Crítica"),
      div(
        class = "danger-panel",
        h4("¿Eliminar TODOS los resaltados?", style = "color: #c0392b;"),
        p("Esta acción eliminará todos los códigos aplicados en todos los documentos.", style = "color: #a93226;"),
        p(strong("Esta acción no se puede deshacer."), style = "color: #8b1538; font-size: 16px;")
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarEliminacionTotal", "🗑️ Sí, eliminar todo", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmarEliminacionTotal, {
    rv$tabla <- rv$tabla[0, ]  # Vaciar tabla manteniendo estructura
    
    removeModal()
    showNotification("🗑️ Todos los resaltados han sido eliminados", type = "warning", duration = 4)
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
        title = div(icon("info-circle"), " Información del Fragmento"),
        div(
          div(
            class = "info-panel",
            h4("📝 Texto:"),
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; max-height: 150px; overflow-y: auto; border-left: 4px solid #3498db;",
              texto_fragmento
            )
          ),
          div(
            class = "info-panel", 
            h4("🏷️ Códigos aplicados:"),
            p(codigos_aplicados, style = "font-weight: bold; color: #3498db; font-size: 16px;"),
            h4("📄 Archivo:"),
            p(fragmento_info$Archivo[1], style = "color: #7f8c8d;")
          ),
          if (nrow(fragmento_info) > 1) {
            div(
              class = "info-panel",
              h5("📈 Historial de codificación:"),
              DTOutput("historialCodificacion")
            )
          }
        ),
        footer = modalButton("Cerrar"),
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
            colnames = c("Código", "Categoría", "Aplicado")
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
      title = div(icon("question-circle"), " 🆘 Centro de Ayuda"),
      div(
        div(
          class = "info-panel",
          h4("🎯 Modos de Trabajo:"),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            div(
              h5("✅ Modo Seleccionar:", style = "color: #27ae60;"),
              tags$ul(
                style = "color: #2c3e50;",
                tags$li("Selecciona texto en el visor de documento"),
                tags$li("Aplica códigos automáticamente"),
                tags$li("Resaltado inmediato en tiempo real")
              )
            ),
            div(
              h5("🗑️ Modo Deseleccionar:", style = "color: #e74c3c;"),
              tags$ul(
                style = "color: #2c3e50;",
                tags$li("Clic directo en resaltado"),
                tags$li("Elimina códigos específicos"),
                tags$li("Corrección precisa")
              )
            )
          )
        ),
        
        div(
          class = "info-panel",
          h4("🚀 Características Avanzadas:"),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            div(
              h5("📚 Modo Acumulativo:", style = "color: #3498db;"),
              p("Permite aplicar múltiples códigos al mismo fragmento de texto", style = "color: #2c3e50;")
            ),
            div(
              h5("🌈 Visualización:", style = "color: #9b59b6;"),
              p("Gradientes muestran fragmentos con varios códigos", style = "color: #2c3e50;")
            )
          )
        )
      ),
      footer = modalButton("Entendido"),
      size = "l"
    ))
  })
  
  # ========================================
  # Botón para copiar cita mejorado
  # ========================================
  
  observeEvent(input$copycitation, {
    citation_text <- "Ventura-León, J. (2025). RCualiText (v1.0) [Shiny app]. GitHub. https://github.com/jventural/RCualiText_App"
    
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
      "📋 Cita copiada al portapapeles",
      type = "message",
      duration = 3
    )
  })
  
  # ========================================
  # Tabla de resaltados mejorada
  # ========================================
  
  output$tablaResaltes <- renderDT({
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
          search = "🔍 Buscar:",
          lengthMenu = "Mostrar _MENU_ resaltados",
          info = "Mostrando _START_ a _END_ de _TOTAL_ resaltados",
          paginate = list(previous = "⬅️ Anterior", `next` = "Siguiente ➡️")
        ),
        columnDefs = list(
          list(targets = 0, width = "250px"),
          list(targets = 1, width = "120px"),
          list(targets = 2, width = "120px"),
          list(targets = 3, width = "180px"),
          list(targets = 4, width = "80px")
        )
      ),
      colnames = c("📝 Extracto", "🏷️ Código", "📁 Categoría", "📄 Archivo", "⏰ Hora")
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
      title = div(icon("exclamation-triangle"), " Confirmar Eliminación"),
      div(
        h4("¿Eliminar este resaltado?", style = "color: #e74c3c;"),
        div(
          class = "info-panel",
          p(paste("🏷️ Código:", strong(fila_eliminar$Codigo)), style = "margin: 5px 0;"),
          p(paste("📝 Fragmento:", strong(str_trunc(fila_eliminar$Extracto, 40))), style = "margin: 5px 0;")
        )
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmarEliminacion", "🗑️ Eliminar", class = "btn-danger")
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
    showNotification("🗑️ Resaltado eliminado", type = "message", duration = 3)
  })
  
  # ========================================
  # Descarga mejorada
  # ========================================
  
  output$descarga <- downloadHandler(
    filename = function() paste0("resaltados_rcualitext_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      
      # Hoja principal con todos los resaltados
      addWorksheet(wb, "Resaltados_Detallados")
      writeData(wb, "Resaltados_Detallados", rv$tabla)
      
      # Hoja con resumen de fragmentos únicos
      resumen_fragmentos <- rv$tabla %>%
        group_by(Extracto, Archivo, FragmentId) %>%
        summarise(
          Codigos_Aplicados = paste(Codigo, collapse = "; "),
          Num_Codigos = n(),
          Primera_Codificacion = min(Timestamp),
          Ultima_Codificacion = max(Timestamp),
          .groups = "drop"
        ) %>%
        arrange(desc(Num_Codigos))
      
      addWorksheet(wb, "Resumen_Fragmentos")
      writeData(wb, "Resumen_Fragmentos", resumen_fragmentos)
      
      # Hoja con estadísticas
      estadisticas <- tibble(
        Metrica = c(
          "Total de Resaltados",
          "Fragmentos Únicos",
          "Fragmentos con Múltiples Códigos",
          "Promedio de Códigos por Fragmento",
          "Documentos Procesados",
          "Códigos Diferentes Utilizados"
        ),
        Valor = c(
          nrow(rv$tabla),
          length(unique(rv$tabla$FragmentId)),
          sum(table(rv$tabla$FragmentId) > 1),
          round(nrow(rv$tabla) / length(unique(rv$tabla$FragmentId)), 2),
          length(unique(rv$tabla$Archivo)),
          length(unique(rv$tabla$Codigo))
        )
      )
      
      addWorksheet(wb, "Estadisticas")
      writeData(wb, "Estadisticas", estadisticas)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification("📊 Datos exportados exitosamente", type = "message", duration = 3)
    }
  )
  
  # ========================================
  # Gráficos de análisis actualizados con mejor diseño
  # ========================================
  
  output$plotCodigos <- renderPlotly({
    # Hacer que dependa tanto de la tabla como de los códigos
    req(nrow(rv$tabla) > 0, nrow(rv$codigosDF) >= 0)
    
    tryCatch({
      p <- plot_codigos(rv$tabla, fill = input$fillToggle, code_colors = get_code_colors())
      
      plotly_obj <- plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
      
      # Configurar layout para plotly con leyenda a la derecha
      plotly_obj <- plotly_obj %>%
        plotly::layout(
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 0.5,
            xanchor = "left",
            yanchor = "middle",
            font = list(size = 9),
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 1
          ),
          margin = list(b = 40, l = 70, r = 150, t = 30),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)
      
      # Retornar el objeto
      plotly_obj
      
    }, error = function(e) {
      # En caso de error, mostrar mensaje
      showNotification(paste("Error al generar gráfico:", e$message), type = "error")
      NULL
    })
  })
  
  output$plotRedCentralidad <- renderPlot({
    # Hacer que dependa tanto de la tabla como de los códigos
    req(nrow(rv$tabla) > 0, nrow(rv$codigosDF) >= 0)
    
    result <- plot_network_and_centrality(rv$tabla, code_colors = get_code_colors())
    result$plot
  }, res = 96, bg = "transparent")
  
  # ========================================
  # Guardar / cargar estado actualizado
  # ========================================
  
  output$saveState <- downloadHandler(
    filename = function() paste0("proyecto_rcualitext_", Sys.Date(), ".rds"),
    content = function(file) {
      estado <- list(
        codigosDF = rv$codigosDF,
        categoriasDF = rv$categoriasDF,
        docs = rv$docs,
        idx = rv$idx,
        texto = rv$texto,
        tabla = rv$tabla,
        deselectMode = rv$deselectMode,
        ia_results = rv$ia_results,  # Guardar resultados de IA
        version = "2.2_con_ia",
        metadata = list(
          created = Sys.time(),
          app_version = "RCualiText v2.0 con IA",
          total_codes = nrow(rv$codigosDF),
          total_highlights = nrow(rv$tabla),
          total_docs = length(rv$docs),
          ia_results_count = nrow(rv$ia_results)
        )
      )
      saveRDS(estado, file)
      
      showNotification("💾 Proyecto guardado exitosamente", type = "message", duration = 3)
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
          "📦 Proyecto convertido al nuevo formato con soporte para deselección",
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
      
      # Limpiar categorías vacías en datos existentes
      if (nrow(rv$tabla) > 0) {
        rv$tabla <- rv$tabla %>%
          mutate(Categoria = case_when(
            is.na(Categoria) | Categoria == "" ~ "Sin categoría",
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
          ia_info <- paste0(", ", est$metadata$ia_results_count, " resultados IA")
        }
        metadata_info <- paste0(
          " (", est$metadata$total_codes, " códigos, ",
          est$metadata$total_highlights, " resaltados", ia_info, ")"
        )
      }
      
      showNotification(
        paste0("📂 Proyecto cargado exitosamente", metadata_info),
        type = "message",
        duration = 4
      )
      
    }, error = function(e) {
      showNotification(
        paste("❌ Error al cargar el proyecto:", e$message),
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
      h5(icon("mouse-pointer"), " Modo Seleccionar Activo", style = "color: #3498db; margin-bottom: 15px;"),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
        div(
          h6("✅ Acciones disponibles:", style = "color: #2c3e50; margin-bottom: 8px;"),
          tags$ul(
            style = "font-size: 13px; color: #7f8c8d; margin: 0;",
            tags$li("Selecciona texto en el visor de documento"),
            tags$li("Aplica códigos automáticamente")
          )
        ),
        div(
          h6("⚡ Características:", style = "color: #2c3e50; margin-bottom: 8px;"),
          tags$ul(
            style = "font-size: 13px; color: #7f8c8d; margin: 0;",
            tags$li("Modo acumulativo disponible"),
            tags$li("Clic en resaltado = info")
          )
        )
      )
    )
  })
  
  # Mensaje de bienvenida mejorado
  observe({
    if (is.null(rv$docs) || length(rv$docs) == 0) {
      showNotification(
        "🎉 ¡Bienvenido a RCualiText! Carga tus documentos para comenzar.",
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
                 stop("Formato de diccionario no soportado"))
    
    # Validar columnas
    expected_cols <- c("Categoría", "Código", "Definición")
    if (!all(expected_cols %in% names(df))) {
      # Intentar con nombres alternativos
      if (all(c("Categoria", "Codigo", "Definicion") %in% names(df))) {
        names(df)[names(df) == "Categoria"] <- "Categoría"
        names(df)[names(df) == "Codigo"] <- "Código"
        names(df)[names(df) == "Definicion"] <- "Definición"
      } else {
        stop("El diccionario debe tener columnas: Categoría, Código, Definición")
      }
    }
    df
  })
  
  # Ejecutar análisis IA
  observeEvent(input$run_ia_analysis, {
    # Validaciones
    if (!nzchar(input$openai_api_key)) {
      showNotification("❌ Ingresa tu API Key de OpenAI", type = "error", duration = 3)
      return()
    }
    if (is.null(rv$docs) || length(rv$docs) == 0) {
      showNotification("❌ Carga al menos un documento en la pestaña 'Documento'", type = "error", duration = 3)
      return()
    }
    
    dict <- tryCatch(dict_ia_df(), error = function(e) NULL)
    if (is.null(dict) || nrow(dict) == 0) {
      showNotification("❌ Carga un diccionario de códigos válido", type = "error", duration = 3)
      return()
    }
    
    api_key <- input$openai_api_key
    n_codes <- nrow(dict)
    total <- length(rv$docs) * n_codes
    
    withProgress(message = "🤖 Analizando con IA...", value = 0, {
      results_list <- vector("list", total)
      step <- 0
      
      for (i in seq_along(rv$docs)) {
        doc_name <- rv$docs[[i]]$name
        doc_text <- rv$docs[[i]]$modified  # Usar el texto modificado
        
        for (j in seq_len(n_codes)) {
          step <- step + 1
          catg <- dict$Categoría[j]
          code <- dict$Código[j]
          def <- dict$Definición[j]
          
          incProgress(amount = 1/total,
                      detail = paste(doc_name, "-", code))
          
          prompt <- paste0(
            "Del texto:\n\n", doc_text,
            "\n\nExtrae fragmentos que correspondan a la siguiente definición:\n\"",
            def, "\"\n\nResponde solo con los fragmentos extraídos, uno por línea."
          )
          
          tryCatch({
            txt_out <- call_openai_api(prompt, api_key)
            exs <- str_split(txt_out, "\n")[[1]]
            exs <- exs[exs != "" & !grepl("^\\s*$", exs)]
            
            results_list[[step]] <- tibble(
              Archivo = doc_name,
              Categoria = catg,
              Codigo = code,
              Definicion = def,
              Extracto = if(length(exs) > 0) exs else NA_character_
            )
          }, error = function(e) {
            showNotification(
              paste("❌", code, ":", e$message),
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
          "⚠️ No se pudieron extraer fragmentos. Verifica tu API Key y conexión.",
          type = "warning",
          duration = 5
        )
      } else {
        showNotification(
          paste("✅ Análisis IA completado:", nrow(rv$ia_results), "extractos encontrados"),
          type = "message",
          duration = 3
        )
      }
    })
  })
  
  # Mostrar resultados IA
  output$tabla_ia_results <- renderDT({
    req(nrow(rv$ia_results) > 0)
    
    datatable(
      rv$ia_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(
            first = "Primero",
            last = "Último",
            `next` = "Siguiente",
            previous = "Anterior"
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
        labs(x = "Código", y = "Frecuencia", fill = "Categoría") +
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
      addWorksheet(wb, "Resultados IA")
      
      # Escribir los datos
      writeData(wb, "Resultados IA", rv$ia_results)
      
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
      
      addStyle(wb, sheet = "Resultados IA", headerStyle, rows = 1, cols = 1:5, gridExpand = TRUE)
      
      # Ajustar anchos de columna
      setColWidths(wb, sheet = "Resultados IA", cols = 1:5, widths = c(25, 20, 20, 40, 50))
      
      # Guardar
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# ========================================
# Ejecutar App
# ========================================
shinyApp(ui, server)