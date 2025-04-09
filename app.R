# app.R

library(shiny)
library(shinyjs)
library(colourpicker)
library(officer)
library(DT)
library(openxlsx)
library(shinythemes)  # Se agrega shinythemes para mejorar el look & feel

# ========================================
# Función para leer archivos (.txt, .docx)
# ========================================
leer_archivo <- function(archivo) {
  ext <- tools::file_ext(archivo$datapath)
  
  if (ext == "txt") {
    lineas <- readLines(archivo$datapath, encoding = "UTF-8")
    lineas <- lineas[lineas != ""]
    contenido <- paste(lineas, collapse = "\n")
  } else if (ext %in% c("docx", "doc")) {
    doc <- read_docx(archivo$datapath)
    df  <- docx_summary(doc)
    df  <- df[!is.na(df$text) & df$text != "", ]
    df  <- df[!duplicated(df$text), ]
    contenido <- paste(df$text, collapse = "\n")
  } else {
    contenido <- "Formato de archivo no soportado"
  }
  return(contenido)
}

# ========================================
# Interfaz de Usuario (UI)
# ========================================
ui <- fluidPage(
  theme = shinytheme("readable"),  # Se utiliza un tema moderno (flatly)
  shinyjs::useShinyjs(),
  tags$head(
    # Estilos CSS personalizados para mejorar la apariencia general
    tags$style(HTML("
      body { 
        background-color: #f4f4f4; 
        font-family: 'Segoe UI', sans-serif;
      }
      .navbar, .tab-panel-heading, .well {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .tab-content { 
        background-color: #ffffff; 
        padding: 20px; 
        border: 1px solid #ddd;
        border-radius: 5px;
      }
      .btn {
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_filter input {
        border-radius: 4px;
        border: 1px solid #ccc;
        padding: 5px;
      }
      #displayText {
        background-color: #fff;
        border: 1px solid #ccc;
        padding: 10px;
        border-radius: 4px;
        min-height: 300px;
      }
    "))
  ),
  titlePanel("RCualiText"),
  
  tabsetPanel(
    # -----------------------------------------------------------------
    # 1) Definir Categorías
    # -----------------------------------------------------------------
    tabPanel("Definir Categorías",
             sidebarLayout(
               sidebarPanel(
                 textInput("new_categoria", "Nueva categoría:", value = ""),
                 colourpicker::colourInput("new_color", "Color:", value = "#F0F0F0"),
                 actionButton("addOrUpdateCategoria", "Agregar/Actualizar Categoría"),
                 actionButton("deleteCategoria", "Borrar Categoría")
               ),
               mainPanel(
                 DT::dataTableOutput("tablaCategorias")
               )
             )
    ),
    
    # -----------------------------------------------------------------
    # 2) Definir Familias
    # -----------------------------------------------------------------
    tabPanel("Definir Familias",
             sidebarLayout(
               sidebarPanel(
                 textInput("new_familia", "Nueva Familia:", value = ""),
                 selectizeInput("categories_for_familia", "Categorías de la Familia:",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(create = FALSE, placeholder = "Selecciona categorías ya definidas")),
                 actionButton("addOrUpdateFamilia", "Agregar/Actualizar Familia"),
                 actionButton("deleteFamilia", "Borrar Familia")
               ),
               mainPanel(
                 DT::dataTableOutput("tablaFamilias")
               )
             )
    ),
    
    # -----------------------------------------------------------------
    # 3) Texto (Carga y navegación de documentos)
    # -----------------------------------------------------------------
    tabPanel("Texto",
             sidebarLayout(
               sidebarPanel(
                 fileInput("archivo", "Sube archivos (.txt, .docx)", 
                           multiple = TRUE, accept = c(".txt", ".docx")),
                 selectInput("categoriaTexto", "Selecciona Categoría", choices = NULL),
                 br(),
                 actionButton("prev_doc", "Documento Anterior"),
                 actionButton("next_doc", "Siguiente Documento"),
                 br(),
                 textOutput("doc_info")
               ),
               mainPanel(
                 tags$div(
                   id = "displayText",
                   style = "white-space: pre-wrap;",
                   uiOutput("contenido")
                 )
               )
             )
    ),
    
    # -----------------------------------------------------------------
    # 4) Tabla de Resaltes
    # -----------------------------------------------------------------
    tabPanel("Tabla de Resaltes",
             sidebarLayout(
               sidebarPanel(
                 downloadButton("descarga", "Descargar tabla (XLSX)")
               ),
               mainPanel(
                 DT::dataTableOutput("tablaResaltes")
               )
             )
    )
  ),
  
  # Capturar la selección de texto SOLO dentro del contenedor #displayText
  tags$script(HTML("
    document.getElementById('displayText').addEventListener('mouseup', function() {
      var selectedText = window.getSelection().toString();
      Shiny.setInputValue('selectedText', selectedText);
    });
  "))
)

# ========================================
# Servidor
# ========================================
server <- function(input, output, session) {
  
  # Estructuras reactivas
  rv <- reactiveValues(
    # Categorías: mapeo nombre -> list(color = ...)
    categorias   = list(),
    categoriasDF = data.frame(
      Categoria = character(),
      Color     = character(),
      stringsAsFactors = FALSE
    ),
    
    # Familias: DF con columnas: Familia y Categorías (texto separado por comas)
    familiasDF   = data.frame(
      Familia    = character(),
      Categorias = character(),
      stringsAsFactors = FALSE
    ),
    
    # Documentos cargados: lista de listas, donde cada elemento tiene:
    # name: nombre del archivo,
    # original: contenido original,
    # modified: contenido modificado (con resaltados)
    docs         = NULL,
    currentIndex = 0,
    texto        = "",
    
    # Tabla de resaltes: se agrega columna "Archivo"
    tabla        = data.frame(
      Extracto  = character(),
      Categoria = character(),
      Familia   = character(),
      Color     = character(),
      Archivo   = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # -----------------------------------------------------------------
  # (A) Definir Categorías
  # -----------------------------------------------------------------
  proxyCat <- dataTableProxy("tablaCategorias")
  
  observeEvent(input$tablaCategorias_rows_selected, {
    selected <- input$tablaCategorias_rows_selected
    if (length(selected) == 1) {
      cat_name  <- rv$categoriasDF$Categoria[selected]
      cat_color <- rv$categoriasDF$Color[selected]
      updateTextInput(session, "new_categoria", value = cat_name)
      updateColourInput(session, "new_color", value = cat_color)
    }
  })
  
  observeEvent(input$addOrUpdateCategoria, {
    req(input$new_categoria)
    new_cat <- input$new_categoria
    new_color <- input$new_color
    selected <- input$tablaCategorias_rows_selected
    
    if (length(selected) == 1) {
      old_cat <- rv$categoriasDF$Categoria[selected]
      rv$categorias[[old_cat]] <- NULL
      rv$categoriasDF <- rv$categoriasDF[-selected, ]
    }
    
    rv$categorias[[new_cat]] <- list(color = new_color)
    rv$categoriasDF <- rbind(
      rv$categoriasDF,
      data.frame(Categoria = new_cat, Color = new_color, stringsAsFactors = FALSE)
    )
    
    updateSelectInput(session, "categoriaTexto", choices = names(rv$categorias))
    updateSelectizeInput(session, "categories_for_familia", choices = names(rv$categorias), server = TRUE)
    selectRows(proxyCat, NULL)
  })
  
  observeEvent(input$deleteCategoria, {
    selected <- input$tablaCategorias_rows_selected
    if (length(selected) == 1) {
      cat_to_delete <- rv$categoriasDF$Categoria[selected]
      rv$categorias[[cat_to_delete]] <- NULL
      rv$categoriasDF <- rv$categoriasDF[-selected, ]
      updateSelectInput(session, "categoriaTexto", choices = names(rv$categorias))
      updateSelectizeInput(session, "categories_for_familia", choices = names(rv$categorias), server = TRUE)
      selectRows(proxyCat, NULL)
    }
  })
  
  output$tablaCategorias <- DT::renderDataTable({
    DT::datatable(
      rv$categoriasDF,
      selection = "single",
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
  
  # -----------------------------------------------------------------
  # (B) Definir Familias
  # -----------------------------------------------------------------
  proxyFam <- dataTableProxy("tablaFamilias")
  
  observeEvent(input$tablaFamilias_rows_selected, {
    selected <- input$tablaFamilias_rows_selected
    if (length(selected) == 1) {
      fam_name <- rv$familiasDF$Familia[selected]
      fam_cats <- rv$familiasDF$Categorias[selected]
      fam_cats_vector <- unlist(strsplit(fam_cats, ",\\s*"))
      updateTextInput(session, "new_familia", value = fam_name)
      updateSelectizeInput(session, "categories_for_familia", selected = fam_cats_vector)
    }
  })
  
  observeEvent(input$addOrUpdateFamilia, {
    req(input$new_familia)
    fam_name <- input$new_familia
    fam_cats <- input$categories_for_familia
    selected <- input$tablaFamilias_rows_selected
    
    if (length(selected) == 1) {
      rv$familiasDF <- rv$familiasDF[-selected, ]
    }
    
    cat_string <- paste(fam_cats, collapse = ", ")
    newFamRow <- data.frame(
      Familia = fam_name,
      Categorias = cat_string,
      stringsAsFactors = FALSE
    )
    rv$familiasDF <- rbind(rv$familiasDF, newFamRow)
    selectRows(proxyFam, NULL)
  })
  
  observeEvent(input$deleteFamilia, {
    selected <- input$tablaFamilias_rows_selected
    if (length(selected) == 1) {
      rv$familiasDF <- rv$familiasDF[-selected, ]
      selectRows(proxyFam, NULL)
    }
  })
  
  output$tablaFamilias <- DT::renderDataTable({
    DT::datatable(
      rv$familiasDF,
      selection = "single",
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
  
  # -----------------------------------------------------------------
  # (C) Cargar Texto y Navegación de Documentos
  # -----------------------------------------------------------------
  observeEvent(input$archivo, {
    req(input$archivo)
    docs <- lapply(seq_len(nrow(input$archivo)), function(i) {
      # Para cada archivo, guardamos el nombre, el texto original y una copia modificable
      txt <- leer_archivo(input$archivo[i, ])
      list(name = input$archivo$name[i],
           original = txt,
           modified = txt)
    })
    rv$docs <- docs
    rv$currentIndex <- 1
    rv$texto <- rv$docs[[1]]$modified
    output$contenido <- renderUI({
      HTML(rv$texto)
    })
  })
  
  observeEvent(input$prev_doc, {
    if (!is.null(rv$currentIndex) && rv$currentIndex > 1) {
      rv$currentIndex <- rv$currentIndex - 1
      rv$texto <- rv$docs[[rv$currentIndex]]$modified
      output$contenido <- renderUI({ HTML(rv$texto) })
    }
  })
  
  observeEvent(input$next_doc, {
    if (!is.null(rv$currentIndex) && rv$currentIndex < length(rv$docs)) {
      rv$currentIndex <- rv$currentIndex + 1
      rv$texto <- rv$docs[[rv$currentIndex]]$modified
      output$contenido <- renderUI({ HTML(rv$texto) })
    }
  })
  
  output$doc_info <- renderText({
    if (!is.null(rv$docs) && !is.null(rv$currentIndex)) {
      paste("Documento", rv$currentIndex, "de", length(rv$docs), ":", rv$docs[[rv$currentIndex]]$name)
    } else {
      ""
    }
  })
  
  # -----------------------------------------------------------------
  # (D) Resaltar Texto y Registrar en Tabla de Resaltes (incluye nombre del archivo)
  # -----------------------------------------------------------------
  observeEvent(input$selectedText, {
    selected_text <- trimws(input$selectedText)
    if (is.null(selected_text) || nchar(selected_text) == 0) return()
    
    req(input$categoriaTexto)
    cat_selected <- input$categoriaTexto
    
    if (cat_selected %in% names(rv$categorias)) {
      color_cat <- rv$categorias[[cat_selected]]$color
    } else {
      color_cat <- "#FFFF00"
    }
    
    # Buscar la familia a la que pertenece esta categoría
    fam_selected <- ""
    if (nrow(rv$familiasDF) > 0) {
      for (i in seq_len(nrow(rv$familiasDF))) {
        cats <- unlist(strsplit(rv$familiasDF$Categorias[i], ",\\s*"))
        if (cat_selected %in% cats) {
          fam_selected <- rv$familiasDF$Familia[i]
          break
        }
      }
    }
    
    # Agregar registro a la tabla de resaltes, incluyendo el nombre del archivo actual
    nuevo_registro <- data.frame(
      Extracto  = selected_text,
      Categoria = cat_selected,
      Familia   = fam_selected,
      Color     = color_cat,
      Archivo   = if (!is.null(rv$docs)) rv$docs[[rv$currentIndex]]$name else "",
      stringsAsFactors = FALSE
    )
    rv$tabla <- rbind(rv$tabla, nuevo_registro)
    
    # Resaltar la primera aparición EXACTA del texto seleccionado en el documento actual
    rv$texto <- sub(
      selected_text,
      paste0("<span style='background-color:", color_cat, ";' title='", 
             cat_selected, if (fam_selected != "") paste0(" (", fam_selected, ")") else "", "'>",
             selected_text, "</span>"),
      rv$texto,
      fixed = TRUE
    )
    
    # Actualizar el documento modificado para conservar el resaltado
    rv$docs[[rv$currentIndex]]$modified <- rv$texto
    
    output$contenido <- renderUI({
      HTML(rv$texto)
    })
    session$sendCustomMessage("clearSelection", "clear")
  })
  
  # -----------------------------------------------------------------
  # (E) Tabla de Resaltes
  # -----------------------------------------------------------------
  output$tablaResaltes <- DT::renderDataTable({
    DT::datatable(rv$tabla, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$descarga <- downloadHandler(
    filename = function() {
      paste("resaltes-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Resaltes")
      writeData(wb, "Resaltes", rv$tabla)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# ========================================
# JavaScript adicional para limpiar la selección
# ========================================
jsCode <- "
Shiny.addCustomMessageHandler('clearSelection', function(message) {
  if (window.getSelection) {
    if (window.getSelection().empty) {
      window.getSelection().empty();
    } else if (window.getSelection().removeAllRanges) {
      window.getSelection().removeAllRanges();
    }
  } else if (document.selection) {
    document.selection.empty();
  }
});
"

# ========================================
# Ejecutar la Aplicación
# ========================================
shinyApp(
  ui = tagList(ui, tags$script(HTML(jsCode))),
  server = server
)