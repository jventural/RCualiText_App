# ========================================
# R/ux_enhance.R
# Keyboard shortcuts (1-9), dark mode toggle, rich-text memo editor
# ========================================

ux_enhance_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "ux_enhance",
    fluidRow(
      box(
        width = 4, title = "Appearance", status = "primary", solidHeader = TRUE,
        checkboxInput("ux_dark_mode", "Dark mode", value = FALSE),
        sliderInput("ux_font_scale", "Font size scale", min = 0.8, max = 1.4,
                    value = 1.0, step = 0.05),
        helpText("Changes apply immediately.")
      ),
      box(
        width = 4, title = "Keyboard shortcuts", status = "success",
        solidHeader = TRUE,
        tags$ul(
          tags$li(tags$kbd("1"), " - ", tags$kbd("9"),
                  " Apply code #1 through #9 (sorted alphabetically) to current selection"),
          tags$li(tags$kbd("Ctrl"), "+", tags$kbd("S"), " Save snapshot"),
          tags$li(tags$kbd("Ctrl"), "+", tags$kbd("."),
                  " Toggle select/deselect mode"),
          tags$li(tags$kbd("?"), " Show shortcuts overlay")
        ),
        hr(),
        helpText("Shortcuts are active globally."),
        actionButton("ux_shortcuts_help", "Show overlay",
                     icon = icon("keyboard"))
      ),
      box(
        width = 4, title = "Rich-text memo editor", status = "info",
        solidHeader = TRUE,
        textInput("ux_memo_title", "Title"),
        tags$textarea(id = "ux_memo_rich", class = "ux-memo-rich",
                      style = "width: 100%; min-height: 200px;",
                      placeholder = "Markdown supported: **bold**, *italic*, # heading, - bullet"),
        actionButton("ux_memo_save", "Save memo", icon = icon("save"),
                     class = "btn-primary"),
        hr(),
        h5("Preview"),
        uiOutput("ux_memo_preview")
      )
    )
  )
}

ux_dark_css <- "
body.ux-dark, body.ux-dark .content-wrapper, body.ux-dark .main-footer {
  background: #1b1f23 !important; color: #e4e4e4 !important;
}
body.ux-dark .box { background: #23272e !important; color: #e4e4e4 !important;
  border: 1px solid #373d46 !important; }
body.ux-dark .box-header { color: #e4e4e4 !important; }
body.ux-dark .form-control, body.ux-dark .selectize-input,
body.ux-dark textarea {
  background: #2c313a !important; color: #e4e4e4 !important;
  border-color: #444 !important;
}
body.ux-dark table.dataTable tbody td,
body.ux-dark table.dataTable thead th { color: #e4e4e4 !important;
  background: #2c313a !important; }
"

ux_shortcut_js <- "
(function() {
  // Code shortcuts 1-9
  document.addEventListener('keydown', function(e) {
    // Skip when typing in inputs
    var tag = (e.target && e.target.tagName) || '';
    if (['INPUT', 'TEXTAREA', 'SELECT'].indexOf(tag) !== -1) return;
    if (e.ctrlKey && e.key === 's') {
      e.preventDefault();
      Shiny.setInputValue('ux_kbd_save', Math.random());
      return;
    }
    if (e.ctrlKey && e.key === '.') {
      e.preventDefault();
      Shiny.setInputValue('ux_kbd_toggle_mode', Math.random());
      return;
    }
    if (e.key === '?') {
      Shiny.setInputValue('ux_kbd_help', Math.random());
      return;
    }
    if (/^[1-9]$/.test(e.key)) {
      Shiny.setInputValue('ux_kbd_code', parseInt(e.key), {priority: 'event'});
    }
  });
  // Font scale
  Shiny.addCustomMessageHandler('ux_font_scale', function(v) {
    document.body.style.fontSize = (v * 100) + '%';
  });
  Shiny.addCustomMessageHandler('ux_toggle_dark', function(on) {
    if (on) document.body.classList.add('ux-dark');
    else document.body.classList.remove('ux-dark');
  });
})();
"

setup_ux_enhance_server <- function(input, output, session, rv) {
  observeEvent(input$ux_dark_mode, {
    session$sendCustomMessage("ux_toggle_dark", isTRUE(input$ux_dark_mode))
  }, ignoreInit = TRUE)

  observeEvent(input$ux_font_scale, {
    session$sendCustomMessage("ux_font_scale", input$ux_font_scale %||% 1)
  }, ignoreInit = TRUE)

  # Code shortcut 1-9
  observeEvent(input$ux_kbd_code, {
    n <- as.integer(input$ux_kbd_code)
    if (is.null(rv$codigosDF) || nrow(rv$codigosDF) == 0) return()
    codes <- sort(rv$codigosDF$Codigo)
    if (n <= length(codes)) {
      updateSelectInput(session, "codigoTexto", selected = codes[n])
      showNotification(paste("Code selected:", codes[n]),
                       type = "message", duration = 2)
    }
  })
  observeEvent(input$ux_kbd_toggle_mode, {
    # Toggle selection mode used by current app (selectMode/deselectMode buttons)
    rv$deselectMode <- !isTRUE(rv$deselectMode)
    session$sendCustomMessage("setDeselectMode", isTRUE(rv$deselectMode))
  })
  observeEvent(input$ux_kbd_save, {
    showNotification("Snapshot requested (Ctrl+S)", type = "message")
    # Wire to versioning:
    session$sendCustomMessage("ux_trigger_save", list())
  })
  observeEvent(input$ux_kbd_help, {
    showModal(modalDialog(
      title = "Keyboard shortcuts",
      tags$ul(
        tags$li(tags$kbd("1"), "..", tags$kbd("9"),
                " apply code 1..9 (alphabetical) to selection"),
        tags$li(tags$kbd("Ctrl"), "+", tags$kbd("S"), " save snapshot"),
        tags$li(tags$kbd("Ctrl"), "+", tags$kbd("."),
                " toggle select/deselect mode"),
        tags$li(tags$kbd("?"), " this help")
      ),
      easyClose = TRUE, footer = NULL))
  })

  observeEvent(input$ux_shortcuts_help, {
    showModal(modalDialog(title = "Shortcuts",
                          tags$ul(
                            tags$li(tags$kbd("1"), "..", tags$kbd("9"),
                                    " apply code to selection"),
                            tags$li(tags$kbd("Ctrl+S"), " save"),
                            tags$li(tags$kbd("Ctrl+."), " toggle mode"),
                            tags$li(tags$kbd("?"), " help")),
                          easyClose = TRUE))
  })

  # Rich markdown memo
  output$ux_memo_preview <- renderUI({
    v <- input$ux_memo_rich
    if (is.null(v) || nchar(v) == 0) return(tags$em("Nothing to preview"))
    if (requireNamespace("markdown", quietly = TRUE)) {
      HTML(markdown::markdownToHTML(text = v, fragment.only = TRUE))
    } else {
      tags$pre(v)
    }
  })
  observeEvent(input$ux_memo_save, {
    if (is.null(rv$memos)) rv$memos <- tibble::tibble(
      memo_id = character(), titulo = character(), contenido = character(),
      vinculo_tipo = character(), vinculo_id = character(),
      timestamp = as.POSIXct(character()))
    rv$memos <- dplyr::bind_rows(rv$memos, tibble::tibble(
      memo_id = paste0("M", format(Sys.time(), "%H%M%S"),
                       sample(100:999, 1)),
      titulo = input$ux_memo_title %||% "(no title)",
      contenido = input$ux_memo_rich %||% "",
      vinculo_tipo = "", vinculo_id = "",
      timestamp = Sys.time()))
    showNotification("Memo saved", type = "message")
  })
}
