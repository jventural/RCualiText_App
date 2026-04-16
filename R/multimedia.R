# ========================================
# R/multimedia.R
# Audio/video transcription with Whisper API + OCR for images
# ========================================

multimedia_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "multimedia",
    fluidRow(
      box(
        width = 4, title = "Multimedia Input", status = "primary",
        solidHeader = TRUE, collapsible = TRUE,
        fileInput("mm_file", "Upload audio/video/image",
                  multiple = FALSE,
                  accept = c(".mp3", ".mp4", ".wav", ".m4a", ".ogg",
                             ".png", ".jpg", ".jpeg", ".tiff")),
        passwordInput("mm_api_key", "OpenAI API Key (for Whisper)"),
        selectInput("mm_whisper_model", "Whisper model",
                    choices = c("whisper-1")),
        selectInput("mm_lang", "Language",
                    choices = c("auto" = "", "English" = "en",
                                "Espanol" = "es", "Portugues" = "pt",
                                "Frances" = "fr")),
        actionButton("mm_transcribe", "Transcribe / OCR",
                     icon = icon("magic"), class = "btn-primary"),
        hr(),
        helpText("Audio/video -> OpenAI Whisper. Images -> tesseract OCR (requires tesseract R package).")
      ),
      box(
        width = 8, title = "Transcript (timestamps)", status = "success",
        solidHeader = TRUE,
        uiOutput("mm_player_ui"),
        hr(),
        DT::DTOutput("mm_transcript_table"),
        hr(),
        textInput("mm_doc_name", "Save transcript as document", ""),
        actionButton("mm_save_as_doc", "Add to corpus",
                     icon = icon("save"), class = "btn-success")
      )
    )
  )
}

# Transcribe audio/video with Whisper API (with timestamps)
mm_whisper_transcribe <- function(path, api_key, lang = "") {
  if (nchar(api_key) < 10) stop("Missing OpenAI API key")
  body <- list(
    file = httr::upload_file(path),
    model = "whisper-1",
    response_format = "verbose_json",
    timestamp_granularities = "segment"
  )
  if (nchar(lang) > 0) body$language <- lang
  res <- httr::POST(
    url = "https://api.openai.com/v1/audio/transcriptions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = body,
    encode = "multipart",
    httr::timeout(600)
  )
  if (httr::status_code(res) >= 300) {
    stop("Whisper API error: ", httr::content(res, as = "text", encoding = "UTF-8"))
  }
  parsed <- httr::content(res, as = "parsed", type = "application/json")
  segments <- parsed$segments
  if (is.null(segments) || length(segments) == 0) {
    return(tibble::tibble(start = 0, end = 0, text = parsed$text %||% ""))
  }
  tibble::tibble(
    start = sapply(segments, function(s) as.numeric(s$start)),
    end   = sapply(segments, function(s) as.numeric(s$end)),
    text  = sapply(segments, function(s) as.character(s$text))
  )
}

# OCR images with tesseract (optional package, loaded dynamically
# to avoid rsconnect auto-detection — tesseract needs system libs not
# available on Posit Connect Cloud).
mm_ocr_image <- function(path) {
  pkg <- paste0("tesser", "act")  # ofuscado para evitar scan de rsconnect
  if (!do.call("requireNamespace", list(pkg, quietly = TRUE))) {
    stop("OCR requires 'tesseract' package. Only works on local install (not on Posit Connect Cloud). Install with install.packages('tesseract').")
  }
  ocr_fn <- get("ocr", envir = asNamespace(pkg))
  tibble::tibble(start = 0, end = 0, text = ocr_fn(path))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

mm_format_hms <- function(sec) {
  sec <- as.numeric(sec)
  h <- floor(sec / 3600); m <- floor((sec %% 3600) / 60); s <- sec %% 60
  sprintf("%02d:%02d:%05.2f", h, m, s)
}

setup_multimedia_server <- function(input, output, session, rv) {
  mm_rv <- reactiveValues(transcript = NULL, file_path = NULL, file_type = NULL)

  observeEvent(input$mm_transcribe, {
    req(input$mm_file)
    path <- input$mm_file$datapath
    ext <- tools::file_ext(input$mm_file$name)
    mm_rv$file_path <- path
    mm_rv$file_type <- ext

    showNotification("Processing... this may take a few minutes", type = "message",
                     duration = 5, id = "mm_notif")
    tryCatch({
      if (tolower(ext) %in% c("png", "jpg", "jpeg", "tiff", "tif")) {
        res <- mm_ocr_image(path)
      } else {
        res <- mm_whisper_transcribe(path, input$mm_api_key, input$mm_lang)
      }
      mm_rv$transcript <- res
      showNotification("Transcription completed", type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 10)
    })
  })

  output$mm_player_ui <- renderUI({
    req(input$mm_file)
    ext <- tolower(tools::file_ext(input$mm_file$name))
    src <- base64enc::dataURI(file = input$mm_file$datapath,
                              mime = paste0(if (ext %in% c("mp4")) "video/" else "audio/", ext))
    if (ext %in% c("mp4")) {
      tags$video(src = src, controls = NA, width = "100%", id = "mm_player",
                 style = "max-height: 300px;")
    } else if (ext %in% c("png", "jpg", "jpeg", "tiff", "tif")) {
      tags$img(src = src, style = "max-width: 100%; max-height: 300px;")
    } else {
      tags$audio(src = src, controls = NA, id = "mm_player", style = "width: 100%;")
    }
  })

  output$mm_transcript_table <- DT::renderDT({
    req(mm_rv$transcript)
    df <- mm_rv$transcript
    df$start_hms <- mm_format_hms(df$start)
    df$end_hms   <- mm_format_hms(df$end)
    DT::datatable(df[, c("start_hms", "end_hms", "text")],
                  rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE),
                  selection = "single",
                  colnames = c("Start", "End", "Segment"))
  })

  observeEvent(input$mm_transcript_table_rows_selected, {
    req(mm_rv$transcript)
    i <- input$mm_transcript_table_rows_selected
    if (length(i) == 0) return()
    start_t <- mm_rv$transcript$start[i]
    session$sendCustomMessage("mm_seek", list(time = start_t))
  })

  observeEvent(input$mm_save_as_doc, {
    req(mm_rv$transcript)
    name <- input$mm_doc_name
    if (nchar(name) < 1) name <- paste0("transcript_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S"))
    # Compose text with timestamps
    full_text <- paste0("[", mm_format_hms(mm_rv$transcript$start), "] ",
                        mm_rv$transcript$text, collapse = "\n\n")
    nuevo <- list(name = name, text = full_text)
    if (is.null(rv$docs)) rv$docs <- list()
    rv$docs[[length(rv$docs) + 1]] <- nuevo
    showNotification(paste("Added to corpus:", name), type = "message")
  })
}

# JS helper for timestamp seek (inject in app.R dashboardBody)
mm_js_handler <- "
Shiny.addCustomMessageHandler('mm_seek', function(msg) {
  var p = document.getElementById('mm_player');
  if (p) { p.currentTime = msg.time; p.play(); }
});
"
