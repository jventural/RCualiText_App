# R/utils.R
# Utility functions for RCualiText App
# Extracted during modularization for maintainability

# ── App version constant ─────────────────────────────────────────────────────
APP_VERSION <- "2.5"

# ── File reader (txt/docx/pdf) ──────────────────────────────────────────────
#' @title Read file content (txt/docx/pdf)
#' @description Reads the content of a file based on its extension and returns the cleaned plain text.
#' @param archivo A list with `datapath` (file path) and `name` (file name), as provided by shiny::fileInput.
#' @return A character string with the file's text content, or "Format not supported" if the extension is unsupported.
leer_archivo <- function(archivo) {
  ext <- tools::file_ext(archivo$datapath)
  if (ext == "txt") {
    lineas <- readLines(archivo$datapath, encoding = "UTF-8")
    lineas_limpias <- lineas[lineas != ""]
    texto_final <- paste(lineas_limpias, collapse = "\n")
    stringr::str_trim(texto_final)
  } else if (ext %in% c("docx", "doc")) {
    doc <- officer::read_docx(archivo$datapath)
    df  <- officer::docx_summary(doc)
    df  <- df[!is.na(df$text) & df$text != "", ]
    df  <- df[!duplicated(df$text), ]
    texto_final <- paste(df$text, collapse = "\n")
    stringr::str_trim(texto_final)
  } else if (ext == "pdf") {
    paginas <- pdftools::pdf_text(archivo$datapath)
    texto_final <- paste(paginas, collapse = "\n")
    texto_final <- gsub("\\r\\n", "\n", texto_final)
    texto_final <- gsub("[ \\t]+", " ", texto_final)
    lineas <- strsplit(texto_final, "\n")[[1]]
    lineas_limpias <- lineas[trimws(lineas) != ""]
    texto_final <- paste(lineas_limpias, collapse = "\n")
    stringr::str_trim(texto_final)
  } else {
    "Format not supported"
  }
}

# ── Collision-free fragment ID using xxhash32 ────────────────────────────────
#' @title Create a unique fragment ID
#' @description Generates a collision-free fragment identifier using the xxhash32 algorithm seeded with the current time and a random number.
#' @return A character string of the form "fragment_<hash>".
crear_fragment_id <- function() {
  paste0("fragment_", digest::digest(paste0(Sys.time(), runif(1)), algo = "xxhash32"))
}

# ── CSS for multiple highlight colors (gradient stripes) ─────────────────────
#' @title Generate CSS for multi-color highlighting
#' @description Builds an inline CSS string that applies either a single background color or a linear gradient with evenly distributed color stripes.
#' @param colores Character vector. Hex or CSS color values to combine in the background.
#' @return A character string with CSS declarations (background/background-color plus box-shadow, etc.).
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

# ── Apply multiple-code highlighting to text (XSS-safe) ─────────────────────
#' @title Apply multi-code highlighting to text
#' @description Wraps matching fragments in the original text with <span> elements that display one or more assigned codes, using XSS-safe HTML escaping for tooltips.
#' @param texto_original Character. The original plain text to annotate.
#' @param fragmentos_df Data frame. Must contain columns `Extracto`, `Archivo`, `Codigo`, `Color`, and `FragmentId`.
#' @param codes_label Character. Label used as prefix in the tooltip for the codes list (default "Codes").
#' @return A character string with HTML-highlighted fragments inserted in place of the original text spans.
aplicar_resaltado_multiple <- function(texto_original, fragmentos_df, codes_label = "Codes") {
  if (nrow(fragmentos_df) == 0) return(texto_original)

  texto_procesado <- texto_original

  fragmentos_agrupados <- fragmentos_df %>%
    dplyr::group_by(Extracto, Archivo) %>%
    dplyr::summarise(
      Codigos = list(Codigo),
      Colores = list(Color),
      FragmentId = dplyr::first(FragmentId),
      .groups = "drop"
    )

  for (i in seq_len(nrow(fragmentos_agrupados))) {
    frag <- fragmentos_agrupados[i, ]
    texto_buscar <- frag$Extracto
    colores <- unlist(frag$Colores)
    fragment_id <- frag$FragmentId
    codigos_aplicados <- htmltools::htmlEscape(paste(unlist(frag$Codigos), collapse = ", "))

    css_style <- generar_css_multiples(colores)

    span_text <- paste0(
      "<span class='highlight-multiple' data-fragment-id='", fragment_id, "' ",
      "title='", codes_label, ": ", codigos_aplicados, "' ",
      "style='", css_style, " padding: 4px 8px; margin: 2px; color: #fff; ",
      "font-weight: 500; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);'>",
      texto_buscar,
      "</span>"
    )

    if (!grepl(paste0("data-fragment-id='", fragment_id, "'"), texto_procesado)) {
      texto_procesado <- sub(texto_buscar, span_text, texto_procesado, fixed = TRUE)
    }
  }

  return(texto_procesado)
}

# ── Safe translation wrapper (HTML-escapes interpolated values) ──────────────
#' @title Safe translation wrapper
#' @description Wraps a translation function so that all interpolated replacement values are HTML-escaped before substitution, preventing XSS injection.
#' @param tr_func Function. The translation function to invoke (e.g. `tr()`).
#' @param key Character. The translation key to look up.
#' @param ... Named arguments with replacement values to interpolate into the translated string.
#' @return A character string with the translated text and safely escaped replacements.
safe_tr <- function(tr_func, key, ...) {
  replacements <- list(...)
  escaped <- lapply(replacements, function(v) htmltools::htmlEscape(as.character(v)))
  do.call(tr_func, c(list(key), escaped))
}

# ── Shared data preparation for code-frequency plots ─────────────────────────
#' @title Prepare code frequency counts
#' @description Aggregates a fragments data frame into per-file, per-code counts, optionally including category information, and orders codes by frequency within each file.
#' @param df Data frame. Must contain columns `Archivo`, `Codigo`, and `Categoria`.
#' @param fill Logical. If TRUE, retains the `Categoria` column in the output (default TRUE).
#' @param sin_cat_label Character. Label used to replace NA or empty categories (default "Uncategorized").
#' @return A tibble with columns `Archivo`, `Codigo` (factor), `Frecuencia`, and optionally `Categoria`.
prepare_code_counts <- function(df, fill = TRUE, sin_cat_label = "Uncategorized") {
  df <- df %>%
    dplyr::mutate(Categoria = dplyr::case_when(
      is.na(Categoria) | Categoria == "" ~ sin_cat_label,
      TRUE ~ Categoria
    ))

  if (fill && "Categoria" %in% names(df)) {
    df %>%
      dplyr::count(Archivo, Codigo, Categoria, name = "Frecuencia") %>%
      dplyr::group_by(Archivo) %>%
      dplyr::mutate(Codigo = factor(Codigo, levels = Codigo[order(Frecuencia)])) %>%
      dplyr::ungroup()
  } else {
    df %>%
      dplyr::count(Archivo, Codigo, name = "Frecuencia") %>%
      dplyr::group_by(Archivo) %>%
      dplyr::mutate(Codigo = factor(Codigo, levels = Codigo[order(Frecuencia)])) %>%
      dplyr::ungroup()
  }
}

# ── Evaluation label/color helpers (used in 4+ places) ──────────────────────
#' @title Get translated evaluation labels
#' @description Returns a named character vector mapping internal evaluation keys to localized labels via the provided translation function.
#' @param tr Function. Translation function that accepts a key string and returns the translated label.
#' @return A named character vector with keys "excellent", "good", "moderate", "low_review", "insufficient".
get_eval_labels <- function(tr) {
  c(
    "excellent"    = tr("evaluacion.excelente"),
    "good"         = tr("evaluacion.buena"),
    "moderate"     = tr("evaluacion.moderada"),
    "low_review"   = tr("evaluacion.baja_revisar"),
    "insufficient" = tr("evaluacion.insuficiente")
  )
}

#' @title Get evaluation colors keyed by translated label
#' @description Returns a named character vector mapping localized evaluation labels to hex color codes for use in plots.
#' @param tr Function. Translation function that accepts a key string and returns the translated label.
#' @return A named character vector of hex colors keyed by the translated labels (excelente/buena/moderada/baja_revisar).
get_eval_colors <- function(tr) {
  stats::setNames(
    c("#27ae60", "#2ecc71", "#f39c12", "#e74c3c"),
    c(
      tr("evaluacion.excelente"),
      tr("evaluacion.buena"),
      tr("evaluacion.moderada"),
      tr("evaluacion.baja_revisar")
    )
  )
}

# ── Generic ggplot download handler factory ──────────────────────────────────
#' @title Create a generic ggplot downloadHandler
#' @description Factory that returns a Shiny `downloadHandler` to save a ggplot with user-configurable width, height and DPI inputs.
#' @param filename_prefix Character. Prefix prepended to the date in the output filename.
#' @param plot_fn Function. A zero-argument function that returns a ggplot object.
#' @param input Shiny input object used to read `sem_plot_width`, `sem_plot_height`, and `sem_plot_dpi`.
#' @param ext Character. File extension for the saved plot (default "png").
#' @param default_w Numeric. Fallback width in inches when input is NULL (default 10).
#' @param default_h Numeric. Fallback height in inches when input is NULL (default 8).
#' @param default_dpi Numeric. Fallback DPI when input is NULL (default 300).
#' @return A Shiny `downloadHandler` object.
download_ggplot_handler <- function(filename_prefix, plot_fn, input,
                                    ext = "png", default_w = 10,
                                    default_h = 8, default_dpi = 300) {
  shiny::downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      w   <- if (!is.null(input$sem_plot_width))  input$sem_plot_width  else default_w
      h   <- if (!is.null(input$sem_plot_height)) input$sem_plot_height else default_h
      dpi <- if (!is.null(input$sem_plot_dpi))    input$sem_plot_dpi    else default_dpi
      p <- plot_fn()
      if (!is.null(p)) {
        ggplot2::ggsave(file, plot = p, width = w, height = h, dpi = dpi, bg = "white")
      }
    }
  )
}
