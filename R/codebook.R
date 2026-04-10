# R/codebook.R
# Codebook operations: merge, split, export
# Extracted during modularization

#' @title Merge two codes
#' @description Merges all highlights from `from_code` into `to_code` and removes `from_code` from the code list.
#' @param codigosDF Tibble of codes with columns Codigo, Color, Parent.
#' @param tabla Tibble of highlights with columns Codigo, Color, etc.
#' @param from_code Character. Source code to merge from.
#' @param to_code Character. Destination code to merge into.
#' @return A list with updated `codigosDF` and `tabla`.
merge_codes <- function(codigosDF, tabla, from_code, to_code) {
  if (from_code == to_code) return(list(codigosDF = codigosDF, tabla = tabla))
  if (!from_code %in% codigosDF$Codigo || !to_code %in% codigosDF$Codigo) {
    stop("Both codes must exist")
  }
  to_color <- codigosDF$Color[codigosDF$Codigo == to_code]
  tabla_new <- tabla %>%
    dplyr::mutate(
      Codigo = ifelse(Codigo == from_code, to_code, Codigo),
      Color = ifelse(Codigo == to_code, to_color, Color)
    )
  codigosDF_new <- codigosDF %>% dplyr::filter(Codigo != from_code)
  # Also reassign parent if any child had from_code as parent
  if ("Parent" %in% names(codigosDF_new)) {
    codigosDF_new$Parent[codigosDF_new$Parent == from_code] <- to_code
  }
  list(codigosDF = codigosDF_new, tabla = tabla_new)
}

#' @title Split a code based on a predicate
#' @description Splits `source_code` into two codes: highlights matching `fragments_ids` go to `new_code`, others keep `source_code`.
#' @param codigosDF Tibble of codes.
#' @param tabla Tibble of highlights.
#' @param source_code Character. Code to split.
#' @param new_code Character. Name of the new code.
#' @param new_color Character. Color for the new code (hex).
#' @param fragment_ids Character vector of FragmentIds that should go to `new_code`.
#' @return A list with updated `codigosDF` and `tabla`.
split_code <- function(codigosDF, tabla, source_code, new_code, new_color, fragment_ids) {
  if (new_code %in% codigosDF$Codigo) stop("New code already exists")
  # Add new code
  new_row <- tibble::tibble(Codigo = new_code, Color = new_color)
  if ("Parent" %in% names(codigosDF)) new_row$Parent <- ""
  codigosDF_new <- dplyr::bind_rows(codigosDF, new_row)
  # Reassign selected fragments
  tabla_new <- tabla %>%
    dplyr::mutate(
      Codigo = ifelse(Codigo == source_code & FragmentId %in% fragment_ids, new_code, Codigo),
      Color = ifelse(Codigo == new_code, new_color, Color)
    )
  list(codigosDF = codigosDF_new, tabla = tabla_new)
}

#' @title Export codebook to DOCX
#' @description Generates a Word document with all codes, their colors, categories, parent codes, definitions, and examples.
#' @param codigosDF Tibble of codes.
#' @param categoriasDF Tibble of categories.
#' @param tabla Tibble of highlights (for examples).
#' @param file Output file path.
#' @param titulo Character. Document title.
#' @param lang Character. Language for headers ("en" or "es").
#' @return Invisibly returns the docx object.
export_codebook_docx <- function(codigosDF, categoriasDF, tabla, file, titulo = "Codebook", lang = "en") {
  doc <- officer::read_docx()

  # Labels by language
  labels <- if (lang == "es") {
    list(intro = "Este libro de c\u00f3digos documenta todos los c\u00f3digos utilizados en el an\u00e1lisis cualitativo.",
         total_codes = "Total de c\u00f3digos:", total_cats = "Total de categor\u00edas:",
         total_frags = "Total de fragmentos codificados:", generated = "Generado:",
         code_col = "C\u00f3digo", cat_col = "Categor\u00eda", parent_col = "C\u00f3digo Padre",
         freq_col = "Frecuencia", color_col = "Color", examples_head = "Ejemplos",
         no_examples = "(sin ejemplos)")
  } else {
    list(intro = "This codebook documents all codes used in the qualitative analysis.",
         total_codes = "Total codes:", total_cats = "Total categories:",
         total_frags = "Total coded fragments:", generated = "Generated:",
         code_col = "Code", cat_col = "Category", parent_col = "Parent Code",
         freq_col = "Frequency", color_col = "Color", examples_head = "Examples",
         no_examples = "(no examples)")
  }

  # Title
  doc <- doc %>%
    officer::body_add_par(titulo, style = "heading 1") %>%
    officer::body_add_par(paste(labels$generated, format(Sys.time(), "%Y-%m-%d %H:%M")), style = "Normal") %>%
    officer::body_add_par("", style = "Normal") %>%
    officer::body_add_par(labels$intro, style = "Normal") %>%
    officer::body_add_par("", style = "Normal") %>%
    officer::body_add_par(paste(labels$total_codes, nrow(codigosDF)), style = "Normal") %>%
    officer::body_add_par(paste(labels$total_cats, nrow(categoriasDF)), style = "Normal") %>%
    officer::body_add_par(paste(labels$total_frags, nrow(tabla)), style = "Normal") %>%
    officer::body_add_par("", style = "Normal")

  # For each code, add details
  for (i in seq_len(nrow(codigosDF))) {
    codigo <- codigosDF$Codigo[i]
    color <- codigosDF$Color[i]
    parent <- if ("Parent" %in% names(codigosDF)) codigosDF$Parent[i] else ""

    # Find category
    cat_found <- ""
    if (nrow(categoriasDF) > 0) {
      for (j in seq_len(nrow(categoriasDF))) {
        cods_cat <- strsplit(categoriasDF$Codigos[j], ",\\s*")[[1]]
        if (codigo %in% cods_cat) {
          cat_found <- categoriasDF$Categoria[j]
          break
        }
      }
    }

    freq <- sum(tabla$Codigo == codigo, na.rm = TRUE)

    doc <- doc %>%
      officer::body_add_par(codigo, style = "heading 2") %>%
      officer::body_add_par(paste(labels$color_col, ":", color), style = "Normal") %>%
      officer::body_add_par(paste(labels$cat_col, ":", ifelse(cat_found == "", "-", cat_found)), style = "Normal") %>%
      officer::body_add_par(paste(labels$parent_col, ":", ifelse(is.na(parent) || parent == "", "-", parent)), style = "Normal") %>%
      officer::body_add_par(paste(labels$freq_col, ":", freq), style = "Normal") %>%
      officer::body_add_par(paste(labels$examples_head, ":"), style = "heading 3")

    # Add up to 3 examples
    ejemplos <- tabla$Extracto[tabla$Codigo == codigo]
    if (length(ejemplos) == 0) {
      doc <- doc %>% officer::body_add_par(labels$no_examples, style = "Normal")
    } else {
      for (ej in utils::head(ejemplos, 3)) {
        doc <- doc %>% officer::body_add_par(paste0("\"", stringr::str_trunc(ej, 200), "\""), style = "Normal")
      }
    }
    doc <- doc %>% officer::body_add_par("", style = "Normal")
  }

  print(doc, target = file)
  invisible(doc)
}

#' @title Export codebook to Excel
#' @description Generates an Excel file with codes summary.
#' @param codigosDF Tibble of codes.
#' @param categoriasDF Tibble of categories.
#' @param tabla Tibble of highlights.
#' @param file Output file path.
#' @return Invisibly returns NULL.
export_codebook_excel <- function(codigosDF, categoriasDF, tabla, file) {
  wb <- openxlsx::createWorkbook()

  # Summary sheet
  summary <- codigosDF %>%
    dplyr::mutate(
      Frecuencia = sapply(Codigo, function(c) sum(tabla$Codigo == c, na.rm = TRUE)),
      Ejemplo = sapply(Codigo, function(c) {
        ej <- tabla$Extracto[tabla$Codigo == c]
        if (length(ej) == 0) "" else stringr::str_trunc(ej[1], 100)
      })
    )
  openxlsx::addWorksheet(wb, "Codebook")
  openxlsx::writeData(wb, "Codebook", summary)
  headerStyle <- openxlsx::createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                                       fgFill = "#2c3e50", textDecoration = "bold")
  openxlsx::addStyle(wb, "Codebook", headerStyle, rows = 1, cols = 1:ncol(summary), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Codebook", cols = 1:ncol(summary), widths = "auto")

  # Categories sheet
  if (nrow(categoriasDF) > 0) {
    openxlsx::addWorksheet(wb, "Categories")
    openxlsx::writeData(wb, "Categories", categoriasDF)
    openxlsx::addStyle(wb, "Categories", headerStyle, rows = 1, cols = 1:ncol(categoriasDF), gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Categories", cols = 1:ncol(categoriasDF), widths = "auto")
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(NULL)
}
