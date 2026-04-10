# R/text_analysis.R
# Text analysis functions for RCualiText App
# Includes: word frequency, KWIC, framework matrix

# Word frequency analysis using tidytext
#' @title Word frequency analysis
#' @description Tokenizes the provided texts with tidytext, removes language-specific and custom stop words, and returns the top words by frequency.
#' @param textos Character vector. Documents to analyze.
#' @param idioma Character. Language code for the stopwords snowball source (default "en"); falls back to English on error.
#' @param min_freq Integer. Minimum number of occurrences a word must have to be retained (default 2).
#' @param max_palabras Integer. Maximum number of words to return (default 100).
#' @param custom_stopwords Character vector or NULL. Additional stop words to remove.
#' @return A tibble with columns `word` and `n`, sorted by descending frequency.
calcular_frecuencia_palabras <- function(textos, idioma = "en", min_freq = 2,
                                          max_palabras = 100, custom_stopwords = NULL) {
  # Create a data frame with document text
  df <- tibble::tibble(doc_id = seq_along(textos), text = textos)

  # Tokenize
  tokens <- df %>%
    tidytext::unnest_tokens(word, text)

  # Get stop words for the language
  sw <- tryCatch({
    tibble::tibble(word = stopwords::stopwords(idioma, source = "snowball"))
  }, error = function(e) {
    tibble::tibble(word = stopwords::stopwords("en", source = "snowball"))
  })

  # Add custom stop words
  if (!is.null(custom_stopwords) && length(custom_stopwords) > 0) {
    sw <- dplyr::bind_rows(sw, tibble::tibble(word = tolower(custom_stopwords)))
  }

  # Remove stop words and count
  resultado <- tokens %>%
    dplyr::anti_join(sw, by = "word") %>%
    dplyr::filter(nchar(word) > 1) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::filter(n >= min_freq) %>%
    dplyr::slice_head(n = max_palabras)

  return(resultado)
}

# KWIC (Keyword in Context) - custom implementation without quanteda
#' @title Keyword in Context (KWIC) search
#' @description Finds all occurrences of a keyword across a set of documents and returns each match with a left/right context window, implemented without quanteda.
#' @param textos Character vector. Document texts to search.
#' @param nombres_docs Character vector. Document names parallel to `textos`.
#' @param keyword Character. Keyword to search for (case-insensitive, partial match).
#' @param window Integer. Number of words to include on each side of the keyword as context (default 5).
#' @return A tibble with columns `Documento`, `Contexto_Izq`, `Keyword`, `Contexto_Der`, `Posicion`, or an empty tibble with that schema when there are no matches.
kwic_search <- function(textos, nombres_docs, keyword, window = 5) {
  if (is.null(keyword) || !nzchar(trimws(keyword))) return(tibble::tibble())

  keyword_lower <- tolower(trimws(keyword))
  resultados <- list()

  for (i in seq_along(textos)) {
    palabras <- strsplit(textos[i], "\\s+")[[1]]
    palabras_lower <- tolower(palabras)

    # Find all occurrences
    matches <- which(grepl(keyword_lower, palabras_lower, fixed = TRUE))

    for (pos in matches) {
      # Get context window
      start_left <- max(1, pos - window)
      end_right <- min(length(palabras), pos + window)

      left_context <- paste(palabras[start_left:(pos - 1)], collapse = " ")
      right_context <- if (pos < length(palabras)) {
        paste(palabras[(pos + 1):end_right], collapse = " ")
      } else ""

      resultados[[length(resultados) + 1]] <- tibble::tibble(
        Documento = nombres_docs[i],
        Contexto_Izq = left_context,
        Keyword = palabras[pos],
        Contexto_Der = right_context,
        Posicion = pos
      )
    }
  }

  if (length(resultados) > 0) {
    dplyr::bind_rows(resultados)
  } else {
    tibble::tibble(Documento = character(), Contexto_Izq = character(),
                   Keyword = character(), Contexto_Der = character(),
                   Posicion = integer())
  }
}

# Generate framework matrix (cases x themes)
#' @title Generate framework matrix (documents x codes)
#' @description Produces a long-format framework matrix where each row is a (document, code) cell containing the concatenated truncated extracts and the number of fragments for that cell.
#' @param tabla Data frame of fragments with columns `Archivo`, `Codigo`, and `Extracto`.
#' @param documentos Character vector. Document names (currently used for context; the full list of documents is derived from `tabla`).
#' @return A tibble with columns `Documento`, `Codigo`, `Contenido`, `N`, or NULL if `tabla` is empty.
generar_framework_matrix <- function(tabla, documentos) {
  if (is.null(tabla) || nrow(tabla) == 0) return(NULL)

  # Get unique documents and codes
  docs <- unique(tabla$Archivo)
  codigos <- unique(tabla$Codigo)

  # Create matrix: rows = documents, columns = codes
  # Each cell contains concatenated extracts (truncated)
  matrix_data <- expand.grid(Documento = docs, Codigo = codigos, stringsAsFactors = FALSE) %>%
    tibble::as_tibble()

  matrix_data <- matrix_data %>%
    dplyr::left_join(
      tabla %>%
        dplyr::group_by(Archivo, Codigo) %>%
        dplyr::summarise(
          Contenido = paste(stringr::str_trunc(Extracto, 80), collapse = " | "),
          N = dplyr::n(),
          .groups = "drop"
        ),
      by = c("Documento" = "Archivo", "Codigo" = "Codigo")
    ) %>%
    dplyr::mutate(
      Contenido = dplyr::if_else(is.na(Contenido), "", Contenido),
      N = dplyr::if_else(is.na(N), 0L, N)
    )

  return(matrix_data)
}

# Generate matrix coding query (codes x documents frequency table)
#' @title Matrix coding query (codes x documents)
#' @description Cross-tabulates codes against documents and returns a wide frequency table similar to NVivo's matrix coding query.
#' @param tabla Data frame of fragments with columns `Archivo` and `Codigo`.
#' @return A tibble with one row per code and one column per document containing fragment counts, or NULL if `tabla` is empty.
matrix_coding_query <- function(tabla) {
  if (is.null(tabla) || nrow(tabla) == 0) return(NULL)

  # Cross-tabulation of codes x documents
  cross_tab <- tabla %>%
    dplyr::count(Archivo, Codigo) %>%
    tidyr::pivot_wider(names_from = Archivo, values_from = n, values_fill = 0)

  return(cross_tab)
}

# Matrix coding: codes x codes co-occurrence
#' @title Code x code co-occurrence matrix
#' @description Builds a symmetric integer matrix counting how many fragments are tagged with each pair of codes, using `FragmentId` to detect multi-coded fragments, and sets the diagonal to the total count per code.
#' @param tabla Data frame of fragments with columns `FragmentId` and `Codigo`.
#' @return A data frame whose first column `Codigo` lists codes and remaining columns contain co-occurrence counts, or NULL if `tabla` is empty or no fragments are multi-coded.
matrix_codes_x_codes <- function(tabla) {
  if (is.null(tabla) || nrow(tabla) == 0) return(NULL)

  # Find fragments with multiple codes
  multi_coded <- tabla %>%
    dplyr::group_by(FragmentId) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  if (nrow(multi_coded) == 0) return(NULL)

  codigos <- unique(tabla$Codigo)
  n <- length(codigos)
  mat <- matrix(0L, nrow = n, ncol = n, dimnames = list(codigos, codigos))

  fragments <- unique(multi_coded$FragmentId)
  for (fid in fragments) {
    codes_in_frag <- multi_coded %>%
      dplyr::filter(FragmentId == fid) %>%
      dplyr::pull(Codigo) %>%
      unique()
    if (length(codes_in_frag) >= 2) {
      pairs <- utils::combn(codes_in_frag, 2)
      for (j in seq_len(ncol(pairs))) {
        mat[pairs[1, j], pairs[2, j]] <- mat[pairs[1, j], pairs[2, j]] + 1L
        mat[pairs[2, j], pairs[1, j]] <- mat[pairs[2, j], pairs[1, j]] + 1L
      }
    }
  }

  # Also add self-counts (total fragments per code)
  for (cod in codigos) {
    mat[cod, cod] <- sum(tabla$Codigo == cod)
  }

  return(as.data.frame(mat) %>% tibble::rownames_to_column("Codigo"))
}
