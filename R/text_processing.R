# R/text_processing.R
# Advanced text processing: regex search, stemming, sentiment analysis, NER via LLM

#' @title Regex search across corpus
#' @description Searches for a regex pattern across all documents, returning matches with context.
#' @param textos Character vector of document texts.
#' @param nombres_docs Character vector of document names.
#' @param pattern Regex pattern to search for.
#' @param ignore_case Logical. Whether to ignore case (default TRUE).
#' @param context_chars Integer. Characters of context to show on each side (default 60).
#' @return A tibble with Documento, Match, Contexto_Izq, Contexto_Der, Posicion.
regex_search <- function(textos, nombres_docs, pattern, ignore_case = TRUE, context_chars = 60) {
  if (is.null(pattern) || !nzchar(trimws(pattern))) return(tibble::tibble())

  resultados <- list()
  for (i in seq_along(textos)) {
    txt <- textos[i]
    matches <- tryCatch({
      gregexpr(pattern, txt, perl = TRUE, ignore.case = ignore_case)[[1]]
    }, error = function(e) {
      warning(paste("Invalid regex:", e$message))
      return(-1)
    })

    if (length(matches) == 1 && matches == -1) next

    match_lengths <- attr(matches, "match.length")

    for (j in seq_along(matches)) {
      pos <- matches[j]
      len <- match_lengths[j]
      match_text <- substring(txt, pos, pos + len - 1)

      left_start <- max(1, pos - context_chars)
      left <- substring(txt, left_start, pos - 1)
      right_end <- min(nchar(txt), pos + len + context_chars - 1)
      right <- substring(txt, pos + len, right_end)

      resultados[[length(resultados) + 1]] <- tibble::tibble(
        Documento = nombres_docs[i],
        Match = match_text,
        Contexto_Izq = left,
        Contexto_Der = right,
        Posicion = pos
      )
    }
  }

  if (length(resultados) > 0) dplyr::bind_rows(resultados) else tibble::tibble()
}

#' @title Stem words using textstem
#' @description Applies stemming to reduce words to their root form (e.g., "running" -> "run").
#' @param palabras Character vector of words.
#' @param idioma Character. Language code (default "en"). Supported: en, es, pt, fr, de, it.
#' @return Character vector of stemmed words.
stem_palabras <- function(palabras, idioma = "en") {
  if (!requireNamespace("textstem", quietly = TRUE)) {
    warning("Package 'textstem' not available - returning unchanged")
    return(palabras)
  }
  tryCatch({
    textstem::stem_words(palabras, language = idioma)
  }, error = function(e) palabras)
}

#' @title Sentiment analysis via OpenAI LLM
#' @description Uses GPT to classify the sentiment (positive/negative/neutral) and intensity of text fragments.
#' @param fragmentos Character vector of text fragments.
#' @param api_key OpenAI API key.
#' @param lang Character. Language for the prompt and response ("en" or "es").
#' @return A tibble with Fragmento, Sentimiento, Intensidad, Justificacion.
sentiment_analysis_llm <- function(fragmentos, api_key, lang = "en") {
  if (is.null(fragmentos) || length(fragmentos) == 0) return(tibble::tibble())
  if (is.null(api_key) || !nzchar(api_key)) stop("OpenAI API Key required")

  # Batch fragments to save tokens
  batch_size <- 10
  all_results <- list()

  for (batch_start in seq(1, length(fragmentos), by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, length(fragmentos))
    batch <- fragmentos[batch_start:batch_end]

    frag_text <- paste(sapply(seq_along(batch), function(i) {
      paste0(i, ". \"", batch[i], "\"")
    }), collapse = "\n")

    if (lang == "es") {
      prompt <- paste0(
        "Analiza el sentimiento de cada fragmento. Para cada uno, responde en una l\u00ednea con este formato exacto:\n",
        "N\u00famero|Sentimiento|Intensidad|Justificaci\u00f3n breve\n\n",
        "Donde:\n",
        "- Sentimiento: Positivo, Negativo, Neutral o Mixto\n",
        "- Intensidad: 1 (leve) a 5 (muy intenso)\n",
        "- Justificaci\u00f3n: una frase breve\n\n",
        "Fragmentos:\n", frag_text, "\n\n",
        "Responde solo con las l\u00edneas en el formato indicado, sin encabezados ni explicaciones."
      )
    } else {
      prompt <- paste0(
        "Analyze the sentiment of each fragment. For each, respond in one line with this exact format:\n",
        "Number|Sentiment|Intensity|Brief justification\n\n",
        "Where:\n",
        "- Sentiment: Positive, Negative, Neutral or Mixed\n",
        "- Intensity: 1 (slight) to 5 (very intense)\n",
        "- Justification: a brief sentence\n\n",
        "Fragments:\n", frag_text, "\n\n",
        "Respond only with lines in the indicated format, without headers or explanations."
      )
    }

    response <- call_openai_api(prompt, api_key)

    # Parse response
    lines <- strsplit(response, "\n")[[1]]
    lines <- lines[nzchar(trimws(lines))]

    for (line in lines) {
      parts <- strsplit(line, "\\|")[[1]]
      if (length(parts) >= 4) {
        num <- as.integer(gsub("[^0-9]", "", parts[1]))
        if (!is.na(num) && num <= length(batch)) {
          all_results[[length(all_results) + 1]] <- tibble::tibble(
            Fragmento = batch[num],
            Sentimiento = trimws(parts[2]),
            Intensidad = suppressWarnings(as.integer(gsub("[^0-9]", "", parts[3]))),
            Justificacion = trimws(parts[4])
          )
        }
      }
    }
  }

  if (length(all_results) > 0) {
    dplyr::bind_rows(all_results)
  } else {
    tibble::tibble(Fragmento = character(), Sentimiento = character(),
                   Intensidad = integer(), Justificacion = character())
  }
}

#' @title Named Entity Recognition via LLM
#' @description Uses GPT to extract named entities (persons, locations, organizations, dates) from text.
#' @param textos Character vector of texts.
#' @param api_key OpenAI API key.
#' @param lang Character. "en" or "es".
#' @return A tibble with Documento, Entidad, Tipo.
ner_llm <- function(textos, nombres_docs, api_key, lang = "en") {
  if (is.null(textos) || length(textos) == 0) return(tibble::tibble())
  if (is.null(api_key) || !nzchar(api_key)) stop("OpenAI API Key required")

  all_entities <- list()

  for (i in seq_along(textos)) {
    texto_truncado <- stringr::str_trunc(textos[i], 6000)

    if (lang == "es") {
      prompt <- paste0(
        "Extrae todas las entidades nombradas del siguiente texto. Clasif\u00edcalas en:\n",
        "PERSONA, LUGAR, ORGANIZACI\u00d3N, FECHA, EVENTO\n\n",
        "Formato de salida: una entidad por l\u00ednea como:\n",
        "Entidad|Tipo\n\n",
        "Texto:\n", texto_truncado, "\n\n",
        "Responde solo con las l\u00edneas en el formato indicado."
      )
    } else {
      prompt <- paste0(
        "Extract all named entities from the following text. Classify them as:\n",
        "PERSON, LOCATION, ORGANIZATION, DATE, EVENT\n\n",
        "Output format: one entity per line as:\n",
        "Entity|Type\n\n",
        "Text:\n", texto_truncado, "\n\n",
        "Respond only with lines in the indicated format."
      )
    }

    response <- tryCatch(call_openai_api(prompt, api_key), error = function(e) NULL)
    if (is.null(response)) next

    lines <- strsplit(response, "\n")[[1]]
    lines <- lines[nzchar(trimws(lines))]

    for (line in lines) {
      parts <- strsplit(line, "\\|")[[1]]
      if (length(parts) >= 2) {
        all_entities[[length(all_entities) + 1]] <- tibble::tibble(
          Documento = nombres_docs[i],
          Entidad = trimws(parts[1]),
          Tipo = trimws(parts[2])
        )
      }
    }
  }

  if (length(all_entities) > 0) {
    dplyr::bind_rows(all_entities) %>% dplyr::distinct()
  } else {
    tibble::tibble(Documento = character(), Entidad = character(), Tipo = character())
  }
}

#' @title Suggest a codebook from corpus using LLM
#' @description Uses GPT to inductively propose a codebook from the corpus content.
#' @param textos Character vector of documents.
#' @param api_key OpenAI API key.
#' @param n_codes Integer. Target number of codes (default 15).
#' @param lang Character. "en" or "es".
#' @return A tibble with Categoria, Codigo, Definicion.
sugerir_codigos_corpus <- function(textos, api_key, n_codes = 15, lang = "en") {
  if (is.null(textos) || length(textos) == 0) return(tibble::tibble())
  if (is.null(api_key) || !nzchar(api_key)) stop("OpenAI API Key required")

  # Concatenate texts (truncated)
  corpus_text <- paste(sapply(textos, function(t) stringr::str_trunc(t, 2000)), collapse = "\n\n---\n\n")
  corpus_text <- stringr::str_trunc(corpus_text, 12000)

  if (lang == "es") {
    prompt <- paste0(
      "Analiza el siguiente corpus de documentos y propone un diccionario de c\u00f3digos para an\u00e1lisis cualitativo inductivo. ",
      "Identifica los temas principales y subtemas. Propone aproximadamente ", n_codes, " c\u00f3digos organizados en 3-5 categor\u00edas.\n\n",
      "Formato de salida: una l\u00ednea por c\u00f3digo con el formato exacto:\n",
      "Categor\u00eda|C\u00f3digo|Definici\u00f3n breve\n\n",
      "Corpus:\n", corpus_text, "\n\n",
      "Responde solo con las l\u00edneas en el formato indicado, sin encabezados ni explicaciones."
    )
  } else {
    prompt <- paste0(
      "Analyze the following corpus of documents and propose a codebook for inductive qualitative analysis. ",
      "Identify the main themes and subthemes. Propose approximately ", n_codes, " codes organized in 3-5 categories.\n\n",
      "Output format: one line per code with the exact format:\n",
      "Category|Code|Brief definition\n\n",
      "Corpus:\n", corpus_text, "\n\n",
      "Respond only with lines in the indicated format, without headers or explanations."
    )
  }

  response <- call_openai_api(prompt, api_key)

  lines <- strsplit(response, "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]

  results <- list()
  for (line in lines) {
    parts <- strsplit(line, "\\|")[[1]]
    if (length(parts) >= 3) {
      results[[length(results) + 1]] <- tibble::tibble(
        Categoria = trimws(parts[1]),
        Codigo = trimws(parts[2]),
        Definicion = trimws(parts[3])
      )
    }
  }

  if (length(results) > 0) {
    dplyr::bind_rows(results)
  } else {
    tibble::tibble(Categoria = character(), Codigo = character(), Definicion = character())
  }
}
