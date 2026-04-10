# =============================================================================
# api.R - OpenAI API Functions
# RCualiText App
#
# Extracted from app.R as part of modularization.
# Contains: API calls (chat + embeddings), LLM coding validation.
# Includes retry logic for 429 rate limits with exponential backoff.
# =============================================================================

# --- Constants ---------------------------------------------------------------

OPENAI_MODEL <- "gpt-4.1"

# --- Core API Functions ------------------------------------------------------

#' @title Call OpenAI Chat Completions API
#' @description Sends a prompt to the OpenAI Chat Completions endpoint and returns the assistant response, retrying with exponential backoff on HTTP 429 rate-limit errors.
#' @param prompt Character. The user prompt to send.
#' @param api_key Character. OpenAI API key.
#' @param system_prompt Character or NULL. Optional custom system prompt; defaults to a qualitative-analysis expert persona.
#' @param max_retries Integer. Maximum number of retry attempts on 429 (default 3).
#' @return Character. The assistant's response content from the first choice.
call_openai_api <- function(prompt, api_key, system_prompt = NULL, max_retries = 3) {
  if (is.null(system_prompt)) {
    system_prompt <- "You are an expert in qualitative textual data analysis and thematic coding."
  }

  for (attempt in seq_len(max_retries)) {
    resp <- httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      httr::timeout(180),
      body = jsonlite::toJSON(list(
        model = OPENAI_MODEL,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = prompt)
        ),
        temperature = 0.3,
        max_tokens = 4096
      ), auto_unbox = TRUE)
    )

    status <- httr::status_code(resp)

    if (status == 200) {
      content <- httr::content(resp)
      return(content$choices[[1]]$message$content)
    }

    if (status == 429 && attempt < max_retries) {
      # Parse retry-after header or use exponential backoff
      retry_after <- httr::headers(resp)[["retry-after"]]
      wait_time <- if (!is.null(retry_after)) {
        as.numeric(retry_after)
      } else {
        min(2^attempt, 30)
      }
      message(paste0("Rate limited (429). Waiting ", wait_time, "s before retry ", attempt + 1, "/", max_retries))
      Sys.sleep(wait_time)
      next
    }

    error_content <- httr::content(resp, "text", encoding = "UTF-8")
    stop(paste("Error OpenAI (", status, "):", error_content))
  }
}

# --- Embeddings --------------------------------------------------------------

#' @title Get OpenAI embeddings for a set of texts
#' @description Calls the OpenAI embeddings endpoint in batches of 100 texts, retrying with backoff on HTTP 429, and assembles the results into a numeric matrix.
#' @param textos Character vector. Texts to embed.
#' @param api_key Character. OpenAI API key.
#' @param modelo Character. Embedding model name (default "text-embedding-3-small").
#' @param max_retries Integer. Maximum number of retry attempts on 429 (default 3).
#' @return Numeric matrix where rows correspond to input texts and columns to embedding dimensions.
obtener_embeddings_openai <- function(textos, api_key, modelo = "text-embedding-3-small", max_retries = 3) {
  all_embeddings <- list()
  batch_size <- 100
  n_batches <- ceiling(length(textos) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(textos))
    batch_textos <- textos[start_idx:end_idx]

    for (attempt in seq_len(max_retries)) {
      resp <- httr::POST(
        "https://api.openai.com/v1/embeddings",
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        httr::timeout(120),
        body = jsonlite::toJSON(list(
          model = modelo,
          input = batch_textos
        ), auto_unbox = TRUE)
      )

      status <- httr::status_code(resp)

      if (status == 200) {
        content <- httr::content(resp)
        for (item in content$data) {
          all_embeddings[[length(all_embeddings) + 1]] <- unlist(item$embedding)
        }
        break
      }

      if (status == 429 && attempt < max_retries) {
        retry_after <- httr::headers(resp)[["retry-after"]]
        wait_time <- if (!is.null(retry_after)) as.numeric(retry_after) else min(2^attempt, 30)
        message(paste0("Rate limited (429). Waiting ", wait_time, "s before retry ", attempt + 1, "/", max_retries))
        Sys.sleep(wait_time)
        next
      }

      stop(paste("Error OpenAI Embeddings:", status))
    }
  }

  embeddings_matrix <- do.call(rbind, all_embeddings)
  return(embeddings_matrix)
}

# --- LLM Coding Validation --------------------------------------------------

#' @title Validate qualitative coding via LLM expert panel
#' @description Builds a prompt asking the LLM to act as a panel of three qualitative-analysis experts that evaluates each fragment/code pair, and returns the structured response.
#' @param fragmentos Character vector. Text fragments to validate.
#' @param codigos Character vector. Codes assigned to each fragment (same length as `fragmentos`).
#' @param api_key Character. OpenAI API key.
#' @param lang Character. Interface language, either "en" or "es" (default "en").
#' @return Character string containing the LLM's structured evaluation of each fragment.
validar_codificacion_llm <- function(fragmentos, codigos, api_key, lang = "en") {
  if (is.null(fragmentos) || length(fragmentos) == 0) {
    stop(if (lang == "es") "No hay fragmentos para validar" else "No fragments to validate")
  }
  if (is.null(api_key) || !nzchar(api_key)) {
    stop(if (lang == "es") "API Key de OpenAI no proporcionada" else "OpenAI API Key not provided")
  }
  frag_label <- if (lang == "es") "Fragmento" else "Fragment"
  code_label <- if (lang == "es") "C\u00f3digo" else "Code"
  fragmentos_texto <- paste(
    sapply(seq_along(fragmentos), function(i) {
      paste0(frag_label, " ", i, " [", code_label, ": ", codigos[i], "]: \"", fragmentos[i], "\"")
    }),
    collapse = "\n"
  )
  if (lang == "es") {
    prompt <- paste0(
      "Act\u00faa como un panel de 3 expertos en an\u00e1lisis cualitativo. Eval\u00faa si los siguientes fragmentos est\u00e1n correctamente codificados:\n\n",
      fragmentos_texto,
      "\n\nPara cada fragmento, proporciona:\n",
      "1. Evaluaci\u00f3n (Correcto/Revisar/Incorrecto)\n",
      "2. Justificaci\u00f3n breve\n",
      "3. C\u00f3digo alternativo sugerido (si aplica)\n\n",
      "Responde en formato estructurado."
    )
  } else {
    prompt <- paste0(
      "Act as a panel of 3 qualitative analysis experts. Evaluate whether the following fragments are correctly coded:\n\n",
      fragmentos_texto,
      "\n\nFor each fragment, provide:\n",
      "1. Evaluation (Correct/Review/Incorrect)\n",
      "2. Brief justification\n",
      "3. Suggested alternative code (if applicable)\n\n",
      "Respond in structured format."
    )
  }
  resultado <- call_openai_api(prompt, api_key)
  if (is.null(resultado) || !nzchar(resultado)) {
    stop(if (lang == "es") "No se recibi\u00f3 respuesta de OpenAI" else "No response received from OpenAI")
  }
  return(resultado)
}
