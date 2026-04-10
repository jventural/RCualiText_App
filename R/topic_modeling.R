# R/topic_modeling.R
# LDA topic modeling for qualitative corpora

#' @title Generate LDA topic model
#' @description Performs Latent Dirichlet Allocation topic modeling on a corpus of documents.
#' @param textos Character vector of document texts.
#' @param nombres_docs Character vector of document names (same length as textos).
#' @param n_topics Integer. Number of topics to extract.
#' @param idioma Character. Language for stop words ("en", "es", etc.).
#' @param n_terms Integer. Number of top terms to show per topic (default 10).
#' @param custom_stopwords Optional character vector of additional stop words.
#' @return A list with lda_model, top_terms (tibble), doc_topics (tibble), n_topics.
generar_topics_lda <- function(textos, nombres_docs, n_topics = 5, idioma = "en", n_terms = 10, custom_stopwords = NULL) {
  if (!requireNamespace("topicmodels", quietly = TRUE)) {
    stop("Package 'topicmodels' required")
  }
  if (!requireNamespace("tm", quietly = TRUE) && !requireNamespace("tidytext", quietly = TRUE)) {
    stop("Package 'tm' or 'tidytext' required")
  }

  # Prepare tidytext data
  df <- tibble::tibble(doc_id = nombres_docs, text = textos)
  tokens <- df %>% tidytext::unnest_tokens(word, text)

  # Stop words
  sw <- tryCatch({
    tibble::tibble(word = stopwords::stopwords(idioma, source = "snowball"))
  }, error = function(e) {
    tibble::tibble(word = stopwords::stopwords("en", source = "snowball"))
  })
  if (!is.null(custom_stopwords)) {
    sw <- dplyr::bind_rows(sw, tibble::tibble(word = tolower(custom_stopwords)))
  }

  tokens_clean <- tokens %>%
    dplyr::anti_join(sw, by = "word") %>%
    dplyr::filter(nchar(word) > 2)

  # Build DTM
  word_counts <- tokens_clean %>%
    dplyr::count(doc_id, word, sort = TRUE)

  if (nrow(word_counts) == 0) {
    stop("No words remain after filtering stop words")
  }

  dtm <- word_counts %>%
    tidytext::cast_dtm(doc_id, word, n)

  # Fit LDA
  set.seed(2026)
  lda_model <- topicmodels::LDA(dtm, k = n_topics, control = list(seed = 2026))

  # Top terms per topic
  topics <- tidytext::tidy(lda_model, matrix = "beta")
  top_terms <- topics %>%
    dplyr::group_by(topic) %>%
    dplyr::slice_max(beta, n = n_terms) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, dplyr::desc(beta))

  # Document-topic probabilities
  doc_topics <- tidytext::tidy(lda_model, matrix = "gamma") %>%
    dplyr::arrange(document, dplyr::desc(gamma))

  list(
    lda_model = lda_model,
    top_terms = top_terms,
    doc_topics = doc_topics,
    n_topics = n_topics
  )
}

#' @title Plot top terms per LDA topic
#' @description Creates a bar chart showing the most probable terms for each topic.
#' @param lda_result A list returned by `generar_topics_lda`.
#' @return A ggplot2 object.
plot_topics <- function(lda_result) {
  if (is.null(lda_result) || is.null(lda_result$top_terms)) return(NULL)
  top_terms <- lda_result$top_terms

  top_terms %>%
    dplyr::mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
    ggplot2::ggplot(ggplot2::aes(beta, term, fill = factor(topic))) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ topic, scales = "free") +
    tidytext::scale_y_reordered() +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(x = "Beta (probability)", y = NULL) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2")
}
