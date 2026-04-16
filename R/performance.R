# ========================================
# R/performance.R
# SQLite embeddings cache + text2vec KWIC index + lazy-loading docs
# ========================================

perf_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "performance",
    fluidRow(
      box(
        width = 4, title = "Embedding cache (SQLite)", status = "primary",
        solidHeader = TRUE,
        textInput("perf_db_path", "SQLite path",
                  value = file.path(tempdir(), "rcualitext_cache.sqlite")),
        actionButton("perf_init_db", "Init / open DB", icon = icon("database"),
                     class = "btn-primary"),
        hr(),
        verbatimTextOutput("perf_db_stats"),
        actionButton("perf_clear_cache", "Clear cache",
                     icon = icon("broom"), class = "btn-warning")
      ),
      box(
        width = 4, title = "text2vec KWIC index", status = "success",
        solidHeader = TRUE,
        actionButton("perf_build_index", "(Re)build index",
                     icon = icon("cogs"), class = "btn-primary"),
        hr(),
        verbatimTextOutput("perf_index_stats"),
        textInput("perf_kwic_term", "Quick search"),
        actionButton("perf_kwic_run", "Search", icon = icon("search")),
        DT::DTOutput("perf_kwic_results")
      ),
      box(
        width = 4, title = "Lazy loading", status = "info", solidHeader = TRUE,
        helpText("Large corpora (>50 docs): load on demand. Active doc stays in memory; others loaded when selected."),
        checkboxInput("perf_lazy_on", "Enable lazy loading", value = FALSE),
        numericInput("perf_lazy_threshold",
                     "Threshold (docs)", value = 50, min = 5, max = 500),
        verbatimTextOutput("perf_lazy_status")
      )
    )
  )
}

# --- SQLite embedding cache helpers ---
perf_hash <- function(txt) digest::digest(txt, algo = "sha1")

perf_init_sqlite <- function(path) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Install DBI and RSQLite: install.packages(c('DBI','RSQLite'))")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS embeddings (
    hash TEXT PRIMARY KEY, model TEXT,
    vector BLOB, created TEXT)")
  con
}

perf_cache_get <- function(con, txt, model = "text-embedding-3-small") {
  if (is.null(con)) return(NULL)
  h <- perf_hash(paste0(model, "|", txt))
  r <- DBI::dbGetQuery(con, "SELECT vector FROM embeddings WHERE hash = ?",
                       params = list(h))
  if (nrow(r) == 0) return(NULL)
  unserialize(r$vector[[1]])
}

perf_cache_put <- function(con, txt, vec,
                           model = "text-embedding-3-small") {
  if (is.null(con)) return(invisible())
  h <- perf_hash(paste0(model, "|", txt))
  DBI::dbExecute(con,
                 "INSERT OR REPLACE INTO embeddings VALUES (?, ?, ?, ?)",
                 params = list(h, model, list(serialize(vec, NULL)),
                               format(Sys.time())))
}

perf_cache_stats <- function(con) {
  if (is.null(con)) return("DB not opened")
  r <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM embeddings")
  file_kb <- tryCatch(round(file.info(
    DBI::dbGetInfo(con)$dbname)$size / 1024, 1), error = function(e) NA)
  paste0("Cached embeddings: ", r$n, "\nFile size (KB): ", file_kb)
}

# --- text2vec index ---
perf_build_kwic_index <- function(docs) {
  if (!requireNamespace("text2vec", quietly = TRUE)) {
    stop("Install text2vec: install.packages('text2vec')")
  }
  if (is.null(docs) || length(docs) == 0) return(NULL)
  texts <- sapply(docs, function(d) d$text)
  names(texts) <- sapply(docs, function(d) d$name)
  tok <- text2vec::word_tokenizer(tolower(texts))
  it <- text2vec::itoken(tok, ids = names(texts), progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(it)
  list(texts = texts, tok = tok, vocab = vocab)
}

perf_kwic_search <- function(idx, term, window = 50) {
  if (is.null(idx)) return(NULL)
  term <- tolower(term)
  hits <- list()
  for (nm in names(idx$texts)) {
    txt <- idx$texts[[nm]]
    m <- gregexpr(paste0("\\b", term, "\\b"), tolower(txt))
    if (m[[1]][1] == -1) next
    for (pos in m[[1]]) {
      a <- max(1, pos - window); b <- min(nchar(txt), pos + nchar(term) + window)
      hits[[length(hits) + 1]] <- tibble::tibble(
        Archivo = nm, Pos = pos,
        Contexto = substring(txt, a, b)
      )
    }
  }
  if (length(hits) == 0) return(tibble::tibble(Archivo = character(),
                                               Pos = integer(),
                                               Contexto = character()))
  dplyr::bind_rows(hits)
}

setup_performance_server <- function(input, output, session, rv) {
  perf_rv <- reactiveValues(con = NULL, kwic_idx = NULL, results = NULL)
  # expose cache on rv so other modules can use it
  rv$embed_cache_con <- NULL

  observeEvent(input$perf_init_db, {
    tryCatch({
      perf_rv$con <- perf_init_sqlite(input$perf_db_path)
      rv$embed_cache_con <- perf_rv$con
      showNotification("SQLite cache ready", type = "message")
    }, error = function(e) {
      showNotification(paste("DB error:", conditionMessage(e)),
                       type = "error")
    })
  })

  output$perf_db_stats <- renderText({
    perf_cache_stats(perf_rv$con)
  })

  observeEvent(input$perf_clear_cache, {
    req(perf_rv$con)
    DBI::dbExecute(perf_rv$con, "DELETE FROM embeddings")
    showNotification("Cache cleared", type = "message")
  })

  observeEvent(input$perf_build_index, {
    tryCatch({
      perf_rv$kwic_idx <- perf_build_kwic_index(rv$docs)
      rv$kwic_index <- perf_rv$kwic_idx
      showNotification("Index built", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  output$perf_index_stats <- renderText({
    if (is.null(perf_rv$kwic_idx)) return("Not built")
    paste0("Docs indexed: ", length(perf_rv$kwic_idx$texts),
           "\nVocab size: ", nrow(perf_rv$kwic_idx$vocab))
  })

  observeEvent(input$perf_kwic_run, {
    req(perf_rv$kwic_idx, input$perf_kwic_term)
    perf_rv$results <- perf_kwic_search(perf_rv$kwic_idx, input$perf_kwic_term)
  })
  output$perf_kwic_results <- DT::renderDT({
    req(perf_rv$results)
    DT::datatable(perf_rv$results, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$perf_lazy_status <- renderText({
    if (!isTRUE(input$perf_lazy_on)) return("Lazy loading: OFF")
    n <- if (!is.null(rv$docs)) length(rv$docs) else 0
    th <- input$perf_lazy_threshold %||% 50
    paste0("Lazy loading: ON\nDocs: ", n, " (threshold ", th, ")")
  })
}
