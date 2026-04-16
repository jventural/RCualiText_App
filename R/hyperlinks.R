# ========================================
# R/hyperlinks.R
# Bidirectional links: code <-> memo <-> quote <-> document
# ========================================

hyperlinks_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "hyperlinks",
    fluidRow(
      box(
        width = 4, title = "Create link", status = "primary", solidHeader = TRUE,
        selectInput("hl_from_type", "From (type)",
                    choices = c("code", "memo", "quote", "document")),
        selectInput("hl_from_id", "From (id)", choices = NULL),
        selectInput("hl_to_type", "To (type)",
                    choices = c("code", "memo", "quote", "document")),
        selectInput("hl_to_id", "To (id)", choices = NULL),
        textAreaInput("hl_note", "Note", ""),
        actionButton("hl_add", "Link", icon = icon("link"),
                     class = "btn-primary"),
        hr(),
        actionButton("hl_delete", "Delete selected link",
                     icon = icon("trash"), class = "btn-danger")
      ),
      box(
        width = 8, title = "All links", status = "success", solidHeader = TRUE,
        DT::DTOutput("hl_table"),
        hr(),
        h4("Navigate"),
        helpText("Click on a link to jump to the target."),
        uiOutput("hl_navigate_ui")
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Link graph", status = "info", solidHeader = TRUE,
        visNetwork::visNetworkOutput("hl_graph", height = "500px")
      )
    )
  )
}

setup_hyperlinks_server <- function(input, output, session, rv) {
  if (is.null(rv$hyperlinks)) rv$hyperlinks <- tibble::tibble(
    id = character(),
    from_type = character(), from_id = character(),
    to_type = character(), to_id = character(),
    note = character(), created = as.POSIXct(character()))

  # Populate choices dynamically
  get_ids <- function(type) {
    switch(type,
           "code" = if (!is.null(rv$codigosDF)) rv$codigosDF$Codigo else character(),
           "memo" = if (!is.null(rv$memos)) rv$memos$titulo else character(),
           "quote" = if (!is.null(rv$tabla)) rv$tabla$FragmentId else character(),
           "document" = if (!is.null(rv$docs)) sapply(rv$docs, function(d) d$name) else character(),
           character())
  }

  observe({
    updateSelectInput(session, "hl_from_id", choices = get_ids(input$hl_from_type))
  })
  observe({
    updateSelectInput(session, "hl_to_id", choices = get_ids(input$hl_to_type))
  })

  observeEvent(input$hl_add, {
    req(input$hl_from_id, input$hl_to_id)
    new_id <- paste0("L", format(Sys.time(), "%H%M%S"),
                     sample(1000:9999, 1))
    rv$hyperlinks <- dplyr::bind_rows(rv$hyperlinks, tibble::tibble(
      id = new_id,
      from_type = input$hl_from_type, from_id = input$hl_from_id,
      to_type = input$hl_to_type, to_id = input$hl_to_id,
      note = input$hl_note, created = Sys.time()))
  })

  observeEvent(input$hl_delete, {
    sel <- input$hl_table_rows_selected
    if (length(sel) == 0) return()
    rv$hyperlinks <- rv$hyperlinks[-sel, ]
  })

  output$hl_table <- DT::renderDT({
    DT::datatable(rv$hyperlinks, rownames = FALSE, selection = "single",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$hl_navigate_ui <- renderUI({
    req(rv$hyperlinks); if (nrow(rv$hyperlinks) == 0) return(NULL)
    sel <- input$hl_table_rows_selected
    if (length(sel) == 0) return(tags$em("Select a link above"))
    row <- rv$hyperlinks[sel, ]
    tagList(
      tags$p("Jump to:"),
      actionLink("hl_jump_from",
                 paste0(row$from_type, ": ", row$from_id),
                 style = "color: #2980b9; font-weight: bold;"),
      tags$br(),
      actionLink("hl_jump_to",
                 paste0(row$to_type, ": ", row$to_id),
                 style = "color: #2980b9; font-weight: bold;")
    )
  })

  hl_jump <- function(type, id) {
    tab <- switch(type,
                  "code" = "codigos", "memo" = "memos",
                  "quote" = "resaltes", "document" = "texto")
    if (!is.null(tab)) {
      updateTabItems(session, "sidebar", tab)
    }
    showNotification(paste("Navigated to", type, ":", id), type = "message")
  }
  observeEvent(input$hl_jump_from, {
    sel <- input$hl_table_rows_selected; req(sel)
    row <- rv$hyperlinks[sel, ]; hl_jump(row$from_type, row$from_id)
  })
  observeEvent(input$hl_jump_to, {
    sel <- input$hl_table_rows_selected; req(sel)
    row <- rv$hyperlinks[sel, ]; hl_jump(row$to_type, row$to_id)
  })

  output$hl_graph <- visNetwork::renderVisNetwork({
    req(rv$hyperlinks); if (nrow(rv$hyperlinks) == 0) return(NULL)
    nodes <- dplyr::bind_rows(
      rv$hyperlinks %>% dplyr::transmute(
        id = paste0(from_type, ":", from_id),
        label = from_id, group = from_type),
      rv$hyperlinks %>% dplyr::transmute(
        id = paste0(to_type, ":", to_id),
        label = to_id, group = to_type)
    ) %>% dplyr::distinct(id, .keep_all = TRUE)
    edges <- rv$hyperlinks %>% dplyr::transmute(
      from = paste0(from_type, ":", from_id),
      to = paste0(to_type, ":", to_id),
      title = note)
    visNetwork::visNetwork(nodes, edges) %>%
      visNetwork::visLegend() %>%
      visNetwork::visOptions(highlightNearest = TRUE) %>%
      visNetwork::visGroups(groupname = "code", color = "#3498db") %>%
      visNetwork::visGroups(groupname = "memo", color = "#f1c40f") %>%
      visNetwork::visGroups(groupname = "quote", color = "#e67e22") %>%
      visNetwork::visGroups(groupname = "document", color = "#27ae60")
  })
}
