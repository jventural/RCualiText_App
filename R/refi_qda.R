# R/refi_qda.R
# REFI-QDA (qdpx) interchange format export
# Standard: https://www.qdasoftware.org/

#' @title Export project to REFI-QDA XML format
#' @description Exports the current project to a REFI-QDA compatible XML file for interoperability with ATLAS.ti, NVivo, MAXQDA.
#' @param codigosDF Tibble of codes.
#' @param tabla Tibble of highlights.
#' @param docs List of documents (each with `name` and `original`).
#' @param file Output file path (should end in .qde).
#' @return Invisibly returns NULL.
export_refi_qda <- function(codigosDF, tabla, docs, file) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' required for REFI-QDA export")
  }

  gen_guid <- function() {
    hex <- function(n) paste0(sample(c(0:9, letters[1:6]), n, replace = TRUE), collapse = "")
    paste0(hex(8), "-", hex(4), "-", hex(4), "-", hex(4), "-", hex(12))
  }

  # Build XML document
  project <- xml2::xml_new_root(
    "Project",
    xmlns = "urn:QDA-XML:project:1.0",
    name = "RCualiText Project",
    creatingUserGUID = gen_guid(),
    creationDateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )

  # Code book
  codebook <- xml2::xml_add_child(project, "CodeBook")
  codes_node <- xml2::xml_add_child(codebook, "Codes")
  code_guids <- list()
  for (i in seq_len(nrow(codigosDF))) {
    guid <- gen_guid()
    code_guids[[codigosDF$Codigo[i]]] <- guid
    xml2::xml_add_child(codes_node, "Code",
      guid = guid,
      name = codigosDF$Codigo[i],
      isCodable = "true",
      color = codigosDF$Color[i]
    )
  }

  # Sources (documents)
  sources <- xml2::xml_add_child(project, "Sources")
  doc_guids <- list()
  for (i in seq_along(docs)) {
    guid <- gen_guid()
    doc_guids[[docs[[i]]$name]] <- guid
    txtSource <- xml2::xml_add_child(sources, "TextSource",
      guid = guid,
      name = docs[[i]]$name,
      plainTextPath = paste0("sources/", docs[[i]]$name)
    )
    content <- xml2::xml_add_child(txtSource, "PlainTextContent")
    xml2::xml_set_text(content, docs[[i]]$original)

    # Add coded selections for this document
    doc_highlights <- tabla[tabla$Archivo == docs[[i]]$name, ]
    for (h in seq_len(nrow(doc_highlights))) {
      pos_start <- regexpr(doc_highlights$Extracto[h], docs[[i]]$original, fixed = TRUE)
      if (pos_start > 0) {
        pos_end <- pos_start + nchar(doc_highlights$Extracto[h]) - 1
        selection <- xml2::xml_add_child(txtSource, "PlainTextSelection",
          guid = gen_guid(),
          name = stringr::str_trunc(doc_highlights$Extracto[h], 50),
          startPosition = as.character(pos_start),
          endPosition = as.character(pos_end)
        )
        code_guid <- code_guids[[doc_highlights$Codigo[h]]]
        if (!is.null(code_guid)) {
          xml2::xml_add_child(selection, "Coding", guid = gen_guid(),
            xml2::xml_add_child(xml2::xml_new_root("CodeRef"), "CodeRef", targetGUID = code_guid))
        }
      }
    }
  }

  xml2::write_xml(project, file)
  invisible(NULL)
}
