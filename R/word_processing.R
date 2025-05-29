# Functions for Word document processing
#extract_word_text()                   # First function
#analyze_xml_changes()
#analyze_xml_changes_with_context()    MISSING
#What about clean_text_for_matching?  NOT COPIED

# These functions handle Word document processing and basic round-trip editing

#' Extract text content from Word document
#' @param docx_path Path to Word document (.docx)
#' @return Character vector of paragraphs
extract_word_text <- function(docx_path) {
  # Ensure officer package is available
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Please install officer package: install.packages('officer')")
  }

  doc <- officer::read_docx(docx_path)
  content <- officer::docx_summary(doc)
  paragraphs <- content[content$content_type == "paragraph", "text"]
  paragraphs[is.na(paragraphs)] <- ""
  return(paragraphs)
}


# For now, let's add the basic XML change analysis function from your shared code
analyze_xml_changes <- function(docx_path) {
  library(xml2)

  cat("=== XML-LEVEL CHANGE ANALYSIS ===\n")

  # Extract the document XML
  temp_dir <- tempdir()
  unzip(docx_path, exdir = temp_dir)

  # Read the main document XML
  doc_xml <- read_xml(file.path(temp_dir, "word", "document.xml"))

  # Look for tracked change elements
  # Word uses these XML elements for tracked changes:
  # <w:ins> for insertions
  # <w:del> for deletions

  insertions <- xml_find_all(doc_xml, ".//w:ins")
  deletions <- xml_find_all(doc_xml, ".//w:del")

  cat("Found", length(insertions), "insertions\n")
  cat("Found", length(deletions), "deletions\n\n")

  changes <- list()

  if (length(insertions) > 0) {
    cat("INSERTIONS:\n")
    for (i in seq_along(insertions)) {
      inserted_text <- xml_text(insertions[[i]])
      author <- xml_attr(insertions[[i]], "author")
      date <- xml_attr(insertions[[i]], "date")
      cat("  Insert", i, ":", inserted_text, "(by:", author, "on:", date, ")\n")

      changes[[length(changes) + 1]] <- list(
        type = "insertion",
        text = inserted_text,
        author = author,
        date = date
      )
    }
  }

  if (length(deletions) > 0) {
    cat("\nDELETIONS:\n")
    for (i in seq_along(deletions)) {
      deleted_text <- xml_text(deletions[[i]])
      author <- xml_attr(deletions[[i]], "author")
      date <- xml_attr(deletions[[i]], "date")
      cat("  Delete", i, ":", deleted_text, "(by:", author, "on:", date, ")\n")

      changes[[length(changes) + 1]] <- list(
        type = "deletion",
        text = deleted_text,
        author = author,
        date = date
      )
    }
  }

  # Clean up
  unlink(temp_dir, recursive = TRUE)

  return(changes)
}
