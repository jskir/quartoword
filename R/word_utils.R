#' Test function to read a simple text file
#' @param file_path Path to a text file
#' @return Character vector of file contents
test_read_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  readLines(file_path)
}

#' Extract text from a Word document
#' @param docx_path Path to Word document (.docx)
#' @return Character vector of paragraphs
extract_word_text <- function(docx_path) {
  # Load required library
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("officer package is required. Install with: install.packages('officer')")
  }

  # Read the Word document
  doc <- officer::read_docx(docx_path)

  # Extract all text content
  content <- officer::docx_summary(doc)

  # Filter for paragraph text only
  paragraphs <- content[content$content_type == "paragraph", "text"]

  # Remove empty paragraphs
  paragraphs[paragraphs != ""]
}
