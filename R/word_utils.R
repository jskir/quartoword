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


csr_text <- extract_word_text("text_document.docx")
head(csr_text, 10)  # Show first 10 paragraphs

# Let's explore what we extracted
length(csr_text)  # How many paragraphs?
csr_text[1:3]     # First few paragraphs

# Extract text from your freshly rendered document
qmd_text <- extract_word_text("templates/simple_csr.docx")

# See what we extracted
head(qmd_text, 10)


#' Parse a QMD file into components
#' @param qmd_path Path to QMD file
#' @return List with text blocks and their locations
parse_qmd_structure <- function(qmd_path) {
  # Read the entire file
  lines <- readLines(qmd_path)

  # Initialize tracking
  components <- list()
  in_code_chunk <- FALSE
  in_yaml <- FALSE
  current_text <- character()
  text_start <- NULL

  # seq_along creates a sequence of integers of the length
  # of the argument. Preferred method because it handles zero
  for (i in seq_along(lines)) {
    line <- lines[i]

    # Check for YAML boundaries
    if (line == "---") {
      # If detected switch the yaml flag T/F
      in_yaml <- !in_yaml
      next
    }

    # Check for code chunk boundaries
    # grepl searches for a pattern match within each element of a character vector
    if (grepl("^```", line)) {
      # If detected, switch the code chunk flag T/F
      in_code_chunk <- !in_code_chunk
      next
    }
    #nzchar returns T/F for empty strings (empty =false)
    #trimws trims white space

    # If we're in regular text (not YAML, not code), and the line is not empty
    if (!in_yaml && !in_code_chunk && nzchar(trimws(line))) {
      # text_start was initialized as null
      if (is.null(text_start)) {
        text_start <- i
      }
      current_text <- c(current_text, line)
    } else if (length(current_text) > 0) {
      # End of a text block
      components[[length(components) + 1]] <- list(
        type = "text",
        content = current_text,
        start_line = text_start,
        end_line = i - 1
      )
      current_text <- character()
      text_start <- NULL
    }
  }

  # Handle final text block
  if (length(current_text) > 0) {
    components[[length(components) + 1]] <- list(
      type = "text",
      content = current_text,
      start_line = text_start,
      end_line = length(lines)
    )
  }

  return(components)
}

# Parse your QMD structure
qmd_structure <- parse_qmd_structure("templates/simple_csr.qmd")
str(qmd_structure)

#' Align Word document text with QMD structure
#' @param word_text Character vector from extract_word_text()
#' @param qmd_structure List from parse_qmd_structure()
#' @return List of alignments between Word and QMD
align_word_to_qmd <- function(word_text, qmd_structure) {
  alignments <- list()
  word_idx <- 1

  for (i in seq_along(qmd_structure)) {
    if (qmd_structure[[i]]$type == "text") {
      qmd_text <- qmd_structure[[i]]$content

      # Try to match QMD text blocks with Word paragraphs
      # We'll use a simple approach: consecutive matching
      matched_word_text <- character()
      match_start <- word_idx

      for (qmd_line in qmd_text) {
        if (word_idx <= length(word_text)) {
          # Simple matching - we'll improve this
          matched_word_text <- c(matched_word_text, word_text[word_idx])
          word_idx <- word_idx + 1
        }
      }

      alignments[[i]] <- list(
        qmd_block = i,
        qmd_lines = qmd_structure[[i]]$start_line:qmd_structure[[i]]$end_line,
        original_text = qmd_text,
        revised_text = matched_word_text
      )
    }
  }

  return(alignments)
}
