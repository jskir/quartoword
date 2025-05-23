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

#' Align Word document text with QMD structure
#' @param word_text Character vector from extract_word_text()
#' @param qmd_structure List from parse_qmd_structure()
#' @return List of alignments between Word and QMD
align_word_to_qmd <- function(word_text, qmd_structure) {
  # Initialize empty list to store our alignment results
  # list() creates an empty list that we can add elements to
  alignments <- list()

  # Keep track of which Word paragraph we're currently looking at
  # We'll increment this as we match paragraphs to QMD blocks
  word_idx <- 1

  # Loop through each component of the QMD structure
  # seq_along() creates a sequence 1, 2, 3... for the length of qmd_structure
  # This is safer than 1:length() because it handles empty lists properly
  for (i in seq_along(qmd_structure)) {

    # Only process text blocks, skip code chunks and YAML
    # The $ operator extracts named elements from lists
    if (qmd_structure[[i]]$type == "text") {

      # Extract the original text content from this QMD block
      # This is a character vector of the original lines
      qmd_text <- qmd_structure[[i]]$content

      # Try to match QMD text blocks with Word paragraphs
      # Strategy: assume Word paragraphs appear in same order as QMD lines

      # character() creates an empty character vector
      # We'll fill this with the corresponding Word text
      matched_word_text <- character()

      # Remember where we started matching in the Word document
      # This helps us track which Word paragraphs correspond to this QMD block
      match_start <- word_idx

      # Loop through each line of text in this QMD block
      # We assume each QMD text line corresponds to one Word paragraph
      for (qmd_line in qmd_text) {

        # Make sure we haven't run out of Word paragraphs
        # length() returns the number of elements in word_text
        if (word_idx <= length(word_text)) {

          # Take the next Word paragraph as the revised version of this QMD line
          # c() combines vectors - here we're adding one more element
          matched_word_text <- c(matched_word_text, word_text[word_idx])

          # Move to the next Word paragraph for the next QMD line
          word_idx <- word_idx + 1
        }
      }

      # Store the alignment information for this QMD block
      # Double brackets [[]] let us assign to list elements
      alignments[[i]] <- list(
        qmd_block = i,  # Which QMD block this is (for reference)

        # Create sequence of line numbers this block spans in the QMD file
        # The : operator creates sequences, so 5:8 gives c(5, 6, 7, 8)
        qmd_lines = qmd_structure[[i]]$start_line:qmd_structure[[i]]$end_line,

        original_text = qmd_text,        # Original QMD text lines
        revised_text = matched_word_text  # Corresponding Word paragraphs (potentially edited)
      )
    }
  }

  # Return the complete list of alignments
  return(alignments)
}



# Test the alignment function
# First, get the structure of your QMD
qmd_structure <- parse_qmd_structure("templates/simple_csr.qmd")

# Get the text from your edited Word document
edited_text <- extract_word_text("templates/simple_csr.docx")

# Now align them
alignments <- align_word_to_qmd(edited_text, qmd_structure)

# Examine what we found
str(alignments)  # str() shows the structure of complex objects


#' Find best matching Word paragraph for each QMD text block
#' @param word_text Character vector from extract_word_text()
#' @param qmd_structure List from parse_qmd_structure()
#' @return List of alignments between Word and QMD
align_word_to_qmd_smart <- function(word_text, qmd_structure) {
  alignments <- list()

  # Get only the text blocks from QMD structure
  text_blocks <- qmd_structure[sapply(qmd_structure, function(x) x$type == "text")]

  for (i in seq_along(text_blocks)) {
    qmd_block <- text_blocks[[i]]

    # For each QMD text block, find the most similar Word paragraph
    qmd_content <- paste(qmd_block$content, collapse = " ")

    # Calculate similarity with each Word paragraph
    # We'll use a simple approach: find paragraphs that share words
    similarities <- sapply(word_text, function(word_para) {
      # Convert both to lowercase and split into words
      qmd_words <- tolower(strsplit(qmd_content, "\\s+")[[1]])
      word_words <- tolower(strsplit(word_para, "\\s+")[[1]])

      # Count shared words (simple similarity measure)
      shared_words <- sum(qmd_words %in% word_words)
      total_words <- length(unique(c(qmd_words, word_words)))

      # Return similarity score (0 to 1)
      if (total_words > 0) shared_words / total_words else 0
    })

    # Find the Word paragraph with highest similarity
    best_match_idx <- which.max(similarities)

    alignments[[i]] <- list(
      qmd_block = i,
      qmd_lines = qmd_block$start_line:qmd_block$end_line,
      original_text = qmd_block$content,
      revised_text = word_text[best_match_idx],
      similarity_score = similarities[best_match_idx]
    )
  }

  return(alignments)
}

# Test the smart alignment
smart_alignments <- align_word_to_qmd_smart(edited_text, qmd_structure)
str(smart_alignments)

# Look for your revision
for (i in seq_along(smart_alignments)) {
  cat("Block", i, ":\n")
  cat("Original:", smart_alignments[[i]]$original_text, "\n")
  cat("Revised:", smart_alignments[[i]]$revised_text, "\n")
  cat("Score:", smart_alignments[[i]]$similarity_score, "\n\n")
}

