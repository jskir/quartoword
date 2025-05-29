#' quartoword: Round-trip editing between Quarto documents and Microsoft Word
#'
#' This file contains the core functions for enabling collaborative review workflows
#' where teams can edit Quarto-generated Word documents and have changes flow back
#' to the original QMD source files.
#'
#' Code by Claude AI Sonnet 4

# Core Functions ----------------------------------------------------------------

#' Extract text from Word document preserving structure
#' @param docx_path Path to Word document (.docx)
#' @return Character vector of paragraphs (including empty ones for structure)
extract_word_text <- function(docx_path) {
  # Check for required package
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("officer package is required. Install with: install.packages('officer')")
  }

  # Read the Word document using officer package
  doc <- officer::read_docx(docx_path)

  # Extract document content summary
  # This gives us a data frame with content type and text for each element
  content <- officer::docx_summary(doc)

  # Filter for paragraph content only (ignoring tables, images, etc.)
  paragraphs <- content[content$content_type == "paragraph", "text"]

  # Convert NA values to empty strings but preserve them for structure
  # Empty paragraphs in Word often represent spacing/formatting
  paragraphs[is.na(paragraphs)] <- ""

  return(paragraphs)
}

#' Parse QMD file into structural components
#' @param qmd_path Path to QMD file
#' @return List of components with type, content, and line positions
parse_qmd_structure <- function(qmd_path) {
  # Read all lines from the QMD file
  lines <- readLines(qmd_path)

  # Initialize tracking variables
  components <- list()
  in_yaml <- FALSE      # Track if we're inside YAML frontmatter
  in_code_chunk <- FALSE # Track if we're inside code blocks

  # Process each line
  for (i in seq_along(lines)) {
    line <- lines[i]

    # Handle YAML frontmatter boundaries (--- at start/end)
    if (line == "---") {
      in_yaml <- !in_yaml  # Toggle YAML state
      next  # Skip processing this line
    }

    # Skip YAML content and code chunks
    if (in_yaml) {
      next
    }

    # Handle code chunk boundaries (``` at start of line)
    if (grepl("^```", line)) {
      in_code_chunk <- !in_code_chunk
      next
    }

    # Skip code chunk content
    if (in_code_chunk) {
      next
    }

    # Classify and store each content line
    if (!nzchar(trimws(line))) {
      # Empty line - preserve for proper Word rendering
      components[[length(components) + 1]] <- list(
        type = "blank",
        content = "",
        start_line = i,
        end_line = i
      )
    } else if (grepl("^#+\\s+", line)) {
      # Markdown header (starts with # ## ### etc.)
      components[[length(components) + 1]] <- list(
        type = "header",
        content = line,
        start_line = i,
        end_line = i
      )
    } else {
      # Regular text content
      components[[length(components) + 1]] <- list(
        type = "text",
        content = line,
        start_line = i,
        end_line = i
      )
    }
  }

  return(components)
}

#' Clean text for better similarity matching
#' @param text Character vector to clean
#' @return Cleaned text with markdown and parameters removed
clean_text_for_matching <- function(text) {
  # Combine multiple lines into single string
  clean_text <- paste(text, collapse = " ")

  # Remove markdown headers (# ## ###)
  clean_text <- gsub("^#+\\s*", "", clean_text)

  # Remove common Quarto/R Markdown patterns that interfere with matching:
  # - Inline code: `code`
  clean_text <- gsub("`[^`]*`", "", clean_text)
  # - Parameter references: {{< param name >}}
  clean_text <- gsub("\\{\\{<.*?>\\}\\}", "", clean_text)
  # - R inline code: `r code`
  clean_text <- gsub("`r\\s+[^`]*`", "", clean_text)
  # - Cross-references: @ref(fig:name) or @tbl-name
  clean_text <- gsub("@[a-zA-Z:()0-9-]*", "", clean_text)

  # Clean up extra whitespace
  clean_text <- trimws(gsub("\\s+", " ", clean_text))

  return(clean_text)
}

#' Align Word document paragraphs with QMD components
#' @param word_text Character vector from extract_word_text()
#' @param qmd_structure List from parse_qmd_structure()
#' @return List of alignments between Word paragraphs and QMD components
align_word_to_qmd <- function(word_text, qmd_structure) {
  alignments <- list()

  # Only align content components (skip blank lines)
  content_blocks <- qmd_structure[sapply(qmd_structure, function(x) x$type %in% c("text", "header"))]

  for (i in seq_along(content_blocks)) {
    qmd_block <- content_blocks[[i]]

    # Clean both QMD and Word text for comparison
    qmd_content_clean <- clean_text_for_matching(qmd_block$content)

    # Calculate similarity with each Word paragraph
    similarities <- sapply(word_text, function(word_para) {
      word_content_clean <- clean_text_for_matching(word_para)

      # Split into words for comparison
      qmd_words <- tolower(strsplit(qmd_content_clean, "\\s+")[[1]])
      word_words <- tolower(strsplit(word_content_clean, "\\s+")[[1]])

      # Calculate similarity as proportion of QMD words found in Word text
      shared_words <- sum(qmd_words %in% word_words)
      total_qmd_words <- length(qmd_words)

      # Return similarity score (0 to 1)
      if (total_qmd_words > 0) shared_words / total_qmd_words else 0
    })

    # Find the best matching Word paragraph
    best_match_idx <- which.max(similarities)

    # Store the alignment
    alignments[[i]] <- list(
      qmd_block = i,
      qmd_type = qmd_block$type,
      qmd_lines = qmd_block$start_line:qmd_block$end_line,
      original_text = qmd_block$content,
      revised_text = word_text[best_match_idx],
      similarity_score = similarities[best_match_idx]
    )
  }

  return(alignments)
}

#' Update QMD file with revised text from alignments
#' @param qmd_path Path to original QMD file
#' @param alignments List from align_word_to_qmd()
#' @return TRUE if successful
update_qmd_with_revisions <- function(qmd_path, alignments) {
  # Read original file
  original_lines <- readLines(qmd_path)
  updated_lines <- original_lines  # Work on a copy

  # Apply each revision
  for (alignment in alignments) {
    # Only update if text actually changed
    original_clean <- clean_text_for_matching(alignment$original_text)
    revised_clean <- clean_text_for_matching(alignment$revised_text)

    if (original_clean != revised_clean) {
      line_number <- alignment$qmd_lines[1]  # Get the line to update

      cat("Updating line", line_number, "\n")
      cat("  Original:", alignment$original_text, "\n")
      cat("  Revised: ", alignment$revised_text, "\n\n")

      # Replace the content while preserving line structure
      updated_lines[line_number] <- alignment$revised_text
    }
  }

  # Write updated content back to file
  writeLines(updated_lines, qmd_path)
  cat("Successfully updated", qmd_path, "\n")

  return(TRUE)
}

# Main Workflow Function --------------------------------------------------------

#' Complete round-trip: QMD -> Word edits -> Updated QMD
#' @param qmd_path Path to QMD file
#' @param word_path Path to edited Word document
#' @param backup Should we create a backup? (default TRUE)
#' @return TRUE if successful
quartoword_update <- function(qmd_path, word_path, backup = TRUE) {

  # Create backup for safety
  if (backup) {
    backup_path <- paste0(qmd_path, ".backup")
    file.copy(qmd_path, backup_path, overwrite = TRUE)
    cat("✅ Created backup:", backup_path, "\n")
  }

  cat("\n🔄 Starting quartoword round-trip update...\n\n")

  # Step 1: Parse QMD structure
  cat("📖 Parsing QMD structure...\n")
  qmd_structure <- parse_qmd_structure(qmd_path)
  cat("   Found", length(qmd_structure), "components\n\n")

  # Step 2: Extract Word text
  cat("📄 Extracting Word document text...\n")
  word_text <- extract_word_text(word_path)
  cat("   Extracted", length(word_text), "paragraphs\n\n")

  # Step 3: Align structures
  cat("🎯 Aligning Word text with QMD structure...\n")
  alignments <- align_word_to_qmd(word_text, qmd_structure)
  cat("   Created", length(alignments), "alignments\n\n")

  # Step 4: Update QMD
  cat("✏️  Updating QMD file with revisions...\n")
  update_qmd_with_revisions(qmd_path, alignments)

  cat("\n🎉 Round-trip complete!\n")
  cat("💡 You can now re-render to incorporate changes with fresh data.\n\n")

  return(TRUE)
}

# Utility Functions -------------------------------------------------------------

#' Create a properly formatted QMD template for testing
#' @param file_path Where to save the template
#' @return TRUE if successful
create_test_qmd <- function(file_path = "test_csr.qmd") {
  template_content <- c(
    "---",
    "title: \"Clinical Study Report\"",
    "author: \"Study Team\"",
    "format: docx",
    "---",
    "",
    "# Executive Summary",
    "",
    "This is the executive summary section.",
    "",
    "# Study Design",
    "",
    "This section describes the study design and methodology.",
    "",
    "# Results",
    "",
    "## Primary Endpoint",
    "",
    "Results of the primary endpoint analysis will go here.",
    "",
    "## Secondary Endpoints",
    "",
    "Secondary endpoint results will be presented in this section.",
    "",
    "# Conclusions",
    "",
    "Study conclusions and interpretation."
  )

  writeLines(template_content, file_path)
  cat("Created test QMD template:", file_path, "\n")

  return(TRUE)
}
