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

#' Clean text for better alignment matching
#' @param text Character vector to clean
#' @return Cleaned character vector
clean_text_for_matching <- function(text) {
  # Combine all text into one string for processing
  # paste() combines character vectors into single strings
  # collapse = " " means join elements with spaces
  clean_text <- paste(text, collapse = " ")

  # Remove markdown headers (# ## ###)
  # gsub() does find-and-replace with regular expressions
  # "^#+\\s*" means: start of line (^), one or more # (+), optional whitespace (\\s*)
  clean_text <- gsub("^#+\\s*", "", clean_text)

  # Remove common Quarto parameter patterns
  # These are the "predictable finite set of characters" you mentioned

  # Remove inline code: `code`
  # "`[^`]*`" means: backtick, any chars except backtick, backtick
  clean_text <- gsub("`[^`]*`", "", clean_text)

  # Remove parameter references: {{< param name >}}
  # "\\{\\{<.*?>\\}\\}" means: literal {{< anything >}}
  clean_text <- gsub("\\{\\{<.*?>\\}\\}", "", clean_text)

  # Remove R inline code: `r code`
  # "`r\\s+[^`]*`" means: `r followed by whitespace and anything until closing `
  clean_text <- gsub("`r\\s+[^`]*`", "", clean_text)

  # Remove cross-references: @ref(fig:name) or @tbl-name
  # Fixed the character class - put hyphen at end to avoid range issues
  # "@[a-zA-Z:()0-9-]*" means: @ followed by letters, colons, parens, numbers, hyphens
  clean_text <- gsub("@[a-zA-Z:()0-9-]*", "", clean_text)

  # Remove extra whitespace
  # "\\s+" means one or more whitespace characters
  # trimws() removes leading and trailing whitespace
  clean_text <- trimws(gsub("\\s+", " ", clean_text))

  return(clean_text)
}

#' Improved alignment using text cleaning
#' @param word_text Character vector from extract_word_text()
#' @param qmd_structure List from parse_qmd_structure()
#' @return List of alignments between Word and QMD
align_word_to_qmd_clean <- function(word_text, qmd_structure) {
  alignments <- list()

  # Get only the text blocks from QMD structure
  # sapply() applies a function to each element of a list
  # Here we're checking if each element has type "text"
  text_blocks <- qmd_structure[sapply(qmd_structure, function(x) x$type == "text")]

  for (i in seq_along(text_blocks)) {
    qmd_block <- text_blocks[[i]]

    # Clean the QMD content for better matching
    qmd_content_clean <- clean_text_for_matching(qmd_block$content)

    # Calculate similarity with each Word paragraph (also cleaned)
    similarities <- sapply(word_text, function(word_para) {
      word_content_clean <- clean_text_for_matching(word_para)

      # Convert both to lowercase and split into words
      # strsplit() splits strings based on patterns
      # "\\s+" means split on any whitespace
      # [[1]] gets the first (and only) element from the list result
      qmd_words <- tolower(strsplit(qmd_content_clean, "\\s+")[[1]])
      word_words <- tolower(strsplit(word_content_clean, "\\s+")[[1]])

      # Count shared words
      # %in% checks if elements of left vector are in right vector
      # sum() counts how many TRUE values we get
      shared_words <- sum(qmd_words %in% word_words)

      # unique() removes duplicates, c() combines vectors
      total_words <- length(unique(c(qmd_words, word_words)))

      # Return similarity score (proportion of shared words)
      if (total_words > 0) shared_words / total_words else 0
    })

    # which.max() returns the index of the maximum value
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


# Test the cleaned alignment
clean_alignments <- align_word_to_qmd_clean(edited_text, qmd_structure)

# Compare results
for (i in seq_along(clean_alignments)) {
  cat("Block", i, ":\n")
  cat("Original:", clean_alignments[[i]]$original_text, "\n")
  cat("Revised:", clean_alignments[[i]]$revised_text, "\n")
  cat("Score:", clean_alignments[[i]]$similarity_score, "\n\n")
}



#' Update QMD file with revised text from Word document
#' @param qmd_path Path to original QMD file
#' @param alignments List from align_word_to_qmd_clean()
#' @return TRUE if successful, FALSE otherwise
update_qmd_with_revisions <- function(qmd_path, alignments) {
  # Read the entire original QMD file
  # readLines() reads a file and returns each line as a character vector element
  original_lines <- readLines(qmd_path)

  # Create a copy that we'll modify
  # We work on a copy so if something goes wrong, original is preserved
  updated_lines <- original_lines

  # Process each alignment to update the corresponding lines
  for (alignment in alignments) {
    # Get the line numbers this text block spans in the QMD
    line_numbers <- alignment$qmd_lines

    # Check if the text actually changed (to avoid unnecessary updates)
    original_text_clean <- clean_text_for_matching(alignment$original_text)
    revised_text_clean <- clean_text_for_matching(alignment$revised_text)

    # Only update if text actually changed
    # This prevents overwriting unchanged sections
    if (original_text_clean != revised_text_clean) {

      cat("Updating lines", min(line_numbers), "to", max(line_numbers), "\n")
      cat("Original:", alignment$original_text, "\n")
      cat("Revised:", alignment$revised_text, "\n\n")

      # For now, simple replacement: replace the original line(s) with revised text
      # This assumes each alignment maps to one line (which works for our simple case)
      if (length(line_numbers) == 1) {
        # Single line replacement
        # Extract just the text content, preserving any markdown formatting if needed
        updated_lines[line_numbers] <- alignment$revised_text
      } else {
        # Multiple lines - replace first line, remove others
        # This handles cases where original had multiple lines but revised is single paragraph
        updated_lines[line_numbers[1]] <- alignment$revised_text

        # Mark other lines for removal by setting to empty
        # We'll clean these up later
        if (length(line_numbers) > 1) {
          updated_lines[line_numbers[-1]] <- ""
        }
      }
    }
  }

  # Remove any empty lines we created
  # updated_lines != "" creates a logical vector (TRUE/FALSE)
  # We use this to subset and keep only non-empty lines
  updated_lines <- updated_lines[updated_lines != ""]

  # Write the updated content back to the file
  # writeLines() writes each element of the character vector as a separate line
  writeLines(updated_lines, qmd_path)

  cat("Successfully updated", qmd_path, "\n")
  return(TRUE)
}

# IMPORTANT: Make a backup first!
file.copy("templates/simple_csr.qmd", "templates/simple_csr_backup.qmd")

# Update the QMD with your revisions
update_qmd_with_revisions("templates/simple_csr.qmd", clean_alignments)

# Check what changed - open the file and look for your "REVISED" text
readLines("templates/simple_csr.qmd")


#' Complete round-trip: QMD -> Word edits -> Updated QMD
#' @param qmd_path Path to QMD file
#' @param word_path Path to edited Word document
#' @param backup Should we create a backup? (default TRUE)
#' @return TRUE if successful
quartoword_update <- function(qmd_path, word_path, backup = TRUE) {

  # Safety first - create backup of original QMD
  if (backup) {
    backup_path <- paste0(qmd_path, ".backup")
    file.copy(qmd_path, backup_path, overwrite = TRUE)
    cat("Created backup:", backup_path, "\n")
  }

  cat("🔄 Starting quartoword round-trip update...\n\n")

  # Step 1: Parse the original QMD structure
  cat("📖 Parsing QMD structure...\n")
  qmd_structure <- parse_qmd_structure(qmd_path)
  cat("Found", length(qmd_structure), "components\n\n")

  # Step 2: Extract text from edited Word document
  cat("📄 Extracting Word document text...\n")
  word_text <- extract_word_text(word_path)
  cat("Extracted", length(word_text), "paragraphs\n\n")

  # Step 3: Align Word text with QMD structure
  cat("🎯 Aligning Word text with QMD structure...\n")
  alignments <- align_word_to_qmd_clean(word_text, qmd_structure)
  cat("Created", length(alignments), "alignments\n\n")

  # Step 4: Update the QMD file
  cat("✏️  Updating QMD file with revisions...\n")
  update_qmd_with_revisions(qmd_path, alignments)

  cat("✅ Round-trip complete! Your QMD file has been updated.\n")
  cat("💡 You can now re-render to see changes with fresh data.\n")

  return(TRUE)
}

# Test the complete workflow
quartoword_update("templates/simple_csr.qmd", "templates/simple_csr.docx")

# Now re-render to see your changes
system("quarto render templates/simple_csr.qmd")


# Restore from backup
file.copy("templates/simple_csr.qmd.backup", "templates/simple_csr.qmd", overwrite = TRUE)

# Let's debug the parser - see what it's actually finding
qmd_structure <- parse_qmd_structure("templates/simple_csr.qmd")
cat("Number of components found:", length(qmd_structure), "\n")

# Look at what it parsed
str(qmd_structure)

# Also let's look at the raw file
cat("Raw QMD file:\n")
readLines("templates/simple_csr.qmd")


#' Parse QMD file recognizing headers as section boundaries
#' @param qmd_path Path to QMD file
#' @return List with text blocks and their locations
parse_qmd_structure_v2 <- function(qmd_path) {
  # Read the entire file
  lines <- readLines(qmd_path)

  components <- list()
  in_yaml <- FALSE
  in_code_chunk <- FALSE

  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]

    # Check for YAML boundaries
    if (line == "---") {
      in_yaml <- !in_yaml
      i <- i + 1
      next
    }

    # Skip YAML content
    if (in_yaml) {
      i <- i + 1
      next
    }

    # Check for code chunk boundaries
    if (grepl("^```", line)) {
      in_code_chunk <- !in_code_chunk
      i <- i + 1
      next
    }

    # Skip code chunks
    if (in_code_chunk) {
      i <- i + 1
      next
    }

    # Check if this line is a header (starts with #)
    if (grepl("^#+\\s+", line)) {
      # This is a header - start a new section
      section_content <- character()
      section_start <- i

      # Add the header line
      section_content <- c(section_content, line)
      i <- i + 1

      # Collect content until we hit the next header or end of file
      while (i <= length(lines) && !grepl("^#+\\s+", lines[i]) && !grepl("^```", lines[i])) {
        # Skip empty lines but include content lines
        if (nzchar(trimws(lines[i]))) {
          section_content <- c(section_content, lines[i])
        }
        i <- i + 1
      }

      # Add this section to components
      components[[length(components) + 1]] <- list(
        type = "text",
        content = section_content,
        start_line = section_start,
        end_line = i - 1
      )
    } else {
      # Not a header, skip this line (orphaned content)
      i <- i + 1
    }
  }

  return(components)
}

# Test the new parser
qmd_structure_v2 <- parse_qmd_structure_v2("templates/simple_csr.qmd")
cat("Number of components found:", length(qmd_structure_v2), "\n")

# Look at what it found
for (i in seq_along(qmd_structure_v2)) {
  cat("\nSection", i, ":\n")
  cat("Lines:", qmd_structure_v2[[i]]$start_line, "to", qmd_structure_v2[[i]]$end_line, "\n")
  cat("Content:", paste(qmd_structure_v2[[i]]$content, collapse = " | "), "\n")
}
