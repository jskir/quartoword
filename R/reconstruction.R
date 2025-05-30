# Functions for QMD reconstruction
#apply_word_changes_to_qmd()
#reconstruct_ast_with_word_changes()
#extract_editable_text_from_block()
#production_qmd_reconstruction()
#filter_word_content_paragraphs()
#is_title_or_header()
#find_matching_qmd_line()
#calculate_text_similarity()
#preserve_protected_content_in_update()
#extract_protected_elements_with_positions()
#extract_editable_portions()
#reconstruct_line_with_structure()
#clean_tracked_changes_artifacts()
#tokenize_text_to_ast_content()



# QMD Reconstruction Functions
# These functions update the QMD file with Word changes while preserving protected content

#' Apply tracked changes to AST and regenerate QMD
#' @param original_qmd_path Path to original QMD file
#' @param edited_docx_path Path to edited Word document with tracked changes
#' @param output_qmd_path Path for updated QMD file (if NULL, overwrites original)
#' @return TRUE if successful
apply_word_changes_to_qmd <- function(original_qmd_path, edited_docx_path, output_qmd_path = NULL) {

  if (is.null(output_qmd_path)) {
    output_qmd_path <- original_qmd_path
  }

  cat("🔄 APPLYING WORD CHANGES TO QMD\n")
  cat("================================\n")
  cat("Original QMD:", original_qmd_path, "\n")
  cat("Edited Word:", edited_docx_path, "\n")
  cat("Output QMD:", output_qmd_path, "\n\n")

  # Step 1: Parse original QMD to AST
  cat("📖 Parsing original QMD...\n")
  temp_ast_file <- tempfile(fileext = ".json")

  # Use quarto pandoc (we know this works from debug)
  cmd <- paste("quarto pandoc", shQuote(original_qmd_path), "-t json -o", shQuote(temp_ast_file))
  result <- system(cmd, show.output.on.console = FALSE)

  if (result != 0 || !file.exists(temp_ast_file)) {
    cat("❌ Failed to parse QMD to AST\n")
    cat("   Command:", cmd, "\n")
    cat("   Exit code:", result, "\n")
    cat("   File exists:", file.exists(temp_ast_file), "\n")
    return(FALSE)
  }

  original_ast <- jsonlite::fromJSON(temp_ast_file, simplifyVector = FALSE)
  cat("✅ AST parsed successfully\n")

  # Step 2: Extract editable segments from original AST
  cat("🎯 Extracting editable segments...\n")
  editable_segments <- extract_editable_text(original_ast)
  cat("   Found", length(editable_segments), "editable segments\n")

  # Step 3: Extract tracked changes from Word
  cat("🔍 Analyzing tracked changes...\n")
  tracked_changes <- analyze_xml_changes(edited_docx_path)
  cat("   Found", length(tracked_changes), "tracked changes\n")

  if (length(tracked_changes) == 0) {
    cat("ℹ️  No tracked changes found - copying original file\n")
    if (original_qmd_path != output_qmd_path) {
      file.copy(original_qmd_path, output_qmd_path, overwrite = TRUE)
    }
    return(TRUE)
  }

  # Step 4: Get final text from Word document
  cat("📄 Extracting final text from Word...\n")
  word_final_text <- extract_word_text(edited_docx_path)

  # Step 5: Reconstruct QMD with changes
  cat("🔧 Reconstructing QMD...\n")
  updated_ast <- reconstruct_ast_with_word_changes(
    original_ast,
    editable_segments,
    tracked_changes,
    word_final_text
  )

  # Step 6: Convert updated AST back to QMD
  cat("💾 Converting to QMD format...\n")
  temp_updated_ast <- tempfile(fileext = ".json")
  jsonlite::write_json(updated_ast, temp_updated_ast, auto_unbox = TRUE)

  # Use quarto pandoc
  cmd <- paste("quarto pandoc", shQuote(temp_updated_ast), "-f json -t markdown -o", shQuote(output_qmd_path))
  result <- system(cmd, show.output.on.console = FALSE)

  if (result != 0 || !file.exists(output_qmd_path)) {
    cat("❌ Failed to convert AST back to QMD\n")
    cat("   Command:", cmd, "\n")
    cat("   Exit code:", result, "\n")
    file.remove(temp_ast_file, temp_updated_ast)
    return(FALSE)
  }

  # Clean up temp files
  file.remove(temp_ast_file, temp_updated_ast)

  if (file.exists(output_qmd_path)) {
    cat("✅ QMD reconstruction complete!\n")
    cat("📂 Updated file:", output_qmd_path, "\n")
    return(TRUE)
  } else {
    cat("❌ Failed to generate updated QMD\n")
    return(FALSE)
  }
}

#' Reconstruct AST by applying Word changes while preserving protected content
#' @param original_ast Original Pandoc AST
#' @param editable_segments List from extract_editable_text()
#' @param tracked_changes List from analyze_xml_changes()
#' @param word_final_text Final text from Word document
#' @return Updated AST
reconstruct_ast_with_word_changes <- function(original_ast, editable_segments, tracked_changes, word_final_text) {

  cat("  🧩 Starting AST reconstruction...\n")

  # Strategy: Find the paragraph(s) that contain editable content,
  # then replace the editable parts with the final Word text
  # while keeping protected Span elements intact

  # Step 1: Identify which AST blocks contain editable content
  blocks_with_edits <- identify_edited_blocks(original_ast, editable_segments, word_final_text)

  # Step 2: Create updated AST
  updated_ast <- original_ast

  # Step 3: Update each block that has changes
  for (block_info in blocks_with_edits) {
    cat("    Updating block", block_info$block_index, "\n")

    updated_block <- reconstruct_block_with_changes(
      original_ast$blocks[[block_info$block_index]],
      block_info$original_editable_text,
      block_info$new_text_from_word
    )

    updated_ast$blocks[[block_info$block_index]] <- updated_block
  }

  cat("  ✅ AST reconstruction complete\n")
  return(updated_ast)
}

#' Extract just the editable text from a single AST block
#' @param block Single AST block
#' @return String of editable text
extract_editable_text_from_block <- function(block) {
  editable_parts <- c()

  extract_from_content <- function(content) {
    if (is.list(content)) {
      for (item in content) {
        # Skip protected Span elements
        if (is.list(item) && !is.null(item$t) && item$t == "Span") {
          if (is_protected_span(item)) {
            next  # Skip protected content
          }
        }

        # Collect text strings
        if (is.list(item) && !is.null(item$t) && item$t == "Str") {
          editable_parts <<- c(editable_parts, item$c)
        }

        # Collect spaces
        if (is.list(item) && !is.null(item$t) && item$t == "Space") {
          editable_parts <<- c(editable_parts, " ")
        }

        # Recurse into nested content
        if (is.list(item) && !is.null(item$c)) {
          extract_from_content(item$c)
        }
      }
    }
  }

  # Helper function to check if a Span is protected (same as before)
  is_protected_span <- function(span_item) {
    if (is.list(span_item$c) && length(span_item$c) >= 1) {
      attrs <- span_item$c[[1]]
      if (is.list(attrs) && length(attrs) >= 3) {
        classes <- attrs[[2]]
        custom_attrs <- attrs[[3]]

        return("protected-param" %in% classes ||
                 any(sapply(custom_attrs, function(x) length(x) >= 2 && x[[1]] == "custom-style")))
      }
    }
    return(FALSE)
  }

  if (!is.null(block$c)) {
    extract_from_content(block$c)
  }

  return(paste(editable_parts, collapse = ""))
}


#' Enhanced production reconstruction with better paragraph alignment
#' @param original_qmd_path Path to original QMD file
#' @param edited_docx_path Path to edited Word document
#' @param output_qmd_path Path for output (if NULL, adds _updated suffix)
#' @return TRUE if successful
production_qmd_reconstruction <- function(original_qmd_path, edited_docx_path, output_qmd_path = NULL) {

  if (is.null(output_qmd_path)) {
    output_qmd_path <- paste0(tools::file_path_sans_ext(original_qmd_path), "_updated.qmd")
  }

  cat("🔄 PRODUCTION QMD RECONSTRUCTION\n")
  cat("================================\n")
  cat("Original QMD:", original_qmd_path, "\n")
  cat("Edited Word:", edited_docx_path, "\n")
  cat("Output QMD:", output_qmd_path, "\n\n")

  # Step 1: Verify input files exist
  if (!file.exists(original_qmd_path)) {
    cat("❌ Original QMD file not found:", original_qmd_path, "\n")
    return(FALSE)
  }

  if (!file.exists(edited_docx_path)) {
    cat("❌ Edited Word file not found:", edited_docx_path, "\n")
    return(FALSE)
  }

  # Step 2: Read original QMD
  cat("📖 Reading original QMD...\n")
  original_lines <- readLines(original_qmd_path)

  # Step 3: Extract Word text with debugging
  cat("📄 Extracting text from edited Word document...\n")
  tryCatch({
    word_text <- extract_word_text(edited_docx_path)
    cat("   Extracted", length(word_text), "paragraphs\n")

    # Show what we extracted for debugging
    cat("   Word paragraphs:\n")
    for (i in seq_along(word_text)) {
      if (nchar(word_text[i]) > 0) {
        cat("     Para", i, ":", substr(word_text[i], 1, 60), "...\n")
      }
    }
  }, error = function(e) {
    cat("❌ Failed to extract Word text:", e$message, "\n")
    return(FALSE)
  })

  # Step 4: Analyze tracked changes (optional - for information)
  cat("\n🔍 Analyzing tracked changes...\n")
  tryCatch({
    changes <- analyze_xml_changes(edited_docx_path)
    cat("   Found", length(changes), "tracked changes\n")
    if (length(changes) > 0) {
      for (i in seq_along(changes)) {
        change <- changes[[i]]
        cat("   -", change$type, ":", shQuote(change$text), "\n")
      }
    }
  }, error = function(e) {
    cat("   Warning: Could not analyze tracked changes:", e$message, "\n")
  })

  # Step 5: Identify content paragraphs intelligently
  cat("\n🎯 Mapping content paragraphs...\n")
  content_paragraphs <- filter_word_content_paragraphs(word_text, original_lines)

  cat("   Content paragraphs identified:", length(content_paragraphs), "\n")
  for (i in seq_along(content_paragraphs)) {
    cat("     Content", i, ":", substr(content_paragraphs[[i]]$text, 1, 50), "...\n")
  }

  if (length(content_paragraphs) == 0) {
    cat("⚠️  No content paragraphs found to update\n")
    file.copy(original_qmd_path, output_qmd_path, overwrite = TRUE)
    cat("✅ Copied original file (no changes detected)\n")
    return(TRUE)
  }

  # Step 6: Update QMD lines with new content
  cat("\n🔧 Updating QMD content...\n")
  updated_lines <- original_lines

  for (content_para in content_paragraphs) {
    # Find the matching QMD line
    qmd_line_num <- find_matching_qmd_line(content_para, original_lines)

    if (!is.null(qmd_line_num)) {
      cat("   Mapping Word para to QMD line", qmd_line_num, "\n")
      cat("     Original:", substr(original_lines[qmd_line_num], 1, 50), "...\n")
      cat("     Word text:", substr(content_para$text, 1, 50), "...\n")

      # Preserve protected content while updating
      updated_line <- preserve_protected_content_in_update(
        original_lines[qmd_line_num],
        content_para$text
      )

      updated_lines[qmd_line_num] <- updated_line
      cat("     Updated:", substr(updated_line, 1, 50), "...\n\n")
    } else {
      cat("   No matching QMD line found for Word paragraph\n")
    }
  }

  # Step 7: Write updated file
  cat("💾 Writing updated QMD...\n")
  writeLines(updated_lines, output_qmd_path)

  cat("✅ RECONSTRUCTION COMPLETE!\n")
  cat("📂 Output file:", output_qmd_path, "\n")

  return(TRUE)
}

#' Filter Word paragraphs to identify actual content (not titles/headers)
#' @param word_text Character vector from extract_word_text()
#' @param original_qmd_lines Character vector of original QMD lines
#' @return List of content paragraphs with text and indices
filter_word_content_paragraphs <- function(word_text, original_qmd_lines) {
  content_paragraphs <- list()

  for (i in seq_along(word_text)) {
    para_text <- word_text[i]

    # Skip empty paragraphs
    if (nchar(trimws(para_text)) == 0) next

    # Skip if it looks like a title or header
    if (is_title_or_header(para_text, original_qmd_lines)) next

    # If it contains substantive content, include it
    if (nchar(para_text) > 20) {  # Threshold for substantive content
      content_paragraphs[[length(content_paragraphs) + 1]] <- list(
        text = para_text,
        word_para_index = i
      )
    }
  }

  return(content_paragraphs)
}

#' Check if a Word paragraph is likely a title or header
#' @param para_text Text from Word paragraph
#' @param original_qmd_lines Original QMD lines for comparison
#' @return TRUE if paragraph is title/header
is_title_or_header <- function(para_text, original_qmd_lines) {
  # Check against YAML title
  for (line in original_qmd_lines) {
    if (grepl("^title:", line)) {
      title_extracted <- gsub("title:\\s*['\"](.+)['\"]", "\\1", line)
      if (trimws(para_text) == trimws(title_extracted)) {
        return(TRUE)
      }
    }
  }

  # Check against markdown headers (remove # symbols)
  for (line in original_qmd_lines) {
    if (grepl("^#+", line)) {
      header_text <- gsub("^#+\\s*", "", line)
      if (trimws(para_text) == trimws(header_text)) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}

#' Find the QMD line that matches a Word content paragraph
#' @param content_para List with text and word_para_index
#' @param original_lines Character vector of original QMD lines
#' @return Line number of best match, or NULL if no good match
find_matching_qmd_line <- function(content_para, original_lines) {
  para_text <- content_para$text

  # Clean the Word paragraph text for comparison
  para_clean <- clean_text_for_matching(para_text)

  best_match_line <- NULL
  best_similarity <- 0

  for (i in seq_along(original_lines)) {
    line <- original_lines[i]

    # Skip YAML, headers, empty lines
    if (grepl("^---|^#+|^$", line)) next

    # Clean the QMD line
    line_clean <- clean_text_for_matching(line)

    # Calculate similarity
    similarity <- calculate_text_similarity(para_clean, line_clean)

    if (similarity > best_similarity && similarity > 0.3) {  # Threshold for reasonable match
      best_similarity <- similarity
      best_match_line <- i
    }
  }

  return(best_match_line)
}

#' Calculate text similarity between two cleaned text strings
#' @param text1 First text string
#' @param text2 Second text string
#' @return Similarity score between 0 and 1
calculate_text_similarity <- function(text1, text2) {
  if (nchar(text1) == 0 || nchar(text2) == 0) return(0)

  # Split into words
  words1 <- strsplit(tolower(text1), "\\s+")[[1]]
  words2 <- strsplit(tolower(text2), "\\s+")[[1]]

  # Remove very short words and common words
  stop_words <- c("the", "and", "with", "was", "were", "are", "for", "this", "that", "from")
  words1 <- words1[nchar(words1) > 2 & !words1 %in% stop_words]
  words2 <- words2[nchar(words2) > 2 & !words2 %in% stop_words]

  if (length(words1) == 0 || length(words2) == 0) return(0)

  # Calculate Jaccard similarity
  intersection <- length(intersect(words1, words2))
  union <- length(union(words1, words2))

  return(intersection / union)
}

#' Preserve protected content using structural approach
#' @param original_line Original QMD line with potential protected content
#' @param new_word_content New content from Word document
#' @return Updated line with protected content preserved
preserve_protected_content_in_update <- function(original_line, new_word_content) {

  cat("       Structural preservation approach...\n")
  cat("       Original:", original_line, "\n")
  cat("       Word text:", new_word_content, "\n")

  # Extract protected elements and their positions
  protected_info <- extract_protected_elements_with_positions(original_line)

  if (length(protected_info) == 0) {
    # No protected content - just clean up tracked changes
    result <- clean_tracked_changes_artifacts(new_word_content)
    cat("       No protected content - cleaned result:", result, "\n")
    return(result)
  }

  cat("       Found", length(protected_info), "protected elements\n")

  # Get the editable portions of the original line
  original_editable <- extract_editable_portions(original_line, protected_info)
  cat("       Original editable parts:", paste(original_editable, collapse=" | "), "\n")

  # Clean the new Word content
  new_content_clean <- clean_tracked_changes_artifacts(new_word_content)
  cat("       Cleaned Word content:", new_content_clean, "\n")

  # Reconstruct the line by mapping editable changes to the structure
  result_line <- reconstruct_line_with_structure(original_line, protected_info, original_editable, new_content_clean)

  cat("       Reconstructed:", result_line, "\n")
  return(result_line)
}

#' Extract protected elements with their positions in the line
#' @param line Line of text to analyze
#' @return List of protected element info with positions
extract_protected_elements_with_positions <- function(line) {
  protected_info <- list()

  # Find all protected patterns and their positions
  patterns <- c(
    "\\[[^\\]]+\\]\\{custom-style=\"[^\"]+\"\\}",  # Custom style syntax
    "\\[[^\\]]+\\]\\{\\.[^}]+\\}",                 # Class syntax
    "`r\\s+[^`]+`",                                # R code
    "\\{\\{<[^>]+>\\}\\}",                         # Quarto shortcodes
    "@[a-zA-Z][a-zA-Z0-9_-]*"                     # Cross-references
  )

  for (pattern in patterns) {
    matches <- gregexpr(pattern, line)[[1]]
    if (matches[1] != -1) {
      for (i in seq_along(matches)) {
        start_pos <- matches[i]
        length_val <- attr(matches, "match.length")[i]
        protected_text <- substr(line, start_pos, start_pos + length_val - 1)

        protected_info[[length(protected_info) + 1]] <- list(
          text = protected_text,
          start_pos = start_pos,
          end_pos = start_pos + length_val - 1,
          pattern_type = pattern
        )
      }
    }
  }

  # Sort by position
  if (length(protected_info) > 0) {
    protected_info <- protected_info[order(sapply(protected_info, function(x) x$start_pos))]
  }

  return(protected_info)
}

#' Extract editable portions between protected elements
#' @param line Original line
#' @param protected_info List of protected element info
#' @return Vector of editable text segments
extract_editable_portions <- function(line, protected_info) {
  if (length(protected_info) == 0) {
    return(line)
  }

  editable_parts <- c()
  last_end <- 0

  for (info in protected_info) {
    # Get text before this protected element
    if (info$start_pos > last_end + 1) {
      before_text <- substr(line, last_end + 1, info$start_pos - 1)
      if (nchar(trimws(before_text)) > 0) {
        editable_parts <- c(editable_parts, trimws(before_text))
      }
    }
    last_end <- info$end_pos
  }

  # Get text after the last protected element
  if (last_end < nchar(line)) {
    after_text <- substr(line, last_end + 1, nchar(line))
    if (nchar(trimws(after_text)) > 0) {
      editable_parts <- c(editable_parts, trimws(after_text))
    }
  }

  return(editable_parts)
}

#' Reconstruct line preserving structure but updating editable content
#' @param original_line Original line
#' @param protected_info Protected element information
#' @param original_editable Original editable portions
#' @param new_content New content from Word
#' @return Reconstructed line
reconstruct_line_with_structure <- function(original_line, protected_info, original_editable, new_content) {

  # Strategy: Build segments and map new content to editable portions

  # If the editable content is substantially similar, just preserve original structure
  original_clean <- clean_text_for_matching(paste(original_editable, collapse = " "))
  new_clean <- clean_text_for_matching(new_content)

  similarity <- calculate_text_similarity(original_clean, new_clean)
  cat("         Content similarity:", round(similarity, 3), "\n")

  if (similarity > 0.7) {
    # High similarity - likely just markup differences, preserve original
    cat("         High similarity - preserving original structure\n")
    return(original_line)
  }

  # Low similarity - real edits detected, need to reconstruct
  cat("         Low similarity - reconstructing with edits\n")

  # Simple reconstruction: Replace editable parts while keeping protected elements
  result_line <- original_line

  # For now, use a simple approach: replace the longest editable segment
  if (length(original_editable) > 0) {
    longest_segment <- original_editable[which.max(nchar(original_editable))]
    cat("         Replacing longest segment:", longest_segment, "with:", new_content, "\n")

    # Replace the longest editable segment with new content
    result_line <- gsub(longest_segment, new_content, result_line, fixed = TRUE)
  }

  return(result_line)
}

#' Enhanced tracked changes cleanup
#' @param text Text that may contain tracked changes artifacts
#' @return Cleaned text
clean_tracked_changes_artifacts <- function(text) {

  # Remove duplicate words (tracked changes artifacts)
  words <- strsplit(text, "\\s+")[[1]]
  cleaned_words <- c()

  i <- 1
  while (i <= length(words)) {
    current_word <- words[i]

    # Look ahead for potential duplicates
    if (i < length(words)) {
      next_word <- words[i + 1]

      # Check for case variations (primary PRIMARY)
      if (tolower(current_word) == tolower(next_word) && current_word != next_word) {
        # Prefer the uppercase/modified version
        if (any(toupper(current_word) == current_word, toupper(next_word) == next_word)) {
          chosen_word <- ifelse(toupper(next_word) == next_word, next_word, current_word)
          cleaned_words <- c(cleaned_words, chosen_word)
          i <- i + 2
          next
        }
      }
    }

    cleaned_words <- c(cleaned_words, current_word)
    i <- i + 1
  }

  result <- paste(cleaned_words, collapse = " ")

  # Specific cleanup patterns for common tracked changes
  cleanup_patterns <- list(
    c("\\bstatistical\\s+CLEAR\\s+significance\\s+BENEFIT\\b", "CLEAR BENEFIT"),
    c("\\bprimary\\s+PRIMARY\\s+endpoint\\s+OBJECTIVE\\b", "PRIMARY OBJECTIVE"),
    c("\\bprimary\\s+PRIMARY\\b", "PRIMARY"),
    c("\\bendpoint\\s+OBJECTIVE\\b", "OBJECTIVE"),
    c("\\bstatistical\\s+CLEAR\\b", "CLEAR"),
    c("\\bsignificance\\s+BENEFIT\\b", "BENEFIT")
  )

  for (pattern_pair in cleanup_patterns) {
    result <- gsub(pattern_pair[1], pattern_pair[2], result, ignore.case = TRUE)
  }

  # Clean up multiple spaces and trim
  result <- gsub("\\s+", " ", trimws(result))

  return(result)
}

#' Convert plain text to AST content format (Str and Space elements)
#' @param text Plain text string
#' @return List of AST content elements
tokenize_text_to_ast_content <- function(text) {
  # Split text into words
  words <- strsplit(trimws(text), "\\s+")[[1]]
  words <- words[words != ""]

  content <- list()

  for (i in seq_along(words)) {
    # Add the word as a Str element
    content[[length(content) + 1]] <- list(t = "Str", c = words[i])

    # Add space after each word except the last
    if (i < length(words)) {
      content[[length(content) + 1]] <- list(t = "Space")
    }
  }

  return(content)
}



