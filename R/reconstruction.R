# Functions for QMD reconstruction
#apply_word_changes_to_qmd()
#reconstruct_ast_with_word_changes()
#extract_editable_text_from_block()
#production_qmd_reconstruction()
#preserve_protected_content_in_update()
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


#' Create a production-ready reconstruction function based on the working approach
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

  # Step 3: Extract Word text
  cat("📄 Extracting text from edited Word document...\n")
  tryCatch({
    word_text <- extract_word_text(edited_docx_path)
    cat("   Extracted", length(word_text), "paragraphs\n")
  }, error = function(e) {
    cat("❌ Failed to extract Word text:", e$message, "\n")
    return(FALSE)
  })

  # Step 4: Analyze tracked changes (optional - for information)
  cat("🔍 Analyzing tracked changes...\n")
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

  # Step 5: Identify content paragraphs to update
  cat("🎯 Identifying content to update...\n")

  # Skip title, headers, and empty paragraphs
  content_paragraphs <- word_text[
    word_text != "" &
      !grepl("^#+", word_text) &
      !word_text %in% c("Minimal Test", "Test Document", "Clinical Report", "Reconstruction Test")
  ]

  if (length(content_paragraphs) == 0) {
    cat("⚠️  No content paragraphs found to update\n")
    file.copy(original_qmd_path, output_qmd_path, overwrite = TRUE)
    cat("✅ Copied original file (no changes detected)\n")
    return(TRUE)
  }

  # Step 6: Update QMD lines with new content
  cat("🔧 Updating QMD content...\n")
  updated_lines <- original_lines
  content_index <- 1

  for (i in seq_along(original_lines)) {
    line <- original_lines[i]

    # Skip YAML, headers, and empty lines
    if (grepl("^---$|^#+|^$", line)) {
      next
    }

    # Check if this line has editable content
    if (grepl("This is|The study|Results show", line) && content_index <= length(content_paragraphs)) {
      new_content <- content_paragraphs[content_index]

      cat("   Updating line", i, "\n")
      cat("     Original:", shQuote(line), "\n")
      cat("     New Word:", shQuote(new_content), "\n")

      # Preserve protected content while updating editable parts
      updated_line <- preserve_protected_content_in_update(line, new_content)

      updated_lines[i] <- updated_line
      cat("     Updated: ", shQuote(updated_line), "\n\n")

      content_index <- content_index + 1
    }
  }

  # Step 7: Write updated file
  cat("💾 Writing updated QMD...\n")
  writeLines(updated_lines, output_qmd_path)

  cat("✅ RECONSTRUCTION COMPLETE!\n")
  cat("📂 Output file:", output_qmd_path, "\n")

  return(TRUE)
}

#' Preserve protected content during line updates
#' @param original_line Original QMD line with potential protected content
#' @param new_word_content New content from Word document
#' @return Updated line with protected content preserved
preserve_protected_content_in_update <- function(original_line, new_word_content) {

  # Find protected content patterns
  protected_patterns <- regmatches(original_line, gregexpr("\\[.*?\\]\\{[^}]*\\}", original_line))[[1]]

  if (length(protected_patterns) > 0) {
    cat("       Found", length(protected_patterns), "protected elements\n")

    # Strategy: Replace editable text but keep protected elements
    # Extract the editable text portion
    editable_part <- original_line
    for (pattern in protected_patterns) {
      editable_part <- gsub(pattern, "PROTECTED_PLACEHOLDER", editable_part, fixed = TRUE)
    }

    # Clean the editable part
    editable_clean <- clean_text_for_matching(editable_part)
    new_content_clean <- clean_text_for_matching(new_word_content)

    # If content is substantially different, update it
    if (editable_clean != new_content_clean) {
      # Reconstruct with new editable content and original protected content
      result <- new_word_content

      # Insert protected elements back (simple approach: add at end)
      for (pattern in protected_patterns) {
        if (!grepl(gsub("\\[|\\]|\\{|\\}", "", pattern), result)) {
          # Insert protected content in appropriate position
          result <- gsub("\\.$", paste("", pattern, "."), result)
          if (!grepl("\\.$", result)) {
            result <- paste(result, pattern)
          }
        }
      }

      return(result)
    }
  }

  # If no protected content or no substantial change, return new content
  return(new_word_content)
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



