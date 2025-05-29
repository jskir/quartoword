# Quick integration script to test everything together
# Save this as: test_integration.R

# Load required libraries
library(jsonlite)
library(xml2)
# Add any other libraries your existing code uses

# === PASTE YOUR EXISTING FUNCTIONS HERE ===
# Copy extract_protected_elements() from your shared code
extract_protected_elements <- function(ast) {
  protected_elements <- list()

  # Function to recursively find Span elements with protection
  find_spans <- function(content) {
    if (is.list(content)) {
      for (item in content) {
        # Check if item is a list AND has the expected structure
        if (is.list(item) && !is.null(item$t) && item$t == "Span") {
          # Check if it has the expected content structure
          if (is.list(item$c) && length(item$c) >= 1) {
            attrs <- item$c[[1]]

            # Make sure attrs has the expected structure
            if (is.list(attrs) && length(attrs) >= 3) {
              classes <- attrs[[2]]
              custom_attrs <- attrs[[3]]

              # Look for our protection patterns
              if ("protected-param" %in% classes ||
                  any(sapply(custom_attrs, function(x) length(x) >= 2 && x[[1]] == "custom-style"))) {

                protected_elements <<- append(protected_elements, list(item))
              }
            }
          }
        }

        # Recursively search nested content - but only if item has content
        if (is.list(item) && !is.null(item$c) && is.list(item$c)) {
          find_spans(item$c)
        }
      }
    }
  }

  # Search through all blocks
  for (block in ast$blocks) {
    if (!is.null(block$c)) {
      find_spans(block$c)
    }
  }

  return(protected_elements)
}

# Copy extract_editable_text() from your shared code
extract_editable_text <- function(ast) {
  editable_segments <- list()

  extract_from_content <- function(content, current_path = character()) {
    if (is.list(content)) {
      for (i in seq_along(content)) {
        item <- content[[i]]
        item_path <- c(current_path, i)

        # Skip protected Span elements
        if (is.list(item) && !is.null(item$t) && item$t == "Span") {
          if (is_protected_span(item)) {
            # Skip this - it's protected
            next
          }
        }

        # If it's a text string, collect it
        if (is.list(item) && !is.null(item$t) && item$t == "Str") {
          editable_segments <<- append(editable_segments, list(list(
            text = item$c,
            path = item_path,
            type = "editable"
          )))
        }

        # Recurse into content
        if (is.list(item) && !is.null(item$c)) {
          extract_from_content(item$c, item_path)
        }
      }
    }
  }

  # Helper function to check if a Span is protected
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

  # Extract from all blocks
  for (block in ast$blocks) {
    if (!is.null(block$c)) {
      extract_from_content(block$c)
    }
  }

  return(editable_segments)
}

# === PASTE THE NEW MAPPING FUNCTIONS HERE ===
# (Copy the entire "Enhanced Change Mapping Functions" artifact content)

# Enhanced functions for mapping Word tracked changes back to AST locations

# Complete the enhanced XML analysis function from your code
analyze_xml_changes_with_context <- function(docx_path) {
  library(xml2)

  cat("=== ENHANCED XML CHANGE ANALYSIS ===\n")

  # Extract XML
  temp_dir <- tempdir()
  unzip(docx_path, exdir = temp_dir)
  doc_xml <- read_xml(file.path(temp_dir, "word", "document.xml"))

  # Function to get surrounding text context
  get_change_context <- function(change_node, context_words = 5) {
    # Get the parent paragraph
    parent_para <- xml_find_first(change_node, "ancestor::w:p")

    if (!is.null(parent_para)) {
      # Get all text in the paragraph
      para_text <- xml_text(parent_para)

      # Get position of change within paragraph
      change_text <- xml_text(change_node)

      # Find preceding and following text
      # Split paragraph into parts before and after the change
      change_pos <- regexpr(change_text, para_text, fixed = TRUE)

      before_text <- ""
      after_text <- ""

      if (change_pos > 0) {
        before_text <- substr(para_text, 1, change_pos - 1)
        after_pos <- change_pos + nchar(change_text)
        if (after_pos <= nchar(para_text)) {
          after_text <- substr(para_text, after_pos, nchar(para_text))
        }
      }

      return(list(
        change_text = change_text,
        full_paragraph = para_text,
        before_context = trimws(before_text),
        after_context = trimws(after_text),
        paragraph_index = get_paragraph_index(parent_para, doc_xml)
      ))
    }

    return(list(change_text = xml_text(change_node), context = "Unknown"))
  }

  # Helper function to get paragraph index
  get_paragraph_index <- function(para_node, doc_xml) {
    all_paras <- xml_find_all(doc_xml, ".//w:p")
    for (i in seq_along(all_paras)) {
      if (identical(para_node, all_paras[[i]])) {
        return(i)
      }
    }
    return(NA)
  }

  # Collect all changes with context
  changes <- list()

  # Analyze insertions with context
  insertions <- xml_find_all(doc_xml, ".//w:ins")
  cat("INSERTIONS WITH CONTEXT:\n")
  for (i in seq_along(insertions)) {
    context <- get_change_context(insertions[[i]])
    author <- xml_attr(insertions[[i]], "author")
    date <- xml_attr(insertions[[i]], "date")

    change_info <- list(
      type = "insertion",
      text = context$change_text,
      author = author,
      date = date,
      paragraph_index = context$paragraph_index,
      before_context = context$before_context,
      after_context = context$after_context,
      full_paragraph = context$full_paragraph
    )

    changes[[length(changes) + 1]] <- change_info

    cat("Insert", i, ":\n")
    cat("  Text:", context$change_text, "\n")
    cat("  Paragraph:", context$paragraph_index, "\n")
    cat("  Before:", context$before_context, "\n")
    cat("  After:", context$after_context, "\n")
    cat("  Author:", author, "\n\n")
  }

  # Analyze deletions with context
  deletions <- xml_find_all(doc_xml, ".//w:del")
  cat("DELETIONS WITH CONTEXT:\n")
  for (i in seq_along(deletions)) {
    context <- get_change_context(deletions[[i]])
    author <- xml_attr(deletions[[i]], "author")
    date <- xml_attr(deletions[[i]], "date")

    change_info <- list(
      type = "deletion",
      text = context$change_text,
      author = author,
      date = date,
      paragraph_index = context$paragraph_index,
      before_context = context$before_context,
      after_context = context$after_context,
      full_paragraph = context$full_paragraph
    )

    changes[[length(changes) + 1]] <- change_info

    cat("Delete", i, ":\n")
    cat("  Text:", context$change_text, "\n")
    cat("  Paragraph:", context$paragraph_index, "\n")
    cat("  Before:", context$before_context, "\n")
    cat("  After:", context$after_context, "\n")
    cat("  Author:", author, "\n\n")
  }

  # Clean up
  unlink(temp_dir, recursive = TRUE)

  return(changes)
}

# Function to map Word changes back to AST nodes
map_changes_to_ast <- function(word_changes, original_ast, original_editable_segments) {
  cat("=== MAPPING CHANGES TO AST ===\n")

  mapped_changes <- list()

  for (change in word_changes) {
    cat("Processing", change$type, ":", change$text, "\n")

    # Strategy: Find AST nodes that match the context around the change
    matching_nodes <- find_matching_ast_nodes(change, original_ast, original_editable_segments)

    if (length(matching_nodes) > 0) {
      mapped_change <- list(
        change = change,
        ast_matches = matching_nodes,
        confidence = calculate_match_confidence(change, matching_nodes)
      )
      mapped_changes[[length(mapped_changes) + 1]] <- mapped_change

      cat("  Found", length(matching_nodes), "potential AST matches\n")
    } else {
      cat("  No AST matches found\n")
    }
  }

  return(mapped_changes)
}

# Function to find AST nodes that match a Word change
find_matching_ast_nodes <- function(word_change, ast, editable_segments) {
  matches <- list()

  # Strategy 1: Exact text match in context
  for (i in seq_along(editable_segments)) {
    segment <- editable_segments[[i]]

    # Check if this segment's context matches the Word change context
    if (text_context_matches(word_change, segment, ast)) {
      matches[[length(matches) + 1]] <- list(
        segment_index = i,
        segment = segment,
        match_type = "context_match"
      )
    }
  }

  # Strategy 2: Fuzzy matching if no exact matches
  if (length(matches) == 0) {
    matches <- fuzzy_match_segments(word_change, editable_segments)
  }

  return(matches)
}

# Function to check if text context matches between Word and AST
text_context_matches <- function(word_change, ast_segment, ast) {
  # Get the surrounding text context for this AST segment
  ast_context <- get_ast_segment_context(ast_segment, ast)

  # Compare contexts using fuzzy matching
  before_match <- fuzzy_text_match(word_change$before_context, ast_context$before_text)
  after_match <- fuzzy_text_match(word_change$after_context, ast_context$after_text)

  # Require both before and after context to match reasonably well
  return(before_match > 0.7 && after_match > 0.7)
}

# Function to get context around an AST segment
get_ast_segment_context <- function(segment, ast, context_chars = 50) {
  # This is a simplified version - you may need to enhance based on AST structure

  # Find all text segments in the same block
  block_path <- segment$path[1]  # Assuming first element is block index

  if (block_path <= length(ast$blocks)) {
    block <- ast$blocks[[block_path]]
    block_text <- extract_plain_text_from_block(block)

    # Find position of this segment's text in the block
    segment_pos <- regexpr(segment$text, block_text, fixed = TRUE)

    if (segment_pos > 0) {
      before_start <- max(1, segment_pos - context_chars)
      before_text <- substr(block_text, before_start, segment_pos - 1)

      after_start <- segment_pos + nchar(segment$text)
      after_end <- min(nchar(block_text), after_start + context_chars)
      after_text <- substr(block_text, after_start, after_end)

      return(list(
        before_text = trimws(before_text),
        after_text = trimws(after_text)
      ))
    }
  }

  return(list(before_text = "", after_text = ""))
}

# Helper function to extract plain text from an AST block
extract_plain_text_from_block <- function(block) {
  # Recursively extract all Str elements from the block
  extract_strings <- function(content) {
    text_parts <- character()

    if (is.list(content)) {
      for (item in content) {
        if (is.list(item) && !is.null(item$t)) {
          if (item$t == "Str") {
            text_parts <- c(text_parts, item$c)
          } else if (!is.null(item$c)) {
            text_parts <- c(text_parts, extract_strings(item$c))
          }
        }
      }
    }

    return(text_parts)
  }

  strings <- extract_strings(block$c)
  return(paste(strings, collapse = " "))
}

# Fuzzy text matching function
fuzzy_text_match <- function(text1, text2) {
  if (is.null(text1) || is.null(text2) || text1 == "" || text2 == "") {
    return(0)
  }

  # Simple fuzzy matching using string distance
  # You might want to use the 'stringdist' package for more sophisticated matching
  text1_clean <- tolower(trimws(text1))
  text2_clean <- tolower(trimws(text2))

  if (text1_clean == text2_clean) {
    return(1.0)
  }

  # Calculate similarity (this is a simple approach)
  longer_text <- max(nchar(text1_clean), nchar(text2_clean))
  if (longer_text == 0) return(1.0)

  # Count common words
  words1 <- strsplit(text1_clean, "\\s+")[[1]]
  words2 <- strsplit(text2_clean, "\\s+")[[1]]

  common_words <- length(intersect(words1, words2))
  total_words <- length(union(words1, words2))

  if (total_words == 0) return(1.0)

  return(common_words / total_words)
}

# Function to calculate confidence in a match
calculate_match_confidence <- function(word_change, ast_matches) {
  if (length(ast_matches) == 0) return(0)

  # Higher confidence for fewer, more precise matches
  base_confidence <- 1.0 / length(ast_matches)

  # Adjust based on match quality
  for (match in ast_matches) {
    if (match$match_type == "context_match") {
      base_confidence <- base_confidence * 1.2
    }
  }

  return(min(1.0, base_confidence))
}

# Function to apply mapped changes back to AST
apply_changes_to_ast <- function(mapped_changes, original_ast) {
  cat("=== APPLYING CHANGES TO AST ===\n")

  # Create a working copy of the AST
  updated_ast <- original_ast

  # Process changes in reverse order to maintain indices
  mapped_changes <- mapped_changes[order(sapply(mapped_changes, function(x) x$change$paragraph_index), decreasing = TRUE)]

  for (mapped_change in mapped_changes) {
    change <- mapped_change$change
    matches <- mapped_change$ast_matches

    cat("Applying", change$type, ":", change$text, "\n")

    if (length(matches) > 0 && mapped_change$confidence > 0.5) {
      # Apply the change to the best matching AST node
      best_match <- matches[[1]]  # Take the first match for now

      updated_ast <- modify_ast_node(updated_ast, best_match, change)
      cat("  Applied successfully\n")
    } else {
      cat("  Skipped (low confidence or no matches)\n")
    }
  }

  return(updated_ast)
}

# Function to modify a specific AST node
modify_ast_node <- function(ast, ast_match, word_change) {
  # This is where you'd implement the actual AST modification
  # The exact implementation depends on your AST structure

  segment <- ast_match$segment

  if (word_change$type == "insertion") {
    # Insert text at the specified location
    # This is a simplified example - you'll need to handle the AST structure properly
    cat("    Would insert '", word_change$text, "' near '", segment$text, "'\n")
  } else if (word_change$type == "deletion") {
    # Remove text from the specified location
    cat("    Would delete '", word_change$text, "' near '", segment$text, "'\n")
  }

  # Return modified AST (placeholder for now)
  return(ast)
}

# Main function to orchestrate the change mapping process
process_word_changes_to_qmd <- function(original_qmd_path, edited_docx_path, output_qmd_path) {
  cat("=== PROCESSING WORD CHANGES BACK TO QMD ===\n")

  # Step 1: Parse original QMD to AST
  system(paste("quarto pandoc", original_qmd_path, "-t json -o temp_ast.json"))
  ast <- jsonlite::fromJSON("temp_ast.json", simplifyVector = FALSE)

  # Step 2: Extract editable segments from AST
  editable_segments <- extract_editable_text(ast)

  # Step 3: Extract tracked changes from Word document
  word_changes <- analyze_xml_changes_with_context(edited_docx_path)

  # Step 4: Map Word changes to AST locations
  mapped_changes <- map_changes_to_ast(word_changes, ast, editable_segments)

  # Step 5: Apply changes to AST
  updated_ast <- apply_changes_to_ast(mapped_changes, ast)

  # Step 6: Convert updated AST back to QMD
  jsonlite::write_json(updated_ast, "temp_updated_ast.json", auto_unbox = TRUE)
  system(paste("quarto pandoc temp_updated_ast.json -f json -t markdown -o", output_qmd_path))

  # Clean up temp files
  file.remove("temp_ast.json", "temp_updated_ast.json")

  cat("Process complete! Updated QMD saved to:", output_qmd_path, "\n")

  return(list(
    original_segments = editable_segments,
    word_changes = word_changes,
    mapped_changes = mapped_changes,
    success = TRUE
  ))
}



# === SIMPLE TEST FUNCTION ===
test_basic_integration <- function() {
  cat("=== TESTING BASIC INTEGRATION ===\n")

  # Test 1: Check if all functions are available
  cat("Testing function availability:\n")

  functions_to_test <- c(
    "extract_protected_elements",
    "extract_editable_text",
    "analyze_xml_changes_with_context",
    "map_changes_to_ast"
  )

  for (func_name in functions_to_test) {
    if (exists(func_name)) {
      cat("  ✅", func_name, "- Available\n")
    } else {
      cat("  ❌", func_name, "- Missing\n")
    }
  }

  # Test 2: Create a simple test document
  cat("\nCreating test document...\n")

  test_content <- c(
    "---",
    "title: 'Integration Test'",
    "format: docx",
    "---",
    "",
    "# Test Document",
    "",
    "This is editable text with [protected content]{custom-style=\"ProtectedParam\"} here."
  )

  writeLines(test_content, "integration_test.qmd")

  # Test 3: Parse with Pandoc
  cat("Parsing with Pandoc...\n")
  system("quarto pandoc integration_test.qmd -t json -o integration_test.json")

  if (file.exists("integration_test.json")) {
    ast <- fromJSON("integration_test.json", simplifyVector = FALSE)

    # Test 4: Extract segments
    cat("Testing segment extraction...\n")
    protected <- extract_protected_elements(ast)
    editable <- extract_editable_text(ast)

    cat("  Protected elements:", length(protected), "\n")
    cat("  Editable segments:", length(editable), "\n")

    if (length(editable) > 0) {
      cat("  First editable text:", editable[[1]]$text, "\n")
    }

    cat("\n✅ Basic integration test complete!\n")
    cat("Next step: Render to Word and make tracked changes\n")

    return(list(ast = ast, protected = protected, editable = editable))
  } else {
    cat("❌ Failed to create JSON file\n")
    return(NULL)
  }
}

# Run the test
cat("Run test_basic_integration() to check if everything works together\n")
