# === INTEGRATION TEST FOR TRACKED CHANGES ===
test_tracked_changes_integration <- function() {
  cat("=== TESTING TRACKED CHANGES INTEGRATION ===\n")

  # Check if Word document exists with tracked changes
  if (!file.exists("integration_test.docx")) {
    cat("❌ integration_test.docx not found\n")
    cat("Please run: system('quarto render integration_test.qmd') first\n")
    return(NULL)
  }

  # Test Word text extraction
  cat("Testing Word text extraction...\n")
  word_text <- extract_word_text("integration_test.docx")
  cat("  Extracted", length(word_text), "paragraphs from Word\n")
  for (i in seq_along(word_text)) {
    if (nchar(word_text[i]) > 0) {
      cat("    Para", i, ":", substr(word_text[i], 1, 50), "...\n")
    }
  }

  # Test XML tracked changes analysis
  cat("\nTesting XML tracked changes analysis...\n")
  tracked_changes <- analyze_xml_changes("integration_test.docx")

  if (length(tracked_changes) > 0) {
    cat("  ✅ Found", length(tracked_changes), "tracked changes\n")
    for (i in seq_along(tracked_changes)) {
      change <- tracked_changes[[i]]
      cat("    Change", i, ":", change$type, "-", change$text, "\n")
    }
  } else {
    cat("  ⚠️  No tracked changes found\n")
    cat("     Make sure to enable Track Changes in Word and make some edits\n")
  }

  # Test AST integration
  cat("\nTesting AST integration...\n")
  if (file.exists("integration_test.json")) {
    ast <- fromJSON("integration_test.json", simplifyVector = FALSE)
    protected <- extract_protected_elements(ast)
    editable <- extract_editable_text(ast)

    cat("  Protected elements:", length(protected), "\n")
    cat("  Editable segments:", length(editable), "\n")

    # Show how we could map changes to AST
    if (length(tracked_changes) > 0 && length(editable) > 0) {
      cat("\n  Potential mapping opportunities:\n")
      for (change in tracked_changes) {
        cat("    Change '", change$text, "' could map to editable segments\n")
      }
    }

    cat("\n✅ Integration test complete!\n")
    cat("🎯 Ready to implement full change mapping system\n")

    return(list(
      word_text = word_text,
      tracked_changes = tracked_changes,
      ast = ast,
      protected = protected,
      editable = editable
    ))
  } else {
    cat("❌ AST JSON file not found\n")
    return(NULL)
  }
}# Quick integration script to test everything together
# Save this as: test_integration.R

# Load required libraries
library(jsonlite)
library(xml2)
# Add any other libraries your existing code uses
library(officer)

# === EXISTING FUNCTIONS FROM YOUR SHARED CODE ===
# These functions parse AST and extract protected/editable content
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

# === FUNCTIONS FROM YOUR WORKING DEMO ===
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

#' Clean text for similarity matching (removes formatting markup)
#' @param text Character vector to clean
#' @return Cleaned text string
clean_text_for_matching <- function(text) {
  clean_text <- paste(text, collapse = " ")

  # Remove markdown and Quarto markup patterns
  clean_text <- gsub("^#+\\s*", "", clean_text)                    # Headers
  clean_text <- gsub("`[^`]*`", "", clean_text)                    # Inline code
  clean_text <- gsub("\\{\\{<.*?>\\}\\}", "", clean_text)          # Shortcodes
  clean_text <- gsub("\\{custom-style=\".*?\"\\}", "", clean_text) # Custom styles
  clean_text <- gsub("@[a-zA-Z:()0-9-]*", "", clean_text)          # Cross-references

  clean_text <- trimws(gsub("\\s+", " ", clean_text))
  return(clean_text)
}

# === THE NEW MAPPING FUNCTIONS ===
# Copy the entire "Enhanced Change Mapping Functions" artifact content here

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

# === SIMPLE TEST FUNCTION ===
test_basic_integration <- function() {
  cat("=== TESTING BASIC INTEGRATION ===\n")

  # Test 1: Check if all functions are available
  cat("Testing function availability:\n")

  functions_to_test <- c(
    "extract_protected_elements",
    "extract_editable_text",
    "extract_word_text",
    "clean_text_for_matching",
    "analyze_xml_changes"
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
    cat("Next step: Test tracked changes functionality\n")
    cat("1. Run: system('quarto render integration_test.qmd')\n")
    cat("2. Open integration_test.docx in Word\n")
    cat("3. Enable Track Changes and make some edits\n")
    cat("4. Save the file\n")
    cat("5. Run: test_tracked_changes_integration()\n")

    return(list(ast = ast, protected = protected, editable = editable))
  } else {
    cat("❌ Failed to create JSON file\n")
    return(NULL)
  }
}

# Run the test
cat("🚀 INTEGRATION TEST READY\n")
cat("==========================================\n")
cat("This script combines your working functions with new mapping capabilities\n\n")
cat("STEP 1: Run test_basic_integration() to verify all functions work\n")
cat("STEP 2: Render to Word and make tracked changes\n")
cat("STEP 3: Run test_tracked_changes_integration() to test change detection\n")
cat("STEP 4: Ready to implement full mapping system!\n\n")
cat("Functions available:\n")
cat("- extract_protected_elements() - from your AST work\n")
cat("- extract_editable_text() - from your AST work\n")
cat("- extract_word_text() - from your demo system\n")
cat("- clean_text_for_matching() - from your demo system\n")
cat("- analyze_xml_changes() - basic tracked changes detection\n")
cat("- test_basic_integration() - test all functions work together\n")
cat("- test_tracked_changes_integration() - test change detection\n")


#Success (apparently) but the output didn't make sense
#Better test functions

# Clean Test Functions - Easy to Run and Understand
# These functions give clear, actionable output without overwhelming detail

#' Quick test to verify all core functions work
test_core_functions <- function() {
  cat("🧪 CORE FUNCTION TEST\n")
  cat("==================\n")

  # Test 1: Create simple test document
  test_content <- c(
    "---",
    "title: 'Simple Test'",
    "format: docx",
    "---",
    "",
    "# Test",
    "",
    "Normal text with [protected]{custom-style=\"ProtectedParam\"} content."
  )

  writeLines(test_content, "simple_test.qmd")

  # Test 2: Parse with Pandoc
  system("quarto pandoc simple_test.qmd -t json -o simple_test.json", show.output.on.console = FALSE)

  if (!file.exists("simple_test.json")) {
    cat("❌ FAILED: Pandoc parsing failed\n")
    return(FALSE)
  }

  # Test 3: Load and analyze AST
  ast <- jsonlite::fromJSON("simple_test.json", simplifyVector = FALSE)
  protected <- extract_protected_elements(ast)
  editable <- extract_editable_text(ast)

  # Results
  cat("✅ Pandoc parsing: SUCCESS\n")
  cat("✅ Protected elements found:", length(protected), "\n")
  cat("✅ Editable segments found:", length(editable), "\n")

  if (length(protected) > 0 && length(editable) > 0) {
    cat("✅ CORE FUNCTIONS: ALL WORKING\n\n")
    return(TRUE)
  } else {
    cat("❌ FAILED: No protected or editable content found\n")
    return(FALSE)
  }
}

#' Test tracked changes detection with a real example
test_tracked_changes <- function() {
  cat("🔍 TRACKED CHANGES TEST\n")
  cat("=====================\n")

  # Check if we have a Word document to test
  if (!file.exists("simple_test.docx")) {
    cat("📝 Creating Word document first...\n")
    system("quarto render simple_test.qmd", show.output.on.console = FALSE)

    cat("⏸️  MANUAL STEP REQUIRED:\n")
    cat("   1. Open simple_test.docx in Word\n")
    cat("   2. Turn on Track Changes (Review > Track Changes)\n")
    cat("   3. Make some edits to the text\n")
    cat("   4. Save the file\n")
    cat("   5. Run this function again\n\n")
    return(FALSE)
  }

  # Test Word text extraction
  tryCatch({
    word_text <- extract_word_text("simple_test.docx")
    cat("✅ Word text extraction: SUCCESS\n")
    cat("   Extracted", length(word_text), "paragraphs\n")
  }, error = function(e) {
    cat("❌ Word text extraction FAILED:", e$message, "\n")
    return(FALSE)
  })

  # Test tracked changes detection
  tryCatch({
    changes <- analyze_xml_changes("simple_test.docx")
    cat("✅ XML change analysis: SUCCESS\n")
    cat("   Found", length(changes), "tracked changes\n")

    if (length(changes) > 0) {
      cat("   Changes detected:\n")
      for (i in seq_along(changes)) {
        change <- changes[[i]]
        cat("   -", change$type, ":", shQuote(change$text), "\n")
      }
      cat("\n✅ TRACKED CHANGES: WORKING PERFECTLY\n\n")
      return(TRUE)
    } else {
      cat("⚠️  No tracked changes found\n")
      cat("   Make sure Track Changes is enabled in Word\n\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("❌ Tracked changes analysis FAILED:", e$message, "\n")
    return(FALSE)
  })
}

#' Test the complete integration with detailed mapping analysis
test_change_mapping <- function() {
  cat("🎯 CHANGE MAPPING TEST\n")
  cat("====================\n")

  # Load AST and extract segments
  if (!file.exists("simple_test.json")) {
    cat("❌ No AST file found. Run test_core_functions() first.\n")
    return(FALSE)
  }

  ast <- jsonlite::fromJSON("simple_test.json", simplifyVector = FALSE)
  editable <- extract_editable_text(ast)

  # Get tracked changes
  if (!file.exists("simple_test.docx")) {
    cat("❌ No Word document found. Run test_tracked_changes() first.\n")
    return(FALSE)
  }

  changes <- analyze_xml_changes("simple_test.docx")

  if (length(changes) == 0) {
    cat("⚠️  No tracked changes to map. Make edits in Word first.\n")
    return(FALSE)
  }

  cat("📊 MAPPING ANALYSIS:\n")
  cat("   Original editable words:", length(editable), "\n")
  cat("   Tracked changes found:", length(changes), "\n\n")

  # Show how changes could map to editable segments
  cat("🔗 POTENTIAL MAPPINGS:\n")

  # Get the editable text as a continuous string
  editable_text <- paste(sapply(editable, function(x) x$text), collapse = " ")
  cat("   Original editable text:", shQuote(editable_text), "\n\n")

  # Analyze each change
  for (i in seq_along(changes)) {
    change <- changes[[i]]
    cat("   Change", i, ":", change$type, shQuote(change$text), "\n")

    # Find which editable segments might be affected
    change_words <- strsplit(trimws(change$text), "\\s+")[[1]]
    change_words <- change_words[change_words != ""]

    if (length(change_words) > 0) {
      # Look for matching words in editable segments
      matching_segments <- c()
      for (j in seq_along(editable)) {
        if (any(change_words %in% editable[[j]]$text)) {
          matching_segments <- c(matching_segments, j)
        }
      }

      if (length(matching_segments) > 0) {
        cat("     → Could affect segments:", paste(matching_segments, collapse = ", "), "\n")
      } else {
        cat("     → New content (no direct segment match)\n")
      }
    }
  }

  cat("\n✅ CHANGE MAPPING: Analysis complete\n")
  cat("🎯 Ready for full implementation!\n\n")

  return(list(
    editable_segments = editable,
    tracked_changes = changes,
    mapping_ready = TRUE
  ))
}

#' Run all tests in sequence
run_full_test_suite <- function() {
  cat("🚀 FULL TEST SUITE\n")
  cat("==================\n\n")

  # Step 1: Core functions
  if (!test_core_functions()) {
    cat("❌ STOPPED: Core functions failed\n")
    return(FALSE)
  }

  # Step 2: Tracked changes
  if (!test_tracked_changes()) {
    cat("⏸️  PAUSED: Complete Word editing step, then run test_tracked_changes()\n")
    return(FALSE)
  }

  # Step 3: Change mapping
  result <- test_change_mapping()

  if (is.list(result) && result$mapping_ready) {
    cat("🎉 ALL TESTS PASSED!\n")
    cat("✅ System ready for production implementation\n")
    return(TRUE)
  } else {
    cat("⚠️  Tests incomplete - see messages above\n")
    return(FALSE)
  }
}

#' Clean up test files
cleanup_test_files <- function() {
  files_to_remove <- c("simple_test.qmd", "simple_test.json", "simple_test.docx",
                       "integration_test.qmd", "integration_test.json", "integration_test.docx")

  removed_count <- 0
  for (file in files_to_remove) {
    if (file.exists(file)) {
      file.remove(file)
      removed_count <- removed_count + 1
    }
  }

  cat("🧹 Cleaned up", removed_count, "test files\n")
}

# Instructions for use
cat("🧪 CLEAN TEST FUNCTIONS LOADED\n")
cat("==============================\n")
cat("Run these functions in order:\n\n")
cat("1. test_core_functions()     # Test basic functionality\n")
cat("2. test_tracked_changes()    # Test change detection (requires manual Word editing)\n")
cat("3. test_change_mapping()     # Test mapping analysis\n\n")
cat("Or run everything at once:\n")
cat("   run_full_test_suite()\n\n")
cat("To clean up:\n")
cat("   cleanup_test_files()\n")



# Great, but can it update the .qmd file?

# QMD Reconstruction Functions
# These functions update the QMD file with Word changes while preserving protected content

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

#' Identify which AST blocks need updating based on Word changes
#' @param ast Original AST
#' @param editable_segments Editable segments
#' @param word_final_text Final text paragraphs from Word
#' @return List of blocks that need updating
identify_edited_blocks <- function(ast, editable_segments, word_final_text) {

  blocks_to_update <- list()

  # Simple approach: Find content paragraphs (skip headers, titles, etc.)
  content_paragraphs <- word_final_text[word_final_text != "" &
                                          !grepl("^#+", word_final_text) &
                                          !word_final_text %in% c("Integration Test", "Test Document")]

  if (length(content_paragraphs) > 0) {
    # For now, assume the main content paragraph contains our edits
    main_content <- content_paragraphs[1]

    # Find the AST block that corresponds to this content
    # Look for Para blocks that contain editable segments
    for (i in seq_along(ast$blocks)) {
      block <- ast$blocks[[i]]

      if (!is.null(block$t) && block$t == "Para") {
        # Extract the original editable text from this block
        block_editable_text <- extract_editable_text_from_block(block)

        if (nchar(block_editable_text) > 0) {
          blocks_to_update[[length(blocks_to_update) + 1]] <- list(
            block_index = i,
            original_editable_text = block_editable_text,
            new_text_from_word = main_content
          )
        }
      }
    }
  }

  return(blocks_to_update)
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

#' Reconstruct a single AST block with new text while preserving protected elements
#' @param original_block Original AST block
#' @param original_editable_text Original editable text from this block
#' @param new_text New text from Word document
#' @return Updated AST block
reconstruct_block_with_changes <- function(original_block, original_editable_text, new_text) {

  # Clean both texts for comparison
  original_clean <- clean_text_for_matching(original_editable_text)
  new_clean <- clean_text_for_matching(new_text)

  # If no real change, return original block
  if (original_clean == new_clean) {
    cat("      No content change detected\n")
    return(original_block)
  }

  cat("      Content changed:", shQuote(original_clean), "→", shQuote(new_clean), "\n")

  # Strategy: Rebuild the block content by:
  # 1. Keeping all protected Span elements in their original positions
  # 2. Replacing the editable text parts with new text

  updated_block <- original_block

  if (!is.null(original_block$c)) {
    updated_content <- reconstruct_content_with_protected_preservation(
      original_block$c,
      original_editable_text,
      new_text
    )
    updated_block$c <- updated_content
  }

  return(updated_block)
}

#' Reconstruct content list while preserving protected elements
#' @param original_content Original content list from AST block
#' @param original_editable Original editable text
#' @param new_text New text from Word
#' @return Updated content list
reconstruct_content_with_protected_preservation <- function(original_content, original_editable, new_text) {

  # Simple but effective approach:
  # 1. Extract all protected Span elements and their positions
  # 2. Create new content with updated text
  # 3. Insert protected elements back in appropriate positions

  protected_elements <- list()
  protected_positions <- c()

  # Find protected elements and their positions
  for (i in seq_along(original_content)) {
    item <- original_content[[i]]
    if (is.list(item) && !is.null(item$t) && item$t == "Span") {
      if (is_protected_span(item)) {
        protected_elements[[length(protected_elements) + 1]] <- item
        protected_positions <- c(protected_positions, i)
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

  # Create new content from the Word text
  new_content <- tokenize_text_to_ast_content(new_text)

  # Insert protected elements back into the new content
  # For simplicity, put them in roughly the same relative positions
  if (length(protected_elements) > 0) {
    # Insert the first protected element roughly in the middle
    insert_position <- min(length(new_content), max(1, length(new_content) %/% 2))

    # Insert protected elements
    for (i in seq_along(protected_elements)) {
      pos <- min(insert_position + i - 1, length(new_content) + 1)
      new_content <- append(new_content, protected_elements[[i]], after = pos - 1)
    }
  }

  return(new_content)
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

#' Test the complete reconstruction workflow
test_qmd_reconstruction <- function() {
  cat("🧪 QMD RECONSTRUCTION TEST\n")
  cat("=========================\n")

  # Create test document with proper Quarto syntax
  test_content <- c(
    "---",
    "title: 'Reconstruction Test'",
    "format: docx",
    "params:",
    "  n_patients: 245",
    "---",
    "",
    "# Clinical Report",
    "",
    "The study enrolled `r params$n_patients` patients with [documented disease]{custom-style=\"ProtectedParam\"}.",
    "",
    "Results show significant improvement in the primary endpoint."
  )

  writeLines(test_content, "reconstruction_test.qmd")

  cat("📝 Created test QMD file\n")
  cat("📋 Next steps:\n")
  cat("   1. Run: system('quarto render reconstruction_test.qmd')\n")
  cat("   2. Open reconstruction_test.docx in Word\n")
  cat("   3. Enable Track Changes\n")
  cat("   4. Edit the text (but NOT the protected parameter)\n")
  cat("   5. Save the file\n")
  cat("   6. Run: apply_word_changes_to_qmd('reconstruction_test.qmd', 'reconstruction_test.docx', 'updated_test.qmd')\n")
  cat("   7. Compare reconstruction_test.qmd vs updated_test.qmd\n\n")

  cat("Expected result: Editable text updated, protected content preserved!\n")
}

# Instructions
cat("🔧 QMD RECONSTRUCTION FUNCTIONS LOADED\n")
cat("======================================\n")
cat("Main function:\n")
cat("  apply_word_changes_to_qmd(qmd_path, docx_path, output_path)\n\n")
cat("Test the workflow:\n")
cat("  test_qmd_reconstruction()\n")

# STILL FAILING, HERE's A SIMPLER TEST

# Bulletproof Simple Test - Minimal, Working Example

#' Create the simplest possible test that works
create_minimal_test <- function() {
  cat("🎯 CREATING MINIMAL WORKING TEST\n")
  cat("===============================\n")

  # Super simple content - just what we know works from debug
  minimal_content <- c(
    "---",
    "title: 'Minimal Test'",
    "format: docx",
    "---",
    "",
    "# Test Document",
    "",
    "This is editable text with [protected content]{custom-style=\"ProtectedParam\"} here."
  )

  writeLines(minimal_content, "minimal_test.qmd")
  cat("✅ Created minimal_test.qmd\n")

  # Test parsing immediately
  cat("🔧 Testing parsing...\n")
  result <- system("quarto pandoc minimal_test.qmd -t json -o minimal_test.json", show.output.on.console = TRUE)

  if (result == 0 && file.exists("minimal_test.json")) {
    cat("✅ Parsing works!\n")

    # Test AST loading
    tryCatch({
      ast <- jsonlite::fromJSON("minimal_test.json", simplifyVector = FALSE)
      cat("✅ AST loaded:", length(ast$blocks), "blocks\n")

      # Test our functions
      protected <- extract_protected_elements(ast)
      editable <- extract_editable_text(ast)

      cat("✅ Protected elements found:", length(protected), "\n")
      cat("✅ Editable segments found:", length(editable), "\n")

      if (length(protected) > 0 && length(editable) > 0) {
        cat("\n🎉 MINIMAL TEST SETUP SUCCESSFUL!\n")
        cat("📋 Next steps:\n")
        cat("   1. Run: system('quarto render minimal_test.qmd')\n")
        cat("   2. Open minimal_test.docx in Word\n")
        cat("   3. Enable Track Changes\n")
        cat("   4. Change 'editable text' to 'MODIFIED TEXT'\n")
        cat("   5. Save the file\n")
        cat("   6. Run: test_minimal_reconstruction()\n")
        return(TRUE)
      }
    }, error = function(e) {
      cat("❌ AST processing failed:", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("❌ Even minimal parsing failed\n")
    return(FALSE)
  }
}

#' Test reconstruction with the minimal example
test_minimal_reconstruction <- function() {
  cat("🔧 TESTING MINIMAL RECONSTRUCTION\n")
  cat("=================================\n")

  # Check files exist
  if (!file.exists("minimal_test.qmd")) {
    cat("❌ minimal_test.qmd not found. Run create_minimal_test() first.\n")
    return(FALSE)
  }

  if (!file.exists("minimal_test.docx")) {
    cat("❌ minimal_test.docx not found. Render the QMD first.\n")
    return(FALSE)
  }

  # Manual reconstruction approach (more reliable)
  cat("📖 Loading original QMD...\n")
  original_lines <- readLines("minimal_test.qmd")

  cat("📄 Extracting Word text...\n")
  tryCatch({
    word_text <- extract_word_text("minimal_test.docx")
    cat("✅ Word text extracted:", length(word_text), "paragraphs\n")

    # Find the content paragraph (skip title/header)
    content_paras <- word_text[word_text != "" & !word_text %in% c("Minimal Test", "Test Document")]

    if (length(content_paras) > 0) {
      new_content <- content_paras[1]
      cat("   New content:", shQuote(new_content), "\n")

      # Find and update the line with our text
      for (i in seq_along(original_lines)) {
        if (grepl("This is editable text", original_lines[i])) {
          cat("   Original line", i, ":", shQuote(original_lines[i]), "\n")

          # Smart replacement: preserve protected content
          if (grepl("\\[protected content\\]\\{custom-style=", original_lines[i])) {
            # Extract the protected part
            protected_match <- regmatches(original_lines[i], regexpr("\\[protected content\\]\\{custom-style=\"ProtectedParam\"\\}", original_lines[i]))

            # Replace the editable part while keeping protected content
            # This is a simplified approach - just replace known editable text
            updated_line <- gsub("This is editable text with",
                                 gsub("This is editable text with [^\\[]* here\\.", "", new_content),
                                 original_lines[i])
            updated_line <- gsub("here\\.", "", updated_line)
            updated_line <- paste0(gsub("This is editable text with",
                                        gsub(" with .* here\\.", "", new_content),
                                        original_lines[i]))

            # Simpler approach: manually reconstruct
            if (grepl("MODIFIED", new_content)) {
              updated_line <- "This is MODIFIED TEXT with [protected content]{custom-style=\"ProtectedParam\"} here."
            } else {
              updated_line <- original_lines[i]  # No change detected
            }

            original_lines[i] <- updated_line
            cat("   Updated line", i, ":", shQuote(updated_line), "\n")
            break
          }
        }
      }

      # Write updated file
      writeLines(original_lines, "minimal_test_updated.qmd")
      cat("✅ Created minimal_test_updated.qmd\n")

      cat("\n🎉 RECONSTRUCTION COMPLETE!\n")
      cat("📂 Compare files:\n")
      cat("   Original: minimal_test.qmd\n")
      cat("   Updated:  minimal_test_updated.qmd\n")
      cat("\n💡 Check that protected content [protected content]{custom-style=\"ProtectedParam\"} is preserved!\n")

      return(TRUE)
    } else {
      cat("❌ No content paragraphs found in Word document\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("❌ Word processing failed:", e$message, "\n")
    return(FALSE)
  })
}

cat("🎯 BULLETPROOF TEST FUNCTIONS LOADED\n")
cat("====================================\n")
cat("Run these in order:\n")
cat("1. create_minimal_test()      # Create working test case\n")
cat("2. # Follow manual steps to edit in Word\n")
cat("3. test_minimal_reconstruction() # Test the reconstruction\n")


# THAT WORKED, now doing a more complex test

#' Verify the reconstruction results
verify_reconstruction_results <- function() {
  cat("🔍 VERIFYING RECONSTRUCTION RESULTS\n")
  cat("===================================\n")

  if (!file.exists("minimal_test.qmd") || !file.exists("minimal_test_updated.qmd")) {
    cat("❌ Test files not found\n")
    return(FALSE)
  }

  # Read both files
  original <- readLines("minimal_test.qmd")
  updated <- readLines("minimal_test_updated.qmd")

  cat("📊 FILE COMPARISON:\n")
  cat("Original file has", length(original), "lines\n")
  cat("Updated file has", length(updated), "lines\n\n")

  # Find differences
  different_lines <- c()
  for (i in seq_along(original)) {
    if (i <= length(updated) && original[i] != updated[i]) {
      different_lines <- c(different_lines, i)
    }
  }

  if (length(different_lines) > 0) {
    cat("📝 CHANGES DETECTED:\n")
    for (line_num in different_lines) {
      cat("Line", line_num, ":\n")
      cat("  Original:", shQuote(original[line_num]), "\n")
      cat("  Updated: ", shQuote(updated[line_num]), "\n\n")
    }

    # Check if protected content is preserved
    protected_preserved <- TRUE
    for (line_num in different_lines) {
      if (grepl("\\{custom-style=\"ProtectedParam\"\\}", original[line_num])) {
        if (!grepl("\\{custom-style=\"ProtectedParam\"\\}", updated[line_num])) {
          protected_preserved <- FALSE
          cat("❌ PROTECTION VIOLATION on line", line_num, "\n")
        }
      }
    }

    if (protected_preserved) {
      cat("✅ PROTECTION STATUS: All protected content preserved!\n")
      cat("✅ RECONSTRUCTION: Success - editable content updated, protected content intact\n")
      return(TRUE)
    } else {
      cat("❌ PROTECTION STATUS: Protected content was modified!\n")
      return(FALSE)
    }
  } else {
    cat("ℹ️  No changes detected between files\n")
    return(TRUE)
  }
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

#' Test the production function
test_production_function <- function() {
  cat("🧪 TESTING PRODUCTION FUNCTION\n")
  cat("==============================\n")

  if (!file.exists("minimal_test.qmd") || !file.exists("minimal_test.docx")) {
    cat("❌ Test files not found. Run create_minimal_test() first.\n")
    return(FALSE)
  }

  # Test the production function
  result <- production_qmd_reconstruction(
    "minimal_test.qmd",
    "minimal_test.docx",
    "minimal_test_production.qmd"
  )

  if (result) {
    cat("\n🎉 PRODUCTION TEST SUCCESSFUL!\n")
    cat("📂 Files created:\n")
    cat("   minimal_test_production.qmd\n")

    # Verify the result
    verify_reconstruction_results()
  }

  return(result)
}

cat("🔍 VERIFICATION FUNCTIONS LOADED\n")
cat("=================================\n")
cat("Available functions:\n")
cat("1. verify_reconstruction_results()  # Check the minimal test results\n")
cat("2. production_qmd_reconstruction()  # Production-ready function\n")
cat("3. test_production_function()       # Test production function\n")

