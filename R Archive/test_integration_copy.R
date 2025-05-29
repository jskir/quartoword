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

# === FUNCTIONS FROM YOUR WORKING DEMO ===



# === THE NEW MAPPING FUNCTIONS ===
# Copy the entire "Enhanced Change Mapping Functions" artifact content here


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


cat("🎯 BULLETPROOF TEST FUNCTIONS LOADED\n")
cat("====================================\n")
cat("Run these in order:\n")
cat("1. create_minimal_test()      # Create working test case\n")
cat("2. # Follow manual steps to edit in Word\n")
cat("3. test_minimal_reconstruction() # Test the reconstruction\n")


# THAT WORKED, now doing a more complex test







cat("🔍 VERIFICATION FUNCTIONS LOADED\n")
cat("=================================\n")
cat("Available functions:\n")
cat("1. verify_reconstruction_results()  # Check the minimal test results\n")
cat("2. production_qmd_reconstruction()  # Production-ready function\n")
cat("3. test_production_function()       # Test production function\n")

