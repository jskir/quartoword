# Systematic Testing Framework for Quartoword
# Build complexity step-by-step with debugging at each level

# =============================================================================
# LEVEL 1: MULTIPLE PROTECTED ELEMENTS
# =============================================================================

#' Test Level 1: Multiple protected elements in one paragraph
test_level_1_multiple_protected <- function() {
  cat("🧪 LEVEL 1: MULTIPLE PROTECTED ELEMENTS\n")
  cat("======================================\n")

  test_content <- c(
    "---",
    "title: 'Level 1 Test'",
    "format: docx",
    "---",
    "",
    "# Clinical Study Report",
    "",
    "The study enrolled [245]{custom-style=\"ProtectedParam\"} patients using [drug ABC-123]{custom-style=\"ProtectedParam\"} versus [placebo]{custom-style=\"ProtectedParam\"}.",
    "",
    "Results are shown in [Table 1]{custom-style=\"CrossReference\"} and [Figure 2]{custom-style=\"CrossReference\"}.",
    "",
    "The primary endpoint was met with statistical significance."
  )

  # Ensure tests directory exists
  dir.create("tests", showWarnings = FALSE)

  writeLines(test_content, "tests/level1_test.qmd")
  cat("✅ Created tests/level1_test.qmd\n")

  # Test parsing
  cat("🔧 Testing AST parsing...\n")
  result <- system("quarto pandoc tests/level1_test.qmd -t json -o tests/level1_test.json", show.output.on.console = FALSE)

  if (result == 0 && file.exists("tests/level1_test.json")) {
    ast <- jsonlite::fromJSON("tests/level1_test.json", simplifyVector = FALSE)
    protected <- extract_protected_elements(ast)
    editable <- extract_editable_text(ast)

    cat("✅ Parsing successful\n")
    cat("   Protected elements:", length(protected), "\n")
    cat("   Editable segments:", length(editable), "\n")

    # Expected: 5 protected elements (3 params + 2 cross-refs)
    if (length(protected) >= 5) {
      cat("✅ LEVEL 1 READY FOR TESTING\n")
      cat("📋 Manual steps:\n")
      cat("   1. Run: system('quarto render tests/level1_test.qmd')\n")
      cat("   2. Edit tests/level1_test.docx: Change 'statistical significance' to 'CLEAR BENEFIT'\n")
      cat("   3. Change 'primary endpoint' to 'PRIMARY OBJECTIVE'\n")
      cat("   4. DO NOT edit any protected content (numbers, drug names, table refs)\n")
      cat("   5. Run: test_level_1_reconstruction()\n")
      return(TRUE)
    } else {
      cat("❌ Expected 5+ protected elements, found", length(protected), "\n")
      debug_protected_elements(ast)
      return(FALSE)
    }
  } else {
    cat("❌ Parsing failed\n")
    return(FALSE)
  }
}

#' Test Level 1 reconstruction
test_level_1_reconstruction <- function() {
  cat("🔧 LEVEL 1 RECONSTRUCTION TEST\n")
  cat("=============================\n")

  if (!file.exists("tests/level1_test.docx")) {
    cat("❌ tests/level1_test.docx not found. Run level 1 setup first.\n")
    return(FALSE)
  }

  # Test our production function
  result <- production_qmd_reconstruction(
    "tests/level1_test.qmd",
    "tests/level1_test.docx",
    "tests/level1_updated.qmd"
  )

  if (result) {
    cat("\n📊 ANALYZING RESULTS:\n")
    original_lines <- readLines("tests/level1_test.qmd")
    updated_lines <- readLines("tests/level1_updated.qmd")

    # Check if protected content is preserved
    check_protection_level_1(original_lines, updated_lines)
  }

  return(result)
}

# =============================================================================
# LEVEL 2: MULTIPLE PARAGRAPHS WITH MIXED CONTENT
# =============================================================================

#' Test Level 2: Multiple paragraphs with different content types
test_level_2_multiple_paragraphs <- function() {
  cat("🧪 LEVEL 2: MULTIPLE PARAGRAPHS\n")
  cat("===============================\n")

  test_content <- c(
    "---",
    "title: 'Level 2 Test'",
    "format: docx",
    "---",
    "",
    "# Executive Summary",
    "",
    "This Phase III study evaluated [ABC-123]{custom-style=\"ProtectedParam\"} in [245]{custom-style=\"ProtectedParam\"} patients.",
    "",
    "## Primary Objectives",
    "",
    "The primary objective was to demonstrate superior efficacy compared to placebo.",
    "",
    "## Key Results",
    "",
    "The study met its primary endpoint as shown in [Table 1]{custom-style=\"CrossReference\"}.",
    "",
    "Secondary endpoints showed consistent benefit across subgroups.",
    "",
    "## Conclusions",
    "",
    "The results support regulatory approval for this indication."
  )

  writeLines(test_content, "tests/level2_test.qmd")
  cat("✅ Created tests/level2_test.qmd\n")

  # Test parsing
  cat("🔧 Testing multi-paragraph parsing...\n")
  result <- system("quarto pandoc tests/level2_test.qmd -t json -o tests/level2_test.json", show.output.on.console = FALSE)

  if (result == 0) {
    ast <- jsonlite::fromJSON("tests/level2_test.json", simplifyVector = FALSE)

    cat("✅ Parsing successful\n")
    cat("   Total blocks:", length(ast$blocks), "\n")

    # Analyze block types
    analyze_block_structure(ast)

    cat("✅ LEVEL 2 READY FOR TESTING\n")
    cat("📋 Manual steps:\n")
    cat("   1. Render and edit multiple paragraphs\n")
    cat("   2. Test paragraph alignment logic\n")
    cat("   3. Run: test_level_2_reconstruction()\n")
    return(TRUE)
  } else {
    cat("❌ Level 2 parsing failed\n")
    return(FALSE)
  }
}

# =============================================================================
# LEVEL 3: QUARTO-SPECIFIC FEATURES
# =============================================================================

#' Test Level 3: Quarto shortcodes and cross-references
test_level_3_quarto_features <- function() {
  cat("🧪 LEVEL 3: QUARTO FEATURES\n")
  cat("===========================\n")

  test_content <- c(
    "---",
    "title: 'Level 3 Test'",
    "format: docx",
    "---",
    "",
    "# Study Report",
    "",
    "The study enrolled patients with documented disease.",
    "",
    "See @tbl-demographics for baseline characteristics.",
    "",
    "Primary analysis included all randomized patients (n=245).",
    "",
    "::: {.callout-note}",
    "This is important regulatory information.",
    ":::",
    "",
    "Results demonstrate clinical benefit."
  )

  writeLines(test_content, "level3_test.qmd")
  cat("✅ Created level3_test.qmd with Quarto features\n")

  # Test parsing
  cat("🔧 Testing Quarto feature parsing...\n")
  result <- system("quarto pandoc level3_test.qmd -t json -o level3_test.json", show.output.on.console = FALSE)

  if (result == 0) {
    ast <- jsonlite::fromJSON("level3_test.json", simplifyVector = FALSE)

    # Look for Quarto-specific elements
    quarto_elements <- find_quarto_elements(ast)

    cat("✅ Quarto parsing successful\n")
    cat("   Quarto elements found:", length(quarto_elements), "\n")

    return(TRUE)
  } else {
    cat("❌ Level 3 parsing failed - may need Quarto-specific handling\n")
    return(FALSE)
  }
}

# =============================================================================
# LEVEL 4: CLINICAL DOCUMENT SIMULATION
# =============================================================================

#' Test Level 4: Realistic clinical document structure
test_level_4_clinical_document <- function() {
  cat("🧪 LEVEL 4: CLINICAL DOCUMENT\n")
  cat("=============================\n")

  test_content <- c(
    "---",
    "title: 'Clinical Study Report'",
    "subtitle: 'Study ABC-123-001'",
    "format: docx",
    "---",
    "",
    "# Executive Summary",
    "",
    "This randomized, double-blind, placebo-controlled study evaluated [ABC-123]{custom-style=\"ProtectedParam\"} in [245]{custom-style=\"ProtectedParam\"} patients with advanced disease.",
    "",
    "## Study Objectives",
    "",
    "The primary objective was to demonstrate superior progression-free survival compared to placebo.",
    "",
    "Secondary objectives included overall survival and safety evaluation.",
    "",
    "## Study Design",
    "",
    "Patients were randomized 2:1 to receive active treatment or placebo.",
    "",
    "## Key Efficacy Results",
    "",
    "The study met its primary endpoint with statistical significance (p<0.001).",
    "",
    "Median progression-free survival was [12.5 months]{custom-style=\"ProtectedParam\"} versus [6.2 months]{custom-style=\"ProtectedParam\"} for placebo.",
    "",
    "Results are detailed in [Table 14.2.1]{custom-style=\"CrossReference\"} and [Figure 14.2.1]{custom-style=\"CrossReference\"}.",
    "",
    "## Safety Summary",
    "",
    "The safety profile was consistent with the known profile of the drug class.",
    "",
    "Most adverse events were mild to moderate in severity.",
    "",
    "## Conclusions",
    "",
    "The results support the benefit-risk profile for regulatory approval.",
    "",
    "The data demonstrate clinically meaningful improvement in the primary endpoint."
  )

  writeLines(test_content, "tests/level4_clinical.qmd")
  cat("✅ Created realistic clinical document\n")

  # Test parsing
  cat("🔧 Testing clinical document parsing...\n")
  result <- system("quarto pandoc tests/level4_clinical.qmd -t json -o tests/level4_clinical.json", show.output.on.console = FALSE)

  if (result == 0) {
    ast <- jsonlite::fromJSON("tests/level4_clinical.json", simplifyVector = FALSE)
    protected <- extract_protected_elements(ast)

    cat("✅ Clinical document parsing successful\n")
    cat("   Protected elements:", length(protected), "\n")
    cat("   Expected: 6 elements (4 params + 2 cross-refs)\n")

    if (length(protected) >= 6) {
      cat("✅ LEVEL 4 READY - CLINICAL DOCUMENT TEST\n")
      cat("📋 This simulates real medical writing workflow\n")
      return(TRUE)
    } else {
      cat("⚠️  Fewer protected elements than expected\n")
      debug_protected_elements(ast)
      return(FALSE)
    }
  } else {
    cat("❌ Clinical document parsing failed\n")
    return(FALSE)
  }
}

# =============================================================================
# DEBUGGING AND ANALYSIS FUNCTIONS
# =============================================================================

#' Debug protected elements detection
debug_protected_elements <- function(ast) {
  cat("\n🔍 DEBUGGING PROTECTED ELEMENTS\n")
  cat("==============================\n")

  for (i in seq_along(ast$blocks)) {
    block <- ast$blocks[[i]]
    if (!is.null(block$t) && block$t == "Para") {
      cat("Block", i, "(Para):\n")
      find_spans_in_block(block, i)
    }
  }
}

#' Find Span elements in a block for debugging
find_spans_in_block <- function(block, block_num) {
  if (!is.null(block$c)) {
    spans_found <- 0
    for (j in seq_along(block$c)) {
      item <- block$c[[j]]
      if (is.list(item) && !is.null(item$t) && item$t == "Span") {
        spans_found <- spans_found + 1
        cat("  Span", spans_found, ":\n")
        if (is.list(item$c) && length(item$c) >= 1) {
          attrs <- item$c[[1]]
          if (is.list(attrs) && length(attrs) >= 3) {
            custom_attrs <- attrs[[3]]
            cat("    Attributes:", toString(custom_attrs), "\n")
          }
        }
      }
    }
    if (spans_found == 0) {
      cat("  No Span elements found\n")
    }
  }
}

#' Analyze AST block structure
analyze_block_structure <- function(ast) {
  cat("\n📊 BLOCK STRUCTURE ANALYSIS:\n")

  block_types <- sapply(ast$blocks, function(b) if (!is.null(b$t)) b$t else "Unknown")
  block_counts <- table(block_types)

  for (type in names(block_counts)) {
    cat("  ", type, ":", block_counts[type], "\n")
  }
}

#' Check protection preservation for Level 1
check_protection_level_1 <- function(original_lines, updated_lines) {
  cat("🛡️  PROTECTION CHECK:\n")

  # Look for our protected patterns
  protected_patterns <- c(
    "\\[245\\]\\{custom-style=\"ProtectedParam\"\\}",
    "\\[drug ABC-123\\]\\{custom-style=\"ProtectedParam\"\\}",
    "\\[placebo\\]\\{custom-style=\"ProtectedParam\"\\}",
    "\\[Table 1\\]\\{custom-style=\"CrossReference\"\\}",
    "\\[Figure 2\\]\\{custom-style=\"CrossReference\"\\}"
  )

  for (pattern in protected_patterns) {
    orig_matches <- sum(grepl(pattern, original_lines))
    updated_matches <- sum(grepl(pattern, updated_lines))

    cat("  Pattern:", gsub("\\\\", "", pattern), "\n")
    cat("    Original:", orig_matches, "Updated:", updated_matches)

    if (orig_matches == updated_matches) {
      cat(" ✅\n")
    } else {
      cat(" ❌\n")
    }
  }
}

#' Find Quarto-specific elements
find_quarto_elements <- function(ast) {
  # Look for callouts, cross-references, etc.
  quarto_elements <- list()

  for (block in ast$blocks) {
    if (!is.null(block$c)) {
      # Look for @ references, callouts, etc.
      # This is a placeholder - we'll build this out as needed
    }
  }

  return(quarto_elements)
}

# =============================================================================
# MASTER TEST RUNNER
# =============================================================================

#' Run all tests in sequence
run_systematic_tests <- function() {
  cat("🚀 SYSTEMATIC QUARTOWORD TESTING\n")
  cat("================================\n\n")

  # Level 1: Multiple protected elements
  if (!test_level_1_multiple_protected()) {
    cat("❌ STOPPED: Level 1 failed\n")
    return(FALSE)
  }

  cat("\n⏸️  PAUSE: Complete Level 1 manual testing, then continue with:\n")
  cat("   test_level_2_multiple_paragraphs()\n")
  cat("   test_level_3_quarto_features()\n")
  cat("   test_level_4_clinical_document()\n")

  return(TRUE)
}

# Cleanup function
cleanup_systematic_tests <- function() {
  test_files <- c(
    "tests/level1_test.qmd", "tests/level1_test.json", "tests/level1_test.docx", "tests/level1_updated.qmd",
    "tests/level2_test.qmd", "tests/level2_test.json", "tests/level2_test.docx", "tests/level2_updated.qmd",
    "tests/level3_test.qmd", "tests/level3_test.json", "tests/level3_test.docx", "tests/level3_updated.qmd",
    "tests/level4_clinical.qmd", "tests/level4_clinical.json", "tests/level4_clinical.docx", "tests/level4_updated.qmd"
  )

  removed_count <- 0
  for (file in test_files) {
    if (file.exists(file)) {
      file.remove(file)
      removed_count <- removed_count + 1
    }
  }

  cat("🧹 Cleaned up", removed_count, "test files from tests/ directory\n")
}

cat("🧪 SYSTEMATIC TESTING FRAMEWORK LOADED\n")
cat("======================================\n")
cat("Start with: run_systematic_tests()\n")
cat("Then work through levels 1-4 step by step\n")
