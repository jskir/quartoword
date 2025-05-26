

#' protection.R - Visual protection and style handling for quartoword
#'
#' Functions for marking protected content with visual cues and
#' detecting changes to protected elements during round-trip editing.

#' Create a protected parameter test document
#' @param file_path Where to save the test document
#' @return TRUE if successful
create_protected_parameter_test <- function(file_path = "templates/test-docs/protected_params.qmd") {

  # Ensure directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  content <- c(
    "---",
    "title: 'Clinical Study Report - Protected Content Test'",
    "format:",
    "  docx:",
    "    reference-doc: templates/styles/clinical-review-template.docx",
    "params:",
    "  compound_name: 'ABC-123'",
    "  n_subjects: '245'",
    "  study_phase: 'Phase III'",
    "---",
    "",
    "::: {.reviewer-instructions}",
    "**REVIEWER GUIDANCE:** Content with blue backgrounds contains data placeholders and cannot be edited. Content with gray text contains cross-references. Please edit only normal black text. Questions? Contact the statistical programming team.",
    ":::",
    "",
    "# Executive Summary",
    "",
    "This [{{< param study_phase >}}]{.protected-param} study evaluated [{{< param compound_name >}}]{.protected-param} in [{{< param n_subjects >}}]{.protected-param} subjects with advanced disease.",
    "",
    "**EDITABLE:** Please review the following summary for medical accuracy and completeness:",
    "",
    "The study met its primary endpoint and demonstrated clinically meaningful benefit. The safety profile was consistent with the known profile of the drug class.",
    "",
    "# Study Design",
    "",
    "This was a randomized, double-blind, placebo-controlled study of [{{< param compound_name >}}]{.protected-param}.",
    "",
    "**EDITABLE:** Please add any important design considerations:",
    "",
    "The study design was appropriate for the research question and regulatory requirements.",
    "",
    "# Results",
    "",
    "Demographics are presented in [Table 1 - Demographics]{.cross-reference}.",
    "",
    "**EDITABLE:** Please provide clinical interpretation:",
    "",
    "The results support the efficacy of the treatment approach.",
    "",
    "Efficacy results are shown in [Figure 1 - Kaplan-Meier Plot]{.cross-reference}.",
    "",
    "**EDITABLE:** Please review and edit as needed:",
    "",
    "The treatment effect was statistically significant and clinically meaningful."
  )

  writeLines(content, file_path)
  cat("Created protected parameter test:", file_path, "\n")

  return(TRUE)
}

#' Detect changes to protected content in Word document
#' @param word_text Character vector from extract_word_text()
#' @param original_qmd_path Path to original QMD file
#' @return List of warnings about inappropriate edits
detect_protected_content_changes <- function(word_text, original_qmd_path) {
  # For now, a placeholder for the concept
  # Will implement after we test the visual protection

  warnings <- list()

  # TODO: Detect if protected content has been modified
  # - Look for missing style markers
  # - Check if parameter syntax has been altered
  # - Identify cross-references that have been changed

  return(warnings)
}

#' Create a Word style reference template
#' @param template_path Where to save the reference template
create_style_template <- function(template_path = "templates/styles/clinical-review-template.docx") {

  # Create a QMD that demonstrates all our custom styles
  style_demo_content <- c(
    "---",
    "title: 'Style Reference Template for Clinical Reviews'",
    "format: docx",
    "---",
    "",
    "# Style Reference Document",
    "",
    "This document defines the visual styles used in clinical document reviews.",
    "",
    "## Normal Text",
    "",
    "This is normal, editable text that reviewers should feel comfortable editing.",
    "",
    "## Protected Parameters",
    "",
    "Protected parameters should have a light blue background: [PROTECTED PARAMETER EXAMPLE]{custom-style=\"ProtectedParam\"}",
    "",
    "## Cross References",
    "",
    "Cross references should be gray and discourage editing: [Table 1 - Demographics]{custom-style=\"CrossReference\"}",
    "",
    "## Reviewer Instructions",
    "",
    "Instructions should be in a bordered box: [REVIEWER GUIDANCE: Edit only normal black text]{custom-style=\"ReviewerInstructions\"}",
    "",
    "## Included Content",
    "",
    "Content from included files should have a pale background: [This text came from an included file]{custom-style=\"IncludedContent\"}"
  )

  # Write temporary QMD for style creation
  temp_qmd <- "templates/styles/temp_style_demo.qmd"
  writeLines(style_demo_content, temp_qmd)

  cat("Created style demo QMD. Now:\n")
  cat("1. Render it: quarto render", temp_qmd, "\n")
  cat("2. Open the resulting .docx file\n")
  cat("3. Manually format each 'custom-style' section:\n")
  cat("   - ProtectedParam: Light blue background (RGB: 217, 230, 250)\n")
  cat("   - CrossReference: Gray text (RGB: 128, 128, 128)\n")
  cat("   - ReviewerInstructions: Border + light yellow background\n")
  cat("   - IncludedContent: Pale yellow background + italic\n")
  cat("4. Save the formatted document as:", template_path, "\n")
  cat("5. Delete the temp files\n")

  return(temp_qmd)
}

# Test our protected parameter document with the new style template
source("R/protection.R")
create_protected_parameter_test()

# Render with our custom styles
system("quarto render templates/test-docs/protected_params.qmd")


# Create a minimal test without parameters first
simple_protected_content <- c(
  "---",
  "title: 'Simple Protected Content Test'",
  "format: docx",
  "---",
  "",
  "# Test Document",
  "",
  "This is normal editable text.",
  "",
  "This is [PROTECTED CONTENT]{.protected-param} that should be highlighted.",
  "",
  "This is a [Cross Reference]{.cross-reference} that should be gray."
)

# Write and test
writeLines(simple_protected_content, "templates/test-docs/simple_protected.qmd")
system("quarto render templates/test-docs/simple_protected.qmd")

# Create a minimal working test without parameters
simple_test_content <- c(
  "---",
  "title: 'Simple Style Test'",
  "format: docx",
  "---",
  "",
  "Normal text here.",
  "",
  "This is [protected content]{.protected-param}.",
  "",
  "This is a [cross reference]{.cross-reference}."
)

writeLines(simple_test_content, "templates/test-docs/simple_style_test.qmd")
system("quarto render templates/test-docs/simple_style_test.qmd")

# Check what styles actually exist in your Word template
# We can create a test that uses the exact custom-style syntax

exact_style_test <- c(
  "---",
  "title: 'Exact Style Test'",
  "format:",
  "  docx:",
  "    reference-doc: templates/styles/clinical-review-template.docx",
  "---",
  "",
  "Normal text here.",
  "",
  "This is [protected content]{custom-style=\"ProtectedParam\"}.",
  "",
  "This is a [cross reference]{custom-style=\"CrossReference\"}.",
  "",
  "This is [reviewer guidance]{custom-style=\"ReviewerInstructions\"}."
)

writeLines(exact_style_test, "templates/test-docs/exact_style_test.qmd")
system("quarto render templates/test-docs/exact_style_test.qmd")

# Check what's in the styles directory
list.files("templates/styles", full.names = TRUE)

# Also check the current directory in case it saved elsewhere
list.files(pattern = "clinical-review-template.docx", recursive = TRUE)


# Test with basic Word highlighting that doesn't require custom styles
basic_highlight_test <- c(
  "---",
  "title: 'Basic Highlight Test'",
  "format: docx",
  "---",
  "",
  "Normal text.",
  "",
  "**Bold text** and *italic text*.",
  "",
  "Some `code text` that should look different."
)

writeLines(basic_highlight_test, "templates/test-docs/basic_test.qmd")
system("quarto render templates/test-docs/basic_test.qmd")


# Alternative: Use officer to inspect your template
if (requireNamespace("officer", quietly = TRUE)) {
  doc <- officer::read_docx("templates/styles/clinical-review-template.docx")
  styles_info <- officer::styles_info(doc)
  print(styles_info$style_name)  # Show all available style names
}

# Fix the path in the YAML to be relative to the QMD file location
exact_style_test_fixed <- c(
  "---",
  "title: 'Exact Style Test'",
  "format:",
  "  docx:",
  "    reference-doc: ../styles/clinical-review-template.docx",  # Go up one level, then to styles
  "---",
  "",
  "Normal text here.",
  "",
  "This is [protected content]{custom-style=\"ProtectedParam\"}.",
  "",
  "This is a [cross reference]{custom-style=\"CrossReference\"}.",
  "",
  "This is [reviewer guidance]{custom-style=\"ReviewerInstructions\"}."
)

writeLines(exact_style_test_fixed, "templates/test-docs/exact_style_test.qmd")
system("quarto render templates/test-docs/exact_style_test.qmd")

# First, let's see what our extraction picks up from the styled document
source("R/core.R")

# Extract text from the styled Word document
styled_word_text <- extract_word_text("templates/test-docs/exact_style_test.docx")

cat("Styled document contains", length(styled_word_text), "paragraphs:\n")
for (i in seq_along(styled_word_text)) {
  cat(i, ":", styled_word_text[i], "\n")
}

# Extract from the edited version
edited_styled_text <- extract_word_text("templates/test-docs/exact_style_test.docx")

cat("After editing:\n")
for (i in seq_along(edited_styled_text)) {
  cat(i, ":", edited_styled_text[i], "\n")
}

# Add to R/protection.R - a function that knows what should be protected
detect_inappropriate_edits <- function(original_qmd_path, edited_word_text) {

  # Read the original QMD to identify protected elements
  qmd_lines <- readLines(original_qmd_path)

  # Find lines with protected content markers
  protected_patterns <- c(
    "\\{custom-style=\"ProtectedParam\"\\}",
    "\\{custom-style=\"CrossReference\"\\}",
    "\\{\\{< param .*? >\\}\\}"  # Parameter placeholders
  )

  warnings <- list()

  # Extract just the text that should be protected
  for (line in qmd_lines) {
    # Look for protected content patterns
    if (grepl("\\{custom-style=\"ProtectedParam\"\\}", line)) {
      # Extract the text between brackets: [protected content]{custom-style="ProtectedParam"}
      protected_text <- gsub(".*\\[(.*)\\]\\{custom-style.*", "\\1", line)

      # Check if this text was modified in Word
      found_in_word <- any(grepl(protected_text, edited_word_text, fixed = TRUE))

      if (!found_in_word) {
        warnings[[length(warnings) + 1]] <- paste(
          "WARNING: Protected content may have been edited:", protected_text
        )
      }
    }
  }

  return(warnings)
}

# Test this detection
warnings <- detect_inappropriate_edits("templates/test-docs/exact_style_test.qmd", edited_styled_text)
if (length(warnings) > 0) {
  cat("⚠️  PROTECTION VIOLATIONS DETECTED:\n")
  for (w in warnings) {
    cat("  ", w, "\n")
  }
} else {
  cat("✅ No protection violations detected\n")
}

# Debug the detection step by step
original_qmd <- readLines("templates/test-docs/exact_style_test.qmd")
cat("Original QMD content:\n")
for (i in seq_along(original_qmd)) {
  cat(i, ":", original_qmd[i], "\n")
}

cat("\nLooking for protected content lines:\n")
for (line in original_qmd) {
  if (grepl("\\{custom-style=\"ProtectedParam\"\\}", line)) {
    cat("Found protected line:", line, "\n")

    # Extract the protected text
    protected_text <- gsub(".*\\[(.*)\\]\\{custom-style.*", "\\1", line)
    cat("Extracted protected text:", protected_text, "\n")

    # Check if it exists unchanged in Word
    found_exact <- any(grepl(protected_text, edited_styled_text, fixed = TRUE))
    cat("Found exact match in Word:", found_exact, "\n")

    # Check what we actually have in Word
    cat("Word text containing 'protected':\n")
    for (word_line in edited_styled_text) {
      if (grepl("protected", word_line, ignore.case = TRUE)) {
        cat("  Word:", word_line, "\n")
      }
    }
  }
}

# Fixed detection function
detect_inappropriate_edits_v2 <- function(original_qmd_path, edited_word_text) {
  qmd_lines <- readLines(original_qmd_path)
  warnings <- list()

  for (line in qmd_lines) {
    if (grepl("\\{custom-style=\"ProtectedParam\"\\}", line)) {
      # Extract the original protected text
      protected_text <- gsub(".*\\[(.*)\\]\\{custom-style.*", "\\1", line)

      # Find the corresponding line in Word document
      # Look for lines that contain this protected text
      word_matches <- edited_word_text[grepl(protected_text, edited_word_text, fixed = TRUE)]

      if (length(word_matches) > 0) {
        for (word_match in word_matches) {
          # Check if the Word version has EXTRA content beyond the original
          # Remove the original protected text and see what's left
          remaining_text <- gsub(protected_text, "", word_match, fixed = TRUE)

          # If there's significant remaining text (more than just punctuation/spaces)
          if (nzchar(trimws(gsub("[[:punct:][:space:]]", "", remaining_text)))) {
            warnings[[length(warnings) + 1]] <- paste(
              "⚠️  Protected content modified:",
              "\n    Original:", protected_text,
              "\n    In Word: ", word_match,
              "\n    Added text:", trimws(remaining_text)
            )
          }
        }
      }
    }
  }

  return(warnings)
}

# Test the corrected detection
warnings_v2 <- detect_inappropriate_edits_v2("templates/test-docs/exact_style_test.qmd", edited_styled_text)

if (length(warnings_v2) > 0) {
  cat("⚠️  PROTECTION VIOLATIONS DETECTED:\n")
  for (w in warnings_v2) {
    cat(w, "\n\n")
  }
} else {
  cat("✅ No protection violations detected\n")
}

# Enhanced workflow with protection checking
quartoword_update_with_protection <- function(qmd_path, word_path, backup = TRUE) {

  if (backup) {
    backup_path <- paste0(qmd_path, ".backup")
    file.copy(qmd_path, backup_path, overwrite = TRUE)
    cat("✅ Created backup:", backup_path, "\n")
  }

  cat("\n🔄 Starting quartoword round-trip with protection checking...\n\n")

  # STEP 0: Check for protection violations FIRST
  cat("🛡️  Checking for protection violations...\n")
  word_text <- extract_word_text(word_path)
  violations <- detect_inappropriate_edits_v2(qmd_path, word_text)

  if (length(violations) > 0) {
    cat("⚠️  PROTECTION VIOLATIONS FOUND:\n")
    for (v in violations) {
      cat(v, "\n\n")
    }
    cat("❓ Do you want to continue anyway? These edits will be LOST.\n")
    cat("   Type 'yes' to continue or anything else to abort: ")

    user_response <- readline()
    if (tolower(trimws(user_response)) != "yes") {
      cat("❌ Update cancelled. Please review protected content edits.\n")
      return(FALSE)
    }
  } else {
    cat("✅ No protection violations detected\n\n")
  }

  # Continue with normal workflow...
  cat("📖 Parsing QMD structure...\n")
  qmd_structure <- parse_qmd_structure(qmd_path)
  cat("   Found", length(qmd_structure), "components\n\n")

  # ... rest of workflow
  cat("🎯 Aligning and updating...\n")
  alignments <- align_word_to_qmd(word_text, qmd_structure)
  update_qmd_with_revisions(qmd_path, alignments)

  cat("\n🎉 Round-trip complete with protection checking!\n")
  return(TRUE)
}
