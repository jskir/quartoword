

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
