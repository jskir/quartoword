# Create a test file with our markup patterns
test_content <- c(
  "---",
  "title: 'Test'",
  "format: docx",  # Add this line to specify Word output
  "---",
  "",
  "# Header",
  "",
  "Normal text with [protected content]{custom-style=\"ProtectedParam\"} here.",
  "",
  "Also test [{{< param compound >}}]{.protected-param} syntax."
)

writeLines(test_content, "test_parse.qmd")

# Now render to Word
system("quarto render test_parse.qmd")

# Check what files we have
list.files(pattern = "test_parse")


# Parse with Pandoc
system("pandoc test_parse.qmd -t json -o test_parse.json")

# Read the result
library(jsonlite)
ast <- fromJSON("test_parse.json", simplifyVector = FALSE)
str(ast, max.levels = 3)


# Check if pandoc is available and working
system("pandoc --version")

# Also check what files we have
list.files(pattern = "test_parse")

# Let's try a simpler pandoc test first
system("pandoc test_parse.qmd -t markdown")


# Try using Quarto's pandoc
system("quarto pandoc --version")

# If that works, try parsing with Quarto's pandoc
system("quarto pandoc test_parse.qmd -t json -o test_parse.json")

# Check if the file was created
list.files(pattern = "test_parse")

# Read the parsed JSON
library(jsonlite)
ast <- fromJSON("test_parse.json", simplifyVector = FALSE)

# Let's examine the structure
cat("Top level structure:\n")
str(ast, max.levels = 2)

cat("\n\nBlocks in the document:\n")
for (i in seq_along(ast$blocks)) {
  block <- ast$blocks[[i]]
  cat("Block", i, "- Type:", block$t, "\n")
  if (!is.null(block$c)) {
    cat("  Content preview:", substr(toString(block$c), 1, 100), "\n")
  }
}

# Extract protected elements from Pandoc AST
# Fixed version with better error handling
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

# Test the fixed version
protected <- extract_protected_elements(ast)
cat("Found", length(protected), "protected elements:\n")
for (i in seq_along(protected)) {
  cat("Element", i, "content:", toString(protected[[i]]$c), "\n")
}

# Extract editable text from AST (skipping protected elements)
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

# Test it
editable <- extract_editable_text(ast)
cat("Found", length(editable), "editable text segments:\n")
for (i in seq_along(editable)) {
  cat("Segment", i, ":", editable[[i]]$text, "\n")
}

# First, let's render our test file and see what Word gives us
system("quarto render test_parse.qmd")

# Extract from the rendered Word document
word_text <- extract_word_text("test_parse.docx")
cat("Word document paragraphs:\n")
for (i in seq_along(word_text)) {
  cat("Para", i, ":", word_text[i], "\n")
}

# Now let's combine our editable segments and see if they match
editable_combined <- paste(sapply(editable, function(x) x$text), collapse = " ")
cat("\nCombined editable text from QMD:", editable_combined, "\n")





# After you edit the Word file, run this:
edited_word_text <- extract_word_text("test_parse.docx")

cat("Edited Word paragraphs:\n")
for (i in seq_along(edited_word_text)) {
  cat("Para", i, ":", edited_word_text[i], "\n")
}

cat("\nOriginal editable segments:\n")
for (i in seq_along(editable)) {
  cat("Segment", i, ":", editable[[i]]$text, "\n")
}

cat("\nCombined original editable:", paste(sapply(editable, function(x) x$text), collapse = " "), "\n")

# Extract just the editable parts from the edited Word text
# For now, let's manually compare Para 3 and Para 4 (the content paragraphs)
cat("\nComparison:\n")
cat("Original Para 3: Normal text with protected content here.\n")
cat("Edited Para 3  :", edited_word_text[3], "\n")
cat("Original Para 4: Also test  syntax.\n")
cat("Edited Para 4  :", edited_word_text[4], "\n")




detect_changes_with_ast <- function(original_editable_segments, edited_word_text) {
  # Combine original editable text
  original_combined <- paste(sapply(original_editable_segments, function(x) x$text), collapse = " ")

  # Extract editable portions from Word text (skipping protected content)
  # For now, we'll extract from the content paragraphs (paragraphs 3 and 4)
  word_content_paras <- edited_word_text[3:4]

  cat("CHANGE DETECTION:\n")
  cat("Original editable:", original_combined, "\n")
  cat("Word paragraphs  :", paste(word_content_paras, collapse = " "), "\n")

  # Simple comparison for now
  if (original_combined != paste(word_content_paras, collapse = " ")) {
    cat("CHANGES DETECTED!\n")
    return(TRUE)
  } else {
    cat("No changes detected.\n")
    return(FALSE)
  }
}

# Test it
changes_found <- detect_changes_with_ast(editable, edited_word_text)







# Update AST with detected changes while preserving structure
update_ast_with_changes <- function(original_ast, original_editable_segments, edited_word_text) {

  cat("=== AST RECONSTRUCTION ENGINE ===\n")

  # Create a working copy of the AST (we don't want to modify the original)
  updated_ast <- original_ast

  # Extract the changed text from Word document
  # For our test case, we know content is in paragraphs 3 and 4
  word_content <- paste(edited_word_text[3:4], collapse = " ")

  cat("Original editable text:", paste(sapply(original_editable_segments, function(x) x$text), collapse = " "), "\n")
  cat("Word document text    :", word_content, "\n")

  # TODO: This is where we need to map the changes back to specific AST nodes
  # For now, let's identify what changed

  # Simple tokenization approach for demonstration
  original_words <- unlist(strsplit(paste(sapply(original_editable_segments, function(x) x$text), collapse = " "), "\\s+"))

  # Extract just editable words from Word text (skip protected content)
  # This is a simplified approach - we need to be more sophisticated
  word_words <- extract_editable_words_from_word_text(word_content)

  cat("Original words:", paste(original_words, collapse = ", "), "\n")
  cat("Word words    :", paste(word_words, collapse = ", "), "\n")

  # Find differences
  changes <- find_word_level_changes(original_words, word_words)

  if (length(changes) > 0) {
    cat("Changes detected:\n")
    for (change in changes) {
      cat("  ", change$original, "→", change$new, "\n")
    }
  }

  return(updated_ast)
}

# Helper function to extract editable words from Word text
extract_editable_words_from_word_text <- function(word_text) {
  # This is a placeholder - we need logic to skip protected content
  # For our test, we know "protected content" should be skipped

  # Remove known protected content
  cleaned_text <- gsub("protected content", "", word_text)
  # Remove parameter placeholders (they render as empty or specific values)
  cleaned_text <- gsub("\\{\\{<.*?>\\}\\}", "", cleaned_text)

  # Tokenize
  words <- unlist(strsplit(trimws(cleaned_text), "\\s+"))
  # Remove empty strings
  words[words != ""]
}

# Helper function to find word-level changes
find_word_level_changes <- function(original_words, new_words) {
  changes <- list()

  # Simple approach: find positions where words differ
  max_len <- max(length(original_words), length(new_words))

  for (i in 1:max_len) {
    orig_word <- if (i <= length(original_words)) original_words[i] else ""
    new_word <- if (i <= length(new_words)) new_words[i] else ""

    if (orig_word != new_word) {
      changes[[length(changes) + 1]] <- list(
        position = i,
        original = orig_word,
        new = new_word
      )
    }
  }

  return(changes)
}

# Test the AST updater
updated_ast <- update_ast_with_changes(ast, editable, edited_word_text)
