# Functions for AST parsing and manipulation
#extract_protected_elements()          # First function
#extract_editable_text()
#clean_text_for_matching()
#extract_editable_text_from_block()
#is_protected_span()                   # Last function (helper)

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
