#' Test function to read a simple text file
#' @param file_path Path to a text file
#' @return Character vector of file contents
test_read_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  readLines(file_path)
}

