# Load all quartoword functions
source("R/ast_functions.R")
source("R/word_processing.R")
source("R/reconstruction.R")
source("R/verification.R")
source("R/systematic_tests.R")

# Required packages
library(jsonlite)
library(xml2)
library(officer)

cat("✅ Quartoword functions loaded!\n")
cat("Main function: production_qmd_reconstruction(qmd_path, docx_path, output_path)\n")
cat("Testing: run_systematic_tests()\n")
