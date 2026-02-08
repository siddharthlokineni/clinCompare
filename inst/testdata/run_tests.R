# ============================================================
# Run all clinCompare tests and save output to a text file
# Usage:
#   devtools::load_all()
#   source("inst/testdata/run_tests.R")
#
# Output saved to: inst/testdata/test_results_latest.txt
# ============================================================

output_file <- file.path("inst", "testdata", "test_results_latest.txt")

header <- paste0(
  "==============================================================\n",
  "  clinCompare -- Test Results\n",
  "  Run: ", Sys.time(), "\n",
  "  R version: ", R.version.string, "\n",
  "  Package version: ", as.character(packageVersion("clinCompare")), "\n",
  "==============================================================\n\n"
)

cat("Running tests and saving output to:", output_file, "\n")

# Helper: run a test file and capture both stdout and messages
.run_and_capture <- function(test_file) {
  out_file <- tempfile(fileext = ".txt")
  msg_file <- tempfile(fileext = ".txt")
  msg_con <- file(msg_file, open = "wt")

  sink(out_file, type = "output")
  sink(msg_con, type = "message")
  tryCatch(
    source(test_file),
    error = function(e) cat("\n*** ERROR:", conditionMessage(e), "***\n")
  )
  sink(type = "message")
  sink(type = "output")
  close(msg_con)

  list(
    output   = readLines(out_file, warn = FALSE),
    messages = readLines(msg_file, warn = FALSE)
  )
}

# --- test_clinCompare_full.R ---
cat("--- Running test_clinCompare_full.R ---\n")
full <- .run_and_capture("inst/testdata/test_clinCompare_full.R")

# --- test_pharmaverse.R ---
cat("--- Running test_pharmaverse.R ---\n")
pharma <- .run_and_capture("inst/testdata/test_pharmaverse.R")

# Combine and write
all_output <- c(
  header,
  "========== test_clinCompare_full.R ==========",
  "",
  full$output,
  "",
  "--- Messages ---",
  full$messages,
  "",
  "",
  "========== test_pharmaverse.R ==========",
  "",
  pharma$output,
  "",
  "--- Messages ---",
  pharma$messages
)

writeLines(all_output, output_file)
cat("\nResults saved to:", output_file, "\n")
cat("Total lines:", length(all_output), "\n")
