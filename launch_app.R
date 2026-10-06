#!/usr/bin/env Rscript
# ============================================================
# Visium HD Viewer — launcher
#
# Works from a terminal, RStudio, or any R console.
#
# Usage:
#   Rscript launch_app.R
# ...or from an R console:
#   source("launch_app.R")
# ============================================================

# Resolve this script's directory across all invocation methods
get_script_dir <- function() {
  # 1. Rscript launch_app.R  → parse --file= from the command line
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  # 2. source("launch_app.R") → sys.frames() carries the path
  frame_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(frame_file)) {
    return(dirname(normalizePath(frame_file)))
  }

  # 3. RStudio "Source" button → use the editor context if available
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path)))
    }
  }

  # 4. Fall back to the working directory
  getwd()
}

app_dir  <- get_script_dir()
app_file <- file.path(app_dir, "app.R")

if (!file.exists(app_file)) {
  stop("Could not find app.R in: ", app_dir,
       "\nRun this script from the repository directory.")
}

message("Starting Visium HD Viewer from: ", app_dir)

shiny::runApp(app_file, launch.browser = TRUE)
