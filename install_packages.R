#!/usr/bin/env Rscript
# ============================================================
# Visium HD Viewer — dependency installer
#
# Installs every package the app needs. Safe to re-run: packages
# that are already present are skipped.
#
# Usage:
#   Rscript install_packages.R
# ...or from an R console:
#   source("install_packages.R")
# ============================================================

cran_packages <- c(
  "shiny",         # web app framework
  "bslib",         # Bootstrap theming
  "ggplot2",       # plotting
  "ggprism",       # GraphPad Prism themes & palettes
  "patchwork",     # multi-panel plot composition
  "DT",            # interactive metadata tables
  "viridis",       # colourblind-friendly continuous scales
  "RColorBrewer",  # Brewer categorical palettes
  "ggrepel",       # non-overlapping plot labels
  "scales",        # axis formatting / percent labels
  "dplyr",         # data wrangling
  "tidyr",         # long/wide reshaping
  "rstatix",       # statistical tests for plot annotation
  "shinycssloaders", # loading spinners on plots
  "shinyFiles",    # in-app file browser for picking .RDS files
  "Seurat"         # single-cell / spatial object handling
)

repo <- "https://cloud.r-project.org"

message("Checking ", length(cran_packages), " packages...\n")

installed <- rownames(installed.packages())
missing   <- setdiff(cran_packages, installed)

if (length(missing) == 0) {
  message("All packages already installed.")
} else {
  message("Installing ", length(missing), " missing package(s): ",
          paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = repo)
}

# ── Verify everything loads ─────────────────────────────────
message("\nVerifying installation...\n")

failed <- character(0)
for (p in cran_packages) {
  ok <- suppressWarnings(suppressPackageStartupMessages(
    requireNamespace(p, quietly = TRUE)
  ))
  if (ok) {
    message(sprintf("  [ok]   %-14s %s", p, as.character(packageVersion(p))))
  } else {
    message(sprintf("  [FAIL] %-14s could not be loaded", p))
    failed <- c(failed, p)
  }
}

if (length(failed) > 0) {
  message("\nThe following packages failed to install:\n  ",
          paste(failed, collapse = ", "),
          "\n\nSeurat sometimes needs system libraries. On macOS try:\n",
          "  brew install hdf5 gdal geos proj\n",
          "On Ubuntu/Debian try:\n",
          "  sudo apt-get install libhdf5-dev libgdal-dev libgeos-dev libproj-dev\n")
  quit(status = 1)
} else {
  message("\nAll dependencies ready. Launch the app with:\n",
          "  Rscript -e 'shiny::runApp(\"app.R\", launch.browser = TRUE)'\n")
}
