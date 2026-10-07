library(shiny)
library(bslib)
library(Seurat)
library(ggplot2)
library(ggprism)
library(patchwork)
library(DT)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(dplyr)
library(tidyr)
library(rstatix)
library(shinycssloaders)
library(shinyFiles)

# Signac powers ATAC / multiome coverage tracks. It's optional — the app runs
# without it, and the Coverage tab shows a message if it's not installed.
HAS_SIGNAC <- requireNamespace("Signac", quietly = TRUE)
if (HAS_SIGNAC) suppressPackageStartupMessages(library(Signac))

# Wrap a plot output with a loading spinner — used on every plot so the user
# always sees that something is happening while a plot recomputes.
spin <- function(output) {
  shinycssloaders::withSpinner(output, type = 6, color = "#18BC9C", size = 0.8)
}

# ============================================================
# PALETTE DEFINITIONS  (top-level so server can access them)
# ============================================================

# 60-color project palette from the main analysis workflow
PROJECT_CUSTOM <- c(
  "#4876FF","#CD853F","#8B4513","#6B8E23","#708090","#8B008B","#2F4F4F",
  "#8B0000","#483D8B","#556B2F","#8B4726","#4682B4","#6A5ACD","#A0522D",
  "#5F9EA0","#9370DB","#BC8F8F","#B8860B","#3CB371","#7B68EE","#CD5C5C",
  "#4169E1","#D2691E","#6495ED","#DC143C","#008B8B","#B22222","#228B22",
  "#DAA520","#808000","#DB7093","#48D1CC","#C71585","#191970","#800000",
  "#BA55D3","#9932CC","#FF8C00","#E9967A","#8FBC8F","#00CED1","#9400D3",
  "#696969","#1E90FF","#B0C4DE","#FF6347","#40E0D0","#EE82EE","#FF4500",
  "#DA70D6","#87CEEB","#6A5ACD","#00FA9A","#87CEFA","#FFA07A","#32CD32",
  "#00FF7F","#7FFF00","#ADFF2F","#CAF178"
)

.gp <- function(nm) tryCatch(ggprism_data$colour_palettes[[nm]], error = function(e) NULL)

PALETTES <- list(
  "Project Custom (60)"  = PROJECT_CUSTOM,
  "Prism Dark"           = .gp("prism_dark"),
  "Prism Light"          = .gp("prism_light"),
  "Prism Dark 2"         = .gp("prism_dark2"),
  "Prism Light 2"        = .gp("prism_light2"),
  "Candy Bright"         = .gp("candy_bright"),
  "Candy Soft"           = .gp("candy_soft"),
  "Pastels"              = .gp("pastels"),
  "Warm Pastels"         = .gp("warm_pastels"),
  "Ocean"                = .gp("ocean"),
  "Flames"               = .gp("flames"),
  "Floral"               = .gp("floral"),
  "Muted Rainbow"        = .gp("muted_rainbow"),
  "Colorblind Safe"      = .gp("colorblind_safe"),
  "Autumn Leaves"        = .gp("autumn_leaves"),
  "Stained Glass"        = .gp("stained_glass"),
  "Warm & Sunny"         = .gp("warm_and_sunny"),
  "Starry"               = .gp("starry"),
  "Spring"               = .gp("spring"),
  "Summer"               = .gp("summer"),
  "Brewer Set1"          = brewer.pal(9,  "Set1"),
  "Brewer Set2"          = brewer.pal(8,  "Set2"),
  "Brewer Set3"          = brewer.pal(12, "Set3"),
  "Brewer Dark2"         = brewer.pal(8,  "Dark2"),
  "Brewer Paired"        = brewer.pal(12, "Paired")
)
PALETTES <- PALETTES[!sapply(PALETTES, is.null)]   # drop any that failed


# ============================================================
# HELPER FUNCTIONS
# ============================================================

# Sort factor levels numerically when all levels look like numbers (0,1,2…10,11)
# Otherwise falls back to alphabetical sort.
order_factor <- function(x) {
  x  <- as.character(x)
  lvls <- unique(x)
  nums <- suppressWarnings(as.numeric(lvls))
  sorted_lvls <- if (!any(is.na(nums))) lvls[order(nums)] else sort(lvls)
  factor(x, levels = sorted_lvls)
}

# Like order_factor(), but honours a user-supplied level order when given.
# Levels present in the data but absent from `custom_levels` are appended in
# numeric order, so nothing ever silently disappears from a plot.
ordered_factor <- function(x, custom_levels = NULL) {
  if (is.null(custom_levels) || length(custom_levels) == 0)
    return(order_factor(x))
  x       <- as.character(x)
  present <- unique(x)
  head    <- custom_levels[custom_levels %in% present]
  missing <- setdiff(present, custom_levels)
  if (length(missing) > 0) {
    nums    <- suppressWarnings(as.numeric(missing))
    missing <- if (!any(is.na(nums))) missing[order(nums)] else sort(missing)
  }
  factor(x, levels = c(head, missing))
}

# Return n named colours from a palette, extending via interpolation if needed.
# `palettes` defaults to the built-in list but the server passes its reactive
# store so user-uploaded palettes work too.
#
# Colours are assigned by each level's CANONICAL (numeric-aware) order, not by
# the order `levels_vec` happens to arrive in. This locks a cluster's colour to
# its identity: manual re-ordering of a plot changes the display order but never
# the colours, because the returned vector is keyed by name and applied with
# scale_*_manual(), which matches by name.
get_cat_colors <- function(pal_name, levels_vec, palettes = PALETTES) {
  canon <- levels(order_factor(levels_vec))   # stable, order-independent
  n     <- length(canon)
  cols  <- palettes[[pal_name]]
  if (is.null(cols)) cols <- hue_pal()(n)
  out   <- if (n <= length(cols)) cols[seq_len(n)] else colorRampPalette(cols)(n)
  setNames(out, canon)
}

# Parse a free-text blob of colours into a validated hex vector.
# Accepts hex codes (#RGB / #RRGGBB / #RRGGBBAA) and R colour names
# (e.g. "red", "steelblue"), separated by commas, whitespace, or newlines.
# Returns list(colors = <chr>, invalid = <chr>).
parse_colors <- function(text) {
  raw <- unlist(strsplit(text, "[,;\\s]+", perl = TRUE))
  raw <- trimws(raw)
  raw <- raw[nzchar(raw)]
  if (length(raw) == 0) return(list(colors = character(0), invalid = character(0)))

  is_hex  <- grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", raw)
  is_name <- tolower(raw) %in% tolower(grDevices::colors())
  valid   <- is_hex | is_name

  list(colors  = raw[valid],
       invalid = raw[!valid])
}

# Pull colours out of an uploaded file. For CSV/TSV, auto-detects the column
# with the most valid colours (handles annotation tables that have a "Color"
# column alongside other data). For plain text, one token per line/comma.
extract_colors_from_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("csv", "tsv", "txt")) {
    sep <- if (ext == "tsv") "\t" else if (ext == "csv") "," else ""
    df <- tryCatch(
      if (nzchar(sep))
        utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
      else
        NULL,
      error = function(e) NULL
    )
    if (!is.null(df) && ncol(df) >= 1) {
      # Score each column by how many entries are valid colours
      best <- NULL; best_n <- 0
      for (col in names(df)) {
        p <- parse_colors(paste(df[[col]], collapse = "\n"))
        if (length(p$colors) > best_n) { best <- p$colors; best_n <- length(p$colors) }
      }
      if (best_n > 0) return(best)
    }
    # Fall back to reading the whole file as free text
    return(parse_colors(paste(readLines(path, warn = FALSE), collapse = "\n"))$colors)
  }
  parse_colors(paste(readLines(path, warn = FALSE), collapse = "\n"))$colors
}

# Return a ggplot2 theme object (used with patchwork & operator).
get_theme_obj <- function(theme_name, base_size = 12) {
  switch(theme_name,
    prism   = theme_prism(base_size = base_size),
    classic = theme_classic(base_size = base_size),
    minimal = theme_minimal(base_size = base_size),
    bw      = theme_bw(base_size = base_size),
    theme()   # identity / no change
  )
}

# Add a ggplot2 theme to a single plot.
#
# A *complete* theme (theme_classic(), theme_prism(), ...) replaces every
# theme setting made before it, so tweaks added earlier in a plot's build
# (rotated axis labels, hidden legends) were silently discarded. We therefore
# save the plot's incomplete tweaks, apply the complete theme, then re-apply
# the tweaks on top. Plots that already carry a complete theme (Seurat's
# DimPlot/FeaturePlot) are left as before: the new theme simply replaces it.
apply_theme <- function(p, theme_name, base_size = 12) {
  prior <- p$theme
  p <- p + get_theme_obj(theme_name, base_size)
  if (length(prior) > 0 && !isTRUE(attr(prior, "complete"))) p <- p + prior
  p
}

# ── X-axis label rotation ─────────────────────────────────────
# "auto" turns labels vertical when there are many groups or long names,
# which is when 45° labels start to collide.
X_ANGLE_CHOICES <- c("Auto" = "auto", "45°" = "45", "90° (vertical)" = "90",
                     "0° (horizontal)" = "0")

x_label_angle <- function(choice, labels) {
  labels <- as.character(labels)
  n  <- length(labels)
  mx <- if (n > 0) max(nchar(labels), na.rm = TRUE) else 0
  if (is.null(choice) || identical(choice, "auto")) {
    if (n > 12 || mx > 10) 90 else 45
  } else as.numeric(choice)
}

x_label_theme <- function(choice, labels) {
  ang <- x_label_angle(choice, labels)
  if (ang >= 80)      theme(axis.text.x = element_text(angle = 90, hjust = 1,   vjust = 0.5))
  else if (ang <= 10) theme(axis.text.x = element_text(angle = 0,  hjust = 0.5, vjust = 1))
  else                theme(axis.text.x = element_text(angle = 45, hjust = 1,   vjust = 1))
}

# ── Assay layers (raw / log-normalized / scaled / SCT) ────────
# Which of counts / data / scale.data an assay has, with labels that say what
# the numbers actually are for that kind of assay.
LAYER_ORDER <- c("counts", "data", "scale.data")

assay_layer_names <- function(o, assay) {
  lyr <- tryCatch(Layers(o[[assay]]), error = function(e) character(0))
  keep <- vapply(LAYER_ORDER, function(l)
    any(lyr == l | startsWith(lyr, paste0(l, "."))), logical(1))
  LAYER_ORDER[keep]
}

assay_kind <- function(o, assay) {
  if (inherits(o[[assay]], "ChromatinAssay"))   "atac"
  else if (class(o[[assay]])[1] == "SCTAssay")  "sct"
  else                                          "rna"
}

layer_label <- function(o, assay, layer) {
  kind <- assay_kind(o, assay)
  switch(paste(kind, layer, sep = "|"),
    "rna|counts"        = "Raw counts",
    "rna|data"          = "Log-normalized",
    "rna|scale.data"    = "Scaled (z-score)",
    "sct|counts"        = "SCT corrected counts",
    "sct|data"          = "SCT log-normalized",
    "sct|scale.data"    = "SCT Pearson residuals (scaled)",
    "atac|counts"       = "Raw peak counts",
    "atac|data"         = "Normalized (e.g. TF-IDF)",
    "atac|scale.data"   = "Scaled",
    layer)
}

layer_choices <- function(o, assay) {
  ln <- assay_layer_names(o, assay)
  setNames(ln, vapply(ln, function(l) layer_label(o, assay, l), character(1)))
}

default_layer <- function(o, assay) {
  ln <- assay_layer_names(o, assay)
  if ("data" %in% ln) "data" else if (length(ln) > 0) ln[1] else "data"
}

# Normalized "data" layers are log1p-scaled; averaging for dot plots should be
# done on the natural scale (as Seurat::DotPlot does), counts are used as-is.
expm1_if_log <- function(x, layer) if (identical(layer, "data")) expm1(x) else x

# Y-axis title that states what is being plotted
layer_ylab <- function(o, assay, layer) {
  switch(layer,
    counts     = layer_label(o, assay, "counts"),
    data       = paste(layer_label(o, assay, "data"), "expression"),
    scale.data = if (assay_kind(o, assay) == "sct") "SCT Pearson residuals"
                 else "Scaled expression (z-score)",
    "Expression")
}

# genes (rows) x cells matrix for one assay layer. Handles split layers
# (counts.1, counts.2, ...) by joining them. For scale.data only the scaled
# (variable) features exist, so the caller is told which requested features
# were missing via the "missing" attribute.
get_layer_matrix <- function(o, assay, layer, features) {
  a    <- o[[assay]]
  lyr  <- Layers(a)
  hits <- lyr[lyr == layer | startsWith(lyr, paste0(layer, "."))]
  if (length(hits) == 0)
    stop(sprintf("Assay '%s' has no '%s' layer.", assay, layer))
  mats <- lapply(hits, function(l) LayerData(a, layer = l))
  rows <- Reduce(intersect, lapply(mats, rownames))
  feats <- intersect(features, rows)
  m <- do.call(cbind, lapply(mats, function(x) as.matrix(x[feats, , drop = FALSE])))
  m <- m[, match(colnames(o), colnames(m)), drop = FALSE]
  colnames(m) <- colnames(o)
  attr(m, "missing") <- setdiff(features, feats)
  m
}

# Numeric metadata columns — i.e. module scores (AddModuleScore), UCell scores,
# and QC metrics. These live in meta.data, not in the expression matrix, so they
# must be offered separately from genes.
get_numeric_meta <- function(o) {
  meta <- o@meta.data
  nm   <- names(meta)[vapply(meta, is.numeric, logical(1))]
  sort(nm)
}

# Categorical metadata columns suitable for grouping / splitting — factors,
# characters, or low-cardinality numerics. Columns with too many levels
# (e.g. a per-cell barcode) are excluded so split-by stays usable.
get_cat_meta <- function(o, max_levels = 30) {
  meta <- o@meta.data
  keep <- vapply(names(meta), function(col) {
    v <- meta[[col]]
    (is.factor(v) || is.character(v) || (is.numeric(v) && length(unique(v)) <= max_levels)) &&
      length(unique(v)) <= max_levels && length(unique(v)) >= 1
  }, logical(1))
  names(meta)[keep]
}

# ── Multiome / multi-modal assay helpers ──────────────────────
# Is an assay a Signac ChromatinAssay (ATAC)?
is_chromatin <- function(o, assay) inherits(o[[assay]], "ChromatinAssay")

# A human label for each assay, tagging its modality so users know what
# they're plotting (gene expression vs ATAC peaks vs other).
assay_choices <- function(o) {
  labs <- vapply(Assays(o), function(a) {
    cls <- class(o[[a]])[1]
    tag <- if (is_chromatin(o, a))            "ATAC / peaks"
           else if (cls == "SCTAssay")        "gene expr (SCT)"
           else if (grepl("^RNA$", a))        "gene expr"
           else if (grepl("activity", a, ignore.case = TRUE)) "gene activity"
           else if (grepl("chromvar", a, ignore.case = TRUE)) "motif activity"
           else                               "gene expr"
    sprintf("%s — %s", a, tag)
  }, character(1))
  setNames(Assays(o), labs)
}

# Prefer a gene-expression assay for the initial Active Assay, so users land
# on genes rather than hundreds of thousands of ATAC peaks.
default_expr_assay <- function(o) {
  assays <- Assays(o)
  gene_assays <- assays[!vapply(assays, function(a) is_chromatin(o, a), logical(1))]
  if ("SCT" %in% gene_assays) "SCT"
  else if ("RNA" %in% gene_assays) "RNA"
  else if (length(gene_assays) > 0) gene_assays[1]
  else DefaultAssay(o)
}

# Features of a given assay (genes for RNA/SCT, peaks for ATAC)
assay_features <- function(o, assay) rownames(o[[assay]])

# Sequential scales are for expression/UCell (bounded, all-positive).
# Diverging scales are centred at zero for AddModuleScore output, which is
# mean-centred against a control gene set and is routinely negative.
CONT_SCALE_CHOICES <- c(
  "Viridis"                  = "viridis",
  "Plasma"                   = "plasma",
  "YlOrRd"                   = "YlOrRd",
  "Blues"                    = "Blues",
  "Diverging: Blue-Red (0)"  = "div_bwr",
  "Diverging: Purple-Green (0)" = "div_pgr"
)

# Continuous fill scale for spatial plots
cont_fill_scale <- function(scale_name) {
  switch(scale_name,
    viridis = scale_fill_viridis_c(option = "viridis"),
    plasma  = scale_fill_viridis_c(option = "plasma"),
    YlOrRd  = scale_fill_distiller(palette = "YlOrRd", direction = 1),
    Blues   = scale_fill_distiller(palette = "Blues",  direction = 1),
    div_bwr = scale_fill_gradient2(low = "#2166AC", mid = "grey92",
                                   high = "#B2182B", midpoint = 0),
    div_pgr = scale_fill_gradient2(low = "#762A83", mid = "grey92",
                                   high = "#1B7837", midpoint = 0),
    scale_fill_viridis_c()
  )
}

# Continuous color scale for reduction plots
cont_color_scale <- function(scale_name) {
  switch(scale_name,
    viridis = scale_color_viridis_c(option = "viridis"),
    plasma  = scale_color_viridis_c(option = "plasma"),
    YlOrRd  = scale_color_distiller(palette = "YlOrRd", direction = 1),
    Blues   = scale_color_distiller(palette = "Blues",  direction = 1),
    div_bwr = scale_color_gradient2(low = "#2166AC", mid = "grey92",
                                    high = "#B2182B", midpoint = 0),
    div_pgr = scale_color_gradient2(low = "#762A83", mid = "grey92",
                                    high = "#1B7837", midpoint = 0),
    scale_color_viridis_c()
  )
}

# Symmetric limits around zero, so +0.5 and -0.5 read as equally intense.
# Without this a score spanning -0.2..2.0 makes every negative bin look
# identical, which misrepresents depletion.
symmetric_limits <- function(values) {
  m <- suppressWarnings(max(abs(range(values, na.rm = TRUE))))
  if (!is.finite(m) || m == 0) return(NULL)
  c(-m, m)
}

score_centered_fill <- function(values, scale_name) {
  lim <- symmetric_limits(values)
  if (is.null(lim)) return(cont_fill_scale(scale_name))
  switch(scale_name,
    div_bwr = scale_fill_gradient2(low = "#2166AC", mid = "grey92", high = "#B2182B",
                                   midpoint = 0, limits = lim),
    div_pgr = scale_fill_gradient2(low = "#762A83", mid = "grey92", high = "#1B7837",
                                   midpoint = 0, limits = lim),
    viridis = scale_fill_viridis_c(option = "viridis", limits = lim),
    plasma  = scale_fill_viridis_c(option = "plasma",  limits = lim),
    YlOrRd  = scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = lim),
    Blues   = scale_fill_distiller(palette = "Blues",  direction = 1, limits = lim),
    scale_fill_viridis_c(limits = lim)
  )
}

score_centered_color <- function(values, scale_name) {
  lim <- symmetric_limits(values)
  if (is.null(lim)) return(cont_color_scale(scale_name))
  switch(scale_name,
    div_bwr = scale_color_gradient2(low = "#2166AC", mid = "grey92", high = "#B2182B",
                                    midpoint = 0, limits = lim),
    div_pgr = scale_color_gradient2(low = "#762A83", mid = "grey92", high = "#1B7837",
                                    midpoint = 0, limits = lim),
    viridis = scale_color_viridis_c(option = "viridis", limits = lim),
    plasma  = scale_color_viridis_c(option = "plasma",  limits = lim),
    YlOrRd  = scale_color_distiller(palette = "YlOrRd", direction = 1, limits = lim),
    Blues   = scale_color_distiller(palette = "Blues",  direction = 1, limits = lim),
    scale_color_viridis_c(limits = lim)
  )
}

error_plot <- function(msg) {
  # Wrap long messages so they aren't clipped at the plot edges; explicit
  # newlines in the message are preserved.
  wrapped <- paste(
    vapply(strsplit(msg, "\n", fixed = TRUE)[[1]],
           function(l) if (nzchar(l)) paste(strwrap(l, width = 42), collapse = "\n") else "",
           character(1)),
    collapse = "\n")
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = wrapped, size = 4.5, color = "firebrick") +
    theme_void()
}

save_plot <- function(p, fmt, w, h, file) {
  if (fmt == "PDF") {
    pdf(file, width = w, height = h); print(p); dev.off()
  } else {
    png(file, width = w, height = h, units = "in", res = 150)
    print(p); dev.off()
  }
}

# Compute pairwise or vs-reference stats for a long-format expression data frame.
# Returns a stat_res data frame annotated with y positions for ggprism::add_pvalue(),
# or NULL on failure (a notification is shown to the user).
compute_stats <- function(df_long, lvls, input, session) {
  tryCatch({
    # Build comparison list (NULL = all pairwise)
    comps <- if (isTRUE(input$fe_stat_compare == "ref")) {
      ref <- input$fe_stat_ref
      lapply(setdiff(lvls, ref), function(g) c(ref, g))
    } else NULL

    gdf <- df_long %>% group_by(gene)

    raw <- switch(input$fe_stat_test,
      wilcox = gdf %>%
        wilcox_test(expr ~ group, comparisons = comps) %>%
        adjust_pvalue(method = input$fe_stat_padj),

      ttest  = gdf %>%
        t_test(expr ~ group, comparisons = comps) %>%
        adjust_pvalue(method = input$fe_stat_padj)
    )

    stat_res <- raw %>%
      add_significance() %>%
      add_xy_position(x = "group", scales = "free", step.increase = 0.1)

    # Pre-format p-value columns for readable on-plot labels.
    label_col <- input$fe_stat_label

    if (label_col %in% c("p", "p.adj")) {
      stat_res$label_fmt <- vapply(
        stat_res[[label_col]],
        function(v) if (is.na(v)) "ns" else scales::pvalue(v, accuracy = 0.001, add_p = TRUE),
        character(1)
      )
      attr(stat_res, "label_col") <- "label_fmt"
    } else {
      attr(stat_res, "label_col") <- "p.adj.signif"
    }

    stat_res

  }, error = function(e) {
    showNotification(paste("Statistics error:", conditionMessage(e)),
                     type = "warning", duration = 6, session = session)
    NULL
  })
}

# Reusable per-tab theme selector widget.
# Each tab gets its own input ID so themes can differ across plots.
theme_picker_ui <- function(id, selected = "classic") {
  selectInput(id, "Theme",
              choices  = c("Classic"       = "classic",
                           "Prism"         = "prism",
                           "Minimal"       = "minimal",
                           "Black & White" = "bw"),
              selected = selected)
}

# Reusable "custom group order" widget: a checkbox that reveals a
# drag-to-reorder list of the current grouping's levels. The selectize
# `drag_drop` plugin ships with Shiny, so no extra package is needed.
order_ui <- function(check_id, order_id, label = "Custom group order") {
  tagList(
    checkboxInput(check_id, label, FALSE),
    conditionalPanel(
      sprintf("input.%s", check_id),
      selectizeInput(order_id, "Drag to reorder",
                     choices = NULL, multiple = TRUE,
                     options = list(plugins = list("drag_drop"),
                                    placeholder = "levels appear here"))
    )
  )
}


# ============================================================
# UI
# ============================================================
ui <- page_sidebar(
  title = "SeuratScope",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#2C3E50",
    secondary  = "#18BC9C"
  ),

  sidebar = sidebar(
    width = 310,

    # ── Object loading ────────────────────────────────────────
    card(
      card_header(tagList(icon("dna"), " Load Seurat Object")),
      # In-app file browser (shinyFiles). Navigates the filesystem in a modal
      # and returns the chosen path — nothing is copied or uploaded, so this
      # works with objects far too large for a standard fileInput().
      shinyFiles::shinyFilesButton(
        "rds_file", "Choose .RDS File…",
        title = "Select a Seurat .RDS file",
        multiple = FALSE, icon = icon("folder-open"),
        class = "btn-primary w-100"),
      uiOutput("chosen_file"),
      # Fallback: paste a path directly (useful for remote/headless sessions)
      div(class = "text-muted small mt-3 mb-1", "or paste a path:"),
      textInput("rds_path", label = NULL,
                placeholder = "/full/path/to/object.rds"),
      actionButton("load_path", tagList(icon("upload"), " Load from path"),
                   class = "btn-outline-secondary btn-sm w-100"),
      uiOutput("obj_info")
    ),

    # ── Active assay ──────────────────────────────────────────
    conditionalPanel(
      "output.has_object",
      card(
        card_header(tooltip(
          tagList(icon("layer-group"), " Active Assay", icon("circle-info", class = "text-muted")),
          "Which assay's log-normalized data layer is plotted. Switch between e.g. Spatial.008um and sketch."
        )),
        selectInput("active_assay", label = NULL, choices = NULL)
      )
    ),

    # ── Global plot style ─────────────────────────────────────
    card(
      card_header("Plot Style"),
      selectInput("cat_palette", "Colour Palette",
                  choices  = names(PALETTES),
                  selected = "Project Custom (60)"),
      numericInput("base_size", "Base Font Size", 12, 8, 24, 1)
    ),

    # ── Custom palette ────────────────────────────────────────
    card(
      card_header("Add Custom Palette"),
      textInput("pal_name", "Palette name", placeholder = "My palette"),
      textAreaInput("pal_text", "Paste colours",
                    placeholder = "#E64B35, #4DBBD5, #00A087\nor: red, steelblue, gold",
                    height = "80px"),
      fileInput("pal_file", "…or upload a file",
                accept = c(".csv", ".tsv", ".txt"),
                buttonLabel = "Browse", placeholder = "CSV / TSV / TXT"),
      actionButton("pal_add", "Add Palette",
                   icon = icon("plus"), class = "btn-primary w-100"),
      uiOutput("pal_preview")
    )
  ),

  # ── Welcome / empty state (shown until an object is loaded) ──
  conditionalPanel(
    "!output.has_object",
    div(
      class = "text-center",
      style = "max-width:640px;margin:8vh auto;",
      div(icon("dna"), style = "font-size:64px;color:#18BC9C;margin-bottom:16px;"),
      h2("SeuratScope", class = "fw-bold"),
      p(class = "text-muted fs-5",
        "Explore any Seurat object — single-cell, spatial, or Visium HD — no coding required."),
      hr(),
      div(
        class = "text-start d-inline-block",
        style = "margin-top:8px;",
        p(tagList(tags$b("1."), " Click ",
                  tags$span(icon("folder-open"), " Choose .RDS File", class = "text-primary"),
                  " in the sidebar and pick your object.")),
        p(tagList(tags$b("2."), " It loads automatically — large objects take a minute.")),
        p(tagList(tags$b("3."), " Explore the tabs — only the ones your object supports appear: ",
                  "QC, UMAP, expression, spatial maps, composition, and metadata.")),
        p(class = "text-muted",
          icon("lock"), " Your file is read locally and never uploaded.")
      )
    )
  ),

  conditionalPanel(
    "output.has_object",
  navset_card_tab(
    id = "main_tabs",

    # ── QC ───────────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("heart-pulse"), "QC"), value = "qc",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          selectInput("qc_type", "Plot Type",
                      choices = c("Violin"  = "violin",
                                  "Scatter" = "scatter")),

          conditionalPanel("input.qc_type == 'violin'",
            selectizeInput("qc_metrics", "QC Metrics", choices = NULL,
                           multiple = TRUE,
                           options = list(placeholder = "select metrics…")),
            selectInput("qc_group", "Group By", choices = NULL),
            checkboxInput("qc_points", "Show points", FALSE)
          ),

          conditionalPanel("input.qc_type == 'scatter'",
            selectInput("qc_x",     "X axis", choices = NULL),
            selectInput("qc_y",     "Y axis", choices = NULL),
            selectInput("qc_color", "Color By", choices = NULL)
          ),

          theme_picker_ui("qc_theme"),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("qc_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("qc_w", "W", 10, 2, 30, 1)),
            column(3, numericInput("qc_h", "H",  6, 2, 30, 1))
          ),
          downloadButton("qc_dl", "Save", class = "btn-success w-100")
        ),
        spin(plotOutput("qc_plot", height = "620px"))
      )
    ),

    # ── SPATIAL ──────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("map"), "Spatial"), value = "spatial",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          selectInput("sp_type", "Plot Type",
                      choices = c("Clusters / Metadata"      = "dim",
                                  "Gene Expression"          = "feature",
                                  "Module / UCell Score"     = "score")),

          conditionalPanel("input.sp_type == 'dim'",
            selectInput("sp_color_by", "Color By", choices = NULL),
            checkboxInput("sp_label", "Show Labels", TRUE),
            numericInput("sp_label_size", "Label Size", 3, 1, 10, 0.5)
          ),

          conditionalPanel("input.sp_type == 'feature'",
            selectizeInput("sp_gene", "Gene", choices = NULL,
                           options = list(placeholder = "Type gene name...")),
            selectInput("sp_color_scale", "Color Scale",
                        choices = CONT_SCALE_CHOICES)
          ),

          conditionalPanel("input.sp_type == 'score'",
            selectizeInput("sp_score", "Module / UCell Score", choices = NULL,
                           options = list(placeholder = "Select a score...")),
            selectInput("sp_score_scale", "Color Scale",
                        choices  = CONT_SCALE_CHOICES,
                        selected = "viridis"),
            checkboxInput("sp_score_center",
                          "Center colour scale at 0", FALSE)
          ),

          selectInput("sp_image", "Image / Sample", choices = NULL),
          numericInput("sp_pt",    "Point Size", 1,   0.1, 10, 0.1),
          numericInput("sp_alpha", "Alpha",      1.0, 0.1,  1, 0.05),
          theme_picker_ui("sp_theme"),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("sp_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("sp_w", "W", 8, 2, 30, 1)),
            column(3, numericInput("sp_h", "H", 7, 2, 30, 1))
          ),
          downloadButton("sp_dl", "Save", class = "btn-success w-100")
        ),
        spin(plotOutput("sp_plot", height = "620px"))
      )
    ),

    # ── COVERAGE (ATAC / multiome) ───────────────────────────
    nav_panel(
      title = tagList(icon("chart-area"), "Coverage"), value = "coverage",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          selectInput("cv_assay", "ATAC assay", choices = NULL),
          textInput("cv_gene", "Gene or region",
                    placeholder = "e.g. Cd4  or  chr1-100-200"),
          selectInput("cv_group", "Group By", choices = NULL),
          numericInput("cv_up",   "Extend upstream (bp)",   2000, 0, 1e6, 500),
          numericInput("cv_down", "Extend downstream (bp)", 2000, 0, 1e6, 500),
          checkboxInput("cv_peaks", "Show peaks track", TRUE),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("cv_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("cv_w", "W", 10, 2, 30, 1)),
            column(3, numericInput("cv_h", "H",  7, 2, 30, 1))
          ),
          downloadButton("cv_dl", "Save", class = "btn-success w-100")
        ),
        spin(plotOutput("cv_plot", height = "620px"))
      )
    ),

    # ── REDUCTION / UMAP ─────────────────────────────────────
    nav_panel(
      title = tagList(icon("circle-dot"), "UMAP / Reduction"), value = "umap",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          selectInput("dr_red", "Reduction", choices = NULL),
          selectInput("dr_type", "Plot Type",
                      choices = c("Clusters / Metadata"    = "dim",
                                  "Gene Expression"        = "feature",
                                  "Module / UCell Score"   = "score",
                                  "Co-expression (2 genes)" = "coexp")),

          conditionalPanel("input.dr_type == 'dim'",
            selectInput("dr_color_by", "Color By", choices = NULL),
            checkboxInput("dr_label",  "Show Labels",  TRUE),
            checkboxInput("dr_repel",  "Repel Labels", TRUE)
          ),

          conditionalPanel("input.dr_type == 'feature'",
            selectizeInput("dr_gene", "Gene", choices = NULL,
                           options = list(placeholder = "Type gene name...")),
            selectInput("dr_color_scale", "Color Scale",
                        choices = CONT_SCALE_CHOICES)
          ),

          conditionalPanel("input.dr_type == 'score'",
            selectizeInput("dr_score", "Module / UCell Score", choices = NULL,
                           options = list(placeholder = "Select a score...")),
            selectInput("dr_score_scale", "Color Scale",
                        choices  = CONT_SCALE_CHOICES,
                        selected = "viridis"),
            checkboxInput("dr_score_center",
                          "Center colour scale at 0", FALSE)
          ),

          conditionalPanel("input.dr_type == 'coexp'",
            selectizeInput("dr_gene1", "Gene 1 (red)", choices = NULL,
                           options = list(placeholder = "First gene...")),
            selectizeInput("dr_gene2", "Gene 2 (green)", choices = NULL,
                           options = list(placeholder = "Second gene..."))
          ),

          numericInput("dr_pt", "Point Size", 0.5, 0.1, 5, 0.1),
          selectInput("dr_split", "Split by", choices = c("None" = "none")),
          theme_picker_ui("dr_theme"),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("dr_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("dr_w", "W", 8, 2, 30, 1)),
            column(3, numericInput("dr_h", "H", 7, 2, 30, 1))
          ),
          downloadButton("dr_dl", "Save", class = "btn-success w-100")
        ),
        spin(plotOutput("dr_plot", height = "620px"))
      )
    ),

    # ── FEATURE EXPRESSION ───────────────────────────────────
    nav_panel(
      title = tagList(icon("chart-bar"), "Feature Expression"), value = "feature",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          # Data source: which assay, and which layer of it (raw counts,
          # log-normalized, SCT, scaled...). Defaults follow the sidebar's
          # Active Assay but can be changed here independently.
          selectInput("fe_assay", "Assay", choices = NULL),
          selectInput("fe_layer", "Expression values", choices = NULL),
          conditionalPanel("input.fe_type == 'heatmap'",
            helpText(tags$small("Heatmaps always show z-scored values; raw counts are log-normalized first."))
          ),
          conditionalPanel("input.fe_type == 'dot'",
            helpText(tags$small("Dot plots need counts or normalized values (not scaled)."))
          ),
          selectizeInput("fe_genes", "Genes / Scores (one or more)",
                         choices  = NULL,
                         multiple = TRUE,
                         options  = list(placeholder = "Type gene or score name...")),
          helpText(tags$small("Includes genes and module/UCell scores.")),
          selectInput("fe_type", "Plot Type",
                      choices = c("Violin"   = "violin",
                                  "Dot Plot" = "dot",
                                  "Box Plot" = "box",
                                  "Heatmap"  = "heatmap")),
          selectInput("fe_group", "Group By", choices = NULL),
          selectInput("fe_split", "Split by", choices = c("None" = "none")),
          order_ui("fe_order_on", "fe_order"),

          # ── Statistics controls (violin / box only) ───────────
          hr(),
          checkboxInput("fe_show_stats", "Add statistics", FALSE),
          conditionalPanel(
            "input.fe_show_stats && input.fe_type != 'dot'",
            selectInput("fe_stat_test", "Test",
                        choices = c("Wilcoxon (non-parametric)" = "wilcox",
                                    "t-test (parametric)"       = "ttest"),
                        selected = "wilcox"),
            selectInput("fe_stat_label", "Show as",
                        choices = c("Significance stars" = "p.adj.signif",
                                    "Adjusted p-value"   = "p.adj",
                                    "Raw p-value"        = "p"),
                        selected = "p.adj.signif"),
            selectInput("fe_stat_compare", "Comparisons",
                        choices = c("All pairwise"       = "pairwise",
                                    "vs reference group" = "ref"),
                        selected = "pairwise"),
            conditionalPanel("input.fe_stat_compare == 'ref'",
              selectInput("fe_stat_ref", "Reference group", choices = NULL)
            ),
            selectInput("fe_stat_padj", "P-value adjustment",
                        choices = c("Benjamini-Hochberg" = "BH",
                                    "Bonferroni"         = "bonferroni",
                                    "None"               = "none"),
                        selected = "BH")
          ),

          theme_picker_ui("fe_theme"),
          selectInput("fe_xangle", "X-axis label angle",
                      choices = X_ANGLE_CHOICES, selected = "auto"),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("fe_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("fe_w", "W", 10, 2, 30, 1)),
            column(3, numericInput("fe_h", "H",  7, 2, 30, 1))
          ),
          downloadButton("fe_dl", "Save", class = "btn-success w-100")
        ),
        spin(plotOutput("fe_plot", height = "620px"))
      )
    ),

    # ── COMPOSITION ──────────────────────────────────────────
    nav_panel(
      title = tagList(icon("chart-pie"), "Composition"), value = "composition",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          selectInput("co_x",    "X Axis (group by)",    choices = NULL),
          selectInput("co_fill", "Fill (cell type / cluster)", choices = NULL),
          selectInput("co_type", "Plot Type",
                      choices = c("Proportion (stacked)" = "prop",
                                  "Count (stacked)"      = "count_stack",
                                  "Count (grouped)"      = "count_group")),
          selectInput("co_sort", "Sort X Axis",
                      choices = c("As-is"                 = "none",
                                  "Alphabetical"          = "alpha",
                                  "Total cells (desc)"    = "total_desc",
                                  "Total cells (asc)"     = "total_asc",
                                  "Custom order"          = "custom")),
          conditionalPanel("input.co_sort == 'custom'",
            selectizeInput("co_x_order", "Drag to reorder X axis",
                           choices = NULL, multiple = TRUE,
                           options = list(plugins = list("drag_drop"),
                                          placeholder = "levels appear here"))
          ),
          order_ui("co_fill_order_on", "co_fill_order", "Custom fill order"),
          checkboxInput("co_flip",  "Flip Coordinates", FALSE),
          checkboxInput("co_angle", "Rotate X Labels",  TRUE),
          theme_picker_ui("co_theme"),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("co_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("co_w", "W", 9, 2, 30, 1)),
            column(3, numericInput("co_h", "H", 6, 2, 30, 1))
          ),
          downloadButton("co_dl", "Save", class = "btn-success w-100")
        ),
        spin(plotOutput("co_plot", height = "580px"))
      )
    ),

    # ── METADATA ─────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("table"), "Metadata"), value = "metadata",

      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          selectInput("me_col", "Metadata Column", choices = NULL),
          selectInput("me_type", "View",
                      choices = c("Bar Chart"  = "bar",
                                  "Histogram"  = "hist",
                                  "Density"    = "density",
                                  "Data Table" = "table")),
          selectInput("me_group", "Group By (optional)",
                      choices = c("None" = "none")),
          theme_picker_ui("me_theme"),
          hr(),
          h6("Save Plot"),
          fluidRow(
            column(6, selectInput("me_fmt", NULL, choices = c("PNG", "PDF"))),
            column(3, numericInput("me_w", "W", 8, 2, 30, 1)),
            column(3, numericInput("me_h", "H", 6, 2, 30, 1))
          ),
          downloadButton("me_dl", "Save", class = "btn-success w-100")
        ),
        uiOutput("me_content")
      )
    )
  )
  )   # end conditionalPanel(output.has_object)
)


# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  rv <- reactiveValues(obj = NULL)

  # Reactive palette store: built-ins plus any the user adds this session.
  palettes_rv <- reactiveVal(PALETTES)

  # Server-side wrapper so every plot resolves colours from the live store.
  cat_colors <- function(levels_vec) {
    get_cat_colors(input$cat_palette, levels_vec, palettes_rv())
  }

  # ── Add custom palette (from pasted text and/or uploaded file) ──
  observeEvent(input$pal_add, {
    nm <- trimws(input$pal_name)
    if (!nzchar(nm)) {
      showNotification("Give the palette a name first.", type = "warning"); return()
    }
    if (nm %in% names(PALETTES)) {
      showNotification("That name matches a built-in palette. Pick another.",
                       type = "warning"); return()
    }

    # Collect colours from whichever inputs were used
    cols <- character(0)
    if (nzchar(trimws(input$pal_text %||% ""))) {
      p <- parse_colors(input$pal_text)
      cols <- c(cols, p$colors)
      if (length(p$invalid) > 0)
        showNotification(paste("Skipped invalid entries:",
                               paste(p$invalid, collapse = ", ")),
                         type = "warning", duration = 6)
    }
    if (!is.null(input$pal_file)) {
      file_cols <- tryCatch(
        extract_colors_from_file(input$pal_file$datapath, input$pal_file$name),
        error = function(e) { showNotification(paste("File error:", conditionMessage(e)),
                                               type = "error"); character(0) }
      )
      cols <- c(cols, file_cols)
    }
    cols <- unique(cols)

    if (length(cols) == 0) {
      showNotification("No valid colours found. Use hex codes (#RRGGBB) or R colour names.",
                       type = "error"); return()
    }

    # Register and select it
    store <- palettes_rv()
    store[[nm]] <- cols
    palettes_rv(store)
    updateSelectInput(session, "cat_palette",
                      choices = names(store), selected = nm)
    showNotification(sprintf("Added palette '%s' (%d colours).", nm, length(cols)),
                     type = "message")
  })

  # Live swatch preview of the colours currently entered
  output$pal_preview <- renderUI({
    txt <- input$pal_text %||% ""
    cols <- parse_colors(txt)$colors
    if (!is.null(input$pal_file)) {
      cols <- c(cols, tryCatch(
        extract_colors_from_file(input$pal_file$datapath, input$pal_file$name),
        error = function(e) character(0)))
    }
    cols <- unique(cols)
    if (length(cols) == 0) return(NULL)
    swatches <- lapply(cols, function(c) {
      tags$span(style = sprintf(
        "display:inline-block;width:16px;height:16px;margin:2px;border-radius:3px;border:1px solid #ccc;background:%s;", c))
    })
    tagList(tags$div(style = "margin-top:8px;",
                     tags$small(sprintf("%d colour(s):", length(cols))),
                     tags$div(swatches)))
  })

  # ── In-app file browser (shinyFiles) ───────────────────────
  # Roots the browser can navigate: the user's home folder, mounted volumes,
  # and the filesystem root. Returns a path only — no file is ever copied.
  volumes <- c(Home = fs::path_home(),
               shinyFiles::getVolumes()(),
               Root = "/")
  shinyFiles::shinyFileChoose(input, "rds_file", roots = volumes,
                              filetypes = c("rds", "RDS", "Rds"))

  # Path chosen via the file browser
  chosen_path <- reactive({
    req(input$rds_file)
    sel <- shinyFiles::parseFilePaths(volumes, input$rds_file)
    if (nrow(sel) == 0) return(NULL)
    as.character(sel$datapath[[1]])
  })

  # Auto-load as soon as a file is picked in the browser (one action, no
  # separate "Load" click), and also support the paste-a-path fallback.
  observeEvent(chosen_path(),   { load_object(chosen_path()) },        ignoreInit = TRUE)
  observeEvent(input$load_path, { load_object(trimws(input$rds_path)) }, ignoreInit = TRUE)

  # Show which file is currently selected
  output$chosen_file <- renderUI({
    p <- chosen_path()
    if (is.null(p)) return(NULL)
    div(class = "small text-muted mt-2 text-truncate",
        title = p, icon("file"), " ", basename(p))
  })

  # ── Load an object from a path (shared by both entry points) ──
  load_object <- function(path) {
    if (is.null(path) || !nzchar(path)) {
      showNotification("Choose a file or paste a path first.", type = "warning"); return()
    }
    if (!file.exists(path)) {
      showNotification("File not found — check the path.", type = "error"); return()
    }
    if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
      showNotification("That doesn't look like an .RDS file.", type = "warning")
    }

    withProgress(message = "Loading Seurat object", value = 0, {
      incProgress(0.1, detail = "Reading file from disk…")
      o <- tryCatch(readRDS(path), error = function(e) e)

      if (inherits(o, "error")) {
        showNotification(paste("Error:", conditionMessage(o)), type = "error"); return()
      }
      if (!inherits(o, "Seurat")) {
        showNotification("That file is not a Seurat object.", type = "error"); return()
      }

      incProgress(0.7, detail = "Preparing controls…")
      rv$obj <- o
      populate_controls(o)
      adapt_ui(o)
      incProgress(0.2, detail = "Done")
    })
    showNotification(
      tagList(icon("circle-check"), sprintf(" Loaded: %s × %s genes.",
              format(ncol(rv$obj), big.mark = ","), format(nrow(rv$obj), big.mark = ","))),
      type = "message", duration = 5)
  }

  # ── Adaptive UI — show only the tabs this object supports ──────
  # Spatial needs images; UMAP needs a reduction. Hidden tabs are removed
  # from the bar, and the most relevant tab is selected as the landing view.
  adapt_ui <- function(o) {
    has_images <- length(o@images)     > 0
    has_red    <- length(o@reductions) > 0
    has_atac   <- any(vapply(Assays(o), function(a) is_chromatin(o, a), logical(1)))

    if (has_images) nav_show("main_tabs", "spatial")
    else            nav_hide("main_tabs", "spatial")

    if (has_red) nav_show("main_tabs", "umap")
    else         nav_hide("main_tabs", "umap")

    # Coverage is ATAC-only (needs a ChromatinAssay)
    if (has_atac) nav_show("main_tabs", "coverage")
    else          nav_hide("main_tabs", "coverage")

    # Landing tab: spatial for tissue data, else UMAP, else expression
    landing <- if (has_images) "spatial" else if (has_red) "umap" else "feature"
    nav_select("main_tabs", landing)
  }

  # Flag used by conditionalPanels to reveal the tabs once an object is present
  output$has_object <- reactive(!is.null(rv$obj))
  outputOptions(output, "has_object", suspendWhenHidden = FALSE)

  # ── Populate UI Controls ───────────────────────────────────
  populate_controls <- function(o) {
    assays     <- Assays(o)
    meta_cols  <- colnames(o@meta.data)
    images     <- names(o@images)
    reductions <- names(o@reductions)

    # Default to a gene-expression assay (not ATAC peaks), and list that
    # assay's features. Switching Active Assay later refreshes these.
    def_assay <- default_expr_assay(o)
    DefaultAssay(rv$obj) <- def_assay
    features  <- assay_features(o, def_assay)

    def_clust <- if ("seurat_clusters" %in% meta_cols) "seurat_clusters" else meta_cols[1]
    def_red   <- if ("wnn.umap"    %in% reductions) "wnn.umap"
                 else if ("umap.sketch" %in% reductions) "umap.sketch"
                 else if ("umap"   %in% reductions) "umap"
                 else if (length(reductions) > 0)   reductions[1]
                 else NULL
    def_ident <- if ("orig.ident" %in% meta_cols) "orig.ident" else meta_cols[1]

    # Module / UCell scores and other numeric metadata
    scores    <- get_numeric_meta(o)
    def_score <- if (length(scores) > 0) scores[1] else NULL

    # QC metrics — numeric columns matching the usual QC names come first
    # (RNA + ATAC/multiome: TSS enrichment, nucleosome signal, FRiP, doublets)
    qc_pat    <- "^nCount|^nFeature|percent|pct_|mito|ribo|doublet|\\.score|TSS|nucleosome|FRiP|blacklist|atac"
    qc_hits   <- scores[grepl(qc_pat, scores, ignore.case = TRUE)]
    # Pick the most informative QC metrics first (RNA depth/complexity, % mito,
    # then ATAC quality) rather than whatever sorts first alphabetically.
    qc_priority <- c("nCount_RNA", "nFeature_RNA", "percent.mt", "percent.mito",
                     "TSS.enrichment", "nucleosome_signal", "nCount_ATAC", "FRiP")
    qc_pref   <- qc_priority[qc_priority %in% qc_hits]
    qc_def    <- head(unique(c(qc_pref, qc_hits)), if (length(qc_pref) > 0) length(qc_pref) else 3)
    qc_def    <- head(qc_def, 5)
    def_x     <- grep("^nCount",   scores, value = TRUE)[1]
    def_y     <- grep("^nFeature", scores, value = TRUE)[1]
    if (is.na(def_x)) def_x <- scores[1]
    if (is.na(def_y)) def_y <- if (length(scores) > 1) scores[2] else scores[1]

    updateSelectInput(session, "active_assay", choices = assay_choices(o), selected = def_assay)

    # QC
    updateSelectizeInput(session, "qc_metrics", choices = scores, selected = qc_def,
                         server = TRUE)
    updateSelectInput(session, "qc_group", choices = meta_cols, selected = def_ident)
    updateSelectInput(session, "qc_x",     choices = scores, selected = def_x)
    updateSelectInput(session, "qc_y",     choices = scores, selected = def_y)
    updateSelectInput(session, "qc_color", choices = c("Identity" = "none", meta_cols),
                      selected = def_clust)

    # Coverage (ATAC) — list ChromatinAssays, group by a categorical column
    atac_assays <- assays[vapply(assays, function(a) is_chromatin(o, a), logical(1))]
    updateSelectInput(session, "cv_assay",
                      choices = if (length(atac_assays) > 0) atac_assays else c("(no ATAC assay)" = ""))
    # Clusters often exceed the default 30-level cap (e.g. 39 WNN clusters),
    # and they're the most natural thing to group coverage tracks by.
    cv_cols <- get_cat_meta(o, max_levels = 80)
    updateSelectInput(session, "cv_group", choices = cv_cols,
                      selected = if (def_clust %in% cv_cols) def_clust else cv_cols[1])

    # Spatial
    updateSelectInput(session, "sp_color_by", choices = meta_cols, selected = def_clust)
    updateSelectInput(session, "sp_image",
                      choices = if (length(images) > 0) images else c("(no images)" = "NONE"))
    updateSelectizeInput(session, "sp_gene", choices = features, server = TRUE)
    updateSelectizeInput(session, "sp_score",
                         choices  = if (length(scores) > 0) scores else c("(no numeric metadata)" = ""),
                         selected = def_score, server = TRUE)

    # Reduction
    updateSelectInput(session, "dr_red",
                      choices = if (length(reductions) > 0) reductions else c("(none)" = "NONE"),
                      selected = def_red)
    updateSelectInput(session, "dr_color_by", choices = meta_cols, selected = def_clust)
    updateSelectizeInput(session, "dr_gene",  choices = features, server = TRUE)
    updateSelectizeInput(session, "dr_gene1", choices = features, server = TRUE)
    updateSelectizeInput(session, "dr_gene2", choices = features, server = TRUE)
    updateSelectizeInput(session, "dr_score",
                         choices  = if (length(scores) > 0) scores else c("(no numeric metadata)" = ""),
                         selected = def_score, server = TRUE)
    # Split-by: categorical columns only (plus None)
    cat_meta <- get_cat_meta(o)
    updateSelectInput(session, "dr_split", choices = c("None" = "none", cat_meta),
                      selected = "none")

    # Feature expression — genes and scores in one list, scores grouped first
    # so they're easy to find among tens of thousands of gene names.
    fe_choices <- if (length(scores) > 0) {
      list("Module / UCell scores" = as.list(scores),
           "Genes"                 = as.list(features))
    } else {
      list("Genes" = as.list(features))
    }
    updateSelectizeInput(session, "fe_genes", choices = fe_choices, server = TRUE)
    # Data source for the expression plots: assay + layer
    updateSelectInput(session, "fe_assay", choices = assay_choices(o), selected = def_assay)
    updateSelectInput(session, "fe_layer", choices = layer_choices(o, def_assay),
                      selected = default_layer(o, def_assay))
    updateSelectInput(session, "fe_group",    choices = meta_cols, selected = def_clust)
    updateSelectInput(session, "fe_split",    choices = c("None" = "none", cat_meta),
                      selected = "none")
    # Initialise reference-group choices from the default cluster column
    def_clust_lvls <- levels(order_factor(o@meta.data[[def_clust]]))
    updateSelectInput(session, "fe_stat_ref", choices = def_clust_lvls,
                      selected = def_clust_lvls[1])

    # Composition
    updateSelectInput(session, "co_x",    choices = meta_cols, selected = def_ident)
    updateSelectInput(session, "co_fill", choices = meta_cols, selected = def_clust)

    # Metadata
    updateSelectInput(session, "me_col",   choices = meta_cols, selected = meta_cols[1])
    updateSelectInput(session, "me_group", choices = c("None" = "none", meta_cols))
  }

  observeEvent(input$active_assay, {
    o <- rv$obj
    req(o, input$active_assay, input$active_assay %in% Assays(o))
    DefaultAssay(rv$obj) <- input$active_assay

    # Refresh feature selectors to the new assay's features (genes vs peaks).
    # Essential for multiome, where switching RNA ↔ ATAC changes the feature set.
    feats  <- assay_features(o, input$active_assay)
    scores <- get_numeric_meta(o)
    updateSelectizeInput(session, "sp_gene",  choices = feats, server = TRUE)
    updateSelectizeInput(session, "dr_gene",  choices = feats, server = TRUE)
    updateSelectizeInput(session, "dr_gene1", choices = feats, server = TRUE)
    updateSelectizeInput(session, "dr_gene2", choices = feats, server = TRUE)
    # The Feature Expression tab follows the global assay by default; its own
    # observer (below) then refreshes that tab's layers and gene list.
    updateSelectInput(session, "fe_assay", selected = input$active_assay)
  }, ignoreInit = TRUE)

  # Feature Expression: the tab's own assay choice. Refreshes which layers are
  # offered and the searchable gene/peak list, keeping any selected genes that
  # also exist in the new assay.
  observeEvent(input$fe_assay, {
    o <- rv$obj
    req(o, input$fe_assay, input$fe_assay %in% Assays(o))
    a <- input$fe_assay

    cur_layer <- isolate(input$fe_layer)
    lc <- layer_choices(o, a)
    updateSelectInput(session, "fe_layer", choices = lc,
                      selected = if (!is.null(cur_layer) && cur_layer %in% lc) cur_layer
                                 else default_layer(o, a))

    feats  <- assay_features(o, a)
    scores <- get_numeric_meta(o)
    kind   <- if (is_chromatin(o, a)) "Peaks" else "Genes"
    ch <- if (length(scores) > 0)
      setNames(list(as.list(scores), as.list(feats)), c("Module / UCell scores", kind))
    else setNames(list(as.list(feats)), kind)
    keep <- intersect(isolate(input$fe_genes), c(feats, scores))
    updateSelectizeInput(session, "fe_genes", choices = ch, selected = keep, server = TRUE)
  }, ignoreInit = TRUE)

  # Keep reference-group + reorder choices in sync with the Group By selector
  observeEvent(input$fe_group, {
    o <- rv$obj
    req(o, input$fe_group, input$fe_group %in% colnames(o@meta.data))
    lvls <- levels(order_factor(o@meta.data[[input$fe_group]]))
    updateSelectInput(session, "fe_stat_ref", choices = lvls, selected = lvls[1])
    updateSelectizeInput(session, "fe_order", choices = lvls, selected = lvls,
                         server = TRUE)
  })

  # Keep Composition reorder lists in sync with their selectors
  observeEvent(input$co_fill, {
    o <- rv$obj
    req(o, input$co_fill, input$co_fill %in% colnames(o@meta.data))
    lvls <- levels(order_factor(o@meta.data[[input$co_fill]]))
    updateSelectizeInput(session, "co_fill_order", choices = lvls, selected = lvls,
                         server = TRUE)
  })
  observeEvent(input$co_x, {
    o <- rv$obj
    req(o, input$co_x, input$co_x %in% colnames(o@meta.data))
    lvls <- levels(order_factor(o@meta.data[[input$co_x]]))
    updateSelectizeInput(session, "co_x_order", choices = lvls, selected = lvls,
                         server = TRUE)
  })

  # ── Object Info ────────────────────────────────────────────
  output$obj_info <- renderUI({
    o <- rv$obj
    if (is.null(o)) return(NULL)

    assays     <- Assays(o)
    has_images <- length(o@images) > 0

    # Detect Visium HD bin size(s) from assay names (e.g. "Spatial.008um")
    bin_sizes <- character(0)
    for (a in assays) {
      m <- regmatches(a, regexpr("(?i)(?<=\\.)0*(\\d+)um", a, perl = TRUE))
      if (length(m) > 0) {
        num <- as.integer(sub("(?i)um$", "", m[[1]], perl = TRUE))
        bin_sizes <- c(bin_sizes, paste0(num, " µm"))
      }
    }
    bin_sizes    <- unique(bin_sizes)
    is_visium_hd <- length(bin_sizes) > 0

    # Visium HD → "bins", standard Visium → "spots", scRNA-seq → "cells"
    spot_label <- if (is_visium_hd) "bins" else if (has_images) "spots" else "cells"

    stat_row <- function(ic, label, value) {
      div(class = "d-flex align-items-center gap-2 mb-1",
          icon(ic, class = "text-secondary", style = "width:16px;"),
          span(class = "text-muted small", label, ":"),
          span(class = "fw-semibold small ms-auto text-end", value))
    }

    div(
      class = "mt-3 p-2 rounded",
      style = "background:rgba(24,188,156,0.07);",
      if (is_visium_hd) stat_row("border-all", "Binning", paste(bin_sizes, collapse = ", ")),
      stat_row("table-cells", tools::toTitleCase(spot_label), format(ncol(o), big.mark = ",")),
      stat_row("dna",
               if (is_chromatin(o, DefaultAssay(o))) "Peaks" else "Genes",
               format(nrow(o), big.mark = ",")),
      stat_row("layer-group", "Assays",  paste(assays, collapse = ", ")),
      if (has_images)
        stat_row("image", "Images", paste(names(o@images), collapse = ", ")),
      if (length(o@reductions) > 0)
        stat_row("circle-nodes", "Reductions", paste(names(o@reductions), collapse = ", "))
    )
  })


  # ============================================================
  # QC TAB
  # ============================================================
  qc_plot_r <- reactive({
    o <- rv$obj
    req(o)

    if (input$qc_type == "violin") {
      req(input$qc_metrics, length(input$qc_metrics) > 0, input$qc_group)
      grp <- input$qc_group
      tryCatch({
        o@meta.data[[grp]] <- order_factor(o@meta.data[[grp]])
        lvls <- levels(o@meta.data[[grp]])
        cols <- cat_colors(lvls)
        p <- suppressWarnings(
          VlnPlot(o, features = input$qc_metrics, group.by = grp,
                  pt.size = if (isTRUE(input$qc_points)) 0.1 else 0,
                  cols = cols, ncol = min(length(input$qc_metrics), 3))
        )
        p & get_theme_obj(input$qc_theme, input$base_size) &
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")
      }, error = function(e) error_plot(conditionMessage(e)))

    } else {
      req(input$qc_x, input$qc_y)
      tryCatch({
        grp <- if (isTRUE(input$qc_color == "none")) NULL else input$qc_color
        if (!is.null(grp)) o@meta.data[[grp]] <- order_factor(o@meta.data[[grp]])
        p <- FeatureScatter(o, feature1 = input$qc_x, feature2 = input$qc_y,
                            group.by = grp, pt.size = 0.4)
        if (!is.null(grp)) {
          lvls <- levels(o@meta.data[[grp]])
          p <- p + scale_color_manual(values = cat_colors(lvls))
        }
        apply_theme(p, input$qc_theme, input$base_size)
      }, error = function(e) error_plot(conditionMessage(e)))
    }
  })

  output$qc_plot <- renderPlot({ qc_plot_r() })
  output$qc_dl   <- downloadHandler(
    filename = function() paste0("qc_plot.", tolower(input$qc_fmt)),
    content  = function(file) save_plot(qc_plot_r(), input$qc_fmt, input$qc_w, input$qc_h, file)
  )


  # ============================================================
  # COVERAGE TAB (ATAC / multiome)
  # ============================================================
  cv_plot_r <- reactive({
    o <- rv$obj
    req(o)

    if (!HAS_SIGNAC)
      return(error_plot("Signac is not installed.\nInstall it to view ATAC coverage tracks:\n  BiocManager::install(\"Signac\")"))

    assay <- input$cv_assay
    if (is.null(assay) || !nzchar(assay) || !assay %in% Assays(o) || !is_chromatin(o, assay))
      return(error_plot("No ATAC (ChromatinAssay) in this object."))

    region <- trimws(input$cv_gene %||% "")
    if (!nzchar(region))
      return(error_plot("Enter a gene symbol (e.g. Cd4) or a region (chr1-100-200)."))
    req(input$cv_group)

    # Coverage needs fragment files; this object may reference cluster paths
    frags   <- tryCatch(Signac::Fragments(o[[assay]]), error = function(e) list())
    frag_ok <- length(frags) > 0 && any(vapply(frags, function(fr) {
      p <- tryCatch(Signac::GetFragmentData(fr, "path"), error = function(e) "")
      is.character(p) && nzchar(p) && file.exists(p)
    }, logical(1)))
    if (!frag_ok)
      return(error_plot(paste0(
        "ATAC fragment files aren't accessible here, so coverage tracks\n",
        "can't be computed. The object points to fragments at paths that\n",
        "don't exist on this machine (e.g. a cluster path).\n\n",
        "Fix: repoint them with Signac::UpdatePath() to local files, or run\n",
        "the app where the fragments live.")))

    tryCatch({
      oc <- o
      DefaultAssay(oc) <- assay
      grp <- input$cv_group
      oc@meta.data[[grp]] <- order_factor(oc@meta.data[[grp]])
      Idents(oc) <- grp
      Signac::CoveragePlot(
        oc, region = region, group.by = grp, assay = assay,
        extend.upstream   = input$cv_up,
        extend.downstream = input$cv_down,
        annotation = TRUE, peaks = isTRUE(input$cv_peaks)
      )
    }, error = function(e) error_plot(conditionMessage(e)))
  })

  output$cv_plot <- renderPlot({ cv_plot_r() })
  output$cv_dl   <- downloadHandler(
    filename = function() paste0("coverage_plot.", tolower(input$cv_fmt)),
    content  = function(file) save_plot(cv_plot_r(), input$cv_fmt, input$cv_w, input$cv_h, file)
  )


  # ============================================================
  # SPATIAL TAB
  # ============================================================
  sp_plot_r <- reactive({
    o <- rv$obj
    req(o)

    if (length(o@images) == 0 || isTRUE(input$sp_image == "NONE"))
      return(error_plot("No spatial images in this object."))

    img <- input$sp_image
    pt  <- input$sp_pt
    al  <- input$sp_alpha

    if (input$sp_type == "dim") {
      req(input$sp_color_by)
      color_by <- input$sp_color_by

      tryCatch({
        # Apply numeric-aware factor ordering
        o@meta.data[[color_by]] <- order_factor(o@meta.data[[color_by]])
        lvls <- levels(o@meta.data[[color_by]])
        cols <- cat_colors(lvls)

        p <- suppressWarnings(
          SpatialDimPlot(o,
                         group.by       = color_by,
                         images         = img,
                         label          = input$sp_label,
                         label.size     = input$sp_label_size,
                         pt.size.factor = pt,
                         alpha          = al) +
            scale_color_manual(values = cols) +
            scale_fill_manual(values  = cols) +
            theme(legend.position = "right") +
            ggtitle(color_by)
        )
        apply_theme(p, input$sp_theme, input$base_size)

      }, error = function(e) error_plot(conditionMessage(e)))

    } else if (input$sp_type == "feature") {
      gene <- input$sp_gene
      req(gene, nchar(gene) > 0)

      tryCatch({
        p <- SpatialFeaturePlot(o, features = gene, images = img,
                                pt.size.factor = pt, alpha = al) +
          cont_fill_scale(input$sp_color_scale)
        apply_theme(p, input$sp_theme, input$base_size)
      }, error = function(e) error_plot(conditionMessage(e)))

    } else {
      # Module / UCell score — a numeric meta.data column.
      # SpatialFeaturePlot resolves metadata columns natively.
      score <- input$sp_score
      if (is.null(score) || !nzchar(score))
        return(error_plot("No module scores found.\nAdd them with AddModuleScore() or AddModuleScore_UCell()."))
      if (!score %in% colnames(o@meta.data))
        return(error_plot(paste0("Score not found in metadata: ", score)))

      tryCatch({
        p <- SpatialFeaturePlot(o, features = score, images = img,
                                pt.size.factor = pt, alpha = al)

        p <- p + if (isTRUE(input$sp_score_center))
          score_centered_fill(o@meta.data[[score]], input$sp_score_scale)
        else
          cont_fill_scale(input$sp_score_scale)

        apply_theme(p, input$sp_theme, input$base_size)
      }, error = function(e) error_plot(conditionMessage(e)))
    }
  })

  output$sp_plot <- renderPlot({ sp_plot_r() })
  output$sp_dl   <- downloadHandler(
    filename = function() paste0("spatial_plot.", tolower(input$sp_fmt)),
    content  = function(file) save_plot(sp_plot_r(), input$sp_fmt, input$sp_w, input$sp_h, file)
  )


  # ============================================================
  # REDUCTION / UMAP TAB
  # ============================================================
  # Split-by faceting makes Seurat return a patchwork; scales/themes must then
  # be added with `&` (every panel) rather than `+` (the whole composition).
  dr_finish <- function(p, scales = list(), split_on = FALSE) {
    th <- get_theme_obj(input$dr_theme, input$base_size)
    if (isTRUE(split_on)) { for (s in scales) p <- p & s; p & th }
    else                  { for (s in scales) p <- p + s; p + th }
  }

  dr_plot_r <- reactive({
    o <- rv$obj
    req(o)

    if (length(o@reductions) == 0 || isTRUE(input$dr_red == "NONE"))
      return(error_plot("No reductions available in this object."))

    red   <- input$dr_red
    pt    <- input$dr_pt
    split <- if (!is.null(input$dr_split) && input$dr_split != "none") input$dr_split else NULL

    if (input$dr_type == "dim") {
      req(input$dr_color_by)
      color_by <- input$dr_color_by

      tryCatch({
        o@meta.data[[color_by]] <- order_factor(o@meta.data[[color_by]])
        lvls <- levels(o@meta.data[[color_by]])
        cols <- cat_colors(lvls)

        p <- suppressWarnings(
          DimPlot(o, reduction = red, group.by = color_by, split.by = split,
                  label = input$dr_label, repel = input$dr_repel,
                  pt.size = pt, raster = FALSE)
        )
        extras <- list(scale_color_manual(values = cols))
        if (is.null(split)) extras <- c(extras, list(ggtitle(paste(toupper(red), "–", color_by))))
        dr_finish(p, extras, !is.null(split))

      }, error = function(e) error_plot(conditionMessage(e)))

    } else if (input$dr_type == "feature") {
      gene <- input$dr_gene
      req(gene, nchar(gene) > 0)

      tryCatch({
        p <- FeaturePlot(o, features = gene, reduction = red, split.by = split,
                         pt.size = pt, raster = FALSE)
        dr_finish(p, list(cont_color_scale(input$dr_color_scale)), !is.null(split))
      }, error = function(e) error_plot(conditionMessage(e)))

    } else if (input$dr_type == "coexp") {
      g1 <- input$dr_gene1; g2 <- input$dr_gene2
      req(g1, g2, nchar(g1) > 0, nchar(g2) > 0)
      if (g1 == g2) return(error_plot("Pick two different genes for co-expression."))

      tryCatch({
        # blend=TRUE renders gene1, gene2, and their co-expression blend as a
        # 4-panel patchwork; it cannot be combined with split.by.
        p <- suppressMessages(
          FeaturePlot(o, features = c(g1, g2), reduction = red, blend = TRUE,
                      pt.size = pt, raster = FALSE, combine = TRUE)
        )
        p & get_theme_obj(input$dr_theme, input$base_size)
      }, error = function(e) error_plot(conditionMessage(e)))

    } else {
      # Module / UCell score — FeaturePlot resolves metadata columns natively
      score <- input$dr_score
      if (is.null(score) || !nzchar(score))
        return(error_plot("No module scores found.\nAdd them with AddModuleScore() or AddModuleScore_UCell()."))
      if (!score %in% colnames(o@meta.data))
        return(error_plot(paste0("Score not found in metadata: ", score)))

      tryCatch({
        p <- suppressMessages(
          FeaturePlot(o, features = score, reduction = red, split.by = split,
                      pt.size = pt, raster = FALSE)
        )
        scl <- if (isTRUE(input$dr_score_center))
          score_centered_color(o@meta.data[[score]], input$dr_score_scale)
        else
          cont_color_scale(input$dr_score_scale)
        dr_finish(p, list(scl), !is.null(split))
      }, error = function(e) error_plot(conditionMessage(e)))
    }
  })

  output$dr_plot <- renderPlot({ dr_plot_r() })
  output$dr_dl   <- downloadHandler(
    filename = function() paste0("reduction_plot.", tolower(input$dr_fmt)),
    content  = function(file) save_plot(dr_plot_r(), input$dr_fmt, input$dr_w, input$dr_h, file)
  )


  # ============================================================
  # FEATURE EXPRESSION TAB
  # ============================================================
  fe_plot_r <- reactive({
    o <- rv$obj
    req(o, input$fe_genes, length(input$fe_genes) > 0, input$fe_group)

    genes <- input$fe_genes
    grp   <- input$fe_group
    split <- if (!is.null(input$fe_split) && input$fe_split != "none") input$fe_split else NULL

    # Data source chosen on this tab: which assay and which layer of it
    assay <- input$fe_assay
    layer <- input$fe_layer
    req(assay, layer, assay %in% Assays(o))

    tryCatch({
      # Pre-sort group factor (custom order if the user set one)
      custom <- if (isTRUE(input$fe_order_on)) input$fe_order else NULL
      o@meta.data[[grp]] <- ordered_factor(o@meta.data[[grp]], custom)
      lvls <- levels(o@meta.data[[grp]])
      cols <- cat_colors(lvls)

      # Facet spec: by gene alone, or gene × split level when splitting
      facet <- if (is.null(split)) facet_wrap(~gene, scales = "free_y")
               else facet_grid(gene ~ .split, scales = "free_y")

      # Y-axis label says what is plotted: the chosen layer for genes, or
      # "Score" for module/UCell scores (which live in meta.data, not an assay).
      score_cols <- get_numeric_meta(o)
      gene_lab   <- layer_ylab(o, assay, layer)
      n_scores   <- sum(genes %in% score_cols & !genes %in% rownames(o[[assay]]))
      y_lab <- if (n_scores == length(genes)) "Score"
               else if (n_scores > 0)         paste(gene_lab, "/ Score")
               else                           gene_lab

      # ── Helper: cells x features data frame for the chosen assay layer ──
      # Genes/peaks come from the selected layer; module/UCell scores come
      # from meta.data (they don't depend on the layer).
      get_expr_values <- function() {
        gene_req  <- genes[genes %in% rownames(o[[assay]])]
        score_req <- setdiff(genes[genes %in% colnames(o@meta.data)], gene_req)
        if (length(gene_req) + length(score_req) == 0)
          stop(sprintf("None of the selected genes or scores were found in assay '%s'.", assay))

        vals <- data.frame(row.names = colnames(o))
        if (length(gene_req) > 0) {
          m <- get_layer_matrix(o, assay, layer, gene_req)
          miss <- attr(m, "missing")
          if (length(miss) > 0)
            showNotification(
              sprintf("%d of %d selected feature(s) aren't in the '%s' layer (scaled layers only hold variable features): %s",
                      length(miss), length(gene_req), layer_label(o, assay, layer),
                      paste(head(miss, 6), collapse = ", ")),
              type = "warning", duration = 8)
          if (nrow(m) > 0) vals <- cbind(vals, as.data.frame(t(m), check.names = FALSE))
        }
        for (s in score_req) vals[[s]] <- o@meta.data[[s]]

        if (ncol(vals) == 0)
          stop(sprintf("None of the selected features are in the '%s' layer of '%s'. Try 'Log-normalized' or another layer.",
                       layer_label(o, assay, layer), assay))
        vals[, genes[genes %in% colnames(vals)], drop = FALSE]   # keep the user's order
      }

      # Long format used by violin / box / dot plots
      get_expr_long <- function() {
        vals <- get_expr_values()
        ok   <- colnames(vals)

        df <- data.frame(
          group = o@meta.data[[grp]],
          vals,
          check.names = FALSE
        )
        id_cols <- "group"
        if (!is.null(split)) { df$.split <- order_factor(o@meta.data[[split]]); id_cols <- c("group", ".split") }
        df_long <- pivot_longer(df, cols = -all_of(id_cols), names_to = "gene", values_to = "expr")
        # Preserve the order the user selected them in
        df_long$gene  <- factor(df_long$gene, levels = ok)
        df_long$group <- factor(df_long$group, levels = lvls)
        df_long
      }

      if (input$fe_type == "violin") {
        # Build as pure ggplot2 violin so stats brackets attach cleanly to facets
        df_long <- get_expr_long()

        p <- ggplot(df_long, aes(x = group, y = expr, fill = group)) +
          geom_violin(trim = FALSE, scale = "width") +
          geom_boxplot(width = 0.08, fill = "white",
                       outlier.size = 0.5, outlier.alpha = 0.4) +
          scale_fill_manual(values = cols) +
          facet +
          theme(legend.position = "none") +
          labs(x = grp, y = y_lab)

        if (isTRUE(input$fe_show_stats) && is.null(split)) {
          stat_res <- compute_stats(df_long, lvls, input, session)
          if (!is.null(stat_res))
            p <- p + add_pvalue(stat_res,
                                label        = attr(stat_res, "label_col"),
                                tip.length   = 0.01,
                                bracket.size = 0.4,
                                label.size   = 3.2,
                                inherit.aes  = FALSE)
        }

        apply_theme(p, input$fe_theme, input$base_size) +
          x_label_theme(input$fe_xangle, lvls)

      } else if (input$fe_type == "dot") {
        # Dot size = % of cells expressing; colour = average expression scaled
        # across groups. Built here (rather than Seurat::DotPlot, which assumes
        # log-normalized data) so it works for any counts/normalized layer.
        if (layer == "scale.data")
          stop("Dot plots need counts or normalized values. Choose 'Raw counts' or a normalized layer.")
        df_long <- get_expr_long()

        dd <- df_long %>%
          group_by(gene, group) %>%
          summarise(avg = mean(expm1_if_log(expr, layer), na.rm = TRUE),
                    pct = 100 * mean(expr > 0, na.rm = TRUE), .groups = "drop") %>%
          group_by(gene) %>%
          mutate(avg_scaled = {
            la <- log1p(avg); s <- stats::sd(la)
            if (is.na(s) || s == 0) 0 else pmax(pmin((la - mean(la)) / s, 2.5), -2.5)
          }) %>%
          ungroup()
        dd$gene <- factor(dd$gene, levels = rev(levels(df_long$gene)))   # first gene on top

        p <- ggplot(dd, aes(x = group, y = gene, size = pct, color = avg_scaled)) +
          geom_point() +
          scale_color_viridis_c(name = "Avg expression\n(scaled)") +
          scale_size(range = c(0, 6), limits = c(0, 100), name = "% expressing") +
          labs(x = grp, y = NULL)
        apply_theme(p, input$fe_theme, input$base_size) +
          x_label_theme(input$fe_xangle, lvls)

      } else if (input$fe_type == "box") {
        df_long <- get_expr_long()

        p <- ggplot(df_long, aes(x = group, y = expr, fill = group)) +
          geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.3) +
          scale_fill_manual(values = cols) +
          facet +
          theme(legend.position = "none") +
          labs(x = grp, y = y_lab)

        if (isTRUE(input$fe_show_stats) && is.null(split)) {
          stat_res <- compute_stats(df_long, lvls, input, session)
          if (!is.null(stat_res))
            p <- p + add_pvalue(stat_res,
                                label        = attr(stat_res, "label_col"),
                                tip.length   = 0.01,
                                bracket.size = 0.4,
                                label.size   = 3.2,
                                inherit.aes  = FALSE)
        }

        apply_theme(p, input$fe_theme, input$base_size) +
          x_label_theme(input$fe_xangle, lvls)

      } else if (input$fe_type == "heatmap") {
        gene_ok <- genes[genes %in% rownames(o[[assay]])]
        if (length(gene_ok) < 1)
          stop("Heatmap needs genes from the selected assay (module/UCell scores aren't supported here).")

        oh <- o
        DefaultAssay(oh) <- assay
        oh@meta.data[[grp]] <- factor(oh@meta.data[[grp]], levels = lvls)
        Idents(oh) <- grp
        # Downsample very large objects so the heatmap stays legible and fast
        if (ncol(oh) > 8000) {
          set.seed(1)
          oh <- subset(oh, cells = sample(colnames(oh), 8000))
        }

        # Heatmaps plot z-scored values. If the chosen layer is already scaled
        # (e.g. SCT Pearson residuals) and holds the genes, use it as-is;
        # otherwise scale from normalized data, log-normalizing a local copy
        # first when the layer is raw counts or no normalized layer exists.
        # The loaded object itself is never modified.
        use_existing <- FALSE
        if (layer == "scale.data") {
          have <- tryCatch(rownames(LayerData(oh[[assay]], layer = "scale.data")),
                           error = function(e) character(0))
          if (any(gene_ok %in% have)) {
            gene_ok <- gene_ok[gene_ok %in% have]; use_existing <- TRUE
          }
        }
        if (!use_existing) {
          if (layer == "counts" || !"data" %in% Layers(oh[[assay]])) {
            showNotification("Log-normalizing a copy of the counts for the heatmap.",
                             type = "message", duration = 4)
            oh <- NormalizeData(oh, assay = assay, verbose = FALSE)
          }
          oh <- ScaleData(oh, assay = assay, features = gene_ok, verbose = FALSE)
        }
        hm_angle <- x_label_angle(input$fe_xangle, lvls)
        p <- suppressMessages(suppressWarnings(
          DoHeatmap(oh, features = gene_ok, group.by = grp, assay = assay,
                    group.colors = unname(cols[lvls]), size = 3.6,
                    angle = hm_angle) +
            scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                 midpoint = 0, na.value = "white") +
            # The group names are already printed above the colour bars, so the
            # "Identity" legend only repeats them and squeezes the heatmap.
            guides(colour = "none")
        ))
        # DoHeatmap draws its group labels above the panel (clip = "off"), so
        # long labels need headroom or they are cut off at the top of the image.
        # ~3.6 pt per character, scaled by how vertical the labels are.
        top_pt <- max(nchar(lvls)) * 3.6 * sin(hm_angle * pi / 180) + 8
        p + theme(plot.margin = margin(t = top_pt, r = 10, b = 5, l = 5, unit = "pt"))
      }

    }, error = function(e) error_plot(conditionMessage(e)))
  })

  output$fe_plot <- renderPlot({ fe_plot_r() })
  output$fe_dl   <- downloadHandler(
    filename = function() paste0("expression_plot.", tolower(input$fe_fmt)),
    content  = function(file) save_plot(fe_plot_r(), input$fe_fmt, input$fe_w, input$fe_h, file)
  )


  # ============================================================
  # COMPOSITION TAB
  # ============================================================
  co_plot_r <- reactive({
    o <- rv$obj
    req(o, input$co_x, input$co_fill)

    x_var    <- input$co_x
    fill_var <- input$co_fill
    meta     <- o@meta.data

    tryCatch({
      # Build counts
      df <- meta %>%
        mutate(
          .x    = as.character(.data[[x_var]]),
          .fill = as.character(.data[[fill_var]])
        ) %>%
        group_by(.x, .fill) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(.x) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup()

      # Sort X axis
      if (identical(input$co_sort, "custom")) {
        df$.x <- ordered_factor(df$.x, input$co_x_order)
      } else {
        x_order <- switch(input$co_sort,
          alpha      = sort(unique(df$.x)),
          total_desc = df %>% group_by(.x) %>% summarise(tot = sum(n), .groups="drop") %>%
                         arrange(desc(tot)) %>% pull(.x),
          total_asc  = df %>% group_by(.x) %>% summarise(tot = sum(n), .groups="drop") %>%
                         arrange(tot) %>% pull(.x),
          unique(df$.x)   # none / as-is
        )
        df$.x <- factor(df$.x, levels = x_order)
      }

      # Fill ordering — custom if the user set one, else numeric-aware
      fill_custom <- if (isTRUE(input$co_fill_order_on)) input$co_fill_order else NULL
      df$.fill    <- ordered_factor(df$.fill, fill_custom)
      fill_lvls   <- levels(df$.fill)
      cols        <- cat_colors(fill_lvls)

      # Build plot
      if (input$co_type == "prop") {
        p <- ggplot(df, aes(x = .x, y = prop, fill = .fill)) +
          geom_col(position = "stack", width = 0.8) +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          labs(x = x_var, y = "Proportion", fill = fill_var)

      } else if (input$co_type == "count_stack") {
        p <- ggplot(df, aes(x = .x, y = n, fill = .fill)) +
          geom_col(position = "stack", width = 0.8) +
          labs(x = x_var, y = "Cell Count", fill = fill_var)

      } else {
        p <- ggplot(df, aes(x = .x, y = n, fill = .fill)) +
          geom_col(position = position_dodge(width = 0.85), width = 0.8) +
          labs(x = x_var, y = "Cell Count", fill = fill_var)
      }

      p <- p + scale_fill_manual(values = cols)

      if (input$co_angle)
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (input$co_flip)
        p <- p + coord_flip()

      apply_theme(p, input$co_theme, input$base_size)

    }, error = function(e) error_plot(conditionMessage(e)))
  })

  output$co_plot <- renderPlot({ co_plot_r() })
  output$co_dl   <- downloadHandler(
    filename = function() paste0("composition_plot.", tolower(input$co_fmt)),
    content  = function(file) save_plot(co_plot_r(), input$co_fmt, input$co_w, input$co_h, file)
  )


  # ============================================================
  # METADATA TAB
  # ============================================================
  me_plot_r <- reactive({
    o <- rv$obj
    req(o, input$me_col, input$me_type != "table")

    meta   <- o@meta.data
    col    <- input$me_col
    grp    <- if (input$me_group != "none") input$me_group else NULL
    vals   <- meta[[col]]
    is_num <- is.numeric(vals)

    tryCatch({
      if (input$me_type == "bar") {
        if (!is.null(grp)) {
          df <- meta %>%
            group_by(across(all_of(c(grp, col)))) %>%
            summarise(n = n(), .groups = "drop")
          df[[grp]] <- order_factor(df[[grp]])
          df[[col]] <- order_factor(df[[col]])
          fill_lvls <- levels(df[[col]])
          cols      <- cat_colors(fill_lvls)

          p <- ggplot(df, aes(x = .data[[grp]], y = n,
                              fill = factor(.data[[col]], levels = fill_lvls))) +
            geom_col(position = "fill") +
            scale_y_continuous(labels = percent_format()) +
            scale_fill_manual(values = cols) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = grp, y = "Proportion", fill = col)
        } else {
          vals_f    <- order_factor(vals)
          fill_lvls <- levels(vals_f)
          cols      <- cat_colors(fill_lvls)
          df        <- as.data.frame(table(vals_f))
          colnames(df) <- c("value", "count")
          df$value  <- factor(df$value, levels = fill_lvls)

          p <- ggplot(df, aes(x = value, y = count, fill = value)) +
            geom_col(show.legend = FALSE) +
            scale_fill_manual(values = cols) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = col, y = "Count")
        }

      } else if (input$me_type == "hist") {
        if (!is_num) {
          vals_f    <- order_factor(vals)
          fill_lvls <- levels(vals_f)
          cols      <- cat_colors(fill_lvls)
          df        <- as.data.frame(table(vals_f))
          colnames(df) <- c("value", "count")
          df$value  <- factor(df$value, levels = fill_lvls)
          p <- ggplot(df, aes(x = value, y = count, fill = value)) +
            geom_col(show.legend = FALSE) +
            scale_fill_manual(values = cols) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = col, y = "Count")
        } else {
          p <- ggplot(meta, aes(x = .data[[col]]))
          if (!is.null(grp)) {
            grp_lvls <- levels(order_factor(meta[[grp]]))
            cols     <- cat_colors(grp_lvls)
            meta[[grp]] <- factor(meta[[grp]], levels = grp_lvls)
            p <- p + geom_histogram(aes(fill = .data[[grp]]),
                                    position = "identity", alpha = 0.6, bins = 40) +
              scale_fill_manual(values = cols) + labs(fill = grp)
          } else {
            p <- p + geom_histogram(fill = "#2C3E50", bins = 40)
          }
          p <- p + labs(x = col, y = "Count")
        }

      } else if (input$me_type == "density") {
        if (!is_num)
          return(error_plot("Density plot requires a numeric column."))
        p <- ggplot(meta, aes(x = .data[[col]]))
        if (!is.null(grp)) {
          grp_lvls <- levels(order_factor(meta[[grp]]))
          cols     <- cat_colors(grp_lvls)
          meta[[grp]] <- factor(meta[[grp]], levels = grp_lvls)
          p <- p + geom_density(aes(fill = .data[[grp]]), alpha = 0.5) +
            scale_fill_manual(values = cols) + labs(fill = grp)
        } else {
          p <- p + geom_density(fill = "#18BC9C", alpha = 0.7)
        }
        p <- p + labs(x = col)
      }

      apply_theme(p, input$me_theme, input$base_size)

    }, error = function(e) error_plot(conditionMessage(e)))
  })

  output$me_content <- renderUI({
    if (input$me_type == "table") spin(DTOutput("me_table"))
    else                          spin(plotOutput("me_plot", height = "560px"))
  })

  output$me_plot  <- renderPlot({ me_plot_r() })

  output$me_table <- renderDT({
    o <- rv$obj; req(o)
    datatable(o@meta.data,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = TRUE)
  })

  output$me_dl <- downloadHandler(
    filename = function() paste0("metadata_plot.", tolower(input$me_fmt)),
    content  = function(file) {
      p <- me_plot_r(); req(p)
      save_plot(p, input$me_fmt, input$me_w, input$me_h, file)
    }
  )
}

shinyApp(ui, server)
