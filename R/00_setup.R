# Setup: packages, study definitions and data reader ---------------------------
# Requires the development version of r4pde (with the sad_* functions):
#   remotes::install_github("emdelponte/r4pde")
suppressPackageStartupMessages({
  library(r4pde)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)

# One entry per reanalysed study. `pairs` lists the contrasts a - b of interest;
# `reference = "UN"` compares each SAD with unaided estimates of the same raters.
studies <- list(
  alternaria_pecan = list(
    label = "Alternaria black spot, pecan", design = "one condition",
    reference = "UN", pairs = NULL),
  tan_spot_wheat = list(
    label = "Tan spot, wheat", design = "one condition",
    reference = "UN", pairs = NULL),
  soybean_rust = list(
    label = "Soybean rust", design = "two SADs (between raters)",
    reference = NULL,
    pairs = data.frame(a = c("SAD_Old", "SAD_New", "SAD_New"),
                       b = c("UN_Old", "UN_New", "SAD_Old"))),
  coffee_leaf_rust = list(
    label = "Coffee leaf rust", design = "two SADs (within raters)",
    reference = NULL,
    pairs = data.frame(a = c("Old", "New", "New"), b = c("UN", "UN", "Old"))),
  brownspot_rice = list(
    label = "Brown spot, rice", design = "four SADs (between raters)",
    reference = NULL,
    pairs = {
      s <- paste0("SAD_", c("LIN-COL", "LIN-BW", "LOG-COL", "LOG-BW"))
      cmb <- t(utils::combn(s, 2))
      rbind(data.frame(a = s, b = sub("SAD_", "UN_", s)),
            data.frame(a = cmb[, 2], b = cmb[, 1]))
    })
)

method_levels <- c("UN", "SAD", "Old", "New")
group_levels <- c("Old", "New", "LIN-COL", "LIN-BW", "LOG-COL", "LOG-BW")

read_study <- function(name) {
  d <- read.csv(file.path("data", paste0(name, ".csv")), stringsAsFactors = FALSE)
  d$method <- factor(d$method, levels = intersect(method_levels, unique(d$method)))
  grp <- unique(d$group[!is.na(d$group)])
  has_group <- length(grp) > 1
  if (has_group) d$group <- factor(d$group, levels = intersect(group_levels, grp))
  sad_data(d, rater = "rater", image = "image", actual = "actual",
           estimate = "estimate", method = "method",
           group = if (has_group) "group" else NULL)
}

# Values of a curve at given severities, within the observed range only
at_severity <- function(curves, var, at = c(5, 10, 20, 25, 50, 75)) {
  bind_rows(lapply(split(curves, curves$condition, drop = TRUE), function(z) {
    v <- stats::approx(z$actual, z[[var]], at)$y
    tibble(condition = as.character(z$condition[1]), severity = at, value = v)
  })) |> filter(!is.na(value))
}
