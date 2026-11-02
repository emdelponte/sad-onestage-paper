# Figures of the paper -------------------------------------------------------------
# Main text:  Fig. 1 schematic, Fig. 2 simulation, Fig. 3 one-SAD studies,
#             Fig. 4 soybean raters.
# Supplement: Fig. S1 two-SAD studies, Fig. S2 four-SAD study (rice).
# One clean theme, the Okabe-Ito colour-blind palette and panel letters (A, B, ...).
# Each figure is saved as PDF (vector) and PNG (600 dpi), sized for a double
# column of 174 mm.
source("R/00_setup.R")
m <- lapply(setNames(names(studies), names(studies)),
            function(s) readRDS(sprintf("output/models/%s.rds", s)))

# Style ----------------------------------------------------------------------------
fams <- unique(systemfonts::system_fonts()$family)
font <- intersect(c("Arial", "Helvetica", "Liberation Sans"), fams)[1]
if (is.na(font)) font <- "sans"

theme_sad <- function(base_size = 8) {
  theme_classic(base_size = base_size, base_family = font) +
    theme(
      axis.line = element_line(linewidth = 0.3, colour = "grey20"),
      axis.ticks = element_line(linewidth = 0.3, colour = "grey20"),
      axis.ticks.length = unit(1.5, "pt"),
      axis.text = element_text(colour = "grey20", size = base_size - 1),
      axis.title = element_text(colour = "grey10"),
      plot.title = element_text(size = base_size, face = "plain", colour = "grey10",
                                hjust = 0, margin = margin(b = 3)),
      plot.tag = element_text(size = base_size + 2, face = "bold"),
      strip.background = element_blank(),
      strip.text = element_text(size = base_size, hjust = 0),
      legend.title = element_blank(),
      legend.key.height = unit(8, "pt"),
      legend.key.width = unit(14, "pt"),
      legend.text = element_text(size = base_size - 0.5),
      legend.background = element_blank(),
      plot.margin = margin(6, 6, 4, 6)
    )
}
theme_set(theme_sad())

# Okabe-Ito palette
oi <- c(black = "#000000", orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
        yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00", purple = "#CC79A7")
col_method <- c(UN = "grey45", SAD = oi[["blue"]])
col_sad <- c(Old = oi[["orange"]], New = oi[["blue"]])
col_rice <- c(`LIN-COL` = oi[["orange"]], `LIN-BW` = oi[["sky"]],
              `LOG-COL` = oi[["green"]], `LOG-BW` = oi[["purple"]])
lab_rice <- c(`LIN-COL` = "LIN-COL (linear, colour)", `LIN-BW` = "LIN-BW (linear, black and white)",
              `LOG-COL` = "LOG-COL (logarithmic, colour)", `LOG-BW` = "LOG-BW (logarithmic, black and white)")
x_sev <- "Actual severity (%)"

save_fig <- function(p, name, width = 174, height = 100) {
  ggsave(sprintf("output/figures/%s.pdf", name), p, width = width, height = height,
         units = "mm", device = cairo_pdf)
  ggsave(sprintf("output/figures/%s.png", name), p, width = width, height = height,
         units = "mm", dpi = 600, device = ragg::agg_png, bg = "white")
}
tags <- plot_annotation(tag_levels = "A")
zero <- geom_hline(yintercept = 0, linewidth = 0.3, linetype = "22", colour = "grey40")

# Significant ranges of a difference curve, as shaded rectangles
sig_rects <- function(d) {
  d <- d[order(d$actual), ]
  r <- rle(as.logical(d$significant)); end <- cumsum(r$lengths); start <- end - r$lengths + 1
  data.frame(xmin = d$actual[start[r$values]], xmax = d$actual[end[r$values]])
}

# Difference panel: curve, simultaneous band, significant ranges shaded
diff_panel <- function(d, title, colour = oi[["blue"]]) {
  rr <- sig_rects(d)
  ggplot(d, aes(actual, diff)) +
    { if (nrow(rr)) geom_rect(data = rr, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                              inherit.aes = FALSE, fill = "grey92") } +
    zero +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = colour, alpha = 0.18) +
    geom_line(colour = colour, linewidth = 0.6) +
    labs(title = title, x = x_sev, y = "Difference in bias (p.p.)")
}

# Fig. 1 Schematic -------------------------------------------------------------------
node <- function(x, y, w, h, label, fill, border) list(
  annotate("rect", xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
           fill = fill, colour = border, linewidth = 0.35),
  annotate("text", x = x, y = y, label = label, size = 2.55, family = font,
           lineheight = 0.95, colour = "grey10"))
arrow_seg <- function(x, xend, y) annotate("segment", x = x, xend = xend, y = y, yend = y,
  linewidth = 0.35, colour = "grey25", arrow = arrow(length = unit(1.6, "mm"), type = "closed"))
g1 <- "#F2F2F2"; g2 <- "#FDF0D5"; g3 <- "#DCEBF5"
p1 <- ggplot() + coord_cartesian(xlim = c(0, 10.2), ylim = c(0, 4.3), expand = FALSE) +
  theme_void(base_family = font) +
  annotate("text", x = 0.1, y = 4.05, label = "A  Two-stage (conventional)", hjust = 0,
           fontface = "bold", size = 3, family = font) +
  annotate("text", x = 0.1, y = 1.95, label = "B  One-stage (proposed)", hjust = 0,
           fontface = "bold", size = 3, family = font) +
  node(1.2, 3.05, 2.0, 1.0, "Estimates\nrater × image × condition", g1, "grey55") +
  node(4.5, 3.05, 2.6, 1.0, "Lin's statistics per rater\nρc, r, Cb, v, u", g2, oi[["orange"]]) +
  node(8.3, 3.05, 3.4, 1.0, "Compare means between conditions\n(one number per rater)", g2, oi[["orange"]]) +
  arrow_seg(2.2, 3.2, 3.05) + arrow_seg(5.8, 6.6, 3.05) +
  node(1.2, 0.95, 2.0, 1.3, "Estimates\nrater × image × condition", g1, "grey55") +
  node(4.5, 0.95, 2.6, 1.3, "Mixed model of all estimates\nbias and calibration by condition\nrater × method, image effects\nseverity-dependent dispersion", g3, oi[["blue"]]) +
  node(8.3, 0.95, 3.4, 1.3, "Curves along the severity range\nbias, SD among raters,\nexpected absolute error,\ndifferences with simultaneous bands", g3, oi[["blue"]]) +
  arrow_seg(2.2, 3.2, 0.95) + arrow_seg(5.8, 6.6, 0.95)
save_fig(p1, "fig1_schematic", height = 60)

# Fig. 2 Simulation --------------------------------------------------------------------
sim <- read.csv("output/simulation/sim_summary.csv") |>
  group_by(eff_name, n_rater) |>
  summarise(across(c(rej_two, rej_simple, rej_extended), mean), .groups = "drop") |>
  pivot_longer(starts_with("rej_"), names_to = "test", values_to = "rate") |>
  mutate(test = factor(test, c("rej_two", "rej_simple", "rej_extended"),
                       c("Two-stage (paired t test on ρc)", "One-stage, simple model",
                         "One-stage, extended model")))
col_test <- setNames(c(oi[["black"]], oi[["orange"]], oi[["blue"]]), levels(sim$test))
eff <- c(null = "No effect (type I error)", constant = "Constant shift",
         calibration = "Calibration only", precision = "Precision only")
p2 <- wrap_plots(lapply(names(eff), function(e)
  ggplot(filter(sim, eff_name == e), aes(n_rater, rate, colour = test, shape = test)) +
    geom_hline(yintercept = 0.05, linewidth = 0.3, linetype = "22", colour = "grey40") +
    geom_line(linewidth = 0.5) + geom_point(size = 1.4) +
    scale_colour_manual(values = col_test) + scale_shape_manual(values = c(16, 17, 15)) +
    scale_x_continuous(breaks = c(10, 20, 40)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(title = eff[[e]], x = "Number of raters",
         y = if (e == "null") "Rejection rate (α = 0.05)" else NULL)),
  nrow = 1) + plot_layout(guides = "collect") + tags &
  theme(legend.position = "bottom")
save_fig(p2, "fig2_simulation", height = 62)

# Fig. 3 Studies with one SAD: bias and expected absolute error ----------------------------
one <- c("alternaria_pecan", "tan_spot_wheat")
cur <- bind_rows(lapply(one, function(s)
  mutate(m[[s]]$linear$curves, study = studies[[s]]$label))) |>
  mutate(method = factor(as.character(method), c("UN", "SAD"), c("Unaided", "With SAD")))
col_m <- setNames(unname(col_method), c("Unaided", "With SAD"))
bias_one <- function(s) ggplot(filter(cur, study == studies[[s]]$label),
                               aes(actual, bias, colour = method, fill = method)) +
  zero + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = col_m) + scale_fill_manual(values = col_m) +
  labs(title = studies[[s]]$label, x = x_sev, y = "Bias (p.p.)")
eae_one <- function(s) ggplot(filter(cur, study == studies[[s]]$label),
                              aes(actual, expected_abs_error, colour = method)) +
  geom_line(linewidth = 0.6) + scale_colour_manual(values = col_m) +
  expand_limits(y = 0) + guides(colour = "none") +
  labs(title = studies[[s]]$label, x = x_sev, y = "Expected absolute error (p.p.)")
p3 <- (bias_one(one[1]) + bias_one(one[2])) / (eae_one(one[1]) + eae_one(one[2])) +
  plot_layout(guides = "collect") + tags & theme(legend.position = "bottom")
save_fig(p3, "fig3_one_sad", height = 120)

# Fig. 4 Soybean rust: rater bias and calibration -----------------------------------------
rt <- m$soybean_rust$raters |>
  mutate(sad = sub(".*_", "", as.character(condition)),
         method = factor(as.character(method), c("UN", "SAD"), c("Unaided", "With SAD")))
rw <- rt |> select(rater, sad, method, bias_at_center, slope) |>
  pivot_wider(names_from = method, values_from = c(bias_at_center, slope))
lims_x <- range(rt$bias_at_center); lims_y <- range(rt$slope)
rater_panel <- function(g) {
  ggplot(filter(rt, sad == g), aes(bias_at_center, slope)) +
    geom_hline(yintercept = 1, linewidth = 0.3, linetype = "22", colour = "grey40") +
    geom_vline(xintercept = 0, linewidth = 0.3, linetype = "22", colour = "grey40") +
    geom_segment(data = filter(rw, sad == g),
                 aes(x = bias_at_center_Unaided, y = slope_Unaided,
                     xend = `bias_at_center_With SAD`, yend = `slope_With SAD`),
                 colour = col_sad[[g]], alpha = 0.45, linewidth = 0.3,
                 arrow = arrow(length = unit(1.2, "mm"), type = "closed")) +
    geom_point(aes(shape = method), colour = col_sad[[g]], fill = "white", size = 1.6, stroke = 0.5) +
    scale_shape_manual(values = c(Unaided = 21, `With SAD` = 16),
                       guide = guide_legend(override.aes = list(colour = "grey25"))) +
    coord_cartesian(xlim = lims_x, ylim = lims_y) +
    labs(title = sprintf("%s SAD", g), x = "Rater bias at 10% severity (p.p.)",
         y = "Calibration slope (ideal = 1)")
}
p4 <- (rater_panel("Old") + rater_panel("New") + labs(y = NULL)) +
  plot_layout(guides = "collect") + tags & theme(legend.position = "bottom")
save_fig(p4, "fig4_raters_soybean", height = 85)

# Fig. S1 Studies with two SADs: differences in bias (GAMM) ------------------------------
lab_contrast <- c(
  "SAD_Old - UN_Old" = "Old SAD − unaided", "SAD_New - UN_New" = "New SAD − unaided",
  "SAD_New - SAD_Old" = "New SAD − old SAD", "Old - UN" = "Old SAD − unaided",
  "New - UN" = "New SAD − unaided", "New - Old" = "New SAD − old SAD")
two_row <- function(s) {
  d <- m[[s]]$smooth$differences
  lapply(unique(d$contrast), function(k)
    diff_panel(filter(d, contrast == k), sprintf("%s: %s", studies[[s]]$label, lab_contrast[[k]])))
}
ps <- c(two_row("soybean_rust"), two_row("coffee_leaf_rust"))
ps[-c(1, 4)] <- lapply(ps[-c(1, 4)], function(p) p + labs(y = NULL))
pS2 <- wrap_plots(ps, nrow = 2) + tags & theme(plot.title = element_text(size = 7))
save_fig(pS2, "figS1_two_sads", height = 110)

# Fig. S2 Brown spot of rice, four SADs ------------------------------------------------------
cv <- m$brownspot_rice$linear
rb <- filter(cv$curves, grepl("^SAD", condition)) |>
  mutate(sad = factor(sub("SAD_", "", as.character(condition)), names(col_rice)))
pa <- ggplot(rb, aes(actual, bias, colour = sad, fill = sad)) +
  zero + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = col_rice, labels = lab_rice) +
  scale_fill_manual(values = col_rice, labels = lab_rice) +
  labs(title = "Bias with each SAD", x = x_sev, y = "Bias (p.p.)") +
  guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom")
dd <- filter(cv$differences, grepl("^SAD.*SAD", contrast))
short <- function(k) sub(" - ", " − ", gsub("SAD_", "", k))
pd <- lapply(unique(dd$contrast), function(k) diff_panel(filter(dd, contrast == k), short(k)) +
  theme(plot.title = element_text(size = 7)))
pd[-c(1, 4)] <- lapply(pd[-c(1, 4)], function(p) p + labs(y = NULL))
pS3 <- pa + pd + plot_layout(design = "AAA\nBCD\nEFG", heights = c(1.25, 1, 1)) + tags
save_fig(pS3, "figS2_four_sads", height = 165)
