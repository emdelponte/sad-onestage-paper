# Fig. 1 Schematic of the two-stage and one-stage approaches ---------------------
library(ggplot2)
box <- function(x, y, label, fill) annotate("label", x = x, y = y, label = label, fill = fill,
                                            size = 3.4, label.padding = unit(0.45, "lines"),
                                            lineheight = 0.95, label.r = unit(0.2, "lines"))
arr <- function(x, xend, y) annotate("segment", x = x, xend = xend, y = y, yend = y,
                                     arrow = arrow(length = unit(0.18, "cm"), type = "closed"))
p <- ggplot() + xlim(0, 10) + ylim(0, 4) + theme_void() +
  annotate("text", x = 0.1, y = 3.7, label = "Two-stage (conventional)", hjust = 0, fontface = "bold") +
  annotate("text", x = 0.1, y = 1.7, label = "One-stage (proposed)", hjust = 0, fontface = "bold") +
  box(1.2, 2.9, "Estimates\nrater x image\nx condition", "grey92") +
  box(4.2, 2.9, "Lin's statistics\nper rater\n(rho_c, r, Cb, v, u)", "#f9eeda") +
  box(7.6, 2.9, "Compare means\nbetween conditions\n(one number per rater)", "#f9eeda") +
  arr(2.1, 3.1, 2.9) + arr(5.3, 6.3, 2.9) +
  box(1.2, 0.9, "Estimates\nrater x image\nx condition", "grey92") +
  box(4.2, 0.9, "Mixed model of all estimates\nbias + calibration by condition\nrater x method, image,\nseverity-dependent dispersion", "#e5f4e4") +
  box(7.9, 0.9, "Curves along severity:\nbias, SD components,\nexpected absolute error,\ndifferences with simultaneous\nbands (linear and GAMM)", "#e5f4e4") +
  arr(2.1, 2.75, 0.9) + arr(5.65, 6.6, 0.9)
ggsave("output/figures/fig1_schematic.png", p, width = 10, height = 4.2, dpi = 300, bg = "white")
