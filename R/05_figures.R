# Main figures ------------------------------------------------------------------
source("R/00_setup.R")
m <- lapply(setNames(names(studies), names(studies)),
            function(s) readRDS(sprintf("output/models/%s.rds", s)))
th <- theme_bw(base_size = 11)

# Fig. 2 Simulation: rejection rate by number of raters, effect and test
sim <- read.csv("output/simulation/sim_summary.csv") |>
  group_by(eff_name, n_rater) |>
  summarise(across(c(rej_two, rej_simple, rej_extended), mean), .groups = "drop") |>
  pivot_longer(starts_with("rej_"), names_to = "test", values_to = "rate") |>
  mutate(test = factor(test, c("rej_two", "rej_simple", "rej_extended"),
                       c("Two-stage (paired t on rho_c)", "One-stage, simple", "One-stage, extended")),
         eff_name = factor(eff_name, c("null", "constant", "calibration", "precision"),
                           c("No effect (type I error)", "Constant shift", "Calibration only", "Precision only")))
f2 <- ggplot(sim, aes(n_rater, rate, colour = test)) +
  geom_hline(yintercept = 0.05, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ eff_name, nrow = 1) + scale_x_continuous(breaks = c(10, 20, 40)) +
  scale_colour_manual(values = c("#4D4D4D", "#E69F00", "#007f80")) +
  labs(x = "Number of raters", y = "Rejection rate (5%)", colour = NULL) +
  th + theme(legend.position = "bottom")
ggsave("output/figures/fig2_simulation.png", f2, width = 11, height = 3.8, dpi = 300)

# Fig. 3 One condition: bias and expected absolute error
one <- c("alternaria_pecan", "tan_spot_wheat")
f3 <- wrap_plots(lapply(one, function(s) {
  cv <- m[[s]]$linear
  (plot(cv, "bias") + labs(title = studies[[s]]$label)) /
    plot(cv, "expected_abs_error")
}), nrow = 1) + plot_layout(guides = "collect")
ggsave("output/figures/fig3_one_condition.png", f3, width = 10, height = 7, dpi = 300)

# Fig. 4 Two SADs: functional differences with simultaneous bands
two <- c("soybean_rust", "coffee_leaf_rust")
f4 <- wrap_plots(lapply(two, function(s)
  plot(m[[s]]$smooth, "difference") + labs(title = studies[[s]]$label, subtitle = NULL)),
  ncol = 1)
ggsave("output/figures/fig4_two_sads.png", f4, width = 10, height = 7, dpi = 300)

# Fig. 5 Four SADs: bias by SAD and contrasts between SADs (rice)
cv <- m$brownspot_rice$linear
b <- plot(structure(list(curves = filter(cv$curves, grepl("^SAD", condition)),
                         differences = cv$differences, type = "linear", level = cv$level),
                    class = "sad_curve"), "bias") + labs(title = "Brown spot, rice: bias with each SAD")
d <- plot(structure(list(curves = cv$curves,
                         differences = filter(cv$differences, grepl("^SAD.*SAD", contrast)),
                         type = "linear", level = cv$level), class = "sad_curve"), "difference") +
  labs(subtitle = NULL)
ggsave("output/figures/fig5_four_sads.png", b / d + plot_layout(heights = c(1, 1.6)),
       width = 10, height = 9, dpi = 300)

# Fig. 6 Raters: bias at 10% and slope by condition (soybean)
f6 <- plot_sad_raters(m$soybean_rust$raters) + labs(title = "Soybean rust") + th
ggsave("output/figures/fig6_raters_soybean.png", f6, width = 7, height = 5, dpi = 300)
