# Conventional two-stage analysis: Lin's statistics per rater, comparison of
# conditions (mixed model, Tukey letters) and inter-rater reliability -----------
source("R/00_setup.R")

tab_lin <- list(); tab_rel <- list()
for (s in names(studies)) {
  x <- read_study(s)
  cc <- sad_ccc(x)
  cmp <- sad_compare(x, cc)
  rel <- sad_reliability(x)
  write.csv(cc, sprintf("output/tables/ccc_by_rater_%s.csv", s), row.names = FALSE)
  tab_lin[[s]] <- mutate(cmp$table, study = s, .before = 1)
  tab_rel[[s]] <- mutate(rel, study = s, .before = 1)
  p <- sad_plots(x, cc, baseline = "UN")
  ggsave(sprintf("output/figures/explore_%s.png", s),
         (p$scatter / p$box_ccc / (p$error_density | p$sd_vs_actual)) +
           plot_annotation(title = studies[[s]]$label),
         width = 12, height = 11, dpi = 200)
  message("two-stage: ", s)
}
write.csv(bind_rows(tab_lin), "output/tables/table2_lin_statistics.csv", row.names = FALSE)
write.csv(bind_rows(tab_rel), "output/tables/table_reliability.csv", row.names = FALSE)
