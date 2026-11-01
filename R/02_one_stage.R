# One-stage hierarchical model: fit, bias and expected absolute error curves,
# differences between conditions (linear and functional), raters and images ----
source("R/00_setup.R")

pars <- list(); sev <- list(); regs <- list()
for (s in names(studies)) {
  st <- studies[[s]]
  x <- read_study(s)
  fit <- sad_fit(x, dispersion = "quad")
  cv <- sad_curve(fit, reference = st$reference, pairs = st$pairs)
  cs <- sad_curve(fit, type = "smooth", reference = st$reference, pairs = st$pairs)
  rt <- sad_raters(fit)
  im <- sad_images(fit)
  saveRDS(list(fit = fit, linear = cv, smooth = cs, raters = rt, images = im),
          sprintf("output/models/%s.rds", s))

  rr <- rt |> group_by(condition) |>
    summarise(bias10_mean = mean(bias_at_center), slope_mean = mean(slope), .groups = "drop")
  pars[[s]] <- tibble(study = s, structure = fit$structure, aic = AIC(fit$model)) |>
    cross_join(rr)
  sev[[s]] <- bind_rows(
    at_severity(cv$curves, "bias") |> mutate(measure = "bias"),
    at_severity(cv$curves, "sd_rater") |> mutate(measure = "sd_rater"),
    at_severity(cv$curves, "sd_resid") |> mutate(measure = "sd_resid"),
    at_severity(cv$curves, "expected_abs_error") |> mutate(measure = "expected_abs_error")) |>
    mutate(study = s, .before = 1)
  regs[[s]] <- bind_rows(sad_regions(cv) |> mutate(model = "linear"),
                         sad_regions(cs) |> mutate(model = "smooth")) |>
    mutate(study = s, .before = 1)
  write.csv(im, sprintf("output/tables/images_%s.csv", s), row.names = FALSE)
  message("one-stage: ", s, " (", fit$structure, ")")
}
write.csv(bind_rows(pars), "output/tables/table3_model_parameters.csv", row.names = FALSE)
write.csv(bind_rows(sev) |> pivot_wider(names_from = severity, values_from = value),
          "output/tables/table3_curves_at_severity.csv", row.names = FALSE)
write.csv(bind_rows(regs), "output/tables/table_significant_regions.csv", row.names = FALSE)
