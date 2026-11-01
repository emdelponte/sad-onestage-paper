# Sensitivity: linear vs quadratic dispersion, and ordered beta regression ----
source("R/00_setup.R")

out <- list(); ob <- list()
for (s in names(studies)) {
  x <- read_study(s)
  fq <- readRDS(sprintf("output/models/%s.rds", s))$fit
  fl <- sad_fit(x, structure = fq$structure, dispersion = "lin")
  out[[s]] <- tibble(study = s, structure = fq$structure,
                     aic_quad = AIC(fq$model), aic_lin = AIC(fl$model),
                     delta_aic = AIC(fl$model) - AIC(fq$model))
  fb <- try(sad_fit(x, family = "ordbeta", dispersion = "lin"), silent = TRUE)
  if (!inherits(fb, "try-error")) {
    co <- summary(fb$model)$coefficients
    ob[[s]] <- bind_rows(
      as.data.frame(co$cond) |> tibble::rownames_to_column("term") |> mutate(part = "mean"),
      as.data.frame(co$disp) |> tibble::rownames_to_column("term") |> mutate(part = "precision")) |>
      mutate(study = s, structure = fb$structure, .before = 1)
  }
  message("sensitivity: ", s)
}
write.csv(bind_rows(out), "output/tables/sensitivity_dispersion.csv", row.names = FALSE)
write.csv(bind_rows(ob), "output/tables/sensitivity_ordbeta.csv", row.names = FALSE)
