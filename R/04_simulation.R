# Simulation: two-stage paired t test on rho_c vs likelihood ratio tests of a
# simple and an extended one-stage model ----------------------------------------
# Slow (about 2 h with 4 cores for 100 replicates per scenario). Results used in
# the paper are stored in output/simulation/. Run with:
#   Rscript R/04_simulation.R <n_rep> <n_cores>
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(glmmTMB); library(parallel)
})
args <- commandArgs(trailingOnly = TRUE)
n_rep  <- if (length(args) >= 1) as.integer(args[1]) else 100
ncores <- if (length(args) >= 2) as.integer(args[2]) else 4
outdir <- "output/simulation"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
CENTER <- 10

# SAD effect: shift in bias at 10% severity, change in slope, and ratio of the
# rater and residual SDs with/without the SAD (precision)
effects <- list(
  null        = c(shift = 0,  dslope = 0,     prec = 1),
  constant    = c(shift = -3, dslope = 0,     prec = 1),
  calibration = c(shift = 0,  dslope = -0.10, prec = 1),
  precision   = c(shift = 0,  dslope = 0,     prec = 0.65))

lin_rho <- function(e, a) {
  n <- length(e); s1 <- sum((e - mean(e))^2) / n; s2 <- sum((a - mean(a))^2) / n
  2 * sum((e - mean(e)) * (a - mean(a))) / n / (s1 + s2 + (mean(e) - mean(a))^2)
}

simulate_sad <- function(n_rater, n_img, sev_dist, eff) {
  z <- if (sev_dist == "uniform") runif(n_img, 0.5, 80) else 100 * rbeta(n_img, 0.8, 4)
  z_c <- z - CENTER
  v   <- rnorm(n_img, 0, 3.5)                                   # image difficulty
  b0u <- rnorm(n_rater, 0, 5);  b1u <- rnorm(n_rater, 0, 0.18)  # rater, unaided
  b0a <- eff["prec"] * (0.5 * b0u + sqrt(1 - 0.25) * rnorm(n_rater, 0, 5))
  b1a <- eff["prec"] * (0.5 * b1u + sqrt(1 - 0.25) * rnorm(n_rater, 0, 0.18))
  expand_grid(rater = seq_len(n_rater), img = seq_len(n_img), sad = 0:1) |>
    mutate(actual = z[img], z_c = z_c[img],
           sigma = 8 * exp(0.01 * z_c) * ifelse(sad == 1, eff["prec"], 1),
           mu = CENTER + 3 + 1.10 * z_c + sad * (eff["shift"] + eff["dslope"] * z_c) +
             ifelse(sad == 1, b0a[rater] + b1a[rater] * z_c, b0u[rater] + b1u[rater] * z_c) + v[img],
           est = pmin(pmax(mu + rnorm(n(), 0, sigma), 0), 100),
           method = factor(ifelse(sad == 1, "aided", "unaided"), levels = c("unaided", "aided")),
           rater = factor(rater), img = factor(img))
}

one_rep <- function(n_rater, n_img, sev_dist, eff_name) {
  eff <- effects[[eff_name]]
  d <- simulate_sad(n_rater, n_img, sev_dist, eff)
  ts <- d |> group_by(rater, method) |>
    summarise(rho = lin_rho(est, actual), .groups = "drop") |>
    pivot_wider(names_from = method, values_from = rho)
  tt <- t.test(ts$aided, ts$unaided, paired = TRUE)
  ll <- function(m) -m$fit$objective
  fit <- function(f, disp) try(glmmTMB(f, dispformula = disp, data = d), silent = TRUE)
  re_s <- "(1 + z_c | rater) + (1 | img)"                                     # simple
  re_e <- "(0 + method | rater) + (0 + method:z_c | rater) + (1 | img)"       # extended
  d$z_c <- d$z_c / 10   # units of 10 percentage points
  out <- tibble(p_two = tt$p.value, d_rho = unname(tt$estimate))
  for (v in c("simple", "extended")) {
    re <- if (v == "simple") re_s else re_e
    m1 <- fit(as.formula(paste("est ~ z_c * method +", re)), ~ method * z_c)
    m0 <- fit(as.formula(paste("est ~ z_c +", re)), ~ z_c)
    if (inherits(m1, "try-error") || inherits(m0, "try-error")) {
      out[paste0(c("p_", "pd_"), v)] <- NA; next
    }
    out[[paste0("p_", v)]]  <- pchisq(max(2 * (ll(m1) - ll(m0)), 0), 4, lower.tail = FALSE)
    out[[paste0("pd_", v)]] <- isTRUE(m1$sdr$pdHess)
  }
  out
}

scen <- expand_grid(n_rater = c(10, 20, 40), n_img = c(20, 40),
                    sev_dist = c("uniform", "low"), eff_name = names(effects))
jobs <- scen |> slice(rep(seq_len(n()), each = n_rep)) |>
  mutate(rep = rep(seq_len(n_rep), nrow(scen)))
RNGkind("L'Ecuyer-CMRG"); set.seed(2026)
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  j <- jobs[i, ]
  r <- try(one_rep(j$n_rater, j$n_img, j$sev_dist, j$eff_name), silent = TRUE)
  if (inherits(r, "try-error")) r <- tibble(p_two = NA)
  bind_cols(j, r)
}, mc.cores = ncores) |> bind_rows()
saveRDS(res, file.path(outdir, "sim_raw_new.rds"))
summ <- res |> group_by(eff_name, sev_dist, n_rater, n_img) |>
  summarise(rej_two = mean(p_two < 0.05, na.rm = TRUE),
            rej_simple = mean(p_simple < 0.05, na.rm = TRUE),
            rej_extended = mean(p_extended < 0.05, na.rm = TRUE),
            conv_extended = mean(pd_extended, na.rm = TRUE), .groups = "drop")
write.csv(summ, file.path(outdir, "sim_summary_new.csv"), row.names = FALSE)
