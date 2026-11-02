---
name: sad-validation
description: Validate a standard area diagram (SAD) for plant disease severity with the r4pde sad_* workflow (Lin's CCC plus the one-stage hierarchical model). Use when a user has rater-by-image severity estimates with and without a SAD, or with two or more SADs, and wants to analyse, report or plan a validation study.
---

# SAD validation with r4pde

This skill guides the analysis of a standard area diagram (SAD) validation study with the `sad_*` functions of the R package **r4pde**, following Del Ponte & Alves (in preparation), "Beyond Lin's concordance: a one-stage hierarchical framework for validating standard area diagrams for plant disease severity". The full worked example is the package vignette `vignette("sad-validation", package = "r4pde")` (<https://emdelponte.github.io/r4pde/articles/sad-validation.html>). Read it when you need detail beyond this file.

## When to use

- Raters estimated percent severity on a set of images (leaves, fruit, plants) whose actual severity is known, usually from image analysis.
- There are at least two assessment conditions: without and with a SAD (paired, same raters), two or more SADs (between or within raters), or variants of one SAD (colour vs black and white, linear vs logarithmic increments).
- The user wants to know whether the SAD improved accuracy, **and where along the severity range, how and by how much**.

Do not use it for ordinal scales or disease incidence, or when there is no actual (reference) severity.

## Principles to keep in mind

1.  The conventional two-stage analysis (Lin's ρc per rater, then a comparison of means) answers one question: did agreement improve on average? Always run it, so results can be compared with earlier studies. Do not stop there.
2.  ρc is dominated by precision. A SAD that changes only calibration, or shifts every estimate by a few percentage points (p.p.), barely moves ρc. In simulations the paired t test on ρc detected a calibration-only effect in 7% of studies.
3.  Looking at the components (r, Cb, v, u) does not solve this: they are still one number per rater, averaged over the severity range, and they depend on the range of the image set.
4.  The one-stage model uses every estimate and gives bias, rater variation and expected absolute error (EAE) as functions of severity, in p.p., with image difficulty separated from rater error.
5.  Each rater must be allowed to respond differently to each method. A model with rater effects common to all methods inflates the type I error to about 35%.

## Workflow

Install or update r4pde from GitHub (`remotes::install_github("emdelponte/r4pde")`). The one-stage functions also need glmmTMB, MASS and mgcv; `sad_compare()` needs emmeans, multcompView and lmerTest.

### 1. Prepare and check the data: `sad_data()`

``` r
library(r4pde)
# wide format: one column per method
x <- sad_data(d, rater = "rater", image = "leaf", actual = "actual",
              wide = c(UN = "unaided", SAD = "aided"),
              group = "sad")   # group only if different raters used different SADs
# long format: one row per rater x image x method
x <- sad_data(d, rater = "rater", image = "image", actual = "actual",
              estimate = "estimate", method = "method")
x
```

- Read the printed checks: missing values, estimates outside 0–100%, raters who did not assess every image. Decide with the user what to do with them; a reasonable default is to drop raters who lack a whole condition.
- With `group`, conditions become `method_group` (e.g. `UN_Old`, `SAD_New`).
- Ask about the design if it is not clear: who used which SAD, in which order, and whether the same images were used in every condition.

### 2–5. Conventional two-stage analysis

``` r
cc  <- sad_ccc(x)                 # Lin's r, Cb, v, u, rho_c per rater and condition
cmp <- sad_compare(x, cc)         # mixed model (rater random), Tukey letters
sad_reliability(x)                # ICC(A,1) and OCC per condition
p <- sad_plots(x, cc, baseline = "UN")   # p$scatter, p$box_ccc, p$gain_vs_baseline
```

Report the table of `sad_compare()` (means and letters for each statistic) and the reliability coefficients.

### 6. One-stage model: `sad_fit()`

``` r
fit <- sad_fit(x)   # structure = "auto", dispersion = "quad", family = "gaussian"
fit
summary(fit)
```

- Model: `estimate ~ z * condition`, with `z = (actual - 10)/10`, so the intercept is the mean estimate at 10% severity and the slope is the calibration per 10 p.p. (slope 1 = perfect calibration). Keep the defaults `center = 10, scale = 10`: smaller units cause convergence problems.
- `structure = "auto"` tries, in order, `"full"` (correlated rater intercept and slope per method + image + image×condition), `"noim"` (no image×condition), `"sep"` (uncorrelated rater intercepts and slopes) and `"base"` (rater effects common to all methods), and keeps the first that converges cleanly.
- **Always report which structure was retained.** If it is `"base"`, say that the model cannot tell whether the SAD changed the variation among raters, and treat the test of the SAD effect with caution (see principle 5).
- Residual SD varies with severity and condition (`dispersion = "quad"`: log SD quadratic in z).

### 7. Curves along severity: `sad_curve()`, `sad_regions()`

``` r
cv <- sad_curve(fit, reference = "UN")          # each condition vs unaided
# or explicit contrasts, e.g. two SADs:
pairs <- data.frame(a = c("SAD_Old", "SAD_New", "SAD_New"),
                    b = c("UN_Old",  "UN_New",  "SAD_Old"))
cv <- sad_curve(fit, pairs = pairs)
sad_regions(cv)                                  # severity ranges where a contrast is significant
plot(cv, "bias"); plot(cv, "expected_abs_error"); plot(cv, "difference")
```

- `cv$curves` holds `bias`, `sd_rater`, `sd_image`, `sd_resid`, `sd_total` and `expected_abs_error` (EAE) over a grid within the observed range of each condition.
- Differences have **simultaneous** 95% bands, so they can be read anywhere along the curve.
- Tabulate bias and EAE at a few severities of practical interest:

``` r
at <- c(10, 20, 50, 75)
tab <- do.call(rbind, lapply(split(cv$curves, cv$curves$condition), function(z)
  data.frame(condition = z$condition[1], severity = at,
             bias = round(approx(z$actual, z$bias, at)$y, 1),
             EAE  = round(approx(z$actual, z$expected_abs_error, at)$y, 1))))
tab[!is.na(tab$bias), ]
```

- Interpret the EAE in p.p. against the accuracy the user needs. A significant gain in ρc may correspond to an EAE reduction of a fraction of a percentage point.

### 8. Functional check: `sad_curve(type = "smooth")`

``` r
cs <- sad_curve(fit, type = "smooth", pairs = pairs)   # GAMM with mgcv::bam()
sad_regions(cs); plot(cs, "difference")
```

The linear model assumes a straight bias line per condition. When the linear model and the GAMM disagree, report both and give more weight to effects found by both.

### 9. Raters and images

``` r
rt <- sad_raters(fit); plot_sad_raters(rt)   # bias at 10% and slope per rater, linked across conditions
head(sad_images(fit))                        # images all raters over- or underestimate
```

Images with large difficulty deserve a check of their actual severity.

### 10. Sensitivity analyses

``` r
fit_lin <- sad_fit(x, structure = fit$structure, dispersion = "lin")
AIC(fit$model, fit_lin$model)                     # quadratic vs log-linear dispersion
fit_ob  <- sad_fit(x, family = "ordbeta", dispersion = "lin")   # bounded response; slow
summary(fit_ob)
```

The ordered beta fixed effects are on the logit scale: check whether the conclusions about the SAD effects hold, not whether numbers match. Report ΔAIC and the ordered beta contrasts in a supplementary table.

### 11. Planning a new study: `sad_simulate()`

- For a precise estimate of mean ρc and a test of its gain, use the precision and power criteria of Romero-Benavides et al. (2026b) and their Shiny application.
- For severity-dependent effects, check the design by simulation:

``` r
sad_simulate(n_rater = c(10, 20, 40), n_image = c(20, 40), nsim = 200,
             shift = -3, cores = 4)                       # constant shift
sad_simulate(n_rater = c(10, 20, 40), n_image = c(20, 40), nsim = 200,
             shift = 0, slope_change = -0.10, cores = 4)  # calibration only
```

Use `shift = 0, slope_change = 0, precision = 1` to get the type I error. In the paper's simulations, about 40 raters were needed for power near 0.8 against a −3 p.p. shift, and with 10 raters the type I error was 0.13, so a parametric bootstrap of the likelihood ratio test is advisable in small studies. Images should cover the whole severity range.

## What to report

1.  Design: raters, images, conditions, order of assessment, actual severity method.
2.  Conventional analysis: `sad_compare()` table and ICC/OCC.
3.  One-stage model: structure retained, fixed effects, bias curves with simultaneous bands, `sad_regions()` results, a table of bias and EAE at selected severities, the rater plot.
4.  GAMM agreement with the linear model.
5.  Sensitivity analyses (ΔAIC, ordered beta).
6.  Limitations: when the SAD is always used in the second round, its effect is confounded with learning; a control group that assesses twice without the SAD separates the two and fits in the same model.

Write results in plain terms: **where** along the severity range the SAD helped or hurt, **how** (bias, calibration, precision) and **by how much** (p.p.).

## Pitfalls

- Do not conclude "no effect" from a non-significant ρc test alone.
- Do not fit severity in units of 1 p.p.; keep `scale = 10`.
- Do not interpret differences outside the severity range shared by both conditions.
- Do not drop the image random effect: image difficulty otherwise inflates rater error.
- Do not present `"base"`-structure results as if they described rater-level changes.

## Reference

Del Ponte, E. M. & Alves, K. S. (in preparation). Beyond Lin's concordance: a one-stage hierarchical framework for validating standard area diagrams for plant disease severity. Code and data: <https://github.com/emdelponte/sad-onestage-paper>
