# One-stage hierarchical validation of standard area diagrams

Research compendium for the paper proposing a one-stage mixed model, with a
functional comparison of error curves, for the validation of standard area
diagrams (SADs) used to estimate plant disease severity. The paper compares it
with the conventional two-stage approach (Lin's concordance per rater, then a
test of means), using a simulation study and the reanalysis of five published
validation studies.

All analyses use the `sad_*` functions of the
[{r4pde}](https://github.com/emdelponte/r4pde) package.

## Reanalysed studies

| File | Disease, host | Design | Raters × images | Source |
|---|---|---|---|---|
| `alternaria_pecan.csv` | Alternaria black spot, pecan | without vs with one SAD | 34 × 35 | [doi:10.1007/s40858-025-00761-w](https://link.springer.com/article/10.1007/s40858-025-00761-w) |
| `tan_spot_wheat.csv` | Tan spot, wheat | without vs with one SAD | 38 × 40 | [periodicos.ufv.br](https://periodicos.ufv.br/STFT/article/view/23436/12099) |
| `soybean_rust.csv` | Soybean rust | two SADs (Old, New), 20 raters each, without vs with | 40 × 50 | Franceschi et al. (2020) Plant Pathology [doi:10.1111/ppa.13148](https://doi.org/10.1111/ppa.13148) |
| `coffee_leaf_rust.csv` | Coffee leaf rust | without, Old SAD and New SAD, same raters | 17 × 40 | Romero-Benavides et al. (2026) Plant Pathology [doi:10.1111/ppa.70258](https://doi.org/10.1111/ppa.70258) |
| `brownspot_rice.csv` | Brown spot, rice | four SADs (LIN/LOG × COL/BW), 10 raters each, without vs with | 40 × 50 | Schwanck & Del Ponte (2014) J. Phytopathology [doi:10.1111/jph.12246](https://doi.org/10.1111/jph.12246) |

Every file is in long format, with one row per estimate and these columns:

- `study`
- `rater`
- `image`
- `actual`: actual severity (%)
- `group`: SAD used by the rater when SADs were assigned to different raters
- `method`: `UN` = unaided, `SAD`, `Old`, `New`
- `estimate`: estimated severity (%)

Data notes:

- **Tan spot:** raters 39 and 40 had no data and were removed.
- **Coffee:** raters are anonymised (R01 to R17). Three raters excluded in the original study are not included.
- **Rice:** the rater-to-SAD assignment (blocks of 10 raters) was checked against Tables 2 and 3 of the original paper.

## Running the analyses

```r
remotes::install_github("emdelponte/r4pde")
install.packages(c("glmmTMB", "emmeans", "lmerTest", "multcompView", "ggridges",
                   "dplyr", "tidyr", "ggplot2", "patchwork"))
source("run_all.R")
```

| Script | What it does |
|---|---|
| `R/00_setup.R` | packages, study definitions, data reader |
| `R/01_two_stage.R` | Lin's statistics per rater, comparison of conditions, ICC/OCC, exploratory figures |
| `R/02_one_stage.R` | one-stage model, bias and expected absolute error curves, linear and functional (GAMM) differences, raters, images |
| `R/03_sensitivity.R` | linear vs quadratic dispersion, ordered beta regression |
| `R/04_simulation.R` | power and type I error of the two-stage and one-stage tests (slow; stored results in `output/simulation/`) |
| `R/05_figures.R` | figures of the main text (Figs 1–4) and supplement (Figs S1–S2): one theme, Okabe-Ito colour-blind palette, panel letters; PDF and 600 dpi PNG at 174 mm width |

Outputs are written to `output/tables`, `output/figures` and `output/models`.
