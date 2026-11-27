# Simulation: Type I error and power of rater homogeneity tests

Code for the brief report on testing rater homogeneity in small panels.
Run all scripts from the repository root.

| Script | Purpose |
|---|---|
| `sim.R` | Fixed chi-square (JMLE and MML standard errors) and MML Wald test. Run as `Rscript simulation/sim.R 0 2` and `Rscript simulation/sim.R 1 2` (two parallel parts). |
| `sim2.R` | Friedman test and within-object permutation test. Run as `Rscript simulation/sim2.R 0` and `Rscript simulation/sim2.R 1`. |
| `summarize.R` | Combines the output into `summary.csv` and prints size-adjusted power. |
| `fig.R` | Draws Figure 1 (Type I error rates). |
| `summary.csv` | Rejection rates (%) at alpha = .05 for every condition. |

Design: 3, 5, 7, or 10 raters fully crossed with 10, 20, or 40 objects; 4 or 5 categories;
500 replications per null condition; one rater shifted by 0.5 or 1.0 logits (20 objects,
5 categories, 300 replications) for power. Seeds are fixed, so results are reproducible.
