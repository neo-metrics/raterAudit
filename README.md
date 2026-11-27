# raterAudit

raterAudit is a free R Shiny application for auditing judgments made by expert validators and raters.
It needs no coding: upload a spreadsheet and read the results.

- **Content validity:** Aiken's V per item with exact p-values and critical values for any number of raters
  and categories; ICC(2,1) and ICC(2,k) (absolute agreement) with 95% confidence intervals; an APA-style
  report paragraph.
- **Rater severity:** many-facet Rasch rating scale model estimated by marginal maximum likelihood, with fit
  statistics and a Wald test of rater homogeneity.
- **Rater effects:** centrality, extremity, halo, and misfit flags; differential rater functioning (DRF)
  across groups.
- **Sources and stability:** comparison of rater sources (e.g., human vs AI), replicate stability, and
  drift across versions.
- **Export:** all tables in one .xlsx workbook.

## Run

Requires R 4.1 or later.

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "readxl", "writexl", "scales"))

# run directly from GitHub
shiny::runGitHub("raterAudit", "neo-metrics")

# or, after downloading this repository, from its parent folder
shiny::runApp("raterAudit")
```

## Data

- **Wide:** one row per item, one column per rater (e.g., R1–R5). Typical for expert content validation.
- **Long:** one row per score. Required columns: object, rater, score. Optional: criterion, rater source,
  replicate, version, object group.

Three example files are in `examples/` and can also be downloaded from the app's *Start here* tab.

## Files

| Path | Content |
| --- | --- |
| `app.R` | User interface |
| `R/core.R` | Analysis functions (`ra_data`, `ra_from_wide`, `ra_aiken`, `ra_icc`, `ra_mfrm`, `ra_effects`, `ra_drf`, ...) |
| `R/simulate.R` | Simulated example datasets (`ra_example()`) |
| `examples/` | Example data files (wide and long layouts) |
| `validation/` | Scripts and results that check every component against independent references |

## Validation

See [`validation/README.md`](validation/README.md). Aiken's V and ICC match exact enumeration and the psych
package; rater severities agree with an independent MML estimator (within 0.009 logits) and with eRm
(within 0.001 logits); simulation studies show unbiased estimates and controlled error rates.

## Developers

Febrian Solikhin, Antuni Wiyarsi, and Ali Muhson — Universitas Negeri Yogyakarta, Indonesia.

## License

GPL-3. See [`LICENSE`](LICENSE).
