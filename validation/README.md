# raterAudit validation

Run each script from the `raterAudit` folder, e.g. `Rscript validation/01_aiken_icc.R`.
Results below were obtained on 27 November 2026 (R 4.x; psych and eRm from Debian packages).

## 01_aiken_icc.R — exact checks (22/22 PASS)

| Component | Reference | Result |
|---|---|---|
| Aiken's V exact right-tail probability | brute-force enumeration, 2–6 raters × 2–6 categories | identical (to 1e-12) |
| Aiken's V critical values | Aiken (1985) table values cited in applied work (3/4, 5/4, 5/5, 6/4, 7/4, 7/5, 8/4, 9/5 raters/categories) | identical to two decimals |
| V with a missing rating | critical value for the item's own number of raters | correct |
| ICC(2,1), ICC(2,k) and 95% CIs | `psych::ICC` (5 random data sets) | identical (to 1e-6) |
| Quadratic weighted kappa | `psych::cohen.kappa` | identical |
| Exact / adjacent agreement | hand calculation | identical |
| Replicate stability ICC(1), ICC(1,3) | `psych::ICC` | identical (to 1e-6) |

## 02_mfrm_recovery.R — parameter recovery, rater severity (MML)

| Design | Reps | Bias | RMSE | 95% CI coverage | Mean infit |
|---|---|---|---|---|---|
| 25 items × 5 experts, 1–5 | 300 | 0.000 | 0.29 | 95.3% | 0.83 |
| 30 items × 7 experts, 1–4 | 300 | 0.000 | 0.28 | 94.5% | 0.88 |
| 100 objects × 4 raters × 3 criteria, 1–4 | 200 | 0.000 | 0.07 | 94.4% | 0.93 |
| 200 objects × 6 raters × 4 criteria, 1–5 | 100 | 0.000 | 0.04 | 95.3% | 0.96 |

Rater homogeneity Wald test, all raters equally severe (nominal 5%): 2.0% (25 × 5), 1.8% (30 × 7), 4.0% (100 × 4 × 3).
The conventional fixed chi-square that ignores the covariance of the estimates rejected 12.0% and 9.7%; it was replaced by the Wald test.
Mean-squares fall below 1 when each object is rated by few raters; this is expected because object measures are estimated from the same data.

## 03_drf_drift_effects.R — error rates

| Analysis | Condition | Result |
|---|---|---|
| DRF, 60 × 4 × 3 | no DRF | any rater flagged 7.3% (an earlier run with 200 reps: 4.5%) |
| | DRF 0.9 logit | detected 96.0%; another rater also flagged 7.3% |
| | DRF 0.5 logit | detected 39.3% |
| DRF, 120 × 6 × 2 | no DRF | any rater flagged 2.7% |
| | DRF 0.9 logit | detected 96.0%; another rater also flagged 10.0% |
| | DRF 0.5 logit | detected 46.0% |
| Drift, 80 × 2 criteria, 3 replicates × 2 versions | no drift | flagged 0.0% |
| | drift 0.5 logit | detected 99.0%; other rater flagged 0.0% |
| Rater-effect flags, 100 × 5 × 3 | model-consistent raters | centrality 0.0%, extreme 0.0%, halo 0.4%, misfit 0.0% |
| | compressed scale use | centrality detected 100% |
| | extreme scale use | extreme detected 95% |

## 04_independent_estimators.R — agreement with independent estimators

Maximum absolute difference in rater severity (logits) between raterAudit and an independently coded MML
estimator: 0.000–0.009 across six data sets. Against `eRm::LPCM` (conditional ML), where applicable: 0.001.

## Known limitations

- DRF: when one rater truly has DRF, another rater is also flagged in 7–10% of replications.
- Small panels: the Wald test is conservative (about 2% at nominal 5%).
- A rater who always gives the same end score has no finite severity estimate; it is shown as a bound and excluded from the homogeneity test.
- The DRF and drift simulations cover the designs listed above only.
- Not yet compared directly with TAM or FACETS; `05_tam_comparison.R` does this when TAM is installed.
