# Compares raterAudit rater severities (MML) with TAM (tam.mml.mfr).
# Run from the raterAudit folder:  source("validation/05_tam_comparison.R")
# Requires: install.packages("TAM")

source("R/core.R"); source("R/simulate.R")
library(TAM)

compare_tam <- function(x, obj, rat, cr, sc, ...) {
  d <- ra_data(x, obj, rat, cr, sc, ...)
  d <- .first_rep(d)                      # one score per object x rater x criterion
  d$score <- d$score - min(d$score)

  # raterAudit (MML, default)
  ra <- ra_mfrm(d)$raters[, c("rater", "severity")]

  # TAM (MML): criteria = items, raters = facet
  w <- reshape(d[, c("object", "rater", "criterion", "score")],
               idvar = c("object", "rater"), timevar = "criterion", direction = "wide")
  resp <- w[, grep("^score\\.", names(w)), drop = FALSE]
  mod <- tam.mml.mfr(resp = resp, facets = data.frame(rater = w$rater), pid = w$object,
                     formulaA = ~ item + rater + step, verbose = FALSE)
  xs <- mod$xsi.facets
  tm <- xs[xs$facet == "rater", c("parameter", "xsi")]
  tm$rater <- sub("^rater", "", tm$parameter)

  out <- merge(ra, tm[, c("rater", "xsi")], by = "rater")
  names(out)[3] <- "tam_xsi"
  out$tam_xsi <- out$tam_xsi - mean(out$tam_xsi)   # common origin
  out$difference <- out$severity - out$tam_xsi
  cat(sprintf("\nr = %.4f | max |difference| = %.3f logits | SD ratio raterAudit/TAM = %.3f\n",
              cor(out$severity, out$tam_xsi), max(abs(out$difference)),
              sd(out$severity) / sd(out$tam_xsi)))
  print(out, digits = 3, row.names = FALSE)
  invisible(out)
}

cat("== Argumentation: lecturers vs LLM (first replicate, first version) ==")
a <- compare_tam(ra_example("argumentation"), "student", "rater", "criterion", "score",
                 source = "source", replicate = "replicate", version = "version")

cat("\n== Practicum: peers vs lecturer ==")
p <- compare_tam(ra_example("practicum"), "student", "rater", "criterion", "score", source = "source")

# Suggested pass criteria: r >= .99 and max |difference| < 0.15 logits.
# Small differences are expected because the two programs use different quadrature and
# identification constraints.
