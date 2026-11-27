# Validation 2: parameter recovery for the many-facet rating scale model (MML)
source("R/core.R"); source("R/simulate.R")
gen <- function(n_obj, sev, crit, tau, mu = 0, sd = 1) {
  th <- rnorm(n_obj, mu, sd)
  g <- expand.grid(object = seq_len(n_obj), rater = names(sev), criterion = names(crit), stringsAsFactors = FALSE)
  g$score <- .sim_score(th[g$object] - crit[g$criterion] - sev[g$rater], tau) + 1
  g$source <- "all"; g
}
run <- function(label, R, n_obj, sev_fun, crit, tau, mu = 0, sd = 1) {
  res <- lapply(seq_len(R), function(r) {
    sev <- sev_fun(); sev <- sev - mean(sev)
    d <- gen(n_obj, sev, crit, tau, mu, sd)
    f <- tryCatch(ra_mfrm(d), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    rt <- f$raters[match(names(sev), f$raters$rater), ]
    keep <- !rt$extreme
    est <- rt$severity - mean(rt$severity[keep])
    list(err = (est - sev)[keep], cover = (abs(est - sev) <= 1.96 * rt$se)[keep],
         infit = rt$infit[keep], outfit = rt$outfit[keep], conv = f$converged, p = f$fixed_p,
         n_ext = sum(!keep), obj_rel = f$object_reliability)
  })
  res <- Filter(Negate(is.null), res)
  err <- unlist(lapply(res, `[[`, "err"))
  cat(sprintf("%-44s reps=%d conv=%.1f%% | bias=%+.3f RMSE=%.3f | 95%% CI coverage=%.1f%% | mean infit=%.2f outfit=%.2f | extreme raters=%d\n",
      label, length(res), 100 * mean(sapply(res, `[[`, "conv")), mean(err), sqrt(mean(err^2)),
      100 * mean(unlist(lapply(res, `[[`, "cover"))), mean(unlist(lapply(res, `[[`, "infit"))),
      mean(unlist(lapply(res, `[[`, "outfit"))), sum(sapply(res, `[[`, "n_ext"))))
  invisible(sapply(res, `[[`, "p"))
}
set.seed(2027)
tau4 <- c(-1.5, 0, 1.5); tau5 <- c(-2, -0.7, 0.7, 2)
cat("Recovery of rater severity (true severities drawn from N(0, 0.6^2) each replication)\n")
run("A: 25 items x 5 experts, 1-5 scale, high", 300, 25, function() setNames(rnorm(5, 0, 0.6), paste0("R", 1:5)), c(x = 0), tau5, mu = 1.5)
run("B: 30 items x 7 experts, 1-4 scale", 300, 30, function() setNames(rnorm(7, 0, 0.6), paste0("R", 1:7)), c(x = 0), tau4, mu = 1)
run("C: 100 objects x 4 raters x 3 criteria, 1-4", 200, 100, function() setNames(rnorm(4, 0, 0.6), paste0("R", 1:4)), c(a = -0.4, b = 0, c = 0.4), tau4)
run("D: 200 objects x 6 raters x 4 criteria, 1-5", 100, 200, function() setNames(rnorm(6, 0, 0.6), paste0("R", 1:6)), c(a = -0.5, b = 0, c = 0.2, d = 0.3), tau5)
cat("\nType I error of the rater homogeneity Wald chi-square (all raters equally severe; nominal 5%)\n")
for (des in list(list("A0: 25 x 5, 1-5", 25, 5, c(x = 0), tau5, 1.5), list("C0: 100 x 4 x 3, 1-4", 100, 4, c(a = -0.4, b = 0, c = 0.4), tau4, 0))) {
  p <- run(des[[1]], 300, des[[2]], function() setNames(rep(0, des[[3]]), paste0("R", seq_len(des[[3]]))), des[[4]], des[[5]], mu = des[[6]])
  cat(sprintf("   -> rejection rate at p < .05: %.1f%%\n", 100 * mean(p < 0.05)))
}
