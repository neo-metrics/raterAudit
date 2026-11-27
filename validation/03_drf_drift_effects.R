# Validation 3: error rates of DRF, drift, and rater-effect flags
source("R/core.R"); source("R/simulate.R")
args <- commandArgs(TRUE); part <- if (length(args)) args[1] else "all"
sim <- function(n_obj, sev, crit, tau, drf = NULL, tau_r = list(), reps = NULL, versions = NULL, vshift = NULL, cellsd = 0.35) {
  th <- rnorm(n_obj); grp <- sample(c("F", "M"), n_obj, TRUE)
  rows <- list()
  for (r in names(sev)) {
    vs <- if (!is.null(versions) && r %in% names(reps)) versions else ""
    for (v in vs) {
      g <- expand.grid(object = seq_len(n_obj), criterion = names(crit), stringsAsFactors = FALSE)
      shared <- rnorm(nrow(g), 0, if (r %in% names(reps)) cellsd else 0)
      for (k in seq_len(if (r %in% names(reps)) reps[[r]] else 1)) {
        s <- sev[r] + (if (!is.null(drf) && r == drf$rater) ifelse(grp[g$object] == "M", drf$size, 0) else 0) +
             (if (!is.null(vshift) && v == versions[2] && r %in% names(vshift)) vshift[[r]] else 0)
        tr <- if (r %in% names(tau_r)) tau_r[[r]] else tau
        rows[[length(rows) + 1]] <- data.frame(object = g$object, rater = r, criterion = g$criterion,
          score = .sim_score(th[g$object] - crit[g$criterion] - s + shared, tr) + 1, replicate = k, version = v, group = grp[g$object])
      }
    }
  }
  do.call(rbind, rows)
}
prep <- function(x, ...) ra_data(x, "object", "rater", "criterion", "score", ...)

if (part %in% c("all", "drf")) {
  cat("DRF (FDR-adjusted p < .05 and |contrast| >= 0.5)\n")
  designs <- list(
    list(lab = "60 objects x 4 raters x 3 criteria, 1-5", n = 60, sev = c(r1 = 0.4, r2 = -0.6, r3 = -0.6, r4 = -0.5), crit = c(a = -0.3, b = 0, c = 0.3), tau = c(-1.8, -0.6, 0.6, 1.8)),
    list(lab = "120 objects x 6 raters x 2 criteria, 1-4", n = 120, sev = c(r1 = 0.3, r2 = -0.3, r3 = 0, r4 = 0.5, r5 = -0.2, r6 = -0.3), crit = c(a = -0.2, b = 0.2), tau = c(-1.5, 0, 1.5)))
  for (D in designs) {
    R <- 150
    null <- lapply(seq_len(R), function(i) { x <- ra_drf(prep(sim(D$n, D$sev, D$crit, D$tau), group = "group")); x$rater[x$decision == "DRF"] })
    alt9 <- lapply(seq_len(R), function(i) { x <- ra_drf(prep(sim(D$n, D$sev, D$crit, D$tau, drf = list(rater = "r2", size = 0.9)), group = "group")); x$rater[x$decision == "DRF"] })
    alt5 <- lapply(seq_len(R), function(i) { x <- ra_drf(prep(sim(D$n, D$sev, D$crit, D$tau, drf = list(rater = "r2", size = 0.5)), group = "group")); x$rater[x$decision == "DRF"] })
    cat(sprintf("  %s: no DRF -> any rater flagged %.1f%% | DRF 0.9 -> detected %.1f%%, other raters flagged %.1f%% | DRF 0.5 -> detected %.1f%%\n",
        D$lab, 100 * mean(lengths(null) > 0), 100 * mean(sapply(alt9, function(v) "r2" %in% v)),
        100 * mean(sapply(alt9, function(v) any(v != "r2"))), 100 * mean(sapply(alt5, function(v) "r2" %in% v))))
  }
}

if (part %in% c("all", "drift")) {
  cat("Drift across versions (FDR-adjusted p < .05 and |change| >= 0.3); two anchor raters + two raters with 3 replicates x 2 versions\n")
  sev <- c(h1 = -0.2, h2 = 0.2, m1 = 0.3, m2 = 0.1); crit <- c(a = -0.3, b = 0.3); tau <- c(-1.5, 0, 1.5)
  R <- 100
  null <- sapply(seq_len(R), function(i) { x <- ra_drift(prep(sim(80, sev, crit, tau, reps = list(m1 = 3, m2 = 3), versions = c("v1", "v2")), replicate = "replicate", version = "version")); any(x$table$decision == "drift") })
  alt <- sapply(seq_len(R), function(i) { x <- ra_drift(prep(sim(80, sev, crit, tau, reps = list(m1 = 3, m2 = 3), versions = c("v1", "v2"), vshift = list(m1 = 0.5)), replicate = "replicate", version = "version")); t <- x$table; c(hit = any(t$rater == "m1" & t$decision == "drift"), fp = any(t$rater == "m2" & t$decision == "drift")) })
  cat(sprintf("  no drift -> flagged %.1f%% | drift 0.5 logit -> detected %.1f%%, other rater flagged %.1f%%\n", 100 * mean(null), 100 * mean(alt["hit", ]), 100 * mean(alt["fp", ])))
}

if (part %in% c("all", "effects")) {
  cat("Rater-effect flags (100 objects x 5 raters x 3 criteria, 1-5)\n")
  sev <- c(r1 = 0.3, r2 = -0.3, r3 = 0, r4 = 0.2, r5 = -0.2); crit <- c(a = -0.3, b = 0, c = 0.3); tau <- c(-1.8, -0.6, 0.6, 1.8)
  R <- 100
  fl <- function(x) { e <- ra_effects(ra_mfrm(prep(x))); setNames(e$flags, e$rater) }
  null <- lapply(seq_len(R), function(i) fl(sim(100, sev, crit, tau)))
  cen <- lapply(seq_len(R), function(i) fl(sim(100, sev, crit, tau, tau_r = list(r2 = c(-3.5, -1.2, 1.2, 3.5)))))
  ext <- lapply(seq_len(R), function(i) fl(sim(100, sev, crit, tau, tau_r = list(r2 = c(-0.6, -0.2, 0.2, 0.6)))))
  rate <- function(L, pat, who = NULL) 100 * mean(unlist(lapply(L, function(v) grepl(pat, if (is.null(who)) v else v[who]))))
  cat(sprintf("  model-consistent raters: flagged centrality %.1f%%, extreme %.1f%%, halo %.1f%%, misfit/overfit %.1f%% (per rater)\n",
      rate(null, "centrality"), rate(null, "extreme"), rate(null, "halo"), rate(null, "fit")))
  cat(sprintf("  rater with compressed scale use (centrality): detected %.1f%% | other raters flagged centrality %.1f%%\n",
      rate(cen, "centrality", "r2"), 100 * mean(unlist(lapply(cen, function(v) grepl("centrality", v[names(v) != "r2"]))))))
  cat(sprintf("  rater with extreme scale use: detected %.1f%%\n", rate(ext, "extreme", "r2")))
}
