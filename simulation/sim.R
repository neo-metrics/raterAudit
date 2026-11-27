# Brief Report simulation: Type I error and power of rater-homogeneity tests
args <- commandArgs(TRUE); part <- as.integer(args[1]); nparts <- as.integer(args[2])
source("R/core.R"); source("R/simulate.R")
gen <- function(I, J, K, mu, shift) {
  th <- rnorm(I, mu, 1); sev <- rep(0, J); sev[1] <- shift; sev <- sev - mean(sev)
  tau <- if (K == 4) c(-1.5, 0, 1.5) else c(-2, -0.7, 0.7, 2)
  g <- expand.grid(object = seq_len(I), rater = paste0("R", seq_len(J)), stringsAsFactors = FALSE)
  g$criterion <- "x"; g$source <- "all"
  g$score <- .sim_score(th[g$object] - sev[match(g$rater, paste0("R", seq_len(J)))], tau) + 1
  g
}
Q <- function(a, se) { w <- 1 / se^2; sum(w * (a - sum(w * a) / sum(w))^2) }
one <- function(I, J, K, mu, shift) {
  d <- gen(I, J, K, mu, shift)
  fj <- tryCatch(ra_mfrm(d, method = "jmle"), error = function(e) NULL)
  fm <- tryCatch(ra_mfrm(d), error = function(e) NULL)
  if (is.null(fj) || is.null(fm)) return(c(NA, NA, NA, NA))
  k <- !fm$raters$extreme; df <- sum(k) - 1
  if (df < 1) return(c(NA, NA, NA, sum(!k)))
  kj <- !fj$raters$extreme
  pj <- pchisq(Q(fj$raters$severity[kj], fj$raters$se[kj]), sum(kj) - 1, lower.tail = FALSE)
  pm <- pchisq(Q(fm$raters$severity[k], fm$raters$se[k]), df, lower.tail = FALSE)
  pw <- if (fm$method == "MML") fm$fixed_p else NA
  c(pj, pm, pw, sum(!k))
}
conds <- rbind(
  expand.grid(J = c(3, 5, 7, 10), I = c(10, 20, 40), K = c(4, 5), shift = 0, R = 500),
  expand.grid(J = c(3, 5, 7, 10), I = 20, K = 5, shift = c(0.5, 1.0), R = 300))
conds$id <- seq_len(nrow(conds))
mine <- conds[conds$id %% nparts == part, ]
out <- list()
for (i in seq_len(nrow(mine))) {
  cd <- mine[i, ]; set.seed(1000 + cd$id)
  r <- t(replicate(cd$R, one(cd$I, cd$J, cd$K, mu = 1, shift = cd$shift)))
  out[[i]] <- data.frame(cd, rep = seq_len(cd$R), p_jmle_Q = r[, 1], p_mml_Q = r[, 2], p_wald = r[, 3], n_extreme = r[, 4])
  write.csv(do.call(rbind, out), sprintf("simulation/res_part%d.csv", part), row.names = FALSE)
  cat(format(Sys.time()), "done cond", cd$id, "\n")
}
cat("FINISHED\n")
