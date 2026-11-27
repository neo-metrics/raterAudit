args <- commandArgs(TRUE); part <- as.integer(args[1])
# Additional tests that need no model fit: Friedman and within-item permutation test
source("R/core.R"); source("R/simulate.R")
gen_mat <- function(I, J, K, mu, shift) {
  th <- rnorm(I, mu, 1); sev <- rep(0, J); sev[1] <- shift; sev <- sev - mean(sev)
  tau <- if (K == 4) c(-1.5, 0, 1.5) else c(-2, -0.7, 0.7, 2)
  matrix(.sim_score(as.vector(outer(th, sev, "-")), tau) + 1, I, J)
}
perm_p <- function(X, B = 499) {
  stat <- function(M) var(colMeans(M))
  s0 <- stat(X)
  sb <- replicate(B, stat(t(apply(X, 1, sample))))
  (1 + sum(sb >= s0 - 1e-12)) / (B + 1)
}
fried_p <- function(X) { if (all(apply(X, 1, function(r) length(unique(r)) == 1))) return(1)
  suppressWarnings(friedman.test(X)$p.value) }
conds <- rbind(
  expand.grid(J = c(3, 5, 7, 10), I = c(10, 20, 40), K = c(4, 5), shift = 0, R = 500),
  expand.grid(J = c(3, 5, 7, 10), I = 20, K = 5, shift = c(0.5, 1.0), R = 300))
conds$id <- seq_len(nrow(conds))
out <- lapply(which(conds$id %% 2 == part), function(i) {
  cd <- conds[i, ]; set.seed(5000 + cd$id)
  r <- t(replicate(cd$R, { X <- gen_mat(cd$I, cd$J, cd$K, 1, cd$shift); c(fried_p(X), perm_p(X)) }))
  data.frame(cd, rep = seq_len(cd$R), p_friedman = r[, 1], p_perm = r[, 2])
})
write.csv(do.call(rbind, out), sprintf("simulation/res_nonpar%d.csv", part), row.names = FALSE)
cat("FINISHED\n")
