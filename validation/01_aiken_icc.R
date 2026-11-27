# Validation 1: Aiken's V (exact probabilities and critical values) and ICC
source("R/core.R")
ok <- function(cond, msg) cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n")

# (a) exact tail probability: convolution vs brute-force enumeration
for (n in 2:6) for (c in 2:6) {
  S <- rowSums(as.matrix(expand.grid(rep(list(0:(c - 1)), n))))
  brute <- vapply(0:(n * (c - 1)), function(s) mean(S >= s), 0)
  if (max(abs(brute - .aiken_tail(n, c))) > 1e-12) ok(FALSE, sprintf("tail n=%d c=%d", n, c))
}
ok(TRUE, "exact right-tail probabilities equal brute-force enumeration for n = 2..6 raters, c = 2..6 categories")

# (b) V formula against a hand calculation (Aiken, 1985): V = sum(r - lo) / (n (c - 1))
d <- data.frame(object = "i1", rater = paste0("R", 1:5), criterion = "x", score = c(5, 5, 4, 5, 4), source = "all")
a <- ra_aiken(d, 1, 5)
ok(abs(a$V - (4 + 4 + 3 + 4 + 3) / (5 * 4)) < 1e-12, sprintf("V for ratings 5,5,4,5,4 on 1-5 = %.2f (hand: 0.90)", a$V))

# (c) critical values from Aiken's (1985) table (p < .05) as cited in applied studies,
#     e.g. 9 raters x 5 categories = 0.72 (Mendeley Data gwg3zvz2df, citing Aiken, 1985, p. 134)
tab <- rbind(c(3, 4, 1.00), c(5, 4, 0.87), c(5, 5, 0.80), c(6, 4, 0.78), c(7, 4, 0.76), c(7, 5, 0.75),
             c(8, 4, 0.75), c(9, 5, 0.72))
for (i in seq_len(nrow(tab))) {
  v <- aiken_critical(tab[i, 1], 1, tab[i, 2])
  ok(abs(round(v, 2) - tab[i, 3]) < 0.006, sprintf("critical V, %d raters, %d categories = %.3f (table %.2f)", tab[i, 1], tab[i, 2], v, tab[i, 3]))
}

# (d) items with a missing rating use their own n
d2 <- rbind(d, data.frame(object = "i2", rater = paste0("R", 1:4), criterion = "x", score = c(5, 5, 5, 5), source = "all"))
a2 <- ra_aiken(d2, 1, 5)
ok(a2$n_experts[2] == 4 && abs(a2$V_critical[2] - aiken_critical(4, 1, 5)) < 1e-12, "item with 4 of 5 ratings uses the critical value for 4 raters")

# (e) ICC against psych::ICC on random and structured data
suppressMessages(library(psych)); set.seed(3)
for (rep in 1:5) {
  n <- sample(8:40, 1); k <- sample(3:8, 1)
  M <- matrix(pmin(7, pmax(1, round(4 + matrix(rnorm(n, 0, 1.2), n, k) + rnorm(n * k, 0, 0.8) + rep(rnorm(k, 0, 0.4), each = n)))), n, k)
  colnames(M) <- paste0("R", seq_len(k))
  dd <- data.frame(object = rep(seq_len(n), k), rater = rep(paste0("R", seq_len(k)), each = n), criterion = "x", score = as.vector(M), source = "all")
  ic <- ra_icc(dd); P <- psych::ICC(as.data.frame(M), lmer = FALSE)$results
  ours <- c(ic$ICC, ic$ci_lower, ic$ci_upper)
  ref <- c(P$ICC[P$type == "ICC2"], P$ICC[P$type == "ICC2k"], P[P$type == "ICC2", "lower bound"], P[P$type == "ICC2k", "lower bound"],
           P[P$type == "ICC2", "upper bound"], P[P$type == "ICC2k", "upper bound"])
  ok(max(abs(ours - ref)) < 1e-6, sprintf("ICC(2,1), ICC(2,k) and 95%% CIs equal psych::ICC (n = %d, k = %d)", n, k))
}

# (f) pairwise agreement: exact, adjacent, and quadratic weighted kappa vs psych::cohen.kappa
set.seed(4); M <- matrix(sample(1:5, 60, TRUE), 30, 2); M[, 2] <- pmin(5, pmax(1, M[, 1] + sample(-1:1, 30, TRUE)))
dd <- data.frame(object = rep(1:30, 2), rater = rep(c("A", "B"), each = 30), criterion = "x", score = as.vector(M), source = "all")
ag <- ra_agreement(dd)
ck <- psych::cohen.kappa(M, w = outer(1:5, 1:5, function(i, j) (i - j)^2), levels = 1:5)
ok(abs(ag$qwk - ck$weighted.kappa) < 1e-8, sprintf("QWK = %.4f equals psych::cohen.kappa (%.4f)", ag$qwk, ck$weighted.kappa))
ok(abs(ag$exact - mean(M[, 1] == M[, 2])) < 1e-12 && abs(ag$adjacent - mean(abs(M[, 1] - M[, 2]) <= 1)) < 1e-12, "exact and adjacent agreement equal hand calculation")

# (g) stability across replicates: one-way ICC(1) and ICC(1,k=3) vs psych::ICC
source("R/simulate.R")
e <- ra_data(ra_example("argumentation"), "student", "rater", "criterion", "score", replicate = "replicate", version = "version")
st <- ra_stability(e)
for (i in seq_len(nrow(st))) {
  u <- e[paste0(e$rater, " @ ", e$version) == st$unit[i], ]
  M <- as.data.frame(unclass(tapply(u$score, list(paste(u$object, u$criterion), u$replicate), mean)))
  P <- psych::ICC(M, lmer = FALSE)$results
  ok(abs(st$icc_single[i] - P$ICC[P$type == "ICC1"]) < 1e-6 && abs(st$rel_mean_of_3[i] - P$ICC[P$type == "ICC1k"]) < 1e-6,
     sprintf("stability ICC(1) and ICC(1,3) for %s equal psych::ICC", st$unit[i]))
}
