# Validation 4: rater severity vs two independent estimators
#  (a) eRm::LPCM conditional ML (third-party package), (b) MML written independently with a generic optimiser
# Requires the eRm package. Run from the raterAudit folder.
suppressMessages(library(eRm)); source("R/core.R"); source("R/simulate.R")
mml_raters <- function(d, Q = 41) {
  d <- .first_rep(d)
  if ("version" %in% names(d)) d <- d[d$version == ave(d$version, d$rater, FUN = function(v) sort(v)[1]), ]
  x <- d$score - min(d$score); m <- max(x)
  o <- as.integer(factor(d$object)); r <- factor(d$rater); cr <- factor(d$criterion)
  ri <- as.integer(r); ci <- as.integer(cr); R <- nlevels(r); C <- nlevels(cr); N <- max(o)
  gh <- statmod_gh(Q)
  unpack <- function(p) {
    k <- 0; a <- c(p[seq_len(R - 1)], -sum(p[seq_len(R - 1)])); k <- R - 1
    de <- if (C > 1) { v <- p[k + seq_len(C - 1)]; k <- k + C - 1; c(v, -sum(v)) } else 0
    tau <- p[k + seq_len(m)]; k <- k + m
    list(a = a, de = de, tau = tau, s = exp(p[k + 1]))
  }
  nll <- function(p) {
    u <- unpack(p); ll <- matrix(0, N, Q)
    ctau <- c(0, cumsum(u$tau))
    for (q in seq_len(Q)) {
      eta <- u$s * gh$x[q] - u$a[ri] - u$de[ci]
      lp <- outer(eta, 0:m) - matrix(ctau, length(eta), m + 1, byrow = TRUE)
      mx <- apply(lp, 1, max); lse <- mx + log(rowSums(exp(lp - mx)))
      ll[, q] <- as.vector(rowsum(lp[cbind(seq_along(x), x + 1)] - lse, o))
    }
    lw <- ll + matrix(log(gh$w), N, Q, byrow = TRUE)
    mx <- apply(lw, 1, max); -sum(mx + log(rowSums(exp(lw - mx))))
  }
  p0 <- c(rep(0, R - 1), rep(0, max(C - 1, 0)), seq(-1, 1, length.out = m), 0)
  fit <- nlminb(p0, nll, control = list(iter.max = 1000, eval.max = 2000))
  u <- unpack(fit$par); setNames(u$a, levels(r))
}
statmod_gh <- function(n) {   # Gauss-Hermite nodes for N(0,1) via Golub-Welsch
  i <- seq_len(n - 1); J <- matrix(0, n, n); J[cbind(i, i + 1)] <- sqrt(i); J[cbind(i + 1, i)] <- sqrt(i)
  e <- eigen(J, symmetric = TRUE); list(x = e$values, w = e$vectors[1, ]^2)
}
cml_raters <- function(d) {
  d <- .first_rep(d)
  if ("version" %in% names(d)) d <- d[d$version == ave(d$version, d$rater, FUN = function(v) sort(v)[1]), ]
  x <- d$score - min(d$score); m <- max(x)
  d$vi <- paste(d$rater, d$criterion, sep = "|")
  X <- tapply(x, list(d$object, d$vi), mean)
  vi <- colnames(X); X <- unname(as.matrix(X)); storage.mode(X) <- "integer"; rr <- sub("\\|.*", "", vi); cc <- sub(".*\\|", "", vi)
  rl <- sort(unique(rr)); cl <- sort(unique(cc))
  code <- function(f, lv) matrix(sapply(lv[-1], function(l) as.numeric(f == l)), length(f))
  F <- cbind(code(rr, rl), if (length(cl) > 1) code(cc, cl))
  catcols <- if (m >= 2) sapply(2:m, function(k) rep(as.numeric(1:m >= k), length(vi))) else NULL
  Wf <- do.call(rbind, lapply(seq_along(vi), function(j) outer(1:m, F[j, ])))
  W <- cbind(Wf, catcols)
  fit <- LPCM(X, W = W, sum0 = FALSE, se = FALSE)
  eta <- fit$etapar[seq_len(length(rl) - 1)]
  b <- c(0, eta); names(b) <- rl
  -b   # LPCM eta = easiness; return severity
}
first <- function(d) { d <- .first_rep(d); if ("version" %in% names(d)) d <- d[d$version == ave(d$version, d$rater, FUN = function(v) sort(v)[1]), ]; d }
sets <- list(
  "Argumentation example (120 x 4 x 4)" = ra_data(ra_example("argumentation"), "student", "rater", "criterion", "score", source = "source", replicate = "replicate", version = "version"),
  "Practicum example (60 x 4 x 3)" = ra_data(ra_example("practicum"), "student", "rater", "criterion", "score", source = "source"),
  "Expert panel example (30 x 6, 1-4)" = ra_data(ra_example("expert"), "item", "expert", "criterion", "score"))
for (f in c("contoh_validitas_isi.xlsx", "contoh_validitas_isi_7ahli.xlsx")) if (file.exists(f)) {
  raw <- as.data.frame(readxl::read_excel(f)); l <- ra_from_wide(raw, names(raw)[1], names(raw)[-1])
  sets[[f]] <- ra_data(l, names(raw)[1], ".penilai", ".kriteria", ".skor") }
if (file.exists("contoh_validitas_isi_long.xlsx"))
  sets[["contoh_validitas_isi_long.xlsx"]] <- ra_data(as.data.frame(readxl::read_excel("contoh_validitas_isi_long.xlsx")), "Item", "Expert", "Criterion", "Score")
for (nm in names(sets)) {
  d <- first(sets[[nm]]); f <- ra_mfrm(d); s <- setNames(f$raters$severity, f$raters$rater); s <- s - mean(s)
  mm <- mml_raters(d)[names(s)]; mm <- mm - mean(mm)
  dd <- tapply(d$score - min(d$score), list(d$object, paste(d$rater, d$criterion)), mean)
  cm <- if (all(apply(dd, 2, min, na.rm = TRUE) == 0)) { v <- cml_raters(d)[names(s)]; v - mean(v) } else NULL
  cat(sprintf("%-40s max |raterAudit - independent MML| = %.3f", nm, max(abs(s - mm))))
  cat(if (is.null(cm)) "  (eRm CML not applicable: some rater never used the lowest category)\n" else sprintf("  | max |raterAudit - eRm CML| = %.3f\n", max(abs(s - cm))))
}
