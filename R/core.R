# raterAudit: fungsi inti (prototipe v0.1)
# Semua fungsi hanya memakai base R + stats.

# ---------------------------------------------------------------
# Data
# ---------------------------------------------------------------
ra_data <- function(df, object, rater, criterion, score,
                    source = NULL, replicate = NULL, version = NULL, group = NULL) {
  out <- data.frame(
    object    = as.character(df[[object]]),
    rater     = as.character(df[[rater]]),
    criterion = if (!is.null(criterion) && nzchar(criterion) && criterion %in% names(df)) as.character(df[[criterion]]) else "single criterion",
    score     = suppressWarnings(as.numeric(df[[score]])),
    stringsAsFactors = FALSE
  )
  opt <- list(source = source, replicate = replicate, version = version, group = group)
  for (nm in names(opt)) {
    col <- opt[[nm]]
    if (!is.null(col) && nzchar(col) && col %in% names(df)) out[[nm]] <- as.character(df[[col]])
  }
  out <- out[!is.na(out$score), , drop = FALSE]
  if (!"source" %in% names(out)) out$source <- rep("all", nrow(out))
  out$source[is.na(out$source) | out$source == ""] <- "no source"
  if ("replicate" %in% names(out)) out$replicate[is.na(out$replicate) | out$replicate == ""] <- "1"
  if ("version" %in% names(out)) out$version[is.na(out$version)] <- ""
  attr(out, "has") <- c(source = !is.null(source) && nzchar(source),
                        replicate = "replicate" %in% names(out),
                        version = "version" %in% names(out),
                        group = "group" %in% names(out))
  out
}

ra_has <- function(d, what) isTRUE(attr(d, "has")[[what]])

# Data lebar (satu baris per objek, satu kolom per penilai) -> panjang
ra_from_wide <- function(df, id, rater_cols, criterion = NULL, group = NULL) {
  df <- as.data.frame(df)
  keep <- c(id, if (!is.null(criterion) && nzchar(criterion)) criterion, if (!is.null(group) && nzchar(group)) group)
  out <- do.call(rbind, lapply(rater_cols, function(rc) {
    x <- df[, keep, drop = FALSE]
    x$.penilai <- rc
    x$.skor <- suppressWarnings(as.numeric(as.character(df[[rc]])))
    x
  }))
  if (is.null(criterion) || !nzchar(criterion)) out$.kriteria <- "single criterion" else out$.kriteria <- out[[criterion]]
  out
}

# Pemeriksaan desain sebelum analisis
ra_check <- function(d) {
  # keterhubungan jaringan penilai-objek (union-find)
  nodes <- c(paste0("o:", unique(d$object)), paste0("r:", unique(d$rater)))
  parent <- setNames(nodes, nodes)
  find <- function(x) { while (parent[[x]] != x) x <- parent[[x]]; x }
  pairs <- unique(d[, c("object", "rater")])
  for (k in seq_len(nrow(pairs))) {
    a <- find(paste0("o:", pairs$object[k])); b <- find(paste0("r:", pairs$rater[k]))
    if (a != b) parent[[a]] <- b
  }
  roots <- vapply(nodes, find, "")
  n_sub <- length(unique(roots))
  cnt <- table(factor(pairs$rater), factor(pairs$object))
  crossed <- all(cnt > 0)
  cats <- table(d$score)
  list(
    n_obs = nrow(d), n_object = length(unique(d$object)), n_rater = length(unique(d$rater)),
    n_criterion = length(unique(d$criterion)), n_source = length(unique(d$source)),
    score_min = min(d$score), score_max = max(d$score),
    integer_scores = all(abs(d$score - round(d$score)) < 1e-8),
    categories = cats,
    rare_categories = names(cats)[cats < 10],
    subsets = n_sub, connected = n_sub == 1,
    design = if (crossed) "fully crossed" else "unbalanced / partial",
    empty_cells = round(100 * mean(cnt == 0), 1)
  )
}

# ---------------------------------------------------------------
# MFRM (rating scale), estimasi JMLE
# log(P_k / P_k-1) = theta_n - delta_i - alpha_j - tau_k
# ---------------------------------------------------------------
.rsm_probs <- function(eta, tau) {
  m <- length(tau)
  ks <- 0:m
  ctau <- c(0, cumsum(tau))
  num <- outer(eta, ks) - matrix(ctau, length(eta), m + 1, byrow = TRUE)
  num <- num - apply(num, 1, max)
  P <- exp(num)
  P / rowSums(P)
}

ra_mfrm <- function(d, rater_col = "rater", method = c("mml", "jmle"), maxit = 500, tol = 1e-4) {
  method <- match.arg(method)
  smin <- min(d$score)
  x <- d$score - smin
  m <- max(x)
  if (m < 1) stop("Scores must have at least two categories.")
  o <- factor(d$object); r <- factor(d[[rater_col]]); cr <- factor(d$criterion)
  oi <- as.integer(o); ri <- as.integer(r); ci <- as.integer(cr)
  th <- numeric(nlevels(o)); al <- numeric(nlevels(r)); de <- numeric(nlevels(cr))
  tau <- seq(-1, 1, length.out = m)
  clamp <- function(v, b = 1) pmax(pmin(v, b), -b)

  # Extreme elements (all minimum or all maximum scores) have no finite estimate.
  # As in FACETS, they are excluded when estimating the other facets and the
  # thresholds; their own measure is then estimated from a corrected score.
  ext_of <- function(idx) { lo <- tapply(x, idx, max) == 0; hi <- tapply(x, idx, min) == m; as.vector(lo | hi) }
  ext_o <- ext_of(oi); ext_r <- ext_of(ri)
  keep <- !ext_o[oi] & !ext_r[ri]
  if (!any(keep)) keep <- rep(TRUE, length(x))
  w_o <- ifelse(ext_o[oi], 1, keep); w_r <- ifelse(ext_r[ri], 1, keep); w_c <- as.numeric(keep)
  wsum <- function(v, w, idx) as.vector(rowsum(v * w, idx))
  n_o <- wsum(1, w_o, oi); n_r <- wsum(1, w_r, ri); n_c <- wsum(1, w_c, ci)
  obs_o <- pmin(pmax(wsum(x, w_o, oi), 0.3), m * n_o - 0.3)
  obs_r <- pmin(pmax(wsum(x, w_r, ri), 0.3), m * n_r - 0.3)
  obs_c <- pmin(pmax(wsum(x, w_c, ci), 0.3), pmax(m * n_c - 0.3, 0.3))
  nobs <- tabulate(x[keep] + 1, m + 1) + 0.5

  for (it in seq_len(maxit)) {
    eta <- th[oi] - de[ci] - al[ri]
    P <- .rsm_probs(eta, tau)
    E <- as.vector(P %*% (0:m)); V <- as.vector(P %*% (0:m)^2) - E^2
    dth <- clamp((obs_o - wsum(E, w_o, oi)) / pmax(wsum(V, w_o, oi), 1e-8))
    dal <- clamp(-(obs_r - wsum(E, w_r, ri)) / pmax(wsum(V, w_r, ri), 1e-8))
    dde <- clamp(-(obs_c - wsum(E, w_c, ci)) / pmax(wsum(V, w_c, ci), 1e-8))
    nexp <- colSums(P[keep, , drop = FALSE]) + 0.5
    dtau <- clamp(log(nobs[1:m] / nobs[2:(m + 1)]) - log(nexp[1:m] / nexp[2:(m + 1)]), 0.5)
    th <- th + dth
    al <- al + dal; al <- al - mean(if (all(ext_r)) al else al[!ext_r])
    de <- de + dde; de <- de - mean(de)
    tau <- tau + dtau; tau <- tau - mean(tau)
    if (max(abs(c(dth, dal, dde, dtau))) < tol) break
  }
  jmle_it <- it; mml <- NULL
  if (method == "mml" && sum(!ext_r) >= 2) {
    # Marginal ML for raters, criteria and thresholds (objects ~ N(0, s^2)), as in TAM.
    # Raters who always gave the extreme score have no finite estimate and are left out;
    # their severity is then obtained from a corrected score with the others fixed.
    rows <- !ext_r[ri]
    rk <- match(ri[rows], which(!ext_r))
    mml <- .mml_fit(x[rows], oi[rows], rk, ci[rows], m,
                    start = c(al[!ext_r][-sum(!ext_r)], if (nlevels(cr) > 1) de[-nlevels(cr)],
                              tau + mean(th[!ext_o]), log(max(sd(th[!ext_o]), 0.3))))
    al[!ext_r] <- mml$al; de <- mml$de; tau <- mml$tau
    for (it2 in seq_len(maxit)) {
      eta <- th[oi] - de[ci] - al[ri]
      P <- .rsm_probs(eta, tau)
      E <- as.vector(P %*% (0:m)); V <- as.vector(P %*% (0:m)^2) - E^2
      dth <- clamp((obs_o - wsum(E, w_o, oi)) / pmax(wsum(V, w_o, oi), 1e-8))
      dal <- clamp(-(obs_r - wsum(E, w_r, ri)) / pmax(wsum(V, w_r, ri), 1e-8)); dal[!ext_r] <- 0
      th <- th + dth; al <- al + dal
      if (max(abs(c(dth, dal))) < tol) break
    }
  }
  eta <- th[oi] - de[ci] - al[ri]
  P <- .rsm_probs(eta, tau)
  E <- as.vector(P %*% (0:m)); V <- as.vector(P %*% (0:m)^2) - E^2
  res <- x - E; z2 <- res^2 / V

  fitstats <- function(idx, nlev) {
    data.frame(
      n = as.vector(table(factor(idx, levels = seq_len(nlev)))),
      se = 1 / sqrt(as.vector(rowsum(V, idx))),
      infit = as.vector(rowsum(res^2, idx)) / as.vector(rowsum(V, idx)),
      outfit = as.vector(rowsum(z2, idx)) / as.vector(table(idx)),
      obs_avg = as.vector(rowsum(x, idx)) / as.vector(table(idx)) + smin
    )
  }
  # fair average penilai: skor harapan untuk objek & kriteria rata-rata
  grid_eta <- as.vector(outer(th, de, "-"))
  fair_r <- vapply(al, function(a) mean(.rsm_probs(grid_eta - a, tau) %*% (0:m)), 0) + smin
  fair_o <- vapply(th, function(t) mean(.rsm_probs(t - de, tau) %*% (0:m)), 0) + smin

  src <- tapply(d$source, r, function(s) s[1])
  raters <- data.frame(rater = levels(r), source = as.character(src[levels(r)]),
                       severity = al, fitstats(ri, nlevels(r)), fair_avg = fair_r,
                       stringsAsFactors = FALSE)
  objects <- data.frame(object = levels(o), measure = th, fitstats(oi, nlevels(o)), fair_avg = fair_o)
  criteria <- data.frame(criterion = levels(cr), difficulty = de, fitstats(ci, nlevels(cr)))

  if (!is.null(mml)) raters$se[!ext_r] <- mml$se
  raters$extreme <- ext_r
  sep_rel <- function(meas, se) { v <- var(meas); max(0, (v - mean(se^2)) / v) }
  w <- 1 / raters$se^2
  chi <- sum(w * (al - sum(w * al) / sum(w))^2); chi_df <- length(al) - 1
  if (!is.null(mml) && is.finite(mml$wald)) { chi <- mml$wald; chi_df <- sum(!ext_r) - 1 }

  list(
    raters = raters, objects = objects, criteria = criteria,
    tau = tau, min_score = smin, max_cat = m,
    method = if (is.null(mml)) "JMLE" else "MML",
    iterations = if (is.null(mml)) jmle_it else mml$iterations,
    converged = if (is.null(mml)) jmle_it < maxit else mml$converged,
    object_sd = if (is.null(mml)) sd(th) else mml$s,
    rater_reliability = sep_rel(al, raters$se),
    object_reliability = sep_rel(th, objects$measure * 0 + objects$se),
    fixed_chi = chi, fixed_df = chi_df,
    fixed_p = pchisq(chi, chi_df, lower.tail = FALSE),
    resid = data.frame(d, x = x, E = E, V = V, res = res, z2 = z2, stringsAsFactors = FALSE),
    rater_col = rater_col
  )
}

# ---------------------------------------------------------------
# Kesepakatan antarpenilai
# ---------------------------------------------------------------
qwk <- function(a, b, lo, hi) {
  k <- hi - lo + 1
  A <- factor(a - lo, levels = 0:(k - 1)); B <- factor(b - lo, levels = 0:(k - 1))
  O <- table(A, B); O <- O / sum(O)
  Ex <- outer(rowSums(O), colSums(O))
  W <- outer(0:(k - 1), 0:(k - 1), function(i, j) (i - j)^2) / (k - 1)^2
  1 - sum(W * O) / sum(W * Ex)
}

.first_rep <- function(d) {
  if (!"replicate" %in% names(d)) return(d)
  rep_n <- suppressWarnings(as.numeric(d$replicate))
  if (anyNA(rep_n)) rep_n <- as.numeric(factor(d$replicate))
  key <- paste(d$rater, d$object, d$criterion, if ("version" %in% names(d)) d$version else "", sep = "\r")
  keep <- ave(rep_n, key, FUN = min)
  d <- d[rep_n == keep, ]
  if ("version" %in% names(d)) {
    v1 <- ave(seq_len(nrow(d)), d$rater, FUN = function(i) i[1])
    first_v <- d$version[v1]
    d <- d[d$version == first_v, ]
  }
  d
}

ra_agreement <- function(d) {
  d1 <- .first_rep(d)
  d1$key <- paste(d1$object, d1$criterion, sep = "|")
  w <- tapply(d1$score, list(d1$key, d1$rater), mean)
  rs <- colnames(w); lo <- min(d$score); hi <- max(d$score)
  if (length(rs) < 2) return(data.frame())
  cmb <- combn(rs, 2)
  out <- lapply(seq_len(ncol(cmb)), function(k) {
    a <- w[, cmb[1, k]]; b <- w[, cmb[2, k]]
    ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 5) return(NULL)
    a <- round(a[ok]); b <- round(b[ok])
    data.frame(rater_1 = cmb[1, k], rater_2 = cmb[2, k], n = sum(ok),
               exact = mean(a == b), adjacent = mean(abs(a - b) <= 1),
               qwk = qwk(a, b, lo, hi))
  })
  do.call(rbind, out)
}

ra_describe <- function(d) {
  agg <- function(v) c(n = length(v), mean = mean(v), sd = sd(v), min = min(v), max = max(v))
  t1 <- do.call(rbind, tapply(d$score, d$rater, agg))
  src <- tapply(d$source, d$rater, function(s) s[1])
  data.frame(rater = rownames(t1), source = as.character(src[rownames(t1)]), t1, row.names = NULL)
}

# ---------------------------------------------------------------
# Efek penilai: centrality & halo
# ---------------------------------------------------------------
ra_effects <- function(fit) {
  rd <- fit$resid; rc <- fit$rater_col
  by_r <- split(rd, rd[[rc]])
  # halo: korelasi rata-rata antarkriteria dalam penilai
  halo_of <- function(df) {
    w <- tapply(df$score, list(df$object, df$criterion), mean)
    if (ncol(w) < 2) return(NA_real_)
    cm <- suppressWarnings(cor(w, use = "pairwise.complete.obs"))
    mean(cm[upper.tri(cm)], na.rm = TRUE)
  }
  out <- lapply(names(by_r), function(nm) {
    df <- by_r[[nm]]
    dh <- .first_rep(df)
    m <- fit$max_cat
    data.frame(
      rater = nm,
      spread_ratio = var(df$x) / (var(df$E) + mean(df$V)),
      middle_cat_prop = mean(df$x > 0 & df$x < m),
      halo = halo_of(dh)
    )
  })
  ef <- do.call(rbind, out)
  ef <- merge(fit$raters[, c("rater", "source", "infit", "outfit")], ef, by = "rater")
  halo_ref <- median(ef$halo, na.rm = TRUE)
  ef$flags <- apply(ef, 1, function(z) {
    f <- character(0)
    if (as.numeric(z["spread_ratio"]) < 0.75) f <- c(f, "centrality")
    if (as.numeric(z["spread_ratio"]) > 1.35) f <- c(f, "extreme")
    if (!is.na(z["halo"]) && as.numeric(z["halo"]) > halo_ref + 0.15) f <- c(f, "halo")
    if (as.numeric(z["infit"]) > 1.5) f <- c(f, "misfit (noisy)")
    if (as.numeric(z["infit"]) < 0.5) f <- c(f, "overfit (too uniform)")
    if (length(f)) paste(f, collapse = ", ") else "-"
  })
  ef
}

# ---------------------------------------------------------------
# Differential rater functioning (penilai x kelompok)
# ---------------------------------------------------------------
ra_drf <- function(d, max_iter = 5) {
  # Purifikasi iteratif: penilai yang ditandai DRF dikeluarkan dari estimasi
  # ukuran objek ketika menguji penilai lain.
  flagged <- character(0)
  for (k in seq_len(max_iter)) {
    res <- .drf_pass(d, flagged)
    if (is.null(res)) return(NULL)
    new_flag <- sort(unique(res$rater[res$decision == "DRF"]))
    if (identical(new_flag, flagged)) break
    flagged <- new_flag
  }
  attr(res, "purification_rounds") <- k
  attr(res, "excluded") <- flagged
  res
}

.drf_pass <- function(d, flagged) {
  # Bias tiap penilai dihitung terhadap ukuran objek, kriteria, dan ambang
  # yang diestimasi TANPA penilai tersebut (menghindari kontaminasi).
  if (!"group" %in% names(d)) return(NULL)
  grp <- sort(unique(d$group))
  if (length(grp) < 2) return(NULL)
  ref <- grp[1]
  raters <- unique(d$rater)
  if (length(raters) < 2) return(NULL)
  out <- lapply(raters, function(j) {
    keep <- d$rater != j & !d$rater %in% flagged
    if (length(unique(d$rater[keep])) < 1) keep <- d$rater != j
    fw <- ra_mfrm(d[keep, ], method = "jmle")
    dj <- d[d$rater == j, ]
    th <- setNames(fw$objects$measure, fw$objects$object)[dj$object]
    de <- setNames(fw$criteria$difficulty, fw$criteria$criterion)[dj$criterion]
    ok <- !is.na(th) & !is.na(de)
    dj <- dj[ok, ]; th <- th[ok]; de <- de[ok]
    x <- pmin(pmax(dj$score - fw$min_score, 0), fw$max_cat); m <- fw$max_cat
    a <- 0
    for (it in 1:100) {
      P <- .rsm_probs(th - de - a, fw$tau)
      E <- as.vector(P %*% (0:m)); V <- as.vector(P %*% (0:m)^2) - E^2
      step <- max(min((sum(E) - sum(x)) / sum(V), 1), -1)
      a <- a + step
      if (abs(step) < 1e-5) break
    }
    res <- x - E
    # Cluster-robust (sandwich) SE with objects as clusters: the object measures
    # come from the other raters and carry error, which makes residuals within an
    # object correlated; the model-based SE (1 / sqrt(sum V)) ignores this.
    cl <- aggregate(cbind(res, V) ~ g + o, data = data.frame(g = dj$group, o = dj$object, res = res, V = V), sum)
    cells <- aggregate(cbind(res, V) ~ g, data = cl, sum)
    cells$bias <- cells$res / cells$V
    cells$se <- vapply(cells$g, function(gg) {
      k <- cl[cl$g == gg, ]; b <- sum(k$res) / sum(k$V); n <- nrow(k)
      sqrt(n / max(n - 1, 1) * sum((k$res - b * k$V)^2)) / sum(k$V)
    }, 0)
    A <- cells[cells$g == ref, ]
    do.call(rbind, lapply(setdiff(cells$g, ref), function(gg) {
      B <- cells[cells$g == gg, ]
      diff <- B$bias - A$bias; se <- sqrt(A$se^2 + B$se^2)
      data.frame(rater = j, focal_group = gg, reference_group = ref,
                 contrast = diff, se = se, z = diff / se)
    }))
  })
  out <- do.call(rbind, out)
  out$p <- 2 * pnorm(-abs(out$z))
  out$p_fdr <- p.adjust(out$p, "BH")
  out$decision <- ifelse(out$p_fdr < 0.05 & abs(out$contrast) >= 0.5, "DRF",
                    ifelse(out$p_fdr < 0.05, "significant, small effect", "-"))
  rownames(out) <- NULL
  out[order(out$p), ]
}

# ---------------------------------------------------------------
# Perbandingan antarsumber
# ---------------------------------------------------------------
ra_compare_sources <- function(fit, d, reference = NULL) {
  rt <- fit$raters
  srcs <- unique(rt$source)
  if (length(srcs) < 2) return(NULL)
  if (is.null(reference) || !reference %in% srcs) reference <- srcs[1]
  sev <- do.call(rbind, lapply(srcs, function(s) {
    v <- rt$severity[rt$source == s]
    data.frame(source = s, n_raters = length(v), mean_severity = mean(v),
               sd_between_raters = if (length(v) > 1) sd(v) else NA,
               mean_fair_avg = mean(rt$fair_avg[rt$source == s]))
  }))
  ref_v <- rt$severity[rt$source == reference]
  sev$diff_vs_reference <- sev$mean_severity - mean(ref_v)
  # kesepakatan skor rata-rata sumber vs rujukan
  d1 <- .first_rep(d)
  key <- paste(d1$object, d1$criterion)
  m <- tapply(d1$score, list(key, d1$source), mean)
  sev$r_vs_reference <- vapply(sev$source, function(s)
    suppressWarnings(cor(m[, s], m[, reference], use = "pairwise.complete.obs")), 0)
  sev$score_difference <- vapply(sev$source, function(s) mean(m[, s] - m[, reference], na.rm = TRUE), 0)
  attr(sev, "reference") <- reference
  sev
}

# ---------------------------------------------------------------
# Stabilitas antar-ulangan
# ---------------------------------------------------------------
ra_stability <- function(d) {
  if (!"replicate" %in% names(d)) return(NULL)
  vv <- if ("version" %in% names(d)) d$version else ""
  d$unit <- ifelse(vv == "", d$rater, paste0(d$rater, " @ ", vv))
  out <- lapply(split(d, d$unit), function(df) {
    key <- paste(df$object, df$criterion)
    nrep <- tapply(df$score, key, length)
    if (max(nrep) < 2) return(NULL)
    keep <- key %in% names(nrep)[nrep >= 2]
    df <- df[keep, ]; key <- key[keep]
    same <- tapply(df$score, key, function(s) length(unique(s)) == 1)
    wv <- tapply(df$score, key, var)
    # one-way random-effects ICC(1) from ANOVA mean squares (Shrout & Fleiss, 1979)
    nk <- as.vector(tapply(df$score, key, length)); g <- length(nk); N <- sum(nk)
    k0 <- (N - sum(nk^2) / N) / (g - 1)
    MSB <- sum(nk * (tapply(df$score, key, mean) - mean(df$score))^2) / (g - 1)
    MSW <- sum(tapply(df$score, key, function(s) sum((s - mean(s))^2))) / (N - g)
    icc1 <- max(0, (MSB - MSW) / (MSB + (k0 - 1) * MSW))
    k_rep <- round(mean(nrep[nrep >= 2]))
    data.frame(unit = df$unit[1], rater = df$rater[1], cells = length(same), replicates = k_rep,
               all_identical = mean(same), sd_within_cell = sqrt(mean(wv)), icc_single = icc1,
               rel_mean_of_3 = 3 * icc1 / (1 + 2 * icc1))
  })
  do.call(rbind, out)
}

# ---------------------------------------------------------------
# Drift antar-versi
# ---------------------------------------------------------------
ra_drift <- function(d) {
  if (!"version" %in% names(d) || all(d$version == "")) return(NULL)
  d$unit <- ifelse(d$version == "", d$rater, paste0(d$rater, " @ ", d$version))
  fit <- ra_mfrm(d, rater_col = "unit")
  rt <- fit$raters
  map <- unique(d[, c("unit", "rater", "version")]); names(map)[2] <- "base_rater"
  rt <- merge(rt, map, by.x = "rater", by.y = "unit")
  out <- lapply(split(rt, rt$base_rater), function(x) {
    x <- x[x$version != "", ]
    if (nrow(x) < 2) return(NULL)
    x <- x[order(x$version), ]
    do.call(rbind, lapply(2:nrow(x), function(k) {
      diff <- x$severity[k] - x$severity[k - 1]; se <- sqrt(x$se[k]^2 + x$se[k - 1]^2)
      data.frame(rater = x$base_rater[1], from = x$version[k - 1], to = x$version[k],
                 severity_from = x$severity[k - 1], severity_to = x$severity[k],
                 change = diff, se = se, z = diff / se)
    }))
  })
  out <- do.call(rbind, out)
  if (is.null(out)) return(NULL)
  out$p <- 2 * pnorm(-abs(out$z))
  out$p_fdr <- p.adjust(out$p, "BH")
  out$decision <- ifelse(out$p_fdr < 0.05 & abs(out$change) >= 0.3, "drift", "-")
  list(table = out, by_version = rt)
}

# ---- Validasi isi: Aiken's V dan ICC ---------------------------------------

# Distribusi eksak S = sum(skor - lo) bila tiap ahli memilih kategori secara acak
# (hipotesis nol Aiken, 1985). Mengembalikan P(S >= s) untuk s = 0..n(c-1).
.aiken_tail <- function(n, c) {
  dens <- 1
  for (i in seq_len(n)) dens <- as.vector(stats::convolve(dens, rev(rep(1 / c, c)), type = "open"))
  dens <- pmax(dens, 0); dens <- dens / sum(dens)
  rev(cumsum(rev(dens)))
}

# Nilai V minimum agar signifikan (tabel Aiken dihitung langsung).
aiken_critical <- function(n, lo, hi, alpha = 0.05) {
  c <- hi - lo + 1
  tail <- .aiken_tail(n, c)
  s <- which(tail < alpha)[1] - 1
  if (is.na(s)) return(NA_real_)
  s / (n * (c - 1))
}

ra_aiken <- function(d, lo = min(d$score), hi = max(d$score), alpha = 0.05) {
  d <- .first_rep(d)
  c <- hi - lo + 1
  key <- interaction(d$object, d$criterion, drop = TRUE, sep = "\r")
  out <- do.call(rbind, lapply(split(d, key), function(x) {
    n <- nrow(x); s <- sum(x$score - lo)
    med <- stats::median(x$score)
    dev <- x$rater[abs(x$score - med) >= 2]
    data.frame(object = x$object[1], criterion = x$criterion[1], n_experts = n,
               mean = mean(x$score), V = s / (n * (c - 1)),
               p = .aiken_tail(n, c)[s + 1],
               deviating_experts = if (length(dev)) paste(dev, collapse = ", ") else "",
               stringsAsFactors = FALSE)
  }))
  out$V_critical <- vapply(out$n_experts, aiken_critical, 0, lo = lo, hi = hi, alpha = alpha)
  out$decision <- ifelse(out$p < alpha, "Valid", "Revise")
  out <- out[, c("object", "criterion", "n_experts", "mean", "V", "V_critical", "p", "decision", "deviating_experts")]
  if (length(unique(out$criterion)) == 1) out$criterion <- NULL
  rownames(out) <- NULL
  out
}

# ICC dua arah acak, kesepakatan absolut (McGraw & Wong, 1996; Shrout & Fleiss, 1979).
ra_icc <- function(d, conf = 0.95) {
  d <- .first_rep(d)
  d$unit <- interaction(d$object, d$criterion, drop = TRUE, sep = "\r")
  w <- tapply(d$score, list(d$unit, d$rater), mean)
  w <- w[stats::complete.cases(w), , drop = FALSE]
  n <- nrow(w); k <- ncol(w)
  if (n < 3 || k < 2) return(NULL)
  gm <- mean(w)
  MSr <- k * sum((rowMeans(w) - gm)^2) / (n - 1)
  MSc <- n * sum((colMeans(w) - gm)^2) / (k - 1)
  SSe <- sum((w - outer(rowMeans(w), colMeans(w), "+") + gm)^2)
  MSe <- SSe / ((n - 1) * (k - 1))
  icc1 <- (MSr - MSe) / (MSr + (k - 1) * MSe + k / n * (MSc - MSe))
  iccK <- (MSr - MSe) / (MSr + (MSc - MSe) / n)
  a <- (k * icc1) / (n * (1 - icc1)); b <- 1 + (k * icc1 * (n - 1)) / (n * (1 - icc1))
  v <- (a * MSc + b * MSe)^2 / ((a * MSc)^2 / (k - 1) + (b * MSe)^2 / ((n - 1) * (k - 1)))
  al <- 1 - conf
  FL <- stats::qf(1 - al / 2, n - 1, v); FU <- stats::qf(1 - al / 2, v, n - 1)
  lb1 <- (n * (MSr - FL * MSe)) / (FL * (k * MSc + (k * n - k - n) * MSe) + n * MSr)
  ub1 <- (n * (FU * MSr - MSe)) / (k * MSc + (k * n - k - n) * MSe + n * FU * MSr)
  sb <- function(x) k * x / (1 + (k - 1) * x)
  kat <- function(x) as.character(cut(x, c(-Inf, 0.5, 0.75, 0.9, Inf), right = FALSE,
                                       labels = c("poor", "moderate", "good", "excellent")))
  data.frame(index = c("ICC(2,1) single rater", "ICC(2,k) mean of raters"),
             ICC = c(icc1, iccK), ci_lower = c(lb1, sb(lb1)), ci_upper = c(ub1, sb(ub1)),
             category = kat(c(icc1, iccK)), n_objects = n, n_experts = k, stringsAsFactors = FALSE)
}

# Marginal maximum likelihood for the facets rating scale model.
# Parameters: raters (sum to zero), criteria (sum to zero), thresholds, log SD of objects.
.gh_nodes <- function(n) {   # Gauss-Hermite nodes and weights for N(0, 1) (Golub-Welsch)
  i <- seq_len(n - 1); J <- matrix(0, n, n); J[cbind(i, i + 1)] <- sqrt(i); J[cbind(i + 1, i)] <- sqrt(i)
  e <- eigen(J, symmetric = TRUE); list(x = e$values, w = e$vectors[1, ]^2)
}
.mml_fit <- function(x, oi, ri, ci, m, start, Q = 31) {
  oi <- as.integer(factor(oi)); N <- max(oi); R <- max(ri); C <- max(ci)
  gh <- .gh_nodes(Q); n <- length(x); idx <- cbind(seq_len(n), x + 1)
  unpack <- function(p) {
    a <- c(p[seq_len(R - 1)], -sum(p[seq_len(R - 1)])); k <- R - 1
    de <- if (C > 1) { v <- p[k + seq_len(C - 1)]; k <- k + C - 1; c(v, -sum(v)) } else 0
    list(a = a, de = de, tau = p[k + seq_len(m)], s = exp(p[k + m + 1]))
  }
  cache <- new.env()
  eval_all <- function(p) {
    if (!is.null(cache$p) && identical(cache$p, p)) return(cache)
    u <- unpack(p); ctau <- c(0, cumsum(u$tau)); base <- -u$a[ri] - u$de[ci]
    ll <- matrix(0, N, Q); Eq <- vector("list", Q); Gq <- vector("list", Q)
    for (q in seq_len(Q)) {
      lp <- outer(base + u$s * gh$x[q], 0:m) - rep(ctau, each = n)
      mx <- lp[cbind(seq_len(n), max.col(lp, "first"))]
      ex <- exp(lp - mx); den <- rowSums(ex); P <- ex / den
      ll[, q] <- as.vector(rowsum(lp[idx] - mx - log(den), oi, reorder = TRUE))
      Eq[[q]] <- as.vector(P %*% (0:m))
      G <- P[, m + 1, drop = FALSE]                       # P(X >= k), k = 1..m
      if (m > 1) for (k in m:2) G <- cbind(G[, 1] + P[, k], G)
      Gq[[q]] <- G
    }
    lw <- ll + rep(log(gh$w), each = N)
    mx <- apply(lw, 1, max); lik <- mx + log(rowSums(exp(lw - mx)))
    post <- exp(lw - lik)
    g_eta <- numeric(n); g_s <- 0; g_tau <- numeric(m)
    ge <- outer(x, seq_len(m), ">=") * 1
    for (q in seq_len(Q)) {
      w <- post[oi, q]; r <- w * (x - Eq[[q]])
      g_eta <- g_eta + r; g_s <- g_s + sum(r) * gh$x[q]
      g_tau <- g_tau + colSums(w * (Gq[[q]] - ge))
    }
    # gradient of the log-likelihood with respect to the free parameters
    gA <- -as.vector(rowsum(g_eta, ri, reorder = TRUE))
    gp <- gA[seq_len(R - 1)] - gA[R]
    if (C > 1) { gD <- -as.vector(rowsum(g_eta, ci, reorder = TRUE)); gp <- c(gp, gD[seq_len(C - 1)] - gD[C]) }
    gp <- c(gp, g_tau, g_s * u$s)
    cache$p <- p; cache$f <- -sum(lik); cache$g <- -gp
    cache
  }
  nll <- function(p) eval_all(p)$f
  grad <- function(p) eval_all(p)$g
  if (isTRUE(getOption("raterAudit.check_grad"))) {
    h <- 1e-5; num <- vapply(seq_along(start), function(k) { e <- replace(numeric(length(start)), k, h); (nll(start + e) - nll(start - e)) / (2 * h) }, 0)
    print(rbind(analytic = grad(start), numeric = num))
  }
  fit <- nlminb(start, nll, grad, control = list(iter.max = 500, eval.max = 1500, rel.tol = 1e-10))
  u <- unpack(fit$par)
  H <- tryCatch(optimHess(fit$par, nll, grad), error = function(e) NULL)
  se <- rep(NA_real_, R)
  if (!is.null(H)) {
    S <- tryCatch(solve(H), error = function(e) NULL)
    if (!is.null(S)) {
      k <- seq_len(R - 1); Sa <- S[k, k, drop = FALSE]
      se <- sqrt(pmax(c(diag(Sa), sum(Sa)), 0))
      # Wald test of equal severity: H0 all free rater parameters = 0 (sum-to-zero coding)
      wald <- tryCatch(as.numeric(t(fit$par[k]) %*% solve(Sa, fit$par[k])), error = function(e) NA_real_)
    }
  }
  list(al = u$a, de = u$de, tau = u$tau, s = u$s, se = se, wald = if (exists("wald")) wald else NA_real_,
       iterations = fit$iterations, converged = fit$convergence == 0)
}
