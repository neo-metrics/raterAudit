# Data contoh hasil simulasi (efek penilai ditanam)

.sim_score <- function(eta, tau) {
  P <- .rsm_probs(eta, tau)
  apply(P, 1, function(p) sample(seq_along(p) - 1, 1, prob = p))
}

ra_example <- function(which = c("argumentation", "practicum", "expert"), seed = NULL) {
  which <- match.arg(which)
  if (is.null(seed)) seed <- c(argumentation = 2026, practicum = 7, expert = 2026)[[which]]
  set.seed(seed)
  if (which == "argumentation") {
    # 120 mahasiswa x 4 kriteria, skor 0-3
    # 2 dosen (1 kali), 2 LLM (3 ulangan, versi 2026-09 dan 2026-11)
    # Efek: LLM lebih ketat (+0.5); llm_B menumpuk di tengah; llm_A lebih ketat lagi di versi 2026-11 (+0.5)
    n <- 120
    th <- rnorm(n, 0, 1.2); names(th) <- sprintf("S%03d", 1:n)
    grp <- sample(c("F", "M"), n, TRUE)
    crit <- c(claim = -0.6, evidence = 0, reasoning = 0.3, rebuttal = 0.6)
    tau <- c(-1.4, 0, 1.4)
    rows <- list()
    add <- function(rater, src, sev, tau_r, reps, versions, vshift = c(0, 0)) {
      for (vi in seq_along(versions)) {
        g <- expand.grid(object = names(th), criterion = names(crit), stringsAsFactors = FALSE)
        shared <- rnorm(nrow(g), 0, 0.35)  # kecenderungan tetap per sel (ulangan berkorelasi)
        for (rp in seq_len(reps)) {
          eta <- th[g$object] - crit[g$criterion] - (sev + vshift[vi]) + shared
          rows[[length(rows) + 1]] <<- data.frame(
            student = g$object, rater = rater, source = src, criterion = g$criterion,
            score = .sim_score(eta, tau_r), replicate = rp, version = versions[vi],
            gender = grp[match(g$object, names(th))])
        }
      }
    }
    add("lecturer_A", "lecturer", -0.15, tau, 1, "")
    add("lecturer_B", "lecturer", 0.15, tau, 1, "")
    add("llm_A", "AI", 0.5, tau, 3, c("2026-09", "2026-11"), c(0, 0.5))
    add("llm_B", "AI", 0.5, c(-2.6, 0, 2.6), 3, c("2026-09", "2026-11"))
    return(do.call(rbind, rows))
  }
  if (which == "practicum") {
    # 60 mahasiswa x 3 kriteria, skor 1-5; 3 teman + 1 dosen
    # Efek: teman lebih longgar (-0.6); teman_2 lebih ketat pada mahasiswa L (DRF)
    n <- 60
    th <- rnorm(n, 0.3, 1); names(th) <- sprintf("S%02d", 1:n)
    grp <- sample(c("F", "M"), n, TRUE); names(grp) <- names(th)
    crit <- c(procedure = -0.3, data = 0, conclusion = 0.3)
    tau <- c(-1.8, -0.6, 0.6, 1.8)
    g <- expand.grid(object = names(th), criterion = names(crit), stringsAsFactors = FALSE)
    rows <- lapply(list(c("lecturer", "lecturer", 0.4), c("peer_1", "peer", -0.6),
                        c("peer_2", "peer", -0.6), c("peer_3", "peer", -0.5)), function(r) {
      sev <- as.numeric(r[3]) + ifelse(r[1] == "peer_2" & grp[g$object] == "M", 0.9, 0)
      eta <- th[g$object] - crit[g$criterion] - sev
      data.frame(student = g$object, rater = r[1], source = r[2], criterion = g$criterion,
                 score = .sim_score(eta, tau) + 1, gender = grp[g$object])
    })
    return(do.call(rbind, rows))
  }
  # ahli: 30 butir x 1 kriteria (relevansi 1-4), 6 ahli; ahli_6 sangat longgar
  th <- rnorm(30, 1, 0.9); names(th) <- sprintf("I%02d", 1:30)
  tau <- c(-1.2, 0, 1.2)
  sev <- c(expert_1 = 0.2, expert_2 = 0, expert_3 = 0.3, expert_4 = -0.1, expert_5 = 0.1, expert_6 = -1.6)
  rows <- lapply(names(sev), function(a) {
    data.frame(item = names(th), expert = a, source = "expert", criterion = "relevance",
               score = .sim_score(th - sev[a], tau) + 1)
  })
  do.call(rbind, rows)
}
