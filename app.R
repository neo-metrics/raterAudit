# raterAudit — Shiny prototype v0.1
# Run: shiny::runApp("raterAudit")
library(shiny)
library(bslib)
library(DT)
library(ggplot2)

`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/core.R", local = TRUE)
source("R/simulate.R", local = TRUE)

ROLES <- c(object = "Object rated *", rater = "Rater *", criterion = "Criterion",
           score = "Score *", source = "Rater source", replicate = "Replicate",
           version = "Version / prompt", group = "Object group")

GUESS <- list(object = c("student", "object", "item", "butir", "examinee", "mahasiswa", "siswa", "objek", "peserta"),
              rater = c("rater", "expert", "judge", "penilai", "ahli"),
              criterion = c("criterion", "dimension", "aspect", "kriteria", "aspek"),
              score = c("score", "rating", "skor", "nilai"),
              source = c("source", "rater_type", "sumber"),
              replicate = c("replicate", "run", "ulangan"),
              version = c("version", "prompt", "versi"),
              group = c("gender", "group", "region", "class", "kelompok"))

EXAMPLES <- c("Argumentation: lecturers vs LLMs" = "argumentation",
              "Lab practicum: peers vs lecturer" = "practicum",
              "Content validity: expert panel" = "expert")

fmt_p <- function(v) ifelse(v < 0.001, "< .001", formatC(v, format = "f", digits = 3))
fmt <- function(df, digits = 2) {
  pc <- intersect(names(df), c("p", "p_fdr"))
  num <- vapply(df, is.numeric, TRUE) & !names(df) %in% pc
  df[num] <- lapply(df[num], function(v) round(v, digits))
  df[pc] <- lapply(df[pc], fmt_p)
  df
}
dt <- function(df, digits = 2, pageLength = 10) {
  datatable(fmt(df, digits), rownames = FALSE, class = "compact stripe",
            options = list(pageLength = pageLength, dom = "tip", scrollX = TRUE))
}
note <- function(...) div(class = "text-muted small mt-2", ...)
apa <- function(x, d = 2) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = d))  # no leading zero
spread <- function(y, gap) {
  o <- order(y); v <- y[o]
  for (i in seq_along(v)[-1]) if (v[i] - v[i - 1] < gap) v[i] <- v[i - 1] + gap
  v <- v - (mean(v) - mean(y[o]))
  out <- y; out[o] <- v; out
}
src_cols <- c("#2563eb", "#ea580c", "#16a34a", "#9333ea", "#0891b2", "#be123c")

theme <- bs_theme(version = 5, primary = "#1f5fbf", base_font = font_collection("system-ui", "sans-serif"))

ui <- page_navbar(
  title = tags$span(tags$b("raterAudit"), tags$small(" auditing rater-mediated assessment", class = "text-white-50")),
  theme = theme, id = "nav", fillable = FALSE, bg = "#16365f", inverse = TRUE,

  nav_panel("Start here",
    div(class = "container-fluid", style = "max-width: 1100px;",
      layout_columns(col_widths = c(7, 5),
        card(card_header(tags$b("What raterAudit does")),
          p("raterAudit audits assessments in which scores are given by raters: content experts, teachers, peers, or AI models. ",
            "It shows whether the raters agree, who is more severe or lenient, whether a rater treats groups differently, ",
            "and whether AI raters are stable across repeated runs and model or prompt versions."),
          tags$b("Choose the tab that matches your question"),
          tags$table(class = "table table-sm mt-2 mb-0",
            tags$thead(tags$tr(tags$th("Your question"), tags$th("Go to"))),
            tags$tbody(
              tags$tr(tags$td("Are my instrument's items valid according to the experts?"), tags$td(tags$b("2 Content validity"))),
              tags$tr(tags$td("How well do the raters agree?"), tags$td(tags$b("3 Overview"), " and ", tags$b("2 Content validity"))),
              tags$tr(tags$td("Which raters are more severe or more lenient?"), tags$td(tags$b("4 Severity (MFRM)"))),
              tags$tr(tags$td("Does a rater show centrality, halo, or favour one group?"), tags$td(tags$b("5 Rater effects & DRF"))),
              tags$tr(tags$td("Do AI raters agree with humans, and are they stable across runs and versions?"), tags$td(tags$b("6 Sources & stability"))),
              tags$tr(tags$td("I need the tables for my report."), tags$td(tags$b("7 Export")))))
        ),
        card(card_header(tags$b("Getting started")),
          tags$ol(class = "mb-2",
            tags$li("Open ", tags$b("1 Data"), " and upload a CSV or Excel file, or try an example data set."),
            tags$li("Choose the data layout and tell the app which column is which."),
            tags$li("Open the tab that answers your question.")),
          tags$b("Example files"),
          div(class = "d-flex flex-column gap-1 mt-1",
            downloadLink("ex_w5", "Wide: 20 items × 5 experts, 1–5 scale"),
            downloadLink("ex_w7", "Wide: 25 items × 7 experts, 1–7 scale"),
            downloadLink("ex_l2", "Long: 20 items × 6 experts × 2 criteria, 1–4 scale"))
        )
      ),
      layout_columns(col_widths = c(7, 5),
        card(card_header(tags$b("Data layouts")),
          layout_columns(col_widths = c(5, 7),
            div(tags$b("Wide"), " — one row per item, one column per rater",
              tags$table(class = "table table-sm table-bordered mt-1 small",
                tags$tr(tags$th("Item"), tags$th("R1"), tags$th("R2"), tags$th("R3")),
                tags$tr(tags$td("I01"), tags$td("5"), tags$td("4"), tags$td("5")),
                tags$tr(tags$td("I02"), tags$td("3"), tags$td("4"), tags$td("4")))),
            div(tags$b("Long"), " — one row per score; allows criteria, rater sources, replicates, versions, and groups",
              tags$table(class = "table table-sm table-bordered mt-1 small",
                tags$tr(tags$th("Item"), tags$th("Rater"), tags$th("Criterion"), tags$th("Score")),
                tags$tr(tags$td("I01"), tags$td("E1"), tags$td("Relevance"), tags$td("4")),
                tags$tr(tags$td("I01"), tags$td("E1"), tags$td("Clarity"), tags$td("3"))))),
          note("Scores must be whole numbers on an ordinal scale (e.g., 1–4 or 1–5).")
        ),
        card(card_header(tags$b("Methods")),
          tags$ul(class = "small mb-0",
            tags$li("Content validity: Aiken's V with exact critical values (Aiken, 1985); ICC(2,1) and ICC(2,k), absolute agreement (McGraw & Wong, 1996)."),
            tags$li("Rater severity: many-facet Rasch rating scale model estimated by marginal maximum likelihood."),
            tags$li("Rater effects: spread ratio, middle-category use, halo, infit and outfit."),
            tags$li("Differential rater functioning: leave-one-rater-out bias contrasts with cluster-robust SEs and FDR control."),
            tags$li("Stability and drift: replicate ICC and version-specific severities."),
            tags$li("All computations are checked against published values, other R packages, and simulation; see the ", tags$code("validation"), " folder.")))
      ),
      div(class = "text-muted small mt-1 mb-3",
        tags$b("Developers: "), "Febrian Solikhin, Antuni Wiyarsi, Ali Muhson — Universitas Negeri Yogyakarta, Indonesia. ",
        "raterAudit prototype v0.1.")
    )
  ),

  nav_panel("1 Data",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        radioButtons("src", "Data source", c("Example data" = "ex", "Upload file" = "up")),
        conditionalPanel("input.src == 'ex'", selectInput("ex", NULL, EXAMPLES)),
        conditionalPanel("input.src == 'up'",
          fileInput("file", NULL, accept = c(".csv", ".xlsx"), placeholder = "CSV or XLSX")),
        tags$hr(),
        conditionalPanel("input.src == 'up'",
          radioButtons("shape", "Data layout", c("Long: one row per score" = "long",
                                                 "Wide: one column per rater" = "wide"))),
        tags$b("Column roles"),
        uiOutput("mapping"),
        note("Fields marked * are required.")
      ),
      uiOutput("check_boxes"),
      card(card_header("Data preview"), DTOutput("preview"))
    )
  ),

  nav_panel("2 Content validity",
    layout_sidebar(
      sidebar = sidebar(width = 260,
        uiOutput("aik_scale"),
        selectInput("aik_alpha", "Significance level", c(".05" = 0.05, ".01" = 0.01)),
        note("Enter the full scale of the rating form, not only the scores the experts used. Example: a 1–5 form where experts only gave 4 and 5 must still be set to 1–5.")),
      uiOutput("aik_boxes"),
      card(card_header("Report text"), uiOutput("aik_text")),
      card(card_header("Aiken's V by item"), DTOutput("t_aik"),
           note("The critical V is computed from the number of experts and the number of categories using Aiken's (1985) exact right-tail probability, so no table lookup is needed. ",
                "Deviating experts = experts whose rating differs from the item median by two or more categories.")),
      card(card_header("Inter-rater reliability (two-way random-effects ICC, absolute agreement)"), DTOutput("t_icc"),
           note("Interpretation following Koo and Li (2016): < .50 poor; .50–.75 moderate; .75–.90 good; > .90 excellent. ",
                "Only items rated by all experts are used."))
    )
  ),

  nav_panel("3 Overview",
    layout_columns(col_widths = c(7, 5),
      card(card_header("Score category use by rater"), plotOutput("p_cat", height = 330)),
      card(card_header("Descriptive statistics by rater"), DTOutput("t_desc"))
    ),
    card(card_header("Pairwise rater agreement"), DTOutput("t_agree"),
         note("Exact = identical scores; adjacent = scores differ by at most one category; QWK = quadratic weighted kappa. ",
              "For raters with replicates, only the first replicate of the first version is used."))
  ),

  nav_panel("4 Severity (MFRM)",
    uiOutput("mfrm_summary"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Wright map: objects, raters, and criteria on the logit scale"), plotOutput("p_wright", height = 460)),
      card(card_header("Rater severity with 95% intervals"), plotOutput("p_sev", height = 460))
    ),
    card(card_header("Rater measures"), DTOutput("t_raters"),
         note("Severity, criterion difficulty, and thresholds are estimated by marginal maximum likelihood (objects ~ normal), as in TAM; object measures and fit statistics are then computed with these fixed. ",
              "Raters marked extreme always gave the same end score and have no finite estimate; their severity is a bound and they are left out of the homogeneity test. Positive severity = more severe. Fair average = expected score this rater would give to an average object on an average criterion. ",
              "Infit/outfit between 0.5 and 1.5 are considered productive for measurement (Linacre, 2002). With few raters per object, mean-squares are expected to fall somewhat below 1 (about 0.85 with five raters in our simulations).")),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Criteria"), DTOutput("t_crit")),
      card(card_header("Objects"), DTOutput("t_obj"))
    )
  ),

  nav_panel("5 Rater effects & DRF",
    card(card_header("Rater effects"), DTOutput("t_eff"),
         note("Spread ratio = observed score variance / model-expected variance. Values < 0.75 indicate centrality; ",
              "> 1.35 indicate extreme scoring. Halo = mean inter-criterion correlation within a rater; flagged when 0.15 above the median of the other raters.")),
    card(card_header("Differential rater functioning: rater × object group"),
         uiOutput("drf_ui"))
  ),

  nav_panel("6 Sources & stability",
    uiOutput("src_ui"),
    uiOutput("stab_ui"),
    uiOutput("drift_ui")
  ),

  nav_panel("7 Export",
    card(card_header("Download results"),
      p("All result tables in one Excel file (one sheet per analysis), and an R script that reproduces the analysis without the app."),
      div(class = "d-flex gap-2",
        downloadButton("dl_xlsx", "Download Excel", class = "btn-primary"),
        downloadButton("dl_r", "Download R script"))
    )
  ),
  nav_spacer(),
  nav_item(tags$span(class = "text-white-50 small", "prototype v0.1"))
)

server <- function(input, output, session) {

  raw <- reactive({
    if (input$src == "ex") return(ra_example(input$ex))
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    if (ext == "xlsx") as.data.frame(readxl::read_excel(input$file$datapath))
    else read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })

  is_wide <- reactive(input$src == "up" && identical(input$shape, "wide"))

  output$mapping <- renderUI({
    nm <- names(raw())
    pick <- function(r) { g <- intersect(GUESS[[r]], tolower(nm)); if (length(g)) nm[match(g[1], tolower(nm))] else "" }
    if (is_wide()) {
      idc <- if (nzchar(pick("object"))) pick("object") else nm[1]
      num <- nm[vapply(raw(), function(v) is.numeric(v) || all(grepl("^\\s*-?[0-9.]+\\s*$", na.omit(as.character(v)))), TRUE)]
      return(tagList(
        selectInput("w_id", "Object rated (e.g., item) *", nm, selected = idc),
        selectizeInput("w_raters", "Rater columns *", nm, selected = setdiff(num, idc), multiple = TRUE),
        selectInput("w_crit", "Criterion (optional)", c("(single criterion)" = "", nm), selected = ""),
        selectInput("w_group", "Object group (optional)", c("(none)" = "", nm), selected = "")))
    }
    lapply(names(ROLES), function(r) {
      req_role <- r %in% c("object", "rater", "score")
      choices <- if (r == "criterion") c("(single criterion)" = "", nm) else c(if (req_role) c("(choose column)" = "") else c("(none)" = ""), nm)
      selectInput(paste0("map_", r), ROLES[[r]], choices = choices, selected = pick(r))
    })
  })

  output$preview <- renderDT(dt(head(raw(), 500), pageLength = 8))

  dat <- reactive({
    if (is_wide()) {
      req(input$w_id, length(input$w_raters) >= 1, all(c(input$w_id, input$w_raters) %in% names(raw())))
      validate(need(length(input$w_raters) >= 2, "Select at least two rater columns."),
               need(!input$w_id %in% input$w_raters, "The object column cannot also be selected as a rater column."))
      long <- ra_from_wide(raw(), input$w_id, input$w_raters, input$w_crit, input$w_group)
      return(ra_data(long, input$w_id, ".penilai", ".kriteria", ".skor", group = input$w_group))
    }
    m <- function(r) input[[paste0("map_", r)]]
    need_cols <- c(m("object"), m("rater"), m("score"))
    req(length(need_cols) == 3)
    validate(need(all(nzchar(need_cols)), "Choose the Object, Rater, and Score columns in the left panel. If each rater has its own column, choose the Wide layout."))
    req(all(need_cols %in% names(raw())))
    opt <- c(m("criterion"), m("source"), m("replicate"), m("version"), m("group"))
    req(all(opt == "" | opt %in% names(raw())))
    d <- ra_data(raw(), m("object"), m("rater"), m("criterion"), m("score"),
            source = m("source"), replicate = m("replicate"), version = m("version"), group = m("group"))
    validate(need(nrow(d) > 0, "The Score column contains no numbers. If each rater has its own column, choose the Wide layout."))
    d
  })

  valid <- reactive({
    d <- dat()
    validate(
      need(nrow(d) > 0, "No numeric scores were read. Check the score column (numbers, not text)."),
      need(length(unique(d$score)) >= 2, "All scores are identical; at least two score categories are needed."),
      need(all(abs(d$score - round(d$score)) < 1e-8), "Scores must be integers (ordinal categories), e.g., 1–4."),
      need(length(unique(d$rater)) >= 2, "At least two raters are needed."),
      need(ra_check(d)$connected, "The rater–object network is disconnected: some raters share no objects with the others."))
    d
  })

  chk <- reactive(ra_check(dat()))
  fit <- reactive(withProgress(message = "Estimating MFRM…", ra_mfrm(valid())))

  output$check_boxes <- renderUI({
    k <- chk()
    vb <- function(title, value, sub = NULL, theme = "primary")
      value_box(title = title, value = value, p(sub), theme = theme)
    layout_columns(col_widths = c(3, 3, 3, 3),
      vb("Scores", format(k$n_obs, big.mark = ","), sprintf("range %g–%g", k$score_min, k$score_max)),
      vb("Objects × raters", sprintf("%d × %d", k$n_object, k$n_rater), sprintf("%d criteria, %d sources", k$n_criterion, k$n_source)),
      vb("Design", k$design, sprintf("%.0f%% of rater–object cells empty", k$empty_cells), "secondary"),
      vb("Network", if (k$connected) "connected" else paste(k$subsets, "subsets"),
         if (k$connected) "MFRM can be estimated" else "MFRM not valid: link the subsets",
         if (k$connected) "success" else "danger"))
  })

  # ---- Overview
  output$p_cat <- renderPlot({
    d <- dat()
    tb <- as.data.frame(prop.table(table(d$rater, d$score), 1))
    names(tb) <- c("rater", "score", "proportion")
    ggplot(tb, aes(rater, proportion, fill = score)) + geom_col(width = 0.7) +
      scale_fill_brewer(palette = "Blues") + scale_y_continuous(labels = scales::percent) +
      coord_flip() + labs(x = NULL, y = NULL, fill = "Score") + theme_minimal(15)
  })
  output$t_desc <- renderDT(dt(ra_describe(dat())))
  output$t_agree <- renderDT(dt(ra_agreement(dat())))

  # ---- Severity
  output$mfrm_summary <- renderUI({
    f <- fit()
    rt <- f$raters
    most <- rt$rater[which.max(rt$severity)]; least <- rt$rater[which.min(rt$severity)]
    tagList(layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Rater separation reliability", sprintf("%.2f", f$rater_reliability),
                p("close to 1 = raters differ reliably in severity")),
      value_box("Rater homogeneity (Wald test)", sprintf("χ²(%d) = %.1f", f$fixed_df, f$fixed_chi),
                p(if (f$fixed_p < .001) "p < .001: raters are not equally severe" else sprintf("p = %s", apa(f$fixed_p, 3)))),
      value_box("Most severe / most lenient", paste(most, "/", least),
                p(sprintf("difference %.2f logits", diff(range(rt$severity))))),
      value_box("Object separation reliability", sprintf("%.2f", f$object_reliability),
                p(if (f$converged) sprintf("%s estimation converged", f$method)
                  else sprintf("%s estimation did not converge; interpret with caution", f$method)), theme = "secondary")),
      if (length(ext <- extreme_raters())) div(class = "alert alert-warning mt-2",
        tags$b("Raters with extreme scores: "), paste(ext, collapse = ", "),
        ". These raters always gave the minimum or maximum score, so their severity has no finite estimate and is shown as a bound (large SE). ",
        "In content validation this usually signals an expert who is too lenient or does not discriminate between items."))
  })
  extreme_raters <- reactive({
    d <- valid(); r <- tapply(d$score, d$rater, function(v) length(unique(v)) == 1 && (v[1] %in% range(d$score)))
    names(r)[r]
  })

  output$p_wright <- renderPlot({
    f <- fit()
    ob <- f$objects; rt <- f$raters; cr <- f$criteria
    h <- hist(ob$measure, breaks = 25, plot = FALSE)
    bars <- data.frame(ymin = head(h$breaks, -1), ymax = h$breaks[-1], xmax = 0.9 * h$counts / max(h$counts))
    pts <- rbind(
      data.frame(x = 1.2, name = rt$rater, logit = rt$severity, src = rt$source),
      data.frame(x = 2.4, name = cr$criterion, logit = cr$difficulty, src = "criterion"))
    gap <- diff(range(c(ob$measure, pts$logit))) / 26
    pts$ly <- ave(pts$logit, pts$x, FUN = function(v) spread(v, gap))
    srcs <- unique(rt$source)
    pal <- c(setNames(src_cols[seq_along(srcs)], srcs), criterion = "grey35")
    top <- max(c(h$breaks, pts$ly)) + gap * 2
    ggplot() +
      geom_rect(data = bars, aes(xmin = 0, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey75", colour = "white") +
      geom_segment(data = pts, aes(x = x, xend = x + 0.12, y = logit, yend = ly, colour = src), linewidth = 0.3, show.legend = FALSE) +
      geom_point(data = pts, aes(x = x, y = logit, colour = src), size = 3.5) +
      geom_text(data = pts, aes(x = x + 0.15, y = ly, label = name, colour = src), hjust = 0, size = 4.6, show.legend = FALSE) +
      annotate("text", x = c(0.45, 1.2, 2.4), y = top, label = c("Objects", "Raters", "Criteria"), fontface = "bold", size = 4.8) +
      scale_colour_manual(values = pal) + scale_x_continuous(limits = c(0, 3.4)) +
      labs(x = NULL, y = "Logit", colour = NULL) + theme_minimal(15) +
      theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$p_sev <- renderPlot({
    rt <- fit()$raters
    rt$rater <- reorder(rt$rater, rt$severity)
    srcs <- unique(rt$source)
    ggplot(rt, aes(severity, rater, colour = source)) +
      geom_vline(xintercept = 0, colour = "grey70") +
      geom_errorbarh(aes(xmin = severity - 1.96 * se, xmax = severity + 1.96 * se), height = 0.25) +
      geom_point(size = 3.5) + scale_colour_manual(values = setNames(src_cols[seq_along(srcs)], srcs)) +
      labs(x = "Severity (logits)  ← more lenient · more severe →", y = NULL, colour = NULL) +
      theme_minimal(15) + theme(legend.position = "bottom")
  })
  output$t_raters <- renderDT(dt(fit()$raters))
  output$t_crit <- renderDT(dt(fit()$criteria))
  output$t_obj <- renderDT(dt(fit()$objects, pageLength = 6))

  # ---- Rater effects & DRF
  eff <- reactive(ra_effects(fit()))
  output$t_eff <- renderDT({
    datatable(fmt(eff()), rownames = FALSE, class = "compact stripe",
              options = list(dom = "t", pageLength = 50)) |>
      formatStyle("flags", fontWeight = styleEqual("-", "normal", "bold"),
                  color = styleEqual("-", "#64748b", "#b91c1c"))
  })
  drf <- reactive(if (ra_has(dat(), "group")) withProgress(message = "Computing DRF…", ra_drf(dat())) else NULL)
  output$drf_ui <- renderUI({
    if (is.null(drf())) return(p(class = "text-muted", "Map the ", tags$b("Object group"), " column in the Data tab to run this analysis."))
    tagList(
      plotOutput("p_drf", height = 260), DTOutput("t_drf"),
      note("Contrast = rater bias for the focal group minus bias for the reference group (logits); positive = more lenient toward the focal group. ",
           "Each rater's bias is computed against object measures estimated without that rater and without already-flagged raters (iterative purification). DRF = FDR-adjusted p < .05 and |contrast| ≥ 0.5."))
  })
  output$p_drf <- renderPlot({
    x <- drf(); x$lab <- paste0(x$rater, " (", x$focal_group, " vs ", x$reference_group, ")")
    x$lab <- reorder(x$lab, x$contrast)
    ggplot(x, aes(contrast, lab, colour = decision == "DRF")) +
      annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "grey93") +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_errorbarh(aes(xmin = contrast - 1.96 * se, xmax = contrast + 1.96 * se), height = 0.25) +
      geom_point(size = 3) + scale_colour_manual(values = c(`FALSE` = "grey40", `TRUE` = "#b91c1c"), guide = "none") +
      labs(x = "Bias contrast (logits); grey band = |contrast| < 0.5", y = NULL) + theme_minimal(15)
  })
  output$t_drf <- renderDT(dt(drf(), 3))

  # ---- Sources & stability
  cmp <- reactive(if (ra_has(dat(), "source")) ra_compare_sources(fit(), dat(), input$ref_src) else NULL)
  output$src_ui <- renderUI({
    if (!ra_has(dat(), "source") || length(unique(dat()$source)) < 2)
      return(card(card_header("Comparison across rater sources"), p(class = "text-muted", "Requires a Rater source column with at least two sources.")))
    srcs <- unique(dat()$source)
    card(card_header("Comparison across rater sources"),
      selectInput("ref_src", "Reference source", srcs, selected = isolate(input$ref_src) %||% srcs[1], width = 260),
      DTOutput("t_src"),
      note("Difference vs reference = difference in mean severity (logits). r and score difference are computed from each source's mean score per object × criterion."))
  })
  output$t_src <- renderDT(dt(cmp()))

  stab <- reactive(if (ra_has(dat(), "replicate")) ra_stability(dat()) else NULL)
  output$stab_ui <- renderUI({
    if (is.null(stab())) return(card(card_header("Stability across replicates"),
      p(class = "text-muted", "Requires a Replicate column: the same rater scores the same object more than once (e.g., an LLM run three times).")))
    card(card_header("Stability across replicates"),
      layout_columns(col_widths = c(7, 5), DTOutput("t_stab"), plotOutput("p_sb", height = 260)),
      note("Single-replicate ICC = proportion of score variance that is stable across replicates. Right panel: reliability of the mean of k replicates (Spearman–Brown)."))
  })
  output$t_stab <- renderDT(dt(stab()[, names(stab()) != "rater"]))
  output$p_sb <- renderPlot({
    s <- stab(); k <- 1:6
    cur <- do.call(rbind, lapply(seq_len(nrow(s)), function(i)
      data.frame(unit = s$unit[i], k = k, rel = k * s$icc_single[i] / (1 + (k - 1) * s$icc_single[i]))))
    ggplot(cur, aes(k, rel, colour = unit)) + geom_hline(yintercept = 0.8, linetype = 2, colour = "grey60") +
      geom_line(linewidth = 1) + geom_point() + scale_x_continuous(breaks = k) + ylim(0, 1) +
      labs(x = "Number of replicates averaged", y = "Reliability", colour = NULL) + theme_minimal(14) +
      theme(legend.position = "bottom", legend.text = element_text(size = 9))
  })

  drift <- reactive(if (ra_has(dat(), "version")) ra_drift(dat()) else NULL)
  output$drift_ui <- renderUI({
    if (is.null(drift())) return(card(card_header("Drift across versions"),
      p(class = "text-muted", "Requires a Version / prompt column with at least two values for the same rater.")))
    card(card_header("Drift across versions"),
      layout_columns(col_widths = c(5, 7), plotOutput("p_drift", height = 260), DTOutput("t_drift")),
      note("Each rater's severity is estimated separately per version within one MFRM, with unversioned raters as anchors. ",
           "Drift = FDR-adjusted p < .05 and |change| ≥ 0.3 logits. SEs do not yet account for dependence between replicates."))
  })
  output$p_drift <- renderPlot({
    b <- drift()$by_version; b <- b[b$version != "", ]
    ggplot(b, aes(version, severity, group = base_rater, colour = base_rater)) +
      geom_line(linewidth = 1) + geom_pointrange(aes(ymin = severity - 1.96 * se, ymax = severity + 1.96 * se)) +
      labs(x = "Version", y = "Severity (logits)", colour = NULL) + theme_minimal(15) + theme(legend.position = "bottom")
  })
  output$t_drift <- renderDT(dt(drift()$table[, c("rater", "from", "to", "change", "se", "z", "p_fdr", "decision")]))

  # ---- Content validity
  # Experts often use only the top of the scale, so the observed range is not the scale.
  # Default: start at 1 (or 0 if a 0 was used); end at the highest observed score.
  scale_guess <- reactive({
    r <- range(valid()$score)
    c(lo = if (r[1] <= 0) r[1] else 1, hi = r[2])
  })
  output$aik_scale <- renderUI({
    g <- scale_guess()
    tagList(numericInput("aik_lo", "Lowest score on the instrument's scale", g[["lo"]], step = 1),
            numericInput("aik_hi", "Highest score on the instrument's scale", g[["hi"]], step = 1))
  })
  aik_lo <- reactive(if (is.numeric(input$aik_lo)) input$aik_lo else scale_guess()[["lo"]])
  aik_hi <- reactive(if (is.numeric(input$aik_hi)) input$aik_hi else scale_guess()[["hi"]])
  aik <- reactive({
    d <- valid()
    validate(need(aik_lo() <= min(d$score) && aik_hi() >= max(d$score) && aik_hi() > aik_lo(),
                  "The scale range must include every score in the data."))
    ra_aiken(d, aik_lo(), aik_hi(), as.numeric(input$aik_alpha))
  })
  icc <- reactive(ra_icc(valid()))
  output$aik_boxes <- renderUI({
    a <- aik(); ic <- icc(); k <- length(unique(valid()$rater))
    vk <- aiken_critical(k, aik_lo(), aik_hi(), as.numeric(input$aik_alpha))
    nv <- sum(a$decision == "Valid")
    r <- range(valid()$score)
    tagList(div(class = "alert alert-info",
      tags$b(sprintf("Scale used: %g–%g (%d categories). ", aik_lo(), aik_hi(), aik_hi() - aik_lo() + 1)),
      sprintf("Experts actually used scores %g–%g. ", r[1], r[2]),
      "Make sure the scale in the left panel matches the instrument (e.g., 1–5), not only the scores that were used; otherwise V and the critical value are wrong."),
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Experts and scale", sprintf("%d experts", k), p(sprintf("%g–%g scale (%d categories)", aik_lo(), aik_hi(), aik_hi() - aik_lo() + 1))),
      value_box("Critical V", sprintf("%.2f", vk), p(sprintf("for an item to be valid (p < %s)", apa(as.numeric(input$aik_alpha))))),
      value_box("Valid items", sprintf("%d of %d", nv, nrow(a)), p(sprintf("%d item(s) to revise", nrow(a) - nv)),
                theme = if (nv == nrow(a)) "success" else "warning"),
      value_box("ICC(2,k)", if (is.null(ic)) "–" else sprintf("%.2f", ic$ICC[2]),
                p(if (is.null(ic)) "not enough data" else ic$category[2]), theme = "secondary")))
  })
  output$t_aik <- renderDT({
    datatable(fmt(aik(), 2), rownames = FALSE, class = "compact stripe",
              options = list(pageLength = 15, dom = "tip", scrollX = TRUE)) |>
      formatStyle("decision", color = styleEqual(c("Valid", "Revise"), c("#15803d", "#b45309")), fontWeight = "bold")
  })
  output$t_icc <- renderDT({ req(icc()); dt(icc()) })
  output$aik_text <- renderUI({
    a <- aik(); ic <- icc(); k <- length(unique(valid()$rater))
    al <- as.numeric(input$aik_alpha); c <- aik_hi() - aik_lo() + 1
    vk <- aiken_critical(k, aik_lo(), aik_hi(), al)
    nv <- sum(a$decision == "Valid"); rev <- a$object[a$decision != "Valid"]
    t1 <- sprintf("Content validity was rated by %d experts on a %g–%g scale. With %d experts and %d categories, the minimum V required for significance (p < %s) is %s (Aiken, 1985). ",
                  k, aik_lo(), aik_hi(), k, c, apa(al), apa(vk))
    t2 <- sprintf("%d of %d items met this criterion (V = %s–%s)%s. ",
                  nv, nrow(a), apa(min(a$V)), apa(max(a$V)),
                  if (length(rev)) paste0("; items ", paste(rev, collapse = ", "), " require revision") else "")
    t3 <- if (!is.null(ic)) {
      adj <- mean(ra_agreement(valid())$adjacent, na.rm = TRUE)
      x <- sprintf("Inter-rater reliability was ICC(2,k) = %s, 95%% CI [%s, %s], indicating %s reliability (Koo & Li, 2016).",
                   apa(ic$ICC[2]), apa(ic$ci_lower[2]), apa(ic$ci_upper[2]), ic$category[2])
      if (ic$ICC[2] < 0.75 && adj >= 0.8)
        x <- paste0(x, sprintf(" However, adjacent agreement between experts reached %.0f%%; the low ICC mainly reflects the restricted range of ratings (most items were rated highly) rather than disagreement among experts.",
                               100 * adj))
      x
    } else ""
    div(style = "font-size: 1.02rem; line-height: 1.6", t1, t2, t3)
  })

  # ---- Export
  for (ex in list(c("ex_w5", "content_validity_wide_5experts.xlsx"), c("ex_w7", "content_validity_wide_7experts.xlsx"),
                  c("ex_l2", "content_validity_long_2criteria.xlsx"))) local({
    id <- ex[1]; fn <- ex[2]
    output[[id]] <- downloadHandler(filename = fn, content = function(file) file.copy(file.path("examples", fn), file))
  })

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("raterAudit_", Sys.Date(), ".xlsx"),
    content = function(file) {
      ck <- chk()[setdiff(names(chk()), c("categories", "rare_categories"))]
      sheets <- list(design_check = data.frame(item = names(unlist(ck)), value = unlist(ck)),
                     descriptives = ra_describe(dat()), agreement = ra_agreement(dat()),
                     raters = fit()$raters, criteria = fit()$criteria, objects = fit()$objects,
                     rater_effects = eff(), aiken_V = aik())
      if (!is.null(icc())) sheets$ICC <- icc()
      if (!is.null(drf())) sheets$drf <- drf()
      if (!is.null(cmp())) sheets$sources <- cmp()
      if (!is.null(stab())) sheets$stability <- stab()
      if (!is.null(drift())) sheets$drift <- drift()$table
      writexl::write_xlsx(lapply(sheets, fmt, digits = 4), file)
    })
  output$dl_r <- downloadHandler(
    filename = function() "raterAudit_analysis.R",
    content = function(file) {
      src <- if (input$src == "ex") sprintf('raw <- ra_example("%s")', input$ex) else 'raw <- read.csv("your_data.csv")'
      q <- function(v) if (is.null(v) || v == "") "NULL" else sprintf('"%s"', v)
      prep <- if (is_wide()) c(
        sprintf("long <- ra_from_wide(raw, %s, c(%s), %s, %s)", q(input$w_id),
                paste(sprintf('"%s"', input$w_raters), collapse = ", "), q(input$w_crit), q(input$w_group)),
        sprintf('d <- ra_data(long, %s, ".penilai", ".kriteria", ".skor", group = %s)', q(input$w_id), q(input$w_group)))
      else {
        m <- function(r) q(input[[paste0("map_", r)]])
        sprintf("d <- ra_data(raw, %s, %s, %s, %s, source = %s, replicate = %s, version = %s, group = %s)",
                m("object"), m("rater"), m("criterion"), m("score"), m("source"), m("replicate"), m("version"), m("group"))
      }
      writeLines(c("source(\"R/core.R\"); source(\"R/simulate.R\")", src, prep,
        "ra_check(d)", sprintf("ra_aiken(d, lo = %g, hi = %g)", aik_lo(), aik_hi()), "ra_icc(d)",
        "fit <- ra_mfrm(d)", "fit$raters", "ra_agreement(d)", "ra_effects(fit)",
        "ra_drf(d)", "ra_compare_sources(fit, d)", "ra_stability(d)", "ra_drift(d)"), file)
    })
}

shinyApp(ui, server)
