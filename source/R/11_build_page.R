##################################################################
#####  11_build_page.R - the whole thing as one HTML page    #####
##################################################################
##
## Fills R/page_template.html from the panel: every number, the 13 figures as
## data URIs, the source table, and the per-province series the drill-down
## needs. Output is one self-contained file - no server, no assets folder.
##
## Discipline: the template holds PROSE, this script holds NUMBERS. Nothing in
## the page is typed by hand, and an unsubstituted {{token}} is a build failure
## rather than something a reader discovers.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

##################################################################
#####  1. SMALL FORMATTERS                                   #####
##################################################################

f0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
f2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
fq <- function(p) if (nchar(p) == 6) paste0(substr(p, 1, 4), "년 ", substr(p, 6, 6), "분기") else paste0(p, "년")
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

##################################################################
#####  2. THE SERIES THE PAGE NEEDS                          #####
##################################################################

province_series <- function(panel) {
  pop <- panel_pick(panel, "pop_sido", axis = list(행정구역별 = "*"), item = "계")
  pop <- pop[!is.na(pop$sido), c("year", "sido", "value")]
  names(pop)[3] <- "pop"

  per_1k <- function(d) {
    d <- merge(d, pop, by = c("year", "sido"))
    d$v <- 1000 * d$value / d$pop
    d[, c("year", "sido", "v")]
  }

  bed <- annualise(panel_pick(panel, "bed_sido",
                              axis = list(입원실현황별 = "계"), item = "병상수"))
  doc <- annualise(panel_pick(panel, "hr_sido",
                              axis = list(의료인력별 = c("의사", "전문의")),
                              item = "인력현황"))
  ter <- annualise(panel_pick(panel, "inst_sido",
                              axis = list(요양기관종별 = "상급종합병원")))

  acc <- function(key, lev = "입원") {
    d <- panel_pick(panel, key, axis = list(시도별 = "*", 입원및외래별 = lev),
                    item = "진료비")
    d <- d[!is.na(d$sido), c("year", "sido", "value")]
    d
  }
  ri <- acc("care_in_res");  names(ri)[3] <- "a"
  ro <- acc("care_out_res"); names(ro)[3] <- "b"
  self <- merge(ri, ro, by = c("year", "sido"))
  self$v <- 100 * self$a / (self$a + self$b)
  self <- self[!self$year %in% ACCESS_MISMATCH_YEARS, c("year", "sido", "v")]

  ii <- acc("care_in_inst");  names(ii)[3] <- "a"
  io <- acc("care_out_inst"); names(io)[3] <- "b"
  infl <- merge(ii, io, by = c("year", "sido"))
  infl$v <- 100 * infl$b / (infl$a + infl$b)
  infl <- infl[, c("year", "sido", "v")]

  list(
    bed_1k  = per_1k(bed[!is.na(bed$sido), c("year", "sido", "value")]),
    doc_1k  = per_1k(doc[!is.na(doc$sido) & doc$의료인력별 == "의사",
                         c("year", "sido", "value")]),
    spec_1k = per_1k(doc[!is.na(doc$sido) & doc$의료인력별 == "전문의",
                         c("year", "sido", "value")]),
    self_in = self,
    inflow  = infl,
    tert    = setNames(ter[!is.na(ter$sido), c("year", "sido", "value")],
                       c("year", "sido", "v"))
  )
}

## nested {province: {year: value}} plus the 17-province median per year
to_json_blocks <- function(S) {
  provs <- SIDO_17
  series <- list(); median <- list()
  for (nm in names(S)) {
    d <- S[[nm]]
    d <- d[!is.na(d$v) & d$sido %in% provs, ]
    for (p in provs) {
      g <- d[d$sido == p, ]
      if (is.null(series[[p]])) series[[p]] <- list()
      series[[p]][[nm]] <- setNames(as.list(round(g$v, 3)), as.character(g$year))
    }
    m <- stats::aggregate(v ~ year, data = d, FUN = stats::median)
    median[[nm]] <- setNames(as.list(round(m$v, 3)), as.character(m$year))
  }
  jsonlite::toJSON(list(order = provs, series = series, median = median),
                   auto_unbox = TRUE, null = "null", digits = NA)
}

##################################################################
#####  3. EVERY NUMBER THE PROSE QUOTES                      #####
##################################################################

page_values <- function(panel, S) {
  V <- list()
  q <- function(k, ...) V[[k]] <<- paste0(...)

  ## --- 1 volume
  beds <- annualise(panel_pick(panel, "bed_tier",
                               axis = list(입원실현황별 = "계", 요양기관종별 = "계"),
                               item = "병상수"))
  docs <- annualise(panel_pick(panel, "hr_doctor",
                               axis = list(의료인력별 = "의사", 요양기관종별 = "전체"),
                               item = "인력현황"))
  pop  <- panel_pick(panel, "pop_sido", axis = list(행정구역별 = "전국"), item = "계")
  dd <- merge(merge(data.frame(year = beds$year, bed = beds$value),
                    data.frame(year = docs$year, doc = docs$value), by = "year"),
              data.frame(year = pop$year, pop = pop$value), by = "year")
  dd <- dd[order(dd$year), ]
  dd$b1k <- 1000 * dd$bed / dd$pop; dd$d1k <- 1000 * dd$doc / dd$pop
  a <- dd[1, ]; z <- dd[nrow(dd), ]
  q("bed1k_y0", a$year); q("bed1k_v0", f2(a$b1k))
  q("bed1k_y1", z$year); q("bed1k_v1", f2(z$b1k))
  q("doc1k_v0", f2(a$d1k)); q("doc1k_v1", f2(z$d1k))

  ## --- 2 mix
  tier <- annualise(panel_pick(panel, "bed_tier",
                               axis = list(입원실현황별 = "계",
                                           요양기관종별 = c("계", "요양병원", "병원")),
                               item = "병상수"))
  tw <- tidyr::pivot_wider(tier[, c("year", "요양기관종별", "value")],
                           names_from = "요양기관종별", values_from = "value")
  tw <- tw[order(tw$year), ]
  q("bed_y0", tw$year[1])
  q("bed_add_total",   f0(tw$계[nrow(tw)] - tw$계[1]))
  q("bed_add_nursing", f0(tw$요양병원[nrow(tw)] - tw$요양병원[1]))

  psy <- panel_pick(panel, "bed_tier",
                    axis = list(입원실현황별 = "계", 요양기관종별 = "정신병원"),
                    item = "병상수")
  allq <- sort(unique(panel$prd_de[panel$key == "bed_tier"]))
  fq_ <- min(psy$prd_de); pq <- allq[which(allq == fq_) - 1L]
  q("psy_prd", fq(fq_)); q("psy_val", f0(psy$value[psy$prd_de == fq_]))
  for (nm in c("요양병원", "병원")) {
    h <- panel_pick(panel, "bed_tier",
                    axis = list(입원실현황별 = "계", 요양기관종별 = nm), item = "병상수")
    V[[if (nm == "요양병원") "psy_donor_nh" else "psy_donor_h"]] <-
      paste0(f0(h$value[h$prd_de == pq]), " → ", f0(h$value[h$prd_de == fq_]))
  }

  icu <- annualise(panel_pick(panel, "bed_tier",
                              axis = list(입원실현황별 = "중환자실", 요양기관종별 = "계"),
                              item = "병상수"))
  icu <- icu[order(icu$year), ]
  q("icu_y0", icu$year[1]); q("icu_v0", f0(icu$value[1]))
  q("icu_y1", fq(icu$prd_de[nrow(icu)])); q("icu_v1", f0(icu$value[nrow(icu)]))

  ## --- 3 distribution
  b <- S$bed_1k; d <- S$doc_1k
  yr <- max(intersect(b$year, d$year))
  cb <- b[b$year == yr & b$sido %in% SIDO_17, ]
  cd <- d[d$year == yr & d$sido %in% SIDO_17, ]
  cb <- cb[order(-cb$v), ]
  q("dist_year", yr)
  ## ⚠ rho 는 «분포 통계»라 17개 전부로 잰다. 극단값을 빼고 상관을 재면 다른 양이 된다.
  ## 세종을 빼는 것은 「최고·최저·몇 위」라는 «문장»뿐이다(cbh).
  m <- merge(cb, cd, by = "sido")
  q("rho", f2(suppressWarnings(stats::cor(m$v.x, m$v.y, method = "spearman"))))
  cbh <- drop_headline_sido(cb)
  cdh <- drop_headline_sido(cd)
  q("bedmax_sido", cbh$sido[1]);            q("bedmax", f1(cbh$v[1]))
  q("bedmin_sido", cbh$sido[nrow(cbh)]);    q("bedmin", f1(cbh$v[nrow(cbh)]))
  q("bed_ratio", f1(cbh$v[1] / cbh$v[nrow(cbh)]))
  q("n_rank_sido", nrow(cbh))
  q("seoul_bed_rank", which(cbh$sido == "서울"))
  q("seoul_doc_rank", which(cdh$sido[order(-cdh$v)] == "서울"))

  ## --- 4 access
  sf <- S$self_in; inf <- S$inflow
  ay <- max(intersect(sf$year, inf$year))
  cs <- sf[sf$year == ay & sf$sido %in% SIDO_17, ]; cs <- cs[order(cs$v), ]
  ci <- inf[inf$year == ay & inf$sido %in% SIDO_17, ]
  q("acc_year", ay)
  csh <- drop_headline_sido(cs)     # 「최저·최고」 문장에서만 세종을 뺀다
  q("self_min_sido", csh$sido[1]);        q("self_min", f1(csh$v[1]))
  q("self_max_sido", csh$sido[nrow(csh)]); q("self_max", f1(csh$v[nrow(csh)]))
  q("sejong_self", f1(cs$v[cs$sido == "세종"]))
  q("seoul_self", f0(cs$v[cs$sido == "서울"]))
  q("seoul_infl", f0(ci$v[ci$sido == "서울"]))

  ## --- emergency
  er <- panel_pick(panel, "er_bed",
                   axis = list(특수진료실구분별 = "응급실",
                               요양기관종별 = c("상급종합병원", "종합병원", "병원")),
                   item = "병상수")
  ew <- tidyr::pivot_wider(er[, c("prd_de", "요양기관종별", "value")],
                           names_from = "요양기관종별", values_from = "value")
  ew <- ew[order(ew$prd_de), ]
  e0 <- ew[1, ]; e1 <- ew[nrow(ew), ]
  q("er_p0", fq(e0$prd_de)); q("er_p1", fq(e1$prd_de))
  q("er_sang0", f0(e0$상급종합병원)); q("er_sang1", f0(e1$상급종합병원))
  q("er_gen0",  f0(e0$종합병원));     q("er_gen1",  f0(e1$종합병원))
  q("er_hos0",  f0(e0$병원));         q("er_hos1",  f0(e1$병원))

  ef <- panel_pick(panel, "er_facility",
                   axis = list(응급의료기관유형별 = c("계", "권역응급의료센터", "지역응급의료기관"),
                               지역별 = "전체"), drop_na = FALSE)
  fw <- tidyr::pivot_wider(ef[, c("year", "응급의료기관유형별", "value")],
                           names_from = "응급의료기관유형별", values_from = "value")
  fw <- fw[order(fw$year), ]
  f_0 <- fw[1, ]; f_1 <- fw[nrow(fw), ]
  q("erf_y0", f_0$year); q("erf_y1", f_1$year)
  q("erf_kwon0", f0(f_0$권역응급의료센터)); q("erf_kwon1", f0(f_1$권역응급의료센터))
  q("erf_loc0",  f0(f_0$지역응급의료기관)); q("erf_loc1",  f0(f_1$지역응급의료기관))
  q("erf_tot0",  f0(f_0$계));               q("erf_tot1",  f0(f_1$계))

  ## --- 2024 break
  sup <- panel_pick(panel, "hr_doctor",
                    axis = list(의료인력별 = c("전문의", "레지던트", "인턴"),
                                요양기관종별 = "상급종합병원"), item = "인력현황")
  sw <- tidyr::pivot_wider(sup[, c("prd_de", "의료인력별", "value")],
                           names_from = "의료인력별", values_from = "value")
  sw <- sw[order(sw$prd_de), ]
  sw$전공의 <- sw$레지던트 + sw$인턴
  base <- sw[sw$prd_de == "202304", ]
  post <- sw[sw$prd_de > "202304", ]
  tr <- post[which.min(post$전공의), ]
  nw <- sw[nrow(sw), ]
  q("res_base_prd", fq("202304")); q("res_base", f0(base$전공의))
  q("res_trough_prd", fq(tr$prd_de)); q("res_trough", f0(tr$전공의))
  q("res_trough_pct", f1(100 * tr$전공의 / base$전공의))
  q("res_now_prd", fq(nw$prd_de)); q("res_now", f0(nw$전공의))
  q("res_now_pct", f1(100 * nw$전공의 / base$전공의))
  q("spec_base", f0(base$전문의)); q("spec_now", f0(nw$전문의))

  ## --- ⑤활용 · ⑥기준 (17_view_benchmark.R 이 계산한 것을 그대로 받는다)
  bm <- tryCatch(load_step("^benchmark_"), error = function(e) NULL)
  if (!is.null(bm)) {
    rk <- function(d) {
      y <- max(d$year[d$국가 == "대한민국"])
      g <- d[d$year == y, ]; g <- g[order(-g$value), ]
      list(y = y, v = g$value[g$국가 == "대한민국"],
           r = which(g$국가 == "대한민국"), n = nrow(g),
           oth = paste(sprintf("%s %.1f", g$국가[g$국가 != "대한민국"],
                               g$value[g$국가 != "대한민국"]), collapse = " · "))
    }
    b <- rk(bm$bed); d6 <- rk(bm$doc)
    q("bm_year", b$y)
    q("bm_bed", f1(b$v)); q("bm_bed_rank", b$r); q("bm_bed_n", b$n); q("bm_bed_oth", b$oth)
    q("bm_doc", f1(d6$v)); q("bm_doc_rank", d6$r); q("bm_doc_n", d6$n); q("bm_doc_oth", d6$oth)
    ko <- bm$occ[bm$occ$국가 == "대한민국", ]
    q("bm_occ_y0", min(ko$year)); q("bm_occ_y1", max(ko$year)); q("bm_occ_n", nrow(ko))
    q("bm_alos_y0", min(bm$alos$year)); q("bm_alos_y1", max(bm$alos$year))
    q("bm_alos0", f1(bm$alos$value[which.min(bm$alos$year)]))
    q("bm_alos1", f1(bm$alos$value[which.max(bm$alos$year)]))
  }

  ## --- 통계연보 시군구 시계열 (16_yearbook.R 이 계산한 것을 그대로 받는다)
  yb <- tryCatch(load_step("^yearbook_"), error = function(e) NULL)
  if (!is.null(yb)) {
    qq <- yb$quant; ww <- yb$wide; dd <- yb$drop
    y0 <- min(qq$연도); y1 <- max(qq$연도)
    q("yb_y0", y0); q("yb_y1", y1)
    q("yb_n_sgg", length(unique(paste(ww$시도, ww$지역))))
    q("yb_med0", f1(qq$p50[qq$연도 == y0])); q("yb_med1", f1(qq$p50[qq$연도 == y1]))
    q("yb_p10", f1(qq$p10[qq$연도 == y1])); q("yb_p90", f1(qq$p90[qq$연도 == y1]))
    q("yb_drop_list",
      paste(sprintf("%s %+.1f%%p", utils::head(dd$지역, 5), utils::head(dd$변화, 5)),
            collapse = " · "))
  }

  ## --- healthmap (2023 cross-section)
  hm <- tryCatch(load_step("^healthmap_"), error = function(e) NULL)
  if (!is.null(hm)) {
    ind <- hm$indicators; und <- hm$underserved
    loc <- ind[ind$measure == "관내의료이용률" & ind$level == "시군구" & !is.na(ind$value), ]
    zr <- function(sv) sum(loc$value[loc$service == sv] == 0)
    mp <- function(sv) stats::median(loc$value[loc$service == sv & loc$value > 0])
    q("hm_year", ind$year[1])
    q("hm_zero_tert", zr("상급종합병원")); q("hm_zero_kwon", zr("권역응급의료센터"))
    q("hm_med_kwon", f1(mp("권역응급의료센터"))); q("hm_med_all", f1(mp("전체 병원")))
    q("hm_und_b", sum(und$분만)); q("hm_und_c", sum(und$소아청소년과))
    q("hm_und_d", sum(und$인공신장실)); q("hm_und_e", sum(und$응급))
    n3 <- und[rowSums(und[, c("분만", "소아청소년과", "인공신장실", "응급")]) >= 3, ]
    q("hm_multi_n", nrow(n3))
    q("hm_multi_list", paste0(n3$sido, " ", n3$sgg_nm, collapse = ", "))
  }

  ## --- masthead + footer counts, measured not assumed
  q("asof", fq(max(panel$prd_de[panel$prd_se == "Q"])))
  q("n_ind", length(unique(panel$key)))
  q("n_rows", f0(nrow(panel)))
  acc_src <- readLines(file.path(r_dir, "99_acceptance.R"), warn = FALSE)
  q("n_checks", sum(grepl("  expect(", acc_src, fixed = TRUE)))
  q("n_gates",  sum(grepl("expect_error(", acc_src, fixed = TRUE)) + 1L)
  ## ⛔ 2026-09-02 감사: 전 stamp 를 다 세어 공표 페이지가 「표 32장·그림 38장」이라고
  ##    적었다. 그 실행이 실제로 낸 것은 17표·21그림이다(약 1.9배). run_pipeline.R 은
  ##    stamp 로 걸러 맞게 세는데 «같은 파이프라인 안에서 두 숫자가 달랐다».
  q("n_tables", length(list.files(tables_dir, pattern = paste0("_", stamp, "\\.xlsx$"))))
  q("n_figs",   length(list.files(figures_dir, pattern = paste0("_", stamp, "\\.png$"))))
  V
}

##################################################################
#####  4. TILES + SOURCE TABLE                               #####
##################################################################

build_tiles <- function(V) {
  t <- function(k, v, u, d) paste0(
    '<div class="tile"><div class="k">', k, '</div><div class="v">', v,
    if (nzchar(u)) paste0('<small>', u, '</small>') else "", '</div>',
    '<div class="d">', d, '</div></div>')
  paste0(
    t("인구 1,000명당 병상", V$bed1k_v1, "", paste0(V$bed1k_y1, "년")),
    t("인구 1,000명당 의사", V$doc1k_v1, "", paste0(V$bed1k_y1, "년")),
    t("늘어난 병상 중 요양병원 몫", V$bed_add_nursing, "개",
      paste0(V$bed_y0, "년 이후 총 ", V$bed_add_total, "개")),
    t("시도 간 병상 밀도 격차", V$bed_ratio, "배",
      paste0(V$bedmax_sido, " / ", V$bedmin_sido)),
    t("입원 자체충족률 최저", paste0(V$self_min, "%"), "",
      paste0(V$self_min_sido, " · ", V$acc_year, "년")),
    t("상급종합 전공의 저점", paste0(V$res_trough_pct, "%"), "",
      paste0(V$res_trough_prd, " · 2023Q4=100")),
    collapse = "")
}

ORG_NAME <- c("354" = "심평원", "350" = "건강보험공단", "411" = "국립중앙의료원",
              "101" = "행정안전부", "117" = "보건복지부")

build_sources <- function(panel) {
  reg <- load_registry()
  rows <- character(0)
  for (i in seq_len(nrow(reg))) {
    r <- as.list(reg[i, ])
    if (identical(r$kind, "file")) {
      ## File sources have no panel rows; their coverage is the year on the row.
      rows <- c(rows, paste0(
        "<tr><td>", esc(r$label_ko), "</td>",
        "<td><code>", esc(basename(r$tbl_id)), "</code></td>",
        "<td>", ORG_NAME[[r$org_id]] %||% r$org_id, "</td>",
        "<td>파일</td>",
        "<td class='num'>", r$start_prd, "년 단면</td></tr>"))
      next
    }
    d <- panel[panel$key == r$key, ]
    if (nrow(d) == 0) next
    rows <- c(rows, paste0(
      "<tr><td>", esc(r$label_ko), "</td>",
      "<td><code>", r$tbl_id, "</code></td>",
      "<td>", ORG_NAME[[r$org_id]] %||% r$org_id, "</td>",
      "<td>", if (r$prd_se == "Q") "분기" else "연", "</td>",
      "<td class='num'>", fq(min(d$prd_de)), " – ", fq(max(d$prd_de)), "</td></tr>"))
  }
  paste(rows, collapse = "\n")
}

##################################################################
#####  5. ASSEMBLE                                           #####
##################################################################

build_page <- function(panel) {
  cat("=== 11_build_page.R ===\n")
  S    <- province_series(panel)
  V    <- page_values(panel, S)
  html <- paste(readLines(file.path(r_dir, "page_template.html"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  html <- gsub("{{STAMP}}", stamp, html, fixed = TRUE)
  html <- gsub("{{TILES}}", build_tiles(V), html, fixed = TRUE)
  html <- gsub("{{SOURCES}}", build_sources(panel), html, fixed = TRUE)
  html <- gsub("{{DATA_JSON}}", to_json_blocks(S), html, fixed = TRUE)
  for (k in names(V)) {
    html <- gsub(paste0("{{VAL:", k, "}}"), as.character(V[[k]]), html, fixed = TRUE)
  }

  ## Figures travel inside the file: the artifact CSP blocks external images and
  ## a local path means nothing to a reader who opens the link.
  figs <- list.files(figures_dir, pattern = paste0("_", stamp, "\\.png$"), full.names = TRUE)
  for (f in figs) {
    key <- sub(paste0("_", stamp, "\\.png$"), "", basename(f))
    uri <- paste0("data:image/png;base64,",
                  jsonlite::base64_enc(readBin(f, "raw", file.size(f))))
    html <- gsub(paste0("{{FIG:", key, "}}"), uri, html, fixed = TRUE)
  }

  ## A leftover token is a build failure, not a reader's problem.
  left <- regmatches(html, gregexpr("\\{\\{[^}]+\\}\\}", html))[[1]]
  if (length(left) > 0) {
    stop("unsubstituted token(s): ", paste(unique(left), collapse = ", "))
  }

  out <- file.path(output_dir, paste0("보건자원감시_", stamp, ".html"))
  con <- file(out, open = "wb")
  writeBin(charToRaw(enc2utf8(html)), con)
  close(con)
  cat(sprintf("--- page -> %s (%.1f MB, 그림 %d장 embed)\n",
              basename(out), file.size(out) / 1024^2, length(figs)))
  invisible(out)
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  build_page(panel)
}
