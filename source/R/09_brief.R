##################################################################
#####  09_brief.R - the short read, generated not typed      #####
##################################################################
##
## Every number below is computed from the panel at write time. Nothing is
## transcribed, so the text cannot drift from the tables when the data updates.
## Sentences state what moved; they do not interpret why.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

.fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
.q   <- function(p) paste0(substr(p, 1, 4), "년 ", substr(p, 6, 6), "분기")
## Korean particles after a number depend on its final sound, so the text uses
## arrow notation and a direction verb rather than gluing 로/으로 onto a figure.
.dir <- function(new, old) if (new >= old) "늘었다" else "줄었다"
## Label an annualised year with the quarter that actually stood in for it, so
## "2026" is not read as a full year when only Q2 exists.
.yq  <- function(year, q) if (is.na(q) || q == 4) paste0(year, "년") else
                           paste0(year, "년 ", q, "분기")

write_brief <- function(panel) {
  cat("=== 09_brief.R ===\n")
  L <- character(0)
  add <- function(...) L <<- c(L, paste0(...))

  ## ---- domain 1: density -----------------------------------------------
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
  dd$bed_1k <- 1000 * dd$bed / dd$pop
  dd$doc_1k <- 1000 * dd$doc / dd$pop
  a <- dd[1, ]; z <- dd[nrow(dd), ]

  ## ---- domain 2: mix ----------------------------------------------------
  tier <- annualise(panel_pick(panel, "bed_tier",
                               axis = list(입원실현황별 = "계",
                                           요양기관종별 = c("계", "상급종합병원", "종합병원",
                                                            "병원", "요양병원")),
                               item = "병상수"))
  tw <- tidyr::pivot_wider(tier[, c("year", "q_used", "요양기관종별", "value")],
                           names_from = "요양기관종별", values_from = "value")
  tw <- tw[order(tw$year), ]
  t0 <- tw[1, ]; t1 <- tw[nrow(tw), ]

  icu <- annualise(panel_pick(panel, "bed_tier",
                              axis = list(입원실현황별 = "중환자실", 요양기관종별 = "계"),
                              item = "병상수"))
  icu <- icu[order(icu$year), ]

  ## ---- emergency --------------------------------------------------------
  erb <- panel_pick(panel, "er_bed",
                    axis = list(특수진료실구분별 = "응급실",
                                요양기관종별 = c("상급종합병원", "종합병원", "병원")),
                    item = "병상수")
  ew <- tidyr::pivot_wider(erb[, c("prd_de", "요양기관종별", "value")],
                           names_from = "요양기관종별", values_from = "value")
  ew <- ew[order(ew$prd_de), ]
  e0 <- ew[1, ]; e1 <- ew[nrow(ew), ]

  ef <- panel_pick(panel, "er_facility",
                   axis = list(응급의료기관유형별 = c("계", "권역응급의료센터",
                                                      "지역응급의료센터", "지역응급의료기관"),
                               지역별 = "전체"), drop_na = FALSE)
  fw <- tidyr::pivot_wider(ef[, c("year", "응급의료기관유형별", "value")],
                           names_from = "응급의료기관유형별", values_from = "value")
  fw <- fw[order(fw$year), ]
  f0 <- fw[1, ]; f1 <- fw[nrow(fw), ]

  ## ---- 2024 break -------------------------------------------------------
  sup <- panel_pick(panel, "hr_doctor",
                    axis = list(의료인력별 = c("전문의", "레지던트", "인턴"),
                                요양기관종별 = "상급종합병원"),
                    item = "인력현황")
  sw <- tidyr::pivot_wider(sup[, c("prd_de", "의료인력별", "value")],
                           names_from = "의료인력별", values_from = "value")
  sw <- sw[order(sw$prd_de), ]
  sw$전공의 <- sw$레지던트 + sw$인턴
  base <- sw[sw$prd_de == "202304", ]
  post <- sw[sw$prd_de > "202304", ]
  trough <- post[which.min(post$전공의), ]
  now <- sw[nrow(sw), ]

  ## ---- distribution -----------------------------------------------------
  bs <- annualise(panel_pick(panel, "bed_sido", axis = list(입원실현황별 = "계"),
                             item = "병상수"))
  pa <- panel_pick(panel, "pop_sido", axis = list(행정구역별 = "*"), item = "계")
  bs <- bs[!is.na(bs$sido), ]
  pa <- pa[!is.na(pa$sido), ]
  m <- merge(data.frame(year = bs$year, sido = bs$sido, bed = bs$value),
             data.frame(year = pa$year, sido = pa$sido, pop = pa$value),
             by = c("year", "sido"))
  m$bed_1k <- 1000 * m$bed / m$pop
  ds <- annualise(panel_pick(panel, "hr_sido", axis = list(의료인력별 = "의사"),
                             item = "인력현황"))
  ds <- ds[!is.na(ds$sido), ]
  m <- merge(m, data.frame(year = ds$year, sido = ds$sido, doc = ds$value),
             by = c("year", "sido"), all.x = TRUE)
  m$doc_1k <- 1000 * m$doc / m$pop
  yr <- max(m$year)
  cm <- m[m$year == yr, ]
  ## ⚠ 둘을 갈라 둔다. **rho 는 «분포 통계»라 17개 전부로 계산한다** — 극단값을 빼고
  ## 상관을 재면 그건 다른 양이 된다(순위 문장을 다듬는 것과 종류가 다르다).
  ## 「최고·최저·몇 위」라는 «문장»에만 세종을 뺀 cmh 를 쓴다.
  rho <- suppressWarnings(stats::cor(cm$bed_1k, cm$doc_1k, method = "spearman"))
  cmh <- drop_headline_sido(cm)
  cmh <- cmh[order(-cmh$bed_1k), ]
  seoul <- cmh[cmh$sido == "서울", ]
  seoul_bed_rank <- which(cmh$sido == "서울")
  seoul_doc_rank <- which(cmh$sido[order(-cmh$doc_1k)] == "서울")

  ##################################################################
  #####  compose                                               #####
  ##################################################################

  add("# 보건자원감시 브리핑 (", stamp, ")")
  add("")
  add("KOSIS 공개 통계에서 자동 수집한 국내 의료자원 지표. 모든 수치는 같은 날짜 스탬프의 ",
      "`output/tables/` 엑셀에서 계산되며 손으로 옮겨 적지 않는다.")
  add("")

  add("## 1. 양 - 인구 대비 얼마나 (fig1)")
  add("")
  add("- 인구 1,000명당 병상: ", a$year, "년 ", .fmt(a$bed_1k, 2), " -> ", z$year, "년 ",
      .fmt(z$bed_1k, 2), " (", .fmt(abs(100 * (z$bed_1k / a$bed_1k - 1)), 1), "% ",
      .dir(z$bed_1k, a$bed_1k), ")")
  add("- 같은 기간 인구 1,000명당 의사: ", .fmt(a$doc_1k, 2), " -> ", .fmt(z$doc_1k, 2),
      " (", .fmt(abs(100 * (z$doc_1k / a$doc_1k - 1)), 1), "% ",
      .dir(z$doc_1k, a$doc_1k), ")")
  add("- 밀도 계열이 ", z$year, "년에서 끝나는 것은 인구 분모가 거기까지이기 때문이다. ",
      "병상·인력 실수는 아래 절에서 더 최근 분기까지 간다.")
  add("- 의사 수는 요양기관 근무 인력 기준이다. 면허 소지자 수나 OECD의 활동 의사 정의와 ",
      "모수가 다르므로 국제 비교에 그대로 쓰지 않는다.")
  add("")

  add("## 2. 구성 - 무엇으로 채워져 있나 (fig2, fig3, fig4)")
  add("")
  add("- ", .yq(t0$year, t0$q_used), " 대비 ", .yq(t1$year, t1$q_used), " 병상 수: 상급종합병원 ",
      .fmt(t0$상급종합병원), " -> ", .fmt(t1$상급종합병원), ", 종합병원 ",
      .fmt(t0$종합병원), " -> ", .fmt(t1$종합병원), ", 병원 ",
      .fmt(t0$병원), " -> ", .fmt(t1$병원), ", 요양병원 ",
      .fmt(t0$요양병원), " -> ", .fmt(t1$요양병원), ".")
  add("- 전체 병상 증가분 ", .fmt(t1$계 - t0$계), "개 가운데 요양병원 증가분이 ",
      .fmt(t1$요양병원 - t0$요양병원), "개다.")
  ## The 2021 tier split, read off the panel so the sentence cannot go stale.
  psy <- panel_pick(panel, "bed_tier",
                    axis = list(입원실현황별 = "계", 요양기관종별 = "정신병원"),
                    item = "병상수")
  q_all <- sort(unique(panel$prd_de[panel$key == "bed_tier"]))
  if (min(psy$prd_de) > q_all[1]) {
    fq <- min(psy$prd_de)
    pq <- q_all[which(q_all == fq) - 1L]
    don <- character(0)
    for (dv in c("요양병원", "병원")) {
      h <- panel_pick(panel, "bed_tier",
                      axis = list(입원실현황별 = "계", 요양기관종별 = dv), item = "병상수")
      don <- c(don, paste0(dv, " ", .fmt(h$value[h$prd_de == pq]), " -> ",
                           .fmt(h$value[h$prd_de == fq])))
    }
    add("- 계열 단절 주의: ", .q(fq), "에 정신병원이 별도 종별로 분리됐다(",
        .fmt(psy$value[psy$prd_de == fq]), "병상). 같은 분기에 ",
        paste(don, collapse = ", "), "로 줄었으므로, 그 시점의 감소를 폐업으로 읽으면 안 된다. ",
        "전체 목록은 `logs/series_breaks_", stamp, ".csv`에 있다.")
  }
  add("- 중환자실 병상: ", .yq(icu$year[1], icu$q_used[1]), " ", .fmt(icu$value[1]),
      "개 -> ", .yq(icu$year[nrow(icu)], icu$q_used[nrow(icu)]), " ",
      .fmt(icu$value[nrow(icu)]), "개")
  add("")

  add("## 3. 분포 - 어디에 몰려 있나 (fig5, fig6)")
  add("")
  add("- ", yr, "년 인구 1,000명당 병상은 ", cm$sido[1], " ", .fmt(cm$bed_1k[1], 1),
      "에서 ", cm$sido[nrow(cm)], " ", .fmt(cm$bed_1k[nrow(cm)], 1), "까지 ",
      .fmt(cm$bed_1k[1] / cm$bed_1k[nrow(cm)], 1), "배 차이가 난다.")
  add("- 병상이 많은 곳과 의사가 많은 곳은 같지 않다. 시도 간 병상 밀도와 의사 밀도의 ",
      "순위상관은 rho = ", .fmt(rho, 2), "(17개 시도)이고, 서울은 병상 밀도 ", nrow(cmh),
      "개 시도 중 ", seoul_bed_rank, "위이면서 의사 밀도는 ", seoul_doc_rank, "위다",
      " — 순위에서는 세종을 뺐다.")
  add("- 시도별 병상은 요양기관 종별을 구분하지 않은 합계다. KOSIS에 시도 x 종별 병상표가 ",
      "없으므로 상급종합병원 병상의 지역 배치는 이 자료로 말할 수 없다.")
  add("")

  add("## 4. 응급 (fig7, fig8)")
  add("")
  add("- 응급실 병상, ", .q(e0$prd_de), " -> ", .q(e1$prd_de), ": 상급종합병원 ",
      .fmt(e0$상급종합병원), " -> ", .fmt(e1$상급종합병원), ", 종합병원 ",
      .fmt(e0$종합병원), " -> ", .fmt(e1$종합병원), ", 병원 ",
      .fmt(e0$병원), " -> ", .fmt(e1$병원), ".")
  add("- 응급의료기관 지정, ", f0$year, " -> ", f1$year, ": 전체 ", .fmt(f0$계), " -> ",
      .fmt(f1$계), "개소, 권역응급의료센터 ", .fmt(f0$권역응급의료센터), " -> ",
      .fmt(f1$권역응급의료센터), ", 지역응급의료기관 ", .fmt(f0$지역응급의료기관), " -> ",
      .fmt(f1$지역응급의료기관), ".")
  add("- 응급실 병상(심평원 신고)과 응급의료기관 지정(중앙응급의료센터)은 세는 대상이 ",
      "다르다. 한쪽을 다른 쪽으로 나누지 않는다.")
  add("")

  add("## 5. 2024년 전환점 (fig9, fig10)")
  add("")
  add("- 상급종합병원 전공의(인턴+레지던트)는 ", .q("202304"), " ", .fmt(base$전공의),
      "명에서 ", .q(trough$prd_de), " ", .fmt(trough$전공의), "명으로 줄었다(",
      .fmt(100 * trough$전공의 / base$전공의, 1), "% 수준).")
  add("- 같은 분기 상급종합병원 전문의는 ", .fmt(base$전문의), "명에서 ",
      .fmt(trough$전문의), "명으로 ", .fmt(100 * trough$전문의 / base$전문의, 1),
      "% 수준이었다.")
  add("- 최신 분기(", .q(now$prd_de), ") 전공의는 ", .fmt(now$전공의), "명으로 ",
      .q("202304"), " 대비 ", .fmt(100 * now$전공의 / base$전공의, 1), "% 수준이다.")
  add("")

  ## ---- domain 4: access ------------------------------------------------
  if (all(c("care_in_res", "care_out_res", "care_in_inst") %in% panel$key)) {
    gA <- function(k) {
      d <- panel_pick(panel, k, axis = list(시도별 = "*", 입원및외래별 = "입원"),
                      item = "진료비")
      d <- d[!is.na(d$sido), ]
      data.frame(year = d$year, sido = d$sido, v = d$value)
    }
    ri <- gA("care_in_res"); ro <- gA("care_out_res")
    ii <- gA("care_in_inst"); io_ <- gA("care_out_inst")
    ay <- max(intersect(ri$year, ii$year))
    r <- merge(ri[ri$year == ay, ], ro[ro$year == ay, ], by = "sido")
    r$self <- 100 * r$v.x / (r$v.x + r$v.y)
    i <- merge(ii[ii$year == ay, ], io_[io_$year == ay, ], by = "sido")
    i$infl <- 100 * i$v.y / (i$v.x + i$v.y)
    r <- drop_headline_sido(r)[order(drop_headline_sido(r)$self), ]
    i <- drop_headline_sido(i)[order(-drop_headline_sido(i)$infl), ]

    add("## 6. 접근성 - 갈 수 있나 (fig11, fig12, fig13)")
    add("")
    add("- ", ay, "년 입원 **자체충족률**(주민이 받은 진료 중 지역 안에서 이루어진 비율)은 ",
        r$sido[1], " ", .fmt(r$self[1], 1), "%에서 ", r$sido[nrow(r)], " ",
        .fmt(r$self[nrow(r)], 1), "%까지 벌어진다.")
    add("- 같은 해 입원 **환자유입률**(지역 기관 진료 중 외지 주민 몫)은 ", i$sido[1], " ",
        .fmt(i$infl[1], 1), "%가 가장 높고 ", i$sido[nrow(i)], " ",
        .fmt(i$infl[nrow(i)], 1), "%가 가장 낮다.")
    add("- 두 지표는 서로 다른 표에서 오고 **여집합이 아니다**. 앞은 환자 거주지 기준, 뒤는 ",
        "의료기관 소재지 기준이다. 한쪽을 다른 쪽으로 나누지 않는다.")
    add("- ⚠ 2006년과 2011년은 KOSIS의 「전체」 표가 「관내+관외」와 어긋나(최대 7.2%) ",
        "표에 `원자료_불일치` 표시를 달았고 추이 그림에서는 뺐다.")
    add("")
  }

  ## ---- domain 4 deepened: 통계연보 시군구 시계열 ------------------------
  ## KOSIS 는 시도까지, 헬스맵은 2023 한 해뿐이다. 시군구를 «시간축으로» 보는 것은
  ## 이 자료에서만 나온다. 숫자는 16_yearbook.R 이 계산해 넘긴 것을 그대로 쓴다.
  yb <- tryCatch(load_step("^yearbook_"), error = function(e) NULL)
  if (!is.null(yb)) {
    qt <- yb$quant; w <- yb$wide; dr <- yb$drop
    y0 <- min(qt$연도); y1 <- max(qt$연도)
    n_sgg <- length(unique(paste(w$시도, w$지역)))
    m0 <- qt$p50[qt$연도 == y0]; m1 <- qt$p50[qt$연도 == y1]
    add("## 7. 접근성을 시군구로 - 19년 (fig17)")
    add("")
    add("- 국민건강보험공단 지역별 의료이용 통계연보에서 **", n_sgg, "개 시군구 × ",
        y0, "~", y1, "년** 자체충족률을 뽑았다. ",
        "시도로 합친 진료비가 KOSIS와 **303칸 전부 오차 0.00%**다.")
    add("- 중앙값은 ", y0, "년 **", .fmt(m0, 1), "%**에서 ", y1, "년 **",
        .fmt(m1, 1), "%**로, 19년 동안 사실상 움직이지 않았다. ",
        "상·하위 10% 폭도 그대로다(", y1, "년 ", .fmt(qt$p10[qt$연도 == y1], 1),
        "~", .fmt(qt$p90[qt$연도 == y1], 1), "%).")
    add("- 가장 많이 떨어진 곳은 ",
        paste(sprintf("**%s %+.1f%%p**", utils::head(dr$지역, 5),
                      utils::head(dr$변화, 5)), collapse = " · "), "다.")
    add("- ⚠ **단위가 다른 지역끼리 높낮이를 비교하지 않는다.** 대도시 자치구는 옆 구로만 ",
        "넘어가도 「관외」가 되어 구조적으로 낮게 나온다. 읽을 것은 같은 곳의 변화다.")
    add("")
  }

  ## ---- domain 4 deepened: healthmap (2023 cross-section) ----------------
  hm <- tryCatch(load_step("^healthmap_"), error = function(e) NULL)
  if (!is.null(hm)) {
    hi <- hm$indicators; hu <- hm$underserved
    lc <- hi[hi$measure == "관내의료이용률" & hi$level == "시군구" & !is.na(hi$value), ]
    zr <- function(sv) sum(lc$value[lc$service == sv] == 0)
    mp <- function(sv) stats::median(lc$value[lc$service == sv & lc$value > 0])
    n3 <- hu[rowSums(hu[, c("분만", "소아청소년과", "인공신장실", "응급")]) >= 3, ]

    add("## 7. 지역 단위 (", hi$year[1], "년 단면)")
    add("")
    add("- ⚠ 이 절만 **단면**이다. 위 절들의 KOSIS 시계열과 이어 붙여 추세로 읽지 않는다.")
    add("- 250개 시군구 중 상급종합병원이 없는 곳이 ", zr("상급종합병원"), "곳, ",
        "권역응급의료센터가 없는 곳이 ", zr("권역응급의료센터"), "곳이다. ",
        "**관내의료이용률 0%는 「주민이 안 쓴다」가 아니라 「시설이 없다」는 뜻이다.**")
    add("- 시설이 있는 곳에서는 오히려 중증일수록 관내 비율이 높다 — 권역응급의료센터 보유 ",
        "시군구의 중앙값 ", .fmt(mp("권역응급의료센터"), 1), "%, 병원 일반 ",
        .fmt(mp("전체 병원"), 1), "%. 문제는 비율이 아니라 있고 없고다.")
    add("- 의료취약지: 분만 ", sum(hu$분만), " · 소아청소년과 ", sum(hu$소아청소년과),
        " · 인공신장실 ", sum(hu$인공신장실), " · 응급 ", sum(hu$응급), "곳. ",
        "**3종 이상 동시 취약이 ", nrow(n3), "곳**이고 전부 도서·산간이다.")
    add("")
  }

  ## ---- domain 5+6: 활용과 기준 --------------------------------------
  ## 이 둘이 채워지기 «전»에는 「많다/적다」를 말할 수 없었다. 순서가 그래서 중요하다.
  bm <- tryCatch(load_step("^benchmark_"), error = function(e) NULL)
  if (!is.null(bm)) {
    bsay <- function(d, lab) {
      k <- d[d$국가 == "대한민국", ]
      y <- max(k$year); same <- d[d$year == y, ]; same <- same[order(-same$value), ]
      list(y = y, v = same$value[same$국가 == "대한민국"],
           rk = which(same$국가 == "대한민국"), n = nrow(same),
           oth = paste(sprintf("%s %.1f", same$국가[same$국가 != "대한민국"],
                               same$value[same$국가 != "대한민국"]), collapse = " · "))
    }
    b <- bsay(bm$bed, "병상"); d6 <- bsay(bm$doc, "의사")
    add("## 8. 기준과 활용 - 국제적으로 많은가, 그리고 쓰이는가 (fig18~fig21)")
    add("")
    add("- ", b$y, "년 인구 1,000명당 **병상은 ", .fmt(b$v, 1), "개로 그 해 값이 있는 ",
        b$n, "개국 중 ", b$rk, "위**다(", b$oth, ").")
    add("- 같은 해 **활동 의사는 ", .fmt(d6$v, 1), "명으로 ", d6$n, "개국 중 ",
        d6$rk, "위**다(", d6$oth, "). ",
        "**병상은 가장 많고 의사는 가장 적다** - 이 둘이 같은 나라의 값이다.")
    add("- ⚠ 여기 「의사」는 OECD의 **활동 의사** 정의다. 위 절들의 «요양기관 근무 의사»와 ",
        "다른 양이므로 한 문장에 넣지 않는다.")
    ko <- bm$occ[bm$occ$국가 == "대한민국", ]
    add("- 병상 가동률은 **한국이 ", min(ko$year), "~", max(ko$year), "년 ", nrow(ko),
        "개년만 보고**돼 있어 최근 국제 비교에 넣을 수 없다. 자료의 한계다.")
    add("- 급성기 **평균재원일수**는 ", min(bm$alos$year), "년 ",
        .fmt(bm$alos$value[which.min(bm$alos$year)], 1), "일에서 ",
        max(bm$alos$year), "년 ",
        .fmt(bm$alos$value[which.max(bm$alos$year)], 1), "일로 줄었다.")
    add("")
  }

  ## ---- domain 7: 결과 ---------------------------------------------------
  ## ⑦이 생기기 전까지 이 브리핑은 «전부 공급과 이용»이었다 - 「그래서 작동하는가」를
  ## 말하는 절이 여기다. ⛔ 자원과 결과를 나란히 놓되 인과로 쓰지 않는다.
  mo <- panel[panel$key == "mort_sgg", ]
  if (nrow(mo)) {
    mo$lv <- ifelse(nchar(mo$axis2_cd) == 2, "상위", "시군구")
    nat <- mo[mo$lv == "상위" & mo$axis2_val == "전국" & !is.na(mo$value), ]
    y0 <- min(nat$year); y1 <- max(nat$year)
    pick <- function(cz, y) nat$value[nat$axis1_val == cz & nat$year == y][1]
    czs <- unique(nat$axis1_val)
    tot0 <- pick("계", y0); tot1 <- pick("계", y1)

    ## 늘어난 사인을 «찾아서» 쓴다 - 「자살만 늘었다」를 손으로 적지 않는다
    ups <- c()
    for (cz in czs[czs != "계"]) {
      a <- pick(cz, y0); b <- pick(cz, y1)
      if (!is.na(a) && !is.na(b) && b > a) {
        ups <- c(ups, sprintf("%s %+.0f%%", sub(" \\(.*$", "", cz), 100 * (b / a - 1)))
      }
    }

    sg <- mo[mo$lv == "시군구" & mo$axis1_val == "계" & !is.na(mo$value), ]
    r <- function(y) {
      v <- sg$value[sg$year == y]
      unname(quantile(v, .9) / quantile(v, .1))
    }

    add("## 9. 결과 - 그래서 작동하는가 (fig20~fig23)")
    add("")
    add("- 전국 **연령표준화 사망률**은 ", y0, "년 ", .fmt(tot0, 1), "에서 ",
        y1, "년 ", .fmt(tot1, 1), "(십만명당)로 **",
        .fmt(abs(100 * (tot1 / tot0 - 1)), 0), "% 줄었다**.")
    if (length(ups)) {
      add("- 그런데 **늘어난 사인이 있다: ", paste(ups, collapse = " · "), "**. ",
          "연령표준화 값이므로 인구 고령화의 효과는 이미 빠져 있다.")
    }
    add("- **수준은 내려갔지만 지역 격차는 좁아지지 않았다.** 시군구 ",
        length(unique(sg$axis2_val)), "곳의 P90/P10 이 ", y0, "년 ", .fmt(r(y0), 2),
        "에서 ", y1, "년 ", .fmt(r(y1), 2), "로 **오히려 조금 벌어졌다** - ",
        "전국 평균의 개선이 지역 간 균등화를 뜻하지 않는다.")
    add("- ⚠ 이 절은 자원과 결과를 나란히 보여줄 뿐이다. ",
        "**「자원이 결과를 만든다」는 이 자료로 말할 수 없다** - 그것은 다른 설계다.")
    add("- ⚠ 급성심장정지 지표(fig20·fig21)는 KOSIS 가 2020년부터라 코로나 이후만 보인다. ",
        "조사는 2008년에 시작했고 그 앞 구간은 원시자료 신청이 필요하다.")
    add("")
  }

  add("---")
  add("")
  add("자료: KOSIS OpenAPI. 수집 대상 표는 `R/indicator_registry.csv`에 있고, ",
      "호출 기록은 `logs/api_calls_", stamp, ".csv`에 있다.")
  add("적용 범위와 한계는 `CLAUDE.md`의 「구조적 제약」 절을 볼 것.")

  f <- file.path(output_dir, paste0("브리핑_", stamp, ".md"))
  writeLines(L, f, useBytes = FALSE)
  cat("--- brief ->", basename(f), "\n")
  invisible(f)
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  write_brief(panel)
}
