##################################################################
#####  99_acceptance.R - expected values, hard-coded on purpose #####
##################################################################
##
## Every number below was read off a live KOSIS response on 2026-08-31, before
## any of this code existed. They are written down so the pipeline has something
## external to be wrong against: if a refactor, a KOSIS re-code or a silently
## mis-slotted axis changes what we read, this exits 1 rather than shipping.
##
## T4 is the only check whose reference comes from OUTSIDE this pipeline - the
## lab's manually downloaded HIRA exports in 2nd/doc_kor_sig. Two different
## routes to the same quantity is the strongest evidence available here.
##
## The last block tests THE GATES THEMSELVES. A gate that has never fired is an
## untested gate, so one case that must pass and one that must fail are fed
## through each.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

.fails <- new.env(parent = emptyenv()); .fails$n <- 0L; .fails$msgs <- character(0)

expect <- function(label, got, want, tol = 0) {
  ok <- !is.na(got) && abs(got - want) <= tol
  cat(sprintf("%s %-52s got %-12s want %s\n", if (ok) "  ok  " else " FAIL ",
              label, format(got, big.mark = ","), format(want, big.mark = ",")))
  if (!ok) {
    .fails$n <- .fails$n + 1L
    .fails$msgs <- c(.fails$msgs, label)
  }
  invisible(ok)
}

expect_error <- function(label, expr) {
  got <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  ok <- !is.null(got)
  cat(sprintf("%s %-52s %s\n", if (ok) "  ok  " else " FAIL ", label,
              if (ok) "raised as required" else "DID NOT RAISE"))
  if (!ok) { .fails$n <- .fails$n + 1L; .fails$msgs <- c(.fails$msgs, label) }
  invisible(ok)
}

val <- function(panel, key, axis, item = NULL, prd) {
  d <- panel_pick(panel, key, axis = axis, item = item)
  v <- d$value[d$prd_de == prd]
  if (length(v) != 1) return(NA_real_)
  v
}

run_acceptance <- function(panel) {
  cat("=== 99_acceptance.R ===\n\n")

  ## ── T0. 표면 게이트 ─────────────────────────────────────────────────────
  ## ⭐ 게이트가 보는 범위 = 독자가 보는 범위여야 한다. 2026-09-02 에 형제 프로젝트
  ##    (자체충족률논문)에서 두 번 겪었다 — 원고에서 지운 표현이 «그림 범례»에 살아
  ##    있었고, 표의 «열 제목»이 다른 양을 가리키는 것을 어떤 게이트도 안 봤다.
  ##    여기 표면은 브리핑(.md) · 페이지(.html) · 표(시트명·열 제목)다.
  ## ⚠ 규칙은 «이 프로젝트가 사실로 틀리게 말할 수 있는 것»만. 문체는 넣지 않는다.
  surface_bad <- gate_surfaces(list(
    list("OECD 평균",   "「OECD」 행은 값이 하나도 안 온다 - 5개국 평균을 그렇게 부르지 말 것(제약 12)"),
    list("2009년 이후 월별", "2009Q1 부터는 분기가 최소다(제약 2)"),
    list("시도별 종별 병상", "KOSIS 에 그 표는 없다 - 심평원 기관별 API 우회뿐이다(제약 1)"),
    list("세종 제외 없이",  "헤드라인에서 세종을 뺀다는 표기가 빠지면 안 된다")
  ),
    md_globs = file.path(output_dir, c("브리핑_*.md", "*.html")),
    xlsx_dir = tables_dir,
    figures_dir_ = figures_dir)


  cat("--- T1  DT_HIRA45_1 병상수 (요양기관 종별 입원실 현황)\n")
  expect("상급종합병원 2025Q1",
         val(panel, "bed_tier", list(입원실현황별 = "계", 요양기관종별 = "상급종합병원"),
             "병상수", "202501"), 46098)
  expect("상급종합병원 2026Q2",
         val(panel, "bed_tier", list(입원실현황별 = "계", 요양기관종별 = "상급종합병원"),
             "병상수", "202602"), 46269)
  expect("종합병원 2025Q1",
         val(panel, "bed_tier", list(입원실현황별 = "계", 요양기관종별 = "종합병원"),
             "병상수", "202501"), 109737)

  cat("\n--- T2  DT_HIRA49_2 응급실 병상수 (특수진료실 현황)\n")
  er <- function(tier, prd) val(panel, "er_bed",
                                list(특수진료실구분별 = "응급실", 요양기관종별 = tier),
                                "병상수", prd)
  expect("상급종합병원 2013Q2", er("상급종합병원", "201302"), 1542)
  expect("상급종합병원 2026Q2", er("상급종합병원", "202602"), 1833)
  expect("종합병원 2013Q2",     er("종합병원", "201302"),     4625)
  expect("종합병원 2026Q2",     er("종합병원", "202602"),     5715)
  expect("병원 2013Q2",         er("병원", "201302"),         3118)
  expect("병원 2026Q2",         er("병원", "202602"),         1358)

  cat("\n--- T3  DT_41104_411 응급의료기관 지정 (전국)\n")
  ef <- function(type, yr) val(panel, "er_facility",
                               list(응급의료기관유형별 = type, 지역별 = "전체"), NULL, yr)
  expect("권역응급의료센터 2014", ef("권역응급의료센터", "2014"), 20)
  expect("권역응급의료센터 2024", ef("권역응급의료센터", "2024"), 44)
  expect("계 2014",               ef("계", "2014"),               550)
  expect("계 2024",               ef("계", "2024"),               528)

  cat("\n--- T4  DT_MIRE01 상급종합병원 개소수  [외부 대조: doc_kor_sig 수동 엑셀]\n")
  ins <- function(region, prd) val(panel, "inst_sido",
                                   list(요양기관종별 = "상급종합병원", 시도별 = region),
                                   NULL, prd)
  expect("서울 2018Q4", ins("서울특별시", "201804"), 13)
  expect("서울 2020Q4", ins("서울특별시", "202004"), 13)
  expect("서울 2021Q4", ins("서울특별시", "202104"), 14)
  expect("서울 2024Q4", ins("서울특별시", "202404"), 14)
  expect("전국 2018Q4", ins("계", "201804"), 42)
  expect("전국 2023Q4", ins("계", "202304"), 45)   # doc_kor_sig: "2023년 45개소"
  expect("전국 2024Q4", ins("계", "202404"), 47)   # 5기 지정, 그 파일에는 없는 연장분

  cat("\n--- T5  분모: KOSIS 주민등록인구 vs dataonly/kor_pop 연앙인구\n")
  ## Different concepts (year-end registration vs mid-year average), so this is
  ## a tolerance check, not an identity. run inside the gate.
  ok <- tryCatch({ gate_denominator(panel); TRUE }, error = function(e) {
    cat(" FAIL  gate_denominator: ", conditionMessage(e), "\n"); FALSE })
  if (!ok) { .fails$n <- .fails$n + 1L; .fails$msgs <- c(.fails$msgs, "T5 denominator") }

  cat("\n--- T6  org 350 관내/관외 - 「관내」가 누구 기준인가를 고정한다\n")
  ## Raw values, not ratios: these four pin down that the TX series is keyed by
  ## PATIENT RESIDENCE and the DT series by INSTITUTION LOCATION. If a future
  ## KOSIS revision swapped them, every 자체충족률 in this project would invert
  ## and nothing else would notice.
  acc <- function(k, lev = "계") val(panel, k,
                                     list(시도별 = "서울특별시", 입원및외래별 = lev),
                                     "진료비", "2024")
  expect("관내 서울 2024 (거주지 기준)",  acc("care_in_res"),  19903023387)
  expect("관외 서울 2024 (거주지 기준)",  acc("care_out_res"), 2347685721)
  expect("관외 서울 2024 (기관 기준)",    acc("care_out_inst"), 10805486048)
  expect("전체 서울 2024 = 관내 + 관외",
         val(panel, "care_total_res", list(시도별 = "서울특별시", 급여형태별 = "합계"),
             "진료비", "2024"), 19903023387 + 2347685721)

  cat("\n--- T7  헬스맵 (2023 단면)\n")
  ## Pinned the same way the KOSIS layer is. The 12_healthmap gates prove the
  ## file is internally consistent; these prove it is still THE SAME FILE.
  hm <- tryCatch(load_step("^healthmap_"), error = function(e) NULL)
  if (is.null(hm)) {
    cat(" FAIL  healthmap step missing\n")
    .fails$n <- .fails$n + 1L; .fails$msgs <- c(.fails$msgs, "T7 healthmap")
  } else {
    hi <- hm$indicators; hu <- hm$underserved
    lc <- hi[hi$measure == "관내의료이용률" & hi$level == "시군구" & !is.na(hi$value), ]
    expect("상급종합 없는 시군구", sum(lc$value[lc$service == "상급종합병원"] == 0), 211)
    expect("권역응급 없는 시군구", sum(lc$value[lc$service == "권역응급의료센터"] == 0), 210)
    expect("취약지 응급",   sum(hu$응급), 102)
    expect("취약지 분만",   sum(hu$분만), 29)
    expect("3종 이상 취약", sum(rowSums(hu[, c("분만","소아청소년과","인공신장실","응급")]) >= 3), 15)
    expect("시군구 행 수",  length(unique(hi$code[hi$level == "시군구"])), 250)
  }

  cat("\n--- T10 시군구 연령표준화 사망률 (mort_sgg)\n")
  ## ⭐ 이 계열은 «비율»이라 region-sum 게이트가 손대지 못한다(17시도 합 4,967 vs
  ##    전국 300). 자동 게이트가 할 말이 없는 지표일수록 사람이 값을 박아 둔다.
  ## ⛔ 지키는 것은 숫자만이 아니다 - 축 셋짜리 첫 표의 조립과, `NOT:` 문법이
  ##    «폐지된 행정구역 10곳만» 뺐는지(더 빼거나 덜 빼면 250이 아니다)까지다.
  mo <- panel[panel$key == "mort_sgg", ]
  nat <- function(cz, yr) {
    v <- mo$value[mo$axis2_val == "전국" & mo$axis1_val == cz & mo$year == yr]
    if (length(v) == 1L) v else NA_real_
  }
  expect("전국 전체사인 2006 (십만명당)", nat("계", 2006), 480.3)
  expect("전국 전체사인 2024",            nat("계", 2024), 294.6)
  expect("전국 암 2024 (위약 사인)",      nat("악성신생물(암) (C00-C97)", 2024), 79.7)
  expect("전국 뇌혈관 2006",              nat("뇌혈관 질환 (I60-I69)", 2006), 59.0)
  expect("사인 6종",                      length(unique(mo$axis1_val)), 6L)
  expect("단위 종류 1가지(비율 계열)",   length(unique(mo$unit)), 1L)
  ## ⛔ 시도/시군구는 «이름 목록»으로 가르지 않는다 - 이 표는 제주를 `제주도`라
  ##    쓰는데(제주특별자치도가 아니다) 목록으로 가르다 한 번 어긋났다.
  ##    패널이 싣고 있는 `axis2_cd` 로 가른다: 시도 2자리 · 시군구 5자리 (제약 14).
  ## ⚠ 그리고 개수는 «코드»로 센다 - 「동구」는 다섯 시도에 있어 이름은 식별자가 아니다.
  cd <- unique(mo[, c("axis2_val", "axis2_cd")])
  expect("2자리 코드 = 전국 + 17시도", sum(nchar(cd$axis2_cd) == 2), 18L)
  expect("5자리 코드 = 시군구 (NOT: 로 폐지구역 10곳 제외 후)",
         sum(nchar(cd$axis2_cd) == 5), 357L)
  expect("이름은 코드보다 적다 (동구 등 중복)", length(unique(mo$axis2_val)), 265L)

  cat("\n--- T8  브리핑과 HTML이 같은 숫자를 말하는가\n")
  ## The two deliverables compute independently, and on 2026-08-31 they had
  ## already drifted - the brief carried no 헬스맵 section at all while the page
  ## did. Requiring a handful of headline figures to appear in BOTH is the
  ## cheapest thing that makes that impossible to ship again.
  bf <- tryCatch(paste(readLines(latest_file(output_dir, "^브리핑_.*\\.md$"),
                                 warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                 error = function(e) "")
  pg <- tryCatch(paste(readLines(latest_file(output_dir, "^보건자원감시_.*\\.html$"),
                                 warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                 error = function(e) "")
  ## ⚠ "32.4" 는 세종의 자체충족률이었다. 2026-09-01에 「최저·최고」 문장에서 세종을
  ## 빼면서 헤드라인이 충남 55.3% 로 바뀌었다 — 게이트가 그걸 «잡았다»(브리핑에서는
  ## 사라졌는데 페이지에는 남아 있었다). 여기 숫자는 산출물이 바뀌면 같이 바뀐다.
  ## ⛔ 2026-09-04: ⑦결과가 «페이지에는 있고 브리핑에는 없는» 상태로 하루를 보냈다.
  ##    T8 은 못박은 숫자 목록만 보므로 **절이 통째로 빠진 것은 못 잡는다.**
  ##    → 아래 「영역 표제」 검사를 따로 둔다. 숫자보다 거친 검사지만 그 구멍을 막는다.
  for (dom in c("결과")) {
    okb <- grepl(dom, bf, fixed = TRUE); okp <- grepl(dom, pg, fixed = TRUE)
    cat(sprintf("%s %-52s %s\n", if (okb && okp) "  ok  " else " FAIL ",
                paste0("⑦ 영역 「", dom, "」이 브리핑과 페이지 양쪽에"),
                if (okb && okp) "있음" else paste0("브리핑 ", okb, " / 페이지 ", okp)))
    if (!(okb && okp)) {
      .fails$n <- .fails$n + 1L
      .fails$msgs <- c(.fails$msgs, paste0("T8 영역 ", dom))
    }
  }

  shared <- c("13.82", "114,825", "1,833", "609", "55.3", "211", "102",
              "40.6", "42.3")   # 통계연보 시군구 자체충족률 중앙값 2006 -> 2024
  for (v in shared) {
    ok <- grepl(v, bf, fixed = TRUE) && grepl(v, pg, fixed = TRUE)
    cat(sprintf("%s %-52s %s\n", if (ok) "  ok  " else " FAIL ",
                paste0("\"", v, "\" 가 브리핑과 페이지 양쪽에"),
                if (ok) "있음" else paste0("브리핑 ", grepl(v, bf, fixed = TRUE),
                                           " / 페이지 ", grepl(v, pg, fixed = TRUE))))
    if (!ok) { .fails$n <- .fails$n + 1L; .fails$msgs <- c(.fails$msgs, paste0("T8 ", v)) }
  }

  ##################################################################
  #####  T9  세종이 「최저」로 되돌아오지 않았는가              #####
  ##################################################################
  ## 세종은 2012년 신설이라 시도 순위에서 거의 언제나 극단값이 되고, 그 극단은
  ## 자원의 많고 적음이 아니라 행정구역의 특수함을 뜻한다. 그래서 «문장»에서만 뺐는데,
  ## 순위 코드를 손대면 조용히 되돌아온다. 되돌아왔는지를 산출물에서 직접 본다.
  cat("\n--- T9  세종이 「최저」 문장으로 되돌아오지 않았는가\n")
  for (pat in c("자체충족률은 세종", "최저 세종", "세종 [0-9.]+%</b>에서")) {
    hit <- grepl(pat, bf) || grepl(pat, pg)
    cat(sprintf("%s %-52s %s\n", if (!hit) "  ok  " else " FAIL ",
                paste0("\"", pat, "\" 가 헤드라인에"), if (!hit) "없음" else "있음"))
    if (hit) { .fails$n <- .fails$n + 1L; .fails$msgs <- c(.fails$msgs, paste0("T9 ", pat)) }
  }
  ## 그러나 «완전히 사라지면» 그것대로 틀렸다 — 뺐다는 사실과 그 값은 남아 있어야 한다.
  keep <- grepl("세종", pg, fixed = TRUE)
  cat(sprintf("%s %-52s %s\n", if (keep) "  ok  " else " FAIL ",
              "페이지가 «세종을 뺐다는 사실»을 밝히는가", if (keep) "밝힘" else "숨김"))
  if (!keep) { .fails$n <- .fails$n + 1L; .fails$msgs <- c(.fails$msgs, "T9 세종 미표기") }

  ##################################################################
  #####  gate self-tests: one that must pass, one that must fail #####
  ##################################################################

  cat("\n--- G  gates fire when they should\n")

  reg <- load_registry()
  good <- as.list(kosis_rows(reg)[kosis_rows(reg)$key == "bed_tier", ])
  bad_item <- good; bad_item$item_names <- "있을리없는항목"
  bad_axis <- good; bad_axis$axis_spec <- "없는축=계;요양기관종별=TOTAL"
  bad_name <- good
  bad_name$axis_spec <- "입원실현황별=계;요양기관종별=TOTAL|없는기관종별"

  cat(sprintf("  ok   %-52s %s\n", "resolve_row accepts the real registry row",
              if (!is.null(resolve_row(good))) "resolved" else "?"))
  expect_error("resolve_row rejects an unknown item name",  resolve_row(bad_item))
  expect_error("resolve_row rejects an unknown axis name",  resolve_row(bad_axis))
  expect_error("resolve_row rejects an unknown axis value", resolve_row(bad_name))

  ## Region-sum gate: perturb one province by one unit and require a stop.
  expect_error("gate_region_sums catches a corrupted province", {
    p2 <- panel
    i <- which(p2$key == "inst_sido" & p2$axis2_val == "경기도" &
               p2$axis1_val == "상급종합병원")[1]
    p2$value[i] <- p2$value[i] + 1
    gate_region_sums(p2)
  })
  ## Continuity gate: drop a quarter and require a stop.
  expect_error("gate_period_continuity catches a missing quarter", {
    p3 <- panel[!(panel$key == "er_bed" & panel$prd_de == "201902"), ]
    gate_period_continuity(p3)
  })
  ## Access identity: a mismatch in a year that is NOT a known exception must stop.
  expect_error("gate_access_identity catches a new mismatch year", {
    p5 <- panel
    i <- which(p5$key == "care_in_res" & p5$year == 2020 &
               p5$axis1_val == "서울특별시" & p5$axis2_val == "계" &
               p5$itm_nm == "진료비")[1]
    p5$value[i] <- p5$value[i] * 1.05
    gate_access_identity(p5)
  })
  ## Dead-category gate: blank a whole requested category and require a stop.
  expect_error("gate_no_dead_category catches an empty category", {
    p4 <- panel
    p4$value[p4$key == "er_bed" & p4$axis2_val == "종합병원"] <- NA_real_
    gate_no_dead_category(p4)
  })

  cat("\n=== ", if (.fails$n == 0L) "ALL CHECKS PASSED" else
      paste0(.fails$n, " CHECK(S) FAILED: ", paste(.fails$msgs, collapse = "; ")), " ===\n")
  .fails$n
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  n <- run_acceptance(panel)
  if (n > 0L) quit(status = 1L)
}
