##################################################################
#####  10_supplement.R - 보충자료 한 벌로 조립                #####
##################################################################
##
## 왜 별도 스크립트인가. 원고 Limitations 가 「제외된 34곳은 supplement 에 있다」·
## 「면제한 province-year 는 supplement 에 기록했다」라고 «가리키는데» 그 supplement 가
## 없었다(2026-09-02 manuscript-audit 2-5). 가리키는 곳이 비어 있으면 심사에서 잡힌다.
##
## ⛔ 이 스크립트는 «새로 계산하지 않는다». 앞 단계가 저장한 step 을 골라 담기만 한다 —
##    그래야 supplement 의 숫자가 본문과 갈라질 수 없다.
##
## ⚠ 본문 표/그림과 supplement 의 «배분»은 투고 시점 결정이다. 지금은 만들어만 두고,
##    제안은 manuscript/README.md 에 적어 둔다. 여기서 본문 번호를 건드리지 않는다
##    (수리 중에 범위를 넓히면 결함이 는다 — [[scope-creep-during-repair]]).

## ⛔ 2026-09-02 감사: 시트명·열 제목이 «전부 한국어»였다. 영문 저널의 Additional file 이
##    될 수 없다. 그리고 ggplot 보조열 `type_en` 이 S3 로 새어 나갔다(05 가 save_step 전에 붙인다).
## ⚠ 매핑에 없는 이름은 «그대로 나간다» — 빠진 것이 보이게 하려는 것이다.
## ⚠ 엑셀 시트명 상한은 31자다. 넘으면 writexl 이 «조용히 잘라» 「S4 Sensitivity,
##    population we」 처럼 문장이 끊긴 채 나가고, 그 이름이 Additional file 의 표 제목이
##    된다. 짧은 이름을 여기 두고, 사람이 읽는 «온전한 제목»은 96_supplement.md 가 갖는다.
SUPP_SHEET_EN <- c(
  ## ⛔ 2026-09-07: S1·S2 를 맞바꿨다. 보충 표 번호는 «본문 첫 인용 순서»여야 하는데
  ##    Methods 의 절 순서가 Data source → Study population 이라 KOSIS 검증이 먼저 인용된다.
  "S1_자료검증"        = "S1 KOSIS validation",
  "S2_제외시군구"      = "S2 Excluded districts",
  "S3_불평등_연도별"   = "S3 Inequality by year",
  "S4_민감도_인구가중" = "S4 Population weighting",
  "S5_민감도_로짓"     = "S5 Logit variance",
  "S6_민감도_불균형"   = "S6 Unbalanced panel",
  "S7_민감도_절사"     = "S7 Population trimming",
  "S8_시군구변화_분포" = "S8 Change distribution")

SUPP_COL_EN <- c(
  "시군구" = "District", "시도" = "Province", "지역" = "District", "sgg" = "District",
  "관측연수" = "Years observed", "첫해" = "First year", "끝해" = "Last year",
  "사유" = "Reason", "항목" = "Item", "내용" = "Detail",
  "유형" = "Type", "연도" = "Year", "n" = "n", "중앙" = "Median",
  "q25" = "Q1", "q75" = "Q3", "격차" = "Gap", "중앙_구" = "Median, gu",
  "중앙_군" = "Median, gun", "구간" = "Period", "시작" = "Start", "끝" = "End",
  "연평균 %p" = "Annual change (pp)", "지니" = "Gini", "타일" = "Theil index",
  "집단내" = "Within-group", "집단간" = "Between-group",
  "집단간_몫" = "Between-group share (%)", "p90p10" = "P90/P10",
  "총" = "Total Theil index", "행정유형_집단간몫" = "Between-group share, admin type (%)",
  "재정5분위_집단간몫" = "Between-group share, fiscal quintile (%)",
  "재정5분위" = "Fiscal quintile", "절사" = "Trimming",
  "배수_관내" = "In-area growth multiple", "배수_관외" = "Out-of-area growth multiple",
  "상대성장" = "Relative growth", "변화_자체충족" = "Change in index (pp)",
  "자체충족_시작" = "Index, first year", "자체충족_끝" = "Index, last year",
  "총진료량" = "Total expenditure", "자체충족" = "Localization index",
  "자체충족률" = "Localization index", "관내" = "In-area", "관외" = "Out-of-area",
  "비가중_총" = "Theil, unweighted", "비가중_집단간몫" = "Between-group share, unweighted (%)",
  "가중_총" = "Theil, population weighted",
  "가중_집단간몫" = "Between-group share, weighted (%)",
  "분산" = "Variance (logit)", "지표" = "Statistic",
  "비가중" = "Unweighted", "인구가중" = "Population weighted",
  "기준_관내" = "In-area, first year", "기준_관외" = "Out-of-area, first year",
  "기준_총진료량" = "Total expenditure, first year",
  "기준_자체충족" = "Localization index, first year",
  "인구" = "Population", "가중치" = "Weight",
  "지수_관내" = "In-area index (first year = 100)",
  "지수_관외" = "Out-of-area index (first year = 100)",
  "판정" = "Category", "원인" = "Source of change")

## ⛔ 그림용 보조열은 보충자료에 나가지 않는다
SUPP_DROP <- c("type_en", "type_ab", "키", "sgg")

en_sheet <- function(x) { i <- x %in% names(SUPP_SHEET_EN); x[i] <- SUPP_SHEET_EN[x[i]]; x }
en_cols <- function(d) {
  d <- d[, !(names(d) %in% SUPP_DROP), drop = FALSE]
  i <- names(d) %in% names(SUPP_COL_EN); names(d)[i] <- SUPP_COL_EN[names(d)[i]]
  d
}

supplement <- function() {
  cat("=== 10_supplement.R ===\n")

  co <- paper_step("^cohort_",        "01 코호트")
  dc <- paper_step("^decomp_",        "04 분해")
  tj <- paper_step("^trajectory_",    "05 궤적")
  iq <- paper_step("^inequality_",    "06 불평등")
  se <- paper_step("^sensitivity_",   "07 민감도")
  so <- paper_step("^socioeconomic_", "08 사회경제축")

  ## ── S1. 균형패널에서 «빠진» 시군구 — 원고가 이름으로 가리키는 그 명단
  excl <- co$dropped
  if (is.null(excl)) {
    ## 01 이 step 에 안 담았으면 패널에서 직접 만든다(같은 규칙: 19년 전부 있지 않은 곳)
    a <- co$all
    n <- stats::aggregate(list(연수 = a$연도), by = list(시도 = a$시도, 지역 = a$지역),
                          FUN = function(v) length(unique(v)))
    excl <- n[n$연수 < max(n$연수), ]
    excl <- excl[order(excl$시도, excl$지역), ]
  }
  ## ⛔ 2026-09-07: 열 «이름»만 영문화하고 «값»은 한글 그대로 나가고 있었다.
  ##    영문 저널의 보충자료에 34개 지역명이 한글로 실린다. 로마자 표기는 규칙이 아니라
  ##    사전이므로 CSV 로 두고, 없는 이름이 나오면 «조용히 넘기지 않고» 멈춘다.
  rom <- utils::read.csv(file.path(PAPER_ROOT, "R", "romanization_sigungu.csv"),
                         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  rom_key <- paste(rom$sido, rom$jiyeok)
  rom_en  <- paste(rom$sido_en, rom$jiyeok_en)
  key <- as.character(excl$시군구)
  hit <- match(key, rom_key)
  if (anyNA(hit))
    stop(sprintf("S1: 로마자 표기가 없는 시군구 %d곳 — R/romanization_sigungu.csv 에 추가할 것: %s",
                 sum(is.na(hit)), paste(key[is.na(hit)], collapse = ", ")))
  excl$시군구 <- rom_en[hit]
  excl$사유 <- "One or more of the 19 years missing (boundary reorganisation, creation or merger)"
  cat(sprintf("--- S1 제외 시군구 %d곳 (로마자 %d/%d)\n",
              nrow(excl), sum(!is.na(hit)), nrow(excl)))

  ## ── S2. 자료 검증과 «면제한 한 칸» — Limitations 가 가리키는 두 번째 자리
  prov <- data.frame(
    항목 = c("Comparison", "Result", "Exempted cell", "Reason for exemption",
             "What was not done"),
    내용 = c(
      paste("District values aggregated to the province level and compared against the",
            "corresponding Korean Statistical Information Service series for total",
            "expenditure by province of residence."),
      "Agreement in 303 of 304 province-years, median error 0.00%.",
      "Gyeonggi Province, 2009.",
      paste("A defect of the source. The total row for Goyang-si Ilsan-gu is short in the",
            "2009 edition, although the three benefit-type components of the same row sum",
            "correctly. The published total table is incomplete; the parser is not at fault."),
      paste("The tolerance was not widened to let the cell pass. This single cell was",
            "exempted explicitly, and the pipeline halts if any other province-year",
            "disagrees.")),
    stringsAsFactors = FALSE)

  ## ── S8. 시군구별 분해를 «요약»한다 — 본문이 주장하는 것은 분포와 개수다
  ##
  ## ⛔ 2026-09-03: 옛 S14 는 230개 시군구 × 21열(4,851칸)이라 보충자료의 72% 를 먹고
  ##    렌더하면 50쪽 중 40쪽이 됐다. 그것은 «표»가 아니라 자료 부록이다.
  ##    → 원본은 별도 파일(D1)로 내보내고, 여기에는 **본문이 실제로 인용하는 셋**만 남긴다:
  ##    변화량 분포 · 상대성장 중앙값 · 범주별 개수(「국소 축소」가 0인지).
  ## ⛔⛔ 2026-09-07: 이 함수가 영문 열 이름(`Change in index (pp)` 등)으로 조회했는데
  ##    `dc$last`(=04_decomposition.R 의 a1)의 열 이름은 «한국어»다. 전부 NULL 이 나와
  ##    시트가 `Districts 0 · median 공란 · min Inf` 로 나갔고, 그 상태로 본문이
  ##    [Table S8] 을 네 번 인용하고 있었다. R 은 경고만 냈다("no non-missing arguments
  ##    to min") — 로그에 있었지만 아무도 안 봤다.
  ## ⭐ 그래서 이름을 «가정하지 않고 확인»한다: 없으면 stop() 이다.
  DC_COLS <- c(change = "변화_자체충족",   # 변화_자체충족
               rel    = "상대성장",                # 상대성장
               inm    = "배수_관내",               # 배수_관내
               outm   = "배수_관외",               # 배수_관외
               cause  = "원인")                            # 원인

  ## ⭐ 범주는 «고정 수준»으로 센다. table() 만 쓰면 0인 범주가 «행 자체로 사라져»
  ##    「국소 축소 0곳」이라는 본문 주장을 표가 침묵으로 답한다.
  CAUSE_EN <- c("(A) Within-district expenditure fell",
                "(B) Within-district grew but outside grew faster",
                "(C) Within-district grew faster than outside")

  dist_summary <- function(d) {
    if (is.null(d)) return(NULL)
    miss <- DC_COLS[!(DC_COLS %in% names(d))]
    if (length(miss))
      stop(sprintf("S8: 분해 결과에 없는 열 %s (있는 열: %s)",
                   paste(miss, collapse = ", "), paste(names(d), collapse = ", ")))

    ch  <- as.numeric(d[[DC_COLS[["change"]]]])
    rel <- as.numeric(d[[DC_COLS[["rel"]]]])
    inm <- as.numeric(d[[DC_COLS[["inm"]]]])
    om  <- as.numeric(d[[DC_COLS[["outm"]]]])
    cau <- as.character(d[[DC_COLS[["cause"]]]])
    if (!length(ch) || all(is.na(ch))) stop("S8: 변화량이 비어 있다")

    q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE))
    stat <- data.frame(
      Statistic = c("Districts",
                    "Change in index (pp): median",
                    "Change in index (pp): Q1", "Change in index (pp): Q3",
                    "Change in index (pp): min", "Change in index (pp): max",
                    "Districts with an increase (change >= 0)",
                    "Districts with a decline (change < 0)",
                    "Within-district expenditure growth: median (fold)",
                    "Outside-district expenditure growth: median (fold)",
                    "Relative growth (within / outside): median"),
      Value = c(sum(!is.na(ch)),
                round(q(ch, .5), 2), round(q(ch, .25), 2), round(q(ch, .75), 2),
                round(min(ch, na.rm = TRUE), 2), round(max(ch, na.rm = TRUE), 2),
                sum(ch >= 0, na.rm = TRUE), sum(ch < 0, na.rm = TRUE),
                round(q(inm, .5), 2), round(q(om, .5), 2), round(q(rel, .5), 3)),
      stringsAsFactors = FALSE)

    ## 원인 라벨 → 고정 3범주. ⛔ 나머지를 통째로 (C)로 쓸어담지 않는다 —
    ##    04_decomposition.R 이 라벨을 고치면 그 순간 조용히 오분류된다.
    THIRD <- "관내가 관외보다 더 늘었다"
    bad <- unique(cau[!(startsWith(cau, "(A)") | startsWith(cau, "(B)") | cau == THIRD)])
    if (length(bad)) stop(sprintf("S8: 모르는 원인 라벨 %s", paste(bad, collapse = " | ")))
    key <- ifelse(startsWith(cau, "(A)"), CAUSE_EN[1],
           ifelse(startsWith(cau, "(B)"), CAUSE_EN[2], CAUSE_EN[3]))
    cnt <- table(factor(key, levels = CAUSE_EN))
    stat <- rbind(stat, data.frame(
      Statistic = paste0("Districts, ", names(cnt)),
      Value = as.integer(cnt), stringsAsFactors = FALSE))
    stat
  }

  ## ⭐ 원본 per-district 표는 «논문 자료»가 아니라 자료 부록으로 따로 나간다.
  ##    지우는 것이 아니다 - 분석은 남기고 원고에서만 뺀다(2026-09-03 연구자 지시).
  if (!is.null(dc$last)) {
    write_table(list(`시군구별_항등식분해` = dc$last), "D1_자료부록_시군구별분해")
    cat("--- 자료 부록 D1: 시군구별 항등식 분해 (원고 supplement 아님)\n")
  }

  ## ⛔ 인용 0 이던 여섯(옛 S3·S4·S5·S11·S12·S13)은 담지 않는다 — 본문이 한 번도
  ##    부르지 않고 내용은 이미 본문·그림에 있다. 값이 필요하면 output/tables 에 있다.
  sheets <- list(
    `S1_자료검증`        = prov,
    `S2_제외시군구`      = excl,
    `S3_불평등_연도별`   = iq$ineq,
    `S4_민감도_인구가중` = se$w,
    `S5_민감도_로짓`     = se$logit,
    `S6_민감도_불균형`   = se$unbal,
    `S7_민감도_절사`     = se$trim,
    `S8_시군구변화_분포` = dist_summary(dc$last))
  sheets <- sheets[!vapply(sheets, is.null, logical(1))]

  ## ⛔⛔ 게이트: «비어 있는 시트»는 게이트를 전부 통과한다. 2026-09-03 에 S8 이 그렇게
  ##    나갔고(값 0 · 중앙값 공란 · min Inf) 본문은 그 시트를 네 번 인용하고 있었다.
  ##    표가 비었다는 것은 산출이 없다는 뜻이므로 «경고»가 아니라 «정지»다.
  long <- en_sheet(names(sheets))
  if (any(nchar(long) > 31L))
    stop(sprintf("보충자료 시트명이 31자를 넘는다(엑셀이 조용히 자른다): %s",
                 paste(long[nchar(long) > 31L], collapse = ", ")))
  for (nm in names(sheets)) {
    d <- sheets[[nm]]
    if (!nrow(d)) stop(sprintf("보충자료 %s: 행이 없다", nm))
    num <- vapply(d, is.numeric, logical(1))
    bad <- vapply(d, function(v) any(is.infinite(v)) || any(is.nan(v)), logical(1))
    if (any(bad)) stop(sprintf("보충자료 %s: Inf/NaN 이 든 열 %s",
                               nm, paste(names(d)[bad], collapse = ", ")))
    dead <- names(d)[vapply(d, function(v) all(is.na(v) | trimws(as.character(v)) == ""),
                            logical(1))]
    if (length(dead)) stop(sprintf("보충자료 %s: 값이 하나도 없는 열 %s",
                                   nm, paste(dead, collapse = ", ")))
  }

  ## ⚠ 재정자립도 원자료 4,321행은 «담지 않는다» — supplement 에도 원자료를 통째로 넣지
  ##    않는다(감사 3-4). 출처와 매칭 규칙은 Methods 에 있다.

  ## 영문화 — 시트명·열 제목. 매핑에 없는 것은 그대로 나가므로 «보인다».
  ## ⚠ 이미 영문인 열(S8 의 Statistic/Value)까지 「영문 이름이 없다」고 신고하면
  ##    경고가 소음이 되고, 소음이 된 경고는 아무도 안 본다. 한글이 든 것만 센다.
  left <- unique(unlist(lapply(sheets, function(d)
    setdiff(names(d), c(names(SUPP_COL_EN), SUPP_DROP)))))
  left <- left[grepl("\\p{Hangul}", left, perl = TRUE)]
  if (length(left))
    cat(sprintf("  주의  영문 이름이 없는 열 %d개: %s\n", length(left),
                paste(utils::head(left, 12), collapse = ", ")))
  sheets <- lapply(sheets, en_cols)
  names(sheets) <- en_sheet(names(sheets))

  write_table(sheets, "S_supplement")
  cat(sprintf("--- supplement 시트 %d개\n", length(sheets)))
  invisible(sheets)
}
