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
SUPP_SHEET_EN <- c(
  "S1_제외시군구"      = "S1 Excluded districts",
  "S2_자료검증"        = "S2 Validation against KOSIS",
  "S3_궤적_전연도"     = "S3 Trajectory by type, all years",
  "S4_구군격차"        = "S4 Gu-gun gap",
  "S5_구간별"          = "S5 Change by period",
  "S6_불평등_연도별"   = "S6 Inequality indices by year",
  "S7_민감도_인구가중" = "S7 Sensitivity, population weighted",
  "S8_민감도_로짓"     = "S8 Sensitivity, logit variance",
  "S9_민감도_불균형"   = "S9 Sensitivity, unbalanced panel",
  "S10_민감도_절사"    = "S10 Sensitivity, trimming",
  "S11_항등식_가중"    = "S11 Identity decomposition, weighted",
  "S12_두집단축"       = "S12 Between-group share, two groupings",
  "S13_재정5분위_궤적" = "S13 Trajectory by fiscal quintile",
  "S14_시군구별_분해"  = "S14 Identity decomposition by district")

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
  excl$사유 <- "19년 중 결측 연도가 있음(행정구역 개편·신설·통합)"
  cat(sprintf("--- S1 제외 시군구 %d곳\n", nrow(excl)))

  ## ── S2. 자료 검증과 «면제한 한 칸» — Limitations 가 가리키는 두 번째 자리
  prov <- data.frame(
    항목 = c("대조 대상", "대조 결과", "면제한 칸", "면제 사유", "면제하지 않은 것"),
    내용 = c(
      "시군구 값을 시도로 합산 → KOSIS 시도별 진료비(care_total_res)",
      "304 시도-연 중 303칸이 오차 0.00%(중앙값)",
      "2009년 경기도 1칸",
      paste("원자료 결함 — 고양시일산구의 「계」 행이 결손이다.",
            "같은 행의 급여형태 3분류 합은 정확히 맞으므로 연보의 «계» 표가 짧은 것이며,",
            "파서의 문제가 아니다."),
      paste("허용오차를 넓혀 통과시키지 않았다. 이 한 칸만 명시적으로 면제했고,",
            "다른 연도가 어긋나면 파이프라인이 멈춘다.")),
    stringsAsFactors = FALSE)

  ## ── S3~S6. 앞 단계 결과를 그대로 담는다
  sheets <- list(
    `S1_제외시군구`      = excl,
    `S2_자료검증`        = prov,
    `S3_궤적_전연도`     = tj$traj,
    `S4_구군격차`        = tj$gap,
    `S5_구간별`          = tj$seg,
    `S6_불평등_연도별`   = iq$ineq,
    `S7_민감도_인구가중` = se$w,
    `S8_민감도_로짓`     = se$logit,
    `S9_민감도_불균형`   = se$unbal,
    `S10_민감도_절사`    = se$trim,
    `S11_항등식_가중`    = se$dec,
    `S12_두집단축`       = so$both,
    `S13_재정5분위_궤적` = so$traj,
    `S14_시군구별_분해`  = dc$last)
  sheets <- sheets[!vapply(sheets, is.null, logical(1))]

  ## ⚠ 재정자립도 원자료 4,321행은 «담지 않는다» — supplement 에도 원자료를 통째로 넣지
  ##    않는다(감사 3-4). 출처와 매칭 규칙은 Methods 에 있다.

  ## 영문화 — 시트명·열 제목. 매핑에 없는 것은 그대로 나가므로 «보인다».
  left <- unique(unlist(lapply(sheets, function(d)
    setdiff(names(d), c(names(SUPP_COL_EN), SUPP_DROP)))))
  if (length(left))
    cat(sprintf("  주의  영문 이름이 없는 열 %d개: %s\n", length(left),
                paste(utils::head(left, 12), collapse = ", ")))
  sheets <- lapply(sheets, en_cols)
  names(sheets) <- en_sheet(names(sheets))

  write_table(sheets, "S_supplement")
  cat(sprintf("--- supplement 시트 %d개\n", length(sheets)))
  invisible(sheets)
}
