##################################################################
#####  11_tables.R - 본문 표를 «만든다». 손으로 옮기지 않는다  #####
##################################################################
##
## ⛔ 2026-09-02 pipeline-audit 이 잡은 것: `03_results.md` 가 *"Nothing here is transcribed
##    by hand"* 라고 적는데, 본문 Table 1–4 는 xlsx 를 보고 «손으로 친» 마크다운이었다.
##    값은 그때 전부 맞았지만(감사가 40칸 전수 대조) 구조적으로 갈라질 수 있었다.
##
## 이 파일이 하는 일 둘:
##   ① 본문 표 4장을 파이프라인 산출에서 «만들어» 영문 머리행으로 낸다
##      → output/tables/p9_본문표_<stamp>.xlsx  (제출 시 Word 표로 변환할 원본)
##      → output/tables/manuscript_tables_<stamp>.md  (읽기용)
##   ② ⭐ 그 표의 «모든 숫자»가 원고 본문에 있는지 검사한다 — 33개 claim 이 못 덮던 자리다
##
## ⛔ 값을 «다시 계산하지 않는다». 이미 나온 xlsx 를 읽어 옮긴다 — 그래야 본문 표와
##    공표 표가 갈라질 수 없다. 계산은 04~08 의 몫이다.

## 본문 표 = 파이프라인 산출의 «어느 시트»인가 (한 곳에만 적는다)
BODY_TABLES <- list(
  list(n = 1, src = "^p2_table1_",    sheet = 1,
       cap = "Study population and descriptive summary"),
  list(n = 2, src = "^p4_궤적_",      sheet = "정점",
       cap = "Peak year and subsequent change by administrative type"),
  ## ⛔ 2026-09-02: 처음에 이 시트를 통째로 넣었더니 19행 × 15자리 원값이 나왔다.
  ##    본문이 쓰는 것은 «양 끝»이고, 전 연도 시계열은 보충 S12 가 이미 담는다.
  ##    → 첫해·끝해만 고른다. «고르는 것»이지 다시 계산하는 것이 아니다.
  ## ⚠ note: 2006년 총 타일이 0.0517 인데 Results 본문은 0.0516 이라고 쓴다. 둘 다 맞다 —
  ##    이 표는 재정자립도가 매칭된 228곳에서, 본문은 230곳에서 계산한 값이다. 표가 그것을
  ##    말하지 않으면 리뷰어는 «오류»로 읽는다.
  list(n = 3, src = "^p7_사회경제축_", sheet = "두_집단축", ends_only = TRUE,
       cap = "Between-group share of inequality on two groupings, first and last year",
       note = paste("Both groupings are computed on the districts matched to a fiscal",
                    "independence ratio in that year (n = 228 in 2006, 230 from 2015),",
                    "so the total Theil index differs slightly from the value reported in",
                    "the text for the full balanced panel of 230 districts.")),
  ## ⚠ 「영_첫해·영_끝해」(0인 시군구 수)를 빼고 각주로 내린다. 8열이면 열 폭이
  ##    0.81 in 으로 떨어져 머리행의 «끊을 수 없는 토큰» `Between-group` 이 글자 중간에서
  ##    쪼개진다(2026-09-07 `audit_table_widths` 실측: 0.82 in 필요). 그리고 그 두 열은
  ##    다섯 행 중 넷이 0 이라 본문이 이미 한 문장으로 말한다.
  ## ⛔ 2026-09-07 manuscript-audit: Results 가 「유형별 변화는 Table 4 에 있다」고 가리키는데
  ##    그 열이 «없었다». 인라인 마크다운 표를 지우면서(중복 제거) 포인터만 Table 4 로
  ##    돌렸기 때문이다 — 이 논문에서 가장 하중이 큰 강건성 주장을 독자가 확인할 수 없었다.
  ##    → 유형별 «변화» 세 열을 붙이고, 집단간 몫 두 열은 「27 → 46」 한 열로 합친다
  ##      (8열을 넘기면 머리행의 `Between-group` 이 글자 중간에서 쪼개진다).
  list(n = 4, src = "^p8_대조척도_",  sheet = "척도별_요약",
       post = "altmeasure_bytype",   # 이 변형이 「영_*」 두 열을 이미 걷어낸다
       cap = "The index recomputed on four alternative measures",
       note = paste("Change columns are percentage points, 2006 to 2024. One district had no",
                    "within-district inpatient expenditure in each of the two endpoint years",
                    "and is excluded from the Theil computation on that measure, where the",
                    "index is undefined at zero. On every other measure no district was at",
                    "zero.")))

## 한글 열 이름 → 영문. ⚠ 여기에 없으면 그대로 나가므로 «빠진 것이 보인다».
COL_EN <- c(
  "구분" = "Item", "시군구" = "Districts", "지역" = "District", "시도" = "Province",
  "유형" = "Type", "연도" = "Year", "n" = "n",
  "2006년" = "2006", "2024년" = "2024", "중앙값의 차" = "Difference of medians",
  "정점연도" = "Peak year", "정점값" = "Peak value", "최종값" = "2024",
  "정점이후 %p" = "Change since peak (pp)", "정점이후 연평균" = "Annual change since peak (pp)",
  "행정유형_집단간몫" = "Between-group share, administrative type (%)",
  "재정5분위_집단간몫" = "Between-group share, fiscal quintile (%)",
  "척도" = "Measure", "중앙_첫해" = "2006", "중앙_끝해" = "2024",
  "집단간몫_첫해" = "Between-group share 2006 (%)",
  "집단간몫_끝해" = "Between-group share 2024 (%)",
  "영_첫해" = "Districts at zero, 2006", "영_끝해" = "Districts at zero, 2024",
  "집단간몫_변화" = "Between-group share (%)",
  "구" = "Gu change", "시" = "Si change", "군" = "Gun change",
  "총" = "Total Theil index")

## ⛔ 2026-09-07: 머리행만 영문화하고 «칸 값»은 한글 그대로 나갔다 — Table 1·2 의
##    행 라벨(전체·구·시·군)이 영문 저널 본문 표에 실린다. 산문은 같은 것을 이미
##    *gu* / *si* / *gun* 으로 부른다. 값도 같은 규율로 옮긴다.
## ⚠ 매핑에 없는 «한글» 값이 남으면 멈춘다 — 보이게 두면 그대로 나간다는 것을 배웠다.
VAL_EN <- c(
  "전체" = "All districts",
  "구"   = "Gu (urban district)",
  "시"   = "Si (city)",
  "군"   = "Gun (rural county)",
  "총진료비 지수(명목, 기준=100)" = "Total expenditure index (nominal, 2006 = 100)")

build_tables <- function() {
  cat("=== 11_tables.R ===\n")
  ## ⛔ 이 .md 는 통합본에 «그대로 실린다». 빌더 이름·「고치지 말 것」·출처 파일명은
  ##    독자가 보는 표면이 아니라 우리 작업 지시다(BODY_TABLES 가 이미 출처의 정본이고,
  ##    어느 시트에서 왔는지는 아래에서 «로그»로 말한다).
  out <- list(); md <- c("# Manuscript tables", "")

  for (b in BODY_TABLES) {
    f <- latest_file(tables_dir, b$src)
    d <- as.data.frame(readxl::read_excel(f, sheet = b$sheet), stringsAsFactors = FALSE)
    if (isTRUE(b$ends_only) && "연도" %in% names(d))
      d <- d[d$연도 %in% range(d$연도), , drop = FALSE]
    ## 표마다 «한 번만» 하는 변형은 여기 이름으로 건다 — 조건문을 표 루프에 흩지 않는다.
    if (identical(b$post, "altmeasure_bytype")) {
      by <- as.data.frame(readxl::read_excel(f, sheet = "유형별"), stringsAsFactors = FALSE)
      w <- stats::reshape(by[, c("척도", "유형", "변화")], idvar = "척도",
                          timevar = "유형", direction = "wide")
      names(w) <- sub("^변화\\.", "", names(w))
      miss <- setdiff(TYPES, names(w))
      if (length(miss)) stop(sprintf("Table 4: 유형별에 없는 유형 %s", paste(miss, collapse = ", ")))
      d <- merge(d, w[, c("척도", TYPES)], by = "척도", sort = FALSE)
      ## 집단간 몫 두 열 -> 한 열. ⚠ 8열을 넘기면 머리행 토큰이 쪼개진다(폭 검사).
      d$`집단간몫_변화` <- sprintf("%.0f → %.0f", d$집단간몫_첫해, d$집단간몫_끝해)
      ## 변화량은 «부호를 붙여» 쓴다 — Table 1 의 「+1.5 pp」와 같은 관례이고,
      ## 부호 없는 2.8 은 «수준»으로 읽힌다.
      for (tp in TYPES) d[[tp]] <- sprintf("%+.1f", d[[tp]])
      d <- d[, c("척도", "시군구", "중앙_첫해", "중앙_끝해", TYPES, "집단간몫_변화")]
    }
    if (!is.null(b$drop)) {
      miss <- setdiff(b$drop, names(d))
      if (length(miss))
        stop(sprintf("Table %d: 빼려는 열이 없다 — %s", b$n, paste(miss, collapse = ", ")))
      d <- d[, setdiff(names(d), b$drop), drop = FALSE]
    }
    ## 표는 «보고하는 자릿수»로 낸다. 15자리 원값은 표가 아니라 자료다.
    ## ⚠ 「몫(%)」 열은 원고가 정수로 보고한다 — 표에 27.5 인데 본문이 27% 면 리뷰어가 묻는다.
    ##    (27.31 은 n=230, 27.50 은 n=228 이라 값이 다른 것이고 둘 다 정수로 27 이다.)
    for (j in seq_along(d)) if (is.numeric(d[[j]]))
      d[[j]] <- round(d[[j]],
        if (grepl("몫", names(d)[j])) 0L
        else if (max(abs(d[[j]]), na.rm = TRUE) < 1) 4L else 1L)
    ## 영문 머리행 — 매핑에 없는 이름은 그대로 남겨 «빠진 것이 보이게» 한다
    hit <- names(d) %in% names(COL_EN)
    names(d)[hit] <- COL_EN[names(d)[hit]]
    if (any(!hit))
      cat(sprintf("  주의  Table %d: 영문 이름이 없는 열 - %s\n", b$n,
                  paste(names(d)[!hit], collapse = ", ")))

    ## 칸 값 — 한글 라벨을 옮기고, 단위 표기를 산문에 맞춘다
    for (j in seq_along(d)) if (!is.numeric(d[[j]])) {
      v <- as.character(d[[j]])
      k <- v %in% names(VAL_EN); v[k] <- unname(VAL_EN[v[k]])
      ## 「%p」는 이 원고 산문의 단위가 아니다 — 산문은 pp 로 쓴다
      v <- gsub("%p", " pp", v, fixed = TRUE)
      v <- gsub("  +", " ", v)
      ## 음수 부호를 산문과 같은 U+2212 로. (게이트는 양쪽을 정규화하므로 대조에 영향 없음)
      v <- gsub("(^|(?<=[ (]))-(?=[0-9])", "−", v, perl = TRUE)
      d[[j]] <- v
    }
    ko <- unique(unlist(lapply(d, function(v)
      if (is.character(v)) v[grepl("\\p{Hangul}", v, perl = TRUE)] else character(0))))
    if (length(ko))
      stop(sprintf("Table %d: 영문화되지 않은 한글 칸 값 - %s (R/11_tables.R 의 VAL_EN 에 추가할 것)",
                   b$n, paste(ko, collapse = ", ")))
    out[[sprintf("Table %d", b$n)]] <- d

    md <- c(md, sprintf("## Table %d. %s", b$n, b$cap), "",
            paste0("| ", paste(names(d), collapse = " | "), " |"),
            paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
            apply(d, 1, function(r) {
              s <- ifelse(is.na(r), "", trimws(as.character(r)))
              ## 표시용 음수는 산문과 같은 U+2212. xlsx 는 «진짜 숫자»로 남긴다.
              s <- gsub("^-(?=[0-9])", "−", s, perl = TRUE)
              paste0("| ", paste(s, collapse = " | "), " |")
            }),
            if (!is.null(b$note)) c("", b$note) else character(0),
            "")
    cat(sprintf("  Table %d  <- %s [%s]\n", b$n, basename(f), b$sheet))
  }

  write_table(out, "p9_본문표")
  f_md <- file.path(tables_dir, paste0("manuscript_tables_", stamp, ".md"))
  writeLines(md, f_md, useBytes = TRUE)
  cat(sprintf("--- 본문 표 %d장 + %s\n", length(out), basename(f_md)))
  save_step(list(tables = out), name = "bodytables")
  invisible(out)
}

##################################################################
#####  게이트 — 본문 표의 «모든 숫자»가 원고에 있는가          #####
##################################################################
##
## ⭐ 33개 claim 이 못 덮던 자리다. 감사 실측: `03_results.md` 의 고유 수치 131개 중
##    «100개»가 게이트 밖이었고, 그중 Table 4 의 40칸이 통째로 들어 있었다.
## ⛔ 값을 다시 계산하지 않는다 — 산출 표에 «있는 숫자»가 원고에 «있는가»만 본다.
##    두 방향 중 이쪽만 본다: 원고에 있는데 표에 없는 것은 여기 관심이 아니다(서술일 수 있다).
gate_table_numbers <- function(txt) {
  tb <- paper_step("^bodytables_", "11 본문표")$tables
  ## ⛔ 2026-09-02 첫 실행이 드러낸 것: 원고는 «진짜 마이너스»(U+2212)를 쓰고 표는 하이픈을
  ##    쓴다. 정규화하지 않으면 음수가 전부 「원고에 없다」로 나온다 — 실제로 그랬다.
  txt <- gsub("\u2212", "-", txt, fixed = TRUE)
  miss <- list(); n_all <- 0L
  for (nm in names(tb)) {
    v <- unlist(lapply(tb[[nm]], as.character))
    ## 숫자만. 연도(4자리)와 한 자리 정수는 뺀다 — 어디서나 걸려 의미가 없다.
    v <- unique(v[grepl("^-?[0-9]+\\.?[0-9]*$", v)])
    v <- v[!grepl("^(19|20)[0-9]{2}$", v) & nchar(gsub("[^0-9]", "", v)) > 1]
    n_all <- n_all + length(v)
    v <- gsub("\u2212", "-", v, fixed = TRUE)
    gone <- v[!vapply(v, function(x) grepl(x, txt, fixed = TRUE), logical(1))]
    if (length(gone)) miss[[nm]] <- gone
  }
  n_gone <- sum(lengths(miss))
  cat(sprintf("--- gate 본문표 수치: %d개 중 원고에서 못 찾은 것 %d개\n", n_all, n_gone))
  for (nm in names(miss))
    cat(sprintf("  주의  %s: %s\n", nm, paste(miss[[nm]], collapse = ", ")))
  ## ⚠ 실패로 만들지 «않는다». 산출 표에는 있으나 본문이 «일부러» 안 쓰는 값이 있다
  ##    (보충으로 내린 열 등). 실패로 두면 게이트가 소음이 되고, 소음이 되면 아무도 안 본다.
  ##    여기서 할 일은 「덮이지 않은 자리를 매 실행 «보이게» 하는 것」이다.
  invisible(n_gone)
}
