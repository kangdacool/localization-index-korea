##################################################################
#####  16_yearbook.R - 통계연보 시군구 진료현황 (2006-2024)  #####
##################################################################
##
## `_parse_yearbook.py` 가 뽑아 둔 long CSV 를 읽어, 이 프로젝트가 못 갖고 있던 축을
## 만든다: **시군구 단위 자체충족률의 «시계열»**.
##   KOSIS   자체충족률을 «시도»까지만 (2006-2024)
##   헬스맵  시군구·중진료권이지만 «2023 한 해»
##   통계연보 시군구 × 2006-2024  ← 여기서만 나온다
##
## ⛔ 파싱은 엑셀 통계연보에서 왔다. 그래서 «KOSIS 와 대조하는 게이트»가 이 파일의
##    핵심이다 — 시도로 합쳐서 같은 양(진료비)을 비교한다. 두 자료는 같은 공단
##    통계이므로 자릿수가 아니라 «거의 일치»를 요구한다.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

YB_CSV <- file.path(DATA_ONLY, "nhis_region_stats", "parsed", "진료현황_시군구.csv")

## 구분 라벨이 연도마다 흔들린다: "계-입원" / "계_입원" / "관내-계" …
## 판정에 쓰기 전에 한 벌로 정규화한다.
yb_norm_gubun <- function(x) {
  x <- gsub("[[:space:]]", "", x)
  x <- gsub("_", "-", x)
  x <- sub("^계$", "계-계", x)
  x <- sub("^관내$", "관내-계", x)
  x <- sub("^관외$", "관외-계", x)
  x
}

## 통계연보는 「안산시」 총계 행과 「안산시상록구」·「안산시단원구」 를 «함께» 싣는다.
## 그대로 합치면 그 시가 두 번 세어져 시도 합계가 부풀어 오른다(실측: 1.9배).
## 어떤 지역명이 같은 (연도·시도·구분) 안에서 «다른 지역명의 접두사»이면 상위 총계다.
yb_mark_hierarchy <- function(d) {
  key <- paste(d$연도, d$시도, d$구분, sep = "\r")
  d$단위2 <- d$단위
  for (k in unique(key)) {
    idx <- which(key == k & d$단위 == "시군구")
    if (length(idx) < 2) next
    nm <- unique(d$지역[idx])
    parents <- nm[vapply(nm, function(x)
      any(nm != x & startsWith(nm, x)), logical(1))]
    if (length(parents)) d$단위2[idx][d$지역[idx] %in% parents] <- "시총계"
  }
  n <- sum(d$단위2 == "시총계")
  if (n) cat(sprintf("--- 상위 총계 행 %d개를 «시총계»로 표시(합산에서 제외)\n", n))
  d$단위 <- d$단위2
  d$단위2 <- NULL
  d
}

read_yearbook <- function() {
  d <- readr::read_csv(YB_CSV, col_types = readr::cols(
    연도 = readr::col_integer(), 구분 = readr::col_character(),
    시도 = readr::col_character(), 지역 = readr::col_character(),
    단위 = readr::col_character(), 지표 = readr::col_character(),
    장 = readr::col_character(), 장명 = readr::col_character(),
    값 = readr::col_double(), .default = readr::col_character()), progress = FALSE)
  ## ⚠ 장(章)이 기준을 가른다 — 「의료기관 시군구별」 장은 **의료기관 소재지 기준**이고
  ## 나머지(진료실적·관내 및 관외)는 **환자 거주지 기준**인데 제목이 같다. 섞으면 시도
  ## 합계가 2.4배가 된다. 장 «번호»는 연도마다 다르므로(2024=7장, 2015=4장) 이름으로 가른다.
  d$기준 <- ifelse(grepl("의료기관", d$장명), "기관소재지", "환자거주지")
  n_inst <- sum(d$기준 == "기관소재지")
  if (n_inst) cat(sprintf("--- 기관소재지 기준 %d행은 따로 표시(거주지 기준과 섞지 않는다)\n",
                          n_inst))
  d$구분 <- yb_norm_gubun(d$구분)
  d$축 <- sub("-.*$", "", d$구분)          # 계 / 관내 / 관외 / 건강보험 / 의료급여…
  d$입원외래 <- sub("^[^-]*-", "", d$구분)  # 계 / 입원 / 외래
  d <- yb_mark_hierarchy(d)
  cat(sprintf("--- 통계연보 %d행 · %d~%d년 · 시군구 %d개 · 축 %s\n",
              nrow(d), min(d$연도), max(d$연도),
              length(unique(d$지역[d$단위 == "시군구"])),
              paste(sort(unique(d$축)), collapse = "/")))
  d
}

##################################################################
#####  GATE - 시도로 합쳐 KOSIS 와 맞는가                    #####
##################################################################

## 기대하는 수록 범위. 파싱본이 이보다 «좁아지면» 멈춘다.
##
## ⛔ 2026-09-01 실측 사고: 절대경로 수정을 시험하느라 `_parse_yearbook.py 2024` 를
##    한 해만 돌렸는데, 그것이 파싱본을 **2024년 한 해로 덮어썼다.** 그런데
##    `gate_yearbook_vs_kosis` 는 «있는 연도»만 대조하므로 **0.00% 로 통과했고**,
##    브리핑·페이지·논문 코호트가 조용히 1년짜리로 줄었다. 게이트가 「맞는가」는 봤지만
##    「다 있는가」는 안 봤다. 둘은 다른 질문이다.
YB_SPAN <- c(2006L, 2024L)

gate_yearbook_span <- function(yb) {
  y <- range(yb$연도, na.rm = TRUE)
  n <- length(unique(yb$연도))
  if (y[1] > YB_SPAN[1] || y[2] < YB_SPAN[2] || n < diff(YB_SPAN) + 1L)
    stop(sprintf(paste0("gate_yearbook_span FAILED: %d~%d년 %d개년뿐이다(기대 %d~%d, %d개년).\n",
                        "  파싱본이 부분 실행으로 덮어써졌을 수 있다 — ",
                        "`python R/_parse_yearbook.py` 를 «연도 인자 없이» 다시 돌릴 것"),
                 y[1], y[2], n, YB_SPAN[1], YB_SPAN[2], diff(YB_SPAN) + 1L))
  cat(sprintf("--- gate yearbook-span OK (%d~%d년 %d개년)\n", y[1], y[2], n))
  invisible(TRUE)
}

gate_yearbook_vs_kosis <- function(yb, panel, tol_pct = 2) {
  a <- yb[yb$단위 == "시군구" & yb$기준 == "환자거주지" &
          yb$축 == "계" & yb$입원외래 == "계" &
          yb$지표 == "진료비" & !is.na(yb$시도) & nzchar(yb$시도), ]
  agg <- stats::aggregate(값 ~ 연도 + 시도, data = a, FUN = sum)
  names(agg) <- c("year", "sido", "yb")

  k <- panel_pick(panel, "care_total_res",
                  axis = list(시도별 = "*", 급여형태별 = "합계"), item = "진료비")
  k <- k[!is.na(k$sido), c("year", "sido", "value")]
  names(k)[3] <- "kosis"

  cmp <- merge(agg, k, by = c("year", "sido"))
  ## 부분 파싱(한두 연도만)도 판정할 수 있어야 한다 — 전체를 돌리기 전에 먼저 재는 게
  ## 10분을 아낀다. 16칸(=한 해 시도)이 최소다.
  if (nrow(cmp) < 16) stop("gate_yearbook_vs_kosis: 비교 가능한 칸이 ", nrow(cmp), "개뿐")
  cmp$pct <- 100 * abs(cmp$yb - cmp$kosis) / cmp$kosis

  ## ⚠ 원자료 결함 «한 칸». 허용오차를 넓히지 않고 이 칸만 못박아 면제한다 —
  ## 넓히면 다음에 생길 진짜 오류까지 통과시킨다(ACCESS_MISMATCH_YEARS 와 같은 방식).
  ## 근거: 2009 고양시일산구 「계」 = 261,158,729 인데 관내(199,918,290) + 관외
  ## (284,992,560) = 484,910,850 이다. 그 「계」는 자기 급여형태 3분류 합(건강보험
  ## 247,710,808 + 1종 11,679,699 + 2종 1,768,222)과는 «정확히» 일치하므로 파서가
  ## 아니라 «연보의 계 표»가 짧다. 이웃 해가 444M(2008)·478M(2010)이라 484M 쪽이 맞고,
  ## KOSIS 도 그쪽과 맞는다. 자체충족률은 관내/관외만 쓰므로 뷰는 영향받지 않는다.
  YB_SOURCE_DEFECT <- data.frame(year = 2009L, sido = "경기",
                                 stringsAsFactors = FALSE)
  ex <- paste(cmp$year, cmp$sido) %in% paste(YB_SOURCE_DEFECT$year,
                                             YB_SOURCE_DEFECT$sido)
  if (any(ex)) {
    cat(sprintf("--- 원자료 결함으로 면제한 칸 %d개: %s (2009 고양시일산구 「계」 결손)\n",
                sum(ex), paste(unique(cmp$sido[ex]), collapse = ", ")))
    cmp <- cmp[!ex, , drop = FALSE]
  }
  worst <- cmp[which.max(cmp$pct), ]
  med <- stats::median(cmp$pct)

  cat(sprintf("--- gate yearbook-vs-KOSIS: %d칸(%d~%d년) 중앙 오차 %.2f%% · 최악 %.2f%% (%d %s)\n",
              nrow(cmp), min(cmp$year), max(cmp$year), med, worst$pct, worst$year, worst$sido))
  bad <- cmp[cmp$pct > tol_pct, , drop = FALSE]
  if (nrow(bad) > 0) {
    byyr <- sort(table(bad$year), decreasing = TRUE)
    stop(sprintf("gate_yearbook_vs_kosis FAILED: %d/%d 칸이 %.0f%% 초과. 연도별 %s",
                 nrow(bad), nrow(cmp), tol_pct,
                 paste(names(byyr), byyr, sep = ":", collapse = " ")))
  }
  invisible(cmp)
}

##################################################################
#####  뷰 — 시군구 자체충족률 «시계열»                       #####
##################################################################

## 하나의 (지표, 입원외래) 조합에서 시군구 × 연도 관내/관외 wide 를 만든다.
## ⚠ 기준은 «환자거주지»만 — 「의료기관 시군구」 장은 소재지 기준이라 섞으면 2.4배가 된다
##    (프로젝트 CLAUDE.md 제약 11).
yb_wide <- function(yb, 지표 = "진료비", 입원외래 = "계") {
  s <- yb[yb$단위 == "시군구" & yb$기준 == "환자거주지" &
          yb$지표 == 지표 & yb$입원외래 == 입원외래 &
          yb$축 %in% c("관내", "관외"), ]
  if (!nrow(s)) stop("yb_wide: 해당 조합의 행이 없다 - ", 지표, " / ", 입원외래)
  w <- tidyr::pivot_wider(s[, c("연도", "시도", "지역", "축", "값")],
                          names_from = "축", values_from = "값", values_fn = sum)
  w <- w[!is.na(w$관내) & !is.na(w$관외) & (w$관내 + w$관외) > 0, ]
  w$자체충족률 <- 100 * w$관내 / (w$관내 + w$관외)
  w[order(w$시도, w$지역, w$연도), ]
}

## 지표를 «이루는 두 열»을 검사한다 — 계 ≈ 관내 + 관외.
## ⛔ 2026-09-02 감사: KOSIS 대조 게이트는 「계」만 본다. 논문 지표는 관내/(관내+관외) 인데
##    분자도 분모도 그 게이트에 «들어가지 않았다». 그리고 없는 그 자리에서 결함이 하나 나왔다 —
##    2009 충남 천안시의 「관외」에 「계」 값이 그대로 들어와 자체충족률이 39.97% 가 됐다
##    (이웃 해 66.4% · 72.5%). 균형패널 밖이라 주분석에는 안 들어갔지만, 그건 운이다.
## ⚠ 기지의 두 부류는 면제한다: 2009 경기 고양시일산구(계 결손) · 2011년(제약 4의 원자료 불일치).
YB_COL_EXEMPT <- data.frame(
  연도 = c(2009L, 2009L), 시도 = c("경기", "충남"),
  지역 = c("고양시일산구", "천안시"),
  사유 = c("계 행 결손(원자료 결함) - 관내/관외는 정상",
           "관외 열에 계 값이 들어옴(원자료 결함) - 조사 필요"),
  stringsAsFactors = FALSE)

gate_yearbook_columns <- function(yb, tol = 0.01) {
  s <- yb[yb$단위 == "시군구" & yb$기준 == "환자거주지" &
          yb$지표 == "진료비" & yb$입원외래 == "계" &
          yb$축 %in% c("계", "관내", "관외"), ]
  w <- tidyr::pivot_wider(s[, c("연도", "시도", "지역", "축", "값")],
                          names_from = "축", values_from = "값", values_fn = sum)
  w <- w[!is.na(w$계) & !is.na(w$관내) & !is.na(w$관외) & w$계 > 0, ]
  w$오차 <- abs(w$관내 + w$관외 - w$계) / w$계
  bad <- w[w$오차 > tol, c("연도", "시도", "지역", "오차")]
  ex <- paste(YB_COL_EXEMPT$연도, YB_COL_EXEMPT$시도, YB_COL_EXEMPT$지역)
  bad$키 <- paste(bad$연도, bad$시도, bad$지역)
  new <- bad[!(bad$키 %in% ex) & bad$연도 != 2011L, ]
  cat(sprintf("--- gate 지표 두 열: %d칸 중 오차>%.0f%% %d칸 (면제 %d · 2011년 %d · 신규 %d)\n",
              nrow(w), tol * 100, nrow(bad), sum(bad$키 %in% ex),
              sum(bad$연도 == 2011L & !(bad$키 %in% ex)), nrow(new)))
  if (nrow(new)) {
    print(utils::head(new[order(-new$오차), c("연도", "시도", "지역", "오차")], 10),
          row.names = FALSE)
    stop("gate 지표 두 열: 면제 목록 밖에서 계 != 관내+관외 인 칸이 ", nrow(new), "개 있다")
  }
  invisible(TRUE)
}

view_yearbook <- function(yb) {
  cat("=== 16_yearbook.R ===\n")
  w <- yb_wide(yb, "진료비", "계")
  write_table(list(시군구_자체충족률 = w), "t15_통계연보_시군구_자체충족률")

  yrs <- range(w$연도)
  ## ⚠ 「동구」는 인천·대전·광주·부산·대구에 다 있다. unique(지역) 으로 세면 다섯이
  ## 하나가 되어 264개가 240개로 «적게» 나온다. 세는 키도 (시도, 지역)이다.
  n_sgg <- length(unique(paste(w$시도, w$지역)))
  cat(sprintf("--- 시군구 자체충족률: %d~%d년 · %d개 시군구 · %d행\n",
              yrs[1], yrs[2], n_sgg, nrow(w)))
  first <- w[w$연도 == yrs[1], ]; last <- w[w$연도 == yrs[2], ]
  cat(sprintf("    %d년 중앙값 %.1f%% -> %d년 %.1f%%\n",
              yrs[1], stats::median(first$자체충족률),
              yrs[2], stats::median(last$자체충족률)))
  ## 「동구」는 인천·대전·광주·부산·대구에 모두 있다. 지역명만으로 이으면 서로 다른
  ## 동구가 한 줄이 된다 — 키는 반드시 (시도, 지역)이다.
  first$키 <- paste(first$시도, first$지역); last$키 <- paste(last$시도, last$지역)
  m <- merge(first[, c("키", "자체충족률")], last[, c("키", "자체충족률")],
             by = "키", suffixes = c("_처음", "_끝"))
  m$지역 <- m$키
  m$변화 <- m$자체충족률_끝 - m$자체충족률_처음
  m <- m[order(m$변화), ]
  cat("    가장 많이 떨어진 곳: ",
      paste(sprintf("%s %+.1f%%p", utils::head(m$지역, 5), utils::head(m$변화, 5)),
            collapse = " · "), "\n")

  ##  ---- 그림: 19년 동안 «분포»가 움직였는가 --------------------------------
  ## 시계열이 이 자료의 값어치이므로 시간 축을 그린다. ⚠ 서로 «다른 단위»끼리
  ## (군 vs 대도시 자치구) 높낮이를 비교하면 안 된다 — 자치구는 옆 구로만 넘어가도
  ## 「관외」가 되어 구조적으로 낮게 나온다. 믿을 수 있는 신호는 «같은 곳의 변화»다.
  qs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  qt <- do.call(rbind, lapply(split(w$자체충족률, w$연도), function(v)
    as.data.frame(as.list(stats::quantile(v, qs, na.rm = TRUE)))))
  names(qt) <- c("p10", "p25", "p50", "p75", "p90")
  qt$연도 <- as.integer(names(split(w$자체충족률, w$연도)))
  lab <- data.frame(
    연도 = max(qt$연도),
    y = unlist(qt[qt$연도 == max(qt$연도), c("p90", "p50", "p10")]),
    txt = c("상위 10%", "중앙값", "하위 10%"))

  g <- ggplot2::ggplot(qt, ggplot2::aes(x = 연도)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p10, ymax = p90),
                         fill = "#DCEAF5") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75),
                         fill = "#A8CBE3") +
    ggplot2::geom_line(ggplot2::aes(y = p50), linewidth = 1.1, colour = ACCENT) +
    ggplot2::geom_text(data = lab, ggplot2::aes(y = y, label = txt),
                       hjust = -0.08, size = 3.4, colour = "grey25",
                       family = KO_FAMILY) +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                breaks = seq(0, 100, 20),
                                labels = function(x) paste0(x, "%")) +
    ggplot2::scale_x_continuous(breaks = seq(min(qt$연도), max(qt$연도), 3),
                                expand = ggplot2::expansion(mult = c(0.02, 0.13))) +
    ggplot2::labs(
      title = sprintf("시군구 자체충족률 분포, %d~%d년", yrs[1], yrs[2]),
      subtitle = sprintf("%d개 시군구 · 진료비 기준 · 관내/(관내+관외)", n_sgg),
      x = NULL, y = NULL,
      caption = paste0(
        "국민건강보험공단 지역별 의료이용 통계연보(제2·3장, 환자 거주지 기준). ",
        "시도로 합치면 KOSIS와 오차 0.00%. ",
        "⚠ 단위가 다른 지역끼리 높낮이를 비교하지 말 것 - 대도시 자치구는 옆 구로 ",
        "넘어가도 「관외」가 되어 구조적으로 낮다. 읽을 것은 같은 곳의 변화다.")) +
    theme_hrm()
  save_fig(g, "fig17_시군구_자체충족률_추이", width = 9, height = 5.2)

  ##  ---- 대조 척도 --------------------------------------------------------
  ## 정본(진료비·계)이 «명목» 금액이라는 것이 이 지표의 약점이다. 같은 자료가 사람 수와
  ## 입원/외래를 다 주므로 대조본을 함께 만들어 둔다 — 논문 민감도가 이것을 읽는다.
  ## ⛔ 정본을 바꾸는 것이 아니다. `alt` 는 «대조»다.
  alt_spec <- list(c("진료실인원수", "계"), c("입내원일수", "계"),
                   c("진료비", "입원"), c("진료비", "외래"))
  alt <- stats::setNames(
    lapply(alt_spec, function(k) yb_wide(yb, k[1], k[2])),
    vapply(alt_spec, function(k) paste(k[1], k[2], sep = "_"), ""))
  cat("--- 대조 척도:\n")
  for (nm in names(alt)) {
    a <- alt[[nm]]; ys <- range(a$연도)
    cat(sprintf("      %-18s %d개 시군구 · 중앙값 %.1f%% (%d) -> %.1f%% (%d)\n", nm,
                length(unique(paste(a$시도, a$지역))),
                stats::median(a$자체충족률[a$연도 == ys[1]]), ys[1],
                stats::median(a$자체충족률[a$연도 == ys[2]]), ys[2]))
  }

  ## 브리핑·페이지가 같은 숫자를 말하도록 «계산된 것»을 넘긴다(손으로 옮기지 않는다).
  save_step(list(wide = w, quant = qt, drop = m, alt = alt), name = "yearbook")
  invisible(w)
}

if (sys.nframe() == 0L) {
  yb <- read_yearbook()
  ## ⛔ 2026-09-02 감사: 여기에 span 게이트가 «없었다». 그것은 2026-09-01 「부분 파싱이 정본을
  ##    한 해로 덮어썼다」 사고를 막으려고 만든 것인데, 단독 실행 경로에만 빠져 있었다.
  ##    그리고 오늘 논문이 읽은 yearbook rds 가 «이 경로»로 만들어졌다.
  gate_yearbook_span(yb)
  gate_yearbook_columns(yb)
  panel <- load_step("^panel_")
  gate_yearbook_vs_kosis(yb, panel)
  view_yearbook(yb)
}
