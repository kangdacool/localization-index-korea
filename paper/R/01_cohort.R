##################################################################
#####  01_cohort.R - 분석 표본을 «닫는다»                     #####
##################################################################
##
## 무엇을 넣고 무엇을 뺐는지가 논문의 첫 번째 표(흐름도)다. 여기서 정하고, 여기서만 정한다.
## 뒤 스크립트는 이 파일이 낸 표본을 «그대로» 쓴다.
##
## ⚠ 균형패널 vs 불균형패널: 19년 사이에 행정구역이 바뀐 곳이 있다(2010 창원 통합,
##    2012 세종 신설, 2014 청주 통합 ...). 「같은 곳의 변화」를 읽는 것이 이 논문의
##    전제이므로 **주분석은 균형패널**이고, 불균형은 민감도로 남긴다.
##    ⛔ 균형패널이 «대표성»을 가진다고 주장하지 않는다 — 빠진 곳이 어디인지 표로 낸다.

if (!exists("PAPER_ROOT")) source(file.path(dirname(sys.frame(1)$ofile %||% "R"), "00_setup.R"))

`%||%` <- function(a, b) if (is.null(a)) b else a

build_cohort <- function() {
  cat("=== 01_cohort.R ===\n")
  yb <- parent_step("^yearbook_", "통계연보 조립본(16_yearbook.R)")
  w <- yb$wide            # 연도·시도·지역·관내·관외·자체충족률

  ## 키는 반드시 (시도, 지역). 「동구」가 다섯 시도에 있다.
  w$sgg <- paste(w$시도, w$지역)
  n0 <- nrow(w); k0 <- length(unique(w$sgg)); y0 <- range(w$연도)
  flow <- data.frame(단계 = "통계연보 시군구 × 연도(부모 산출)",
                     시군구 = k0, 행 = n0,
                     비고 = sprintf("%d~%d년", y0[1], y0[2]),
                     stringsAsFactors = FALSE)

  ## (1) 관내·관외가 «둘 다» 있는 행만. 하나만 있으면 비율을 만들 수 없다.
  w <- w[!is.na(w$관내) & !is.na(w$관외) & (w$관내 + w$관외) > 0, ]
  flow <- rbind(flow, data.frame(단계 = "관내·관외가 둘 다 있고 합이 0이 아닌 행",
                                 시군구 = length(unique(w$sgg)), 행 = nrow(w),
                                 비고 = "비율을 만들 수 없는 행 제외",
                                 stringsAsFactors = FALSE))

  ## (2) 균형패널: 전 기간 «매년» 관측된 시군구만
  yrs <- sort(unique(w$연도))
  cnt <- table(w$sgg)
  keep <- names(cnt)[cnt == length(yrs)]
  dropped <- setdiff(unique(w$sgg), keep)
  bal <- w[w$sgg %in% keep, ]
  flow <- rbind(flow, data.frame(
    단계 = sprintf("균형패널 — %d년 전부 관측된 시군구", length(yrs)),
    시군구 = length(keep), 행 = nrow(bal),
    비고 = sprintf("%d곳 제외(행정구역 개편 등)", length(dropped)),
    stringsAsFactors = FALSE))

  ## 빠진 곳을 «이름으로» 남긴다. 숫자만 남기면 다음 사람이 다시 세게 된다.
  drop_tab <- do.call(rbind, lapply(dropped, function(g) {
    d <- w[w$sgg == g, ]
    data.frame(시군구 = g, 관측연수 = nrow(d),
               첫해 = min(d$연도), 끝해 = max(d$연도), stringsAsFactors = FALSE)
  }))
  if (!is.null(drop_tab)) drop_tab <- drop_tab[order(-drop_tab$관측연수), ]

  write_table(list(흐름 = flow, 제외된_시군구 = drop_tab), "p1_표본흐름")
  for (i in seq_len(nrow(flow)))
    cat(sprintf("--- %-42s 시군구 %3d · 행 %5d  (%s)\n",
                flow$단계[i], flow$시군구[i], flow$행[i], flow$비고[i]))

  ## ---- 게이트: 표본이 «닫혔는가» -------------------------------------
  ## 부모의 게이트가 「자료가 맞는가」를 본다면, 여기 게이트는 「설계가 성립하는가」를 본다.
  stopifnot(nrow(bal) == length(keep) * length(yrs))
  if (length(keep) < 150)
    stop("01_cohort: 균형패널이 ", length(keep), "곳뿐이다 — 설계를 다시 볼 것")
  gap <- setdiff(yrs, unique(bal$연도))
  if (length(gap)) stop("01_cohort: 균형패널에 빠진 연도 ", paste(gap, collapse = ", "))
  cat(sprintf("--- gate cohort OK: %d곳 × %d년 = %d행, 결번 없음\n",
              length(keep), length(yrs), nrow(bal)))

  save_step(list(balanced = bal, all = w, flow = flow, dropped = drop_tab),
            name = "cohort")
  invisible(list(balanced = bal, all = w))
}
