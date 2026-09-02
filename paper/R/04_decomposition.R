##################################################################
#####  04_decomposition.R - 주분석: 정체는 «어디서» 오는가    #####
##################################################################
##
## 자체충족률 = 관내 / (관내 + 관외) 이므로, 비율이 안 움직였다는 것은 두 가지 중 하나다.
##   (A) 관내가 안 늘었다                     → 지역 안의 공급·이용이 정체
##   (B) 관내는 늘었는데 관외도 «같이» 늘었다 → 늘어난 만큼 밖으로도 더 나갔다
## 정책 함의가 정반대인데 「정체」라는 한 단어로 뭉뜽그리면 그 차이가 사라진다.
##
## 이 분해는 «항등식»이지 모형이 아니다 — 가정이 없고, 그래서 틀릴 수 없다.
## 인과적 주장은 여기서 하지 않는다.

decompose <- function(d) {
  cat("=== 04_decomposition.R ===\n")
  y0 <- min(d$연도); y1 <- max(d$연도)
  a1 <- d[d$연도 == y1, ]

  ## 각 시군구의 관내·관외 성장배수
  a1$배수_관내 <- a1$관내 / a1$기준_관내
  a1$배수_관외 <- a1$관외 / a1$기준_관외
  ## 관내가 관외보다 «덜» 자랐으면 자체충족률은 떨어진다. 그 비가 곧 방향이다.
  a1$상대성장 <- a1$배수_관내 / a1$배수_관외

  a1$판정 <- ifelse(a1$변화_자체충족 >= 0, "유지·상승", "하락")
  a1$원인 <- ifelse(a1$배수_관내 < 1, "(A) 관내 자체가 줄었다",
             ifelse(a1$상대성장 < 1, "(B) 관내는 늘었지만 관외가 더 늘었다",
                    "관내가 관외보다 더 늘었다"))

  tb <- as.data.frame(table(a1$판정, a1$원인), stringsAsFactors = FALSE)
  names(tb) <- c("자체충족률", "관내·관외 성장", "시군구 수")
  tb <- tb[tb$`시군구 수` > 0, ]

  sm <- data.frame(
    지표 = c("관내 진료비 성장배수(중앙값)", "관외 진료비 성장배수(중앙값)",
             "관내/관외 상대성장(중앙값)", "자체충족률 변화(중앙값, %p)"),
    값 = c(sprintf("%.2f배", stats::median(a1$배수_관내)),
           sprintf("%.2f배", stats::median(a1$배수_관외)),
           sprintf("%.3f", stats::median(a1$상대성장)),
           sprintf("%+.1f", stats::median(a1$변화_자체충족))),
    stringsAsFactors = FALSE)

  write_table(list(요약 = sm, 분해 = tb,
                   시군구별 = a1[order(a1$변화_자체충족),
                                 c("시도", "지역", "유형", "기준_자체충족", "자체충족",
                                   "변화_자체충족", "배수_관내", "배수_관외", "상대성장")]),
              "p3_분해")

  cat(sprintf("--- %d→%d년 중앙값: 관내 %.2f배 · 관외 %.2f배 (상대성장 %.3f)\n",
              y0, y1, stats::median(a1$배수_관내), stats::median(a1$배수_관외),
              stats::median(a1$상대성장)))
  print(tb, row.names = FALSE)
  n_b <- sum(a1$원인 == "(B) 관내는 늘었지만 관외가 더 늘었다")
  n_a <- sum(a1$원인 == "(A) 관내 자체가 줄었다")
  cat(sprintf("--- **(A) 관내 감소 %d곳 · (B) 관외가 더 늘어남 %d곳** (전체 %d)\n",
              n_a, n_b, nrow(a1)))

  ## 그림: 가로=관내 성장, 세로=관외 성장. 대각선 위는 「나가는 쪽이 더 자란」 곳이다.
  a1$유형 <- factor(a1$유형, levels = c("구", "시", "군", "기타"))
  a1$type_en <- en_type(a1$유형)
  ## 점은 선종류를 못 쓰므로 «모양»으로 중복 인코딩한다 (색에만 의존하지 않기)
  g <- ggplot2::ggplot(a1, ggplot2::aes(배수_관내, 배수_관외, colour = type_en, shape = type_en)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey55",
                         linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_point(size = 1.9, alpha = 0.85) +
    scale_colour_hrm() +
    ggplot2::scale_shape_manual(values = c(16, 17, 15)) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = sprintf("Growth of in-area and out-of-area expenditure, %d to %d", y0, y1),
      subtitle = "Above the diagonal: out-of-area use grew faster, so the index fell",
      x = "In-area expenditure, growth multiple (log scale)",
      y = "Out-of-area expenditure, growth multiple (log scale)",
      colour = NULL, shape = NULL,
      caption = paste0(nrow(a1), " districts (balanced panel); expenditure basis, nominal. ",
                       "This is an arithmetic decomposition of an identity, not a model, ",
                       "and carries no causal interpretation.")) +
    theme_hrm()
  ## ⚠ 그림 번호 = 본문 첫 언급 순서(2026-09-02). 궤적이 Fig. 1, 이 산점도가 Fig. 2.
  ## ⚠ 축 제목이 길어 캔버스 상단에 닿아 «잘렸다»(2026-09-02 렌더 확인). 높이를 늘린다.
  save_fig_journal(g, "p_fig3_관내관외_성장", height = FIG_W * 0.80)

  save_step(list(last = a1, summary = sm, cross = tb), name = "decomp")
  invisible(a1)
}
