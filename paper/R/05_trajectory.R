##################################################################
#####  05_trajectory.R - 언제 갈라졌나                        #####
##################################################################
##
## 03·04 는 «두 점»(2006, 2024)만 본다. 그래서 「군이 −3.6%p」까지는 말하지만
## **언제부터** 밀렸는지는 말하지 못한다. 정책 시점과 맞대려면 그것이 필요하다.
##
## 여기서 하는 것은 셋이다.
##   ① 유형별 궤적 — 연도별 중앙값과 사분위폭
##   ② 격차 — 「구 − 군」 중앙값 차이의 연도별 추이. 갈라짐을 «한 줄»로 요약한다
##   ③ 구간별 연평균 변화 — 어느 구간에서 기울기가 바뀌었나
##
## ⛔ 여기서도 인과는 없다. 「2010년대 중반에 벌어졌다」까지가 이 자료가 말하는 것이고,
##    «무엇 때문에»는 다른 설계가 답한다.

TYPES <- c("구", "시", "군")

trajectory <- function(d) {
  cat("=== 05_trajectory.R ===\n")
  d <- d[d$유형 %in% TYPES, ]
  yrs <- sort(unique(d$연도))

  ## ---- ① 유형별 궤적 -------------------------------------------------
  tr <- do.call(rbind, lapply(split(d, list(d$유형, d$연도), drop = TRUE), function(g)
    data.frame(유형 = g$유형[1], 연도 = g$연도[1], n = nrow(g),
               중앙 = stats::median(g$자체충족),
               q25 = stats::quantile(g$자체충족, .25),
               q75 = stats::quantile(g$자체충족, .75),
               stringsAsFactors = FALSE)))
  tr$유형 <- factor(tr$유형, levels = TYPES)
  tr <- tr[order(tr$유형, tr$연도), ]

  ## 게이트: 양 끝이 Table 1 과 같아야 한다. 다르면 어느 한쪽이 표본을 달리 잡은 것이다.
  for (tp in TYPES) {
    for (y in range(yrs)) {
      a <- tr$중앙[tr$유형 == tp & tr$연도 == y]
      b <- stats::median(d$자체충족[d$유형 == tp & d$연도 == y])
      if (abs(a - b) > 1e-9)
        stop("05_trajectory: ", tp, " ", y, " 중앙값이 표본과 어긋난다")
    }
  }
  cat("--- gate trajectory OK: 양 끝 중앙값이 Table 1 과 일치\n")

  ## ---- ② 갈라짐: 구 − 군 -------------------------------------------
  gap <- merge(tr[tr$유형 == "구", c("연도", "중앙")],
               tr[tr$유형 == "군", c("연도", "중앙")],
               by = "연도", suffixes = c("_구", "_군"))
  gap$격차 <- gap$중앙_구 - gap$중앙_군
  g0 <- gap$격차[gap$연도 == min(yrs)]; g1 <- gap$격차[gap$연도 == max(yrs)]

  ## ---- ③ 구간별 연평균 변화 ---------------------------------------
  ## 구간은 «자료를 보고» 나눈 것이 아니라 균등 3분할이다 — 눈으로 고른 분기점은
  ## 그 자체가 결과가 된다. 균등 분할이면 적어도 자의성이 한 곳에 모인다.
  cut_yrs <- round(seq(min(yrs), max(yrs), length.out = 4))
  seg <- do.call(rbind, lapply(seq_len(3), function(i) {
    a <- cut_yrs[i]; b <- cut_yrs[i + 1]
    do.call(rbind, lapply(TYPES, function(tp) {
      va <- tr$중앙[tr$유형 == tp & tr$연도 == a]
      vb <- tr$중앙[tr$유형 == tp & tr$연도 == b]
      data.frame(구간 = sprintf("%d–%d", a, b), 유형 = tp,
                 시작 = round(va, 1), 끝 = round(vb, 1),
                 `연평균 %p` = round((vb - va) / (b - a), 2),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
  }))

  ## ---- ④ 정점과 그 이후 -------------------------------------------
  ## ⚠ ③의 등간 3분할은 «자의성을 한 곳에 모으는» 장치일 뿐, 실제 굴곡을 못 잡는다.
  ## 그림을 보니 군의 정점은 구간 경계(2012)가 아니라 **2011년**이었다. 등간 분할이
  ## 그것을 가렸다 — 그래서 정점을 «자료가 말하게» 따로 뽑는다.
  ## ⛔ 단 이 정점은 «찾아낸» 값이라 검정에 쓰지 않는다. 서술에만 쓴다.
  pk <- do.call(rbind, lapply(TYPES, function(tp) {
    g <- tr[tr$유형 == tp, ]
    i <- which.max(g$중앙)
    data.frame(유형 = tp, 정점연도 = g$연도[i], 정점값 = round(g$중앙[i], 1),
               최종값 = round(g$중앙[nrow(g)], 1),
               `정점이후 %p` = round(g$중앙[nrow(g)] - g$중앙[i], 1),
               `정점이후 연평균` = round((g$중앙[nrow(g)] - g$중앙[i]) /
                                          (g$연도[nrow(g)] - g$연도[i]), 2),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))

  ## 코로나 함몰과 회복 — 2019 대비 2021, 그리고 2021 대비 최종.
  ## 모두 떨어졌는데 «누가 돌아왔는가»가 유형을 가른다.
  cv <- do.call(rbind, lapply(TYPES, function(tp) {
    g <- tr[tr$유형 == tp, ]
    v <- function(y) g$중앙[g$연도 == y]
    data.frame(유형 = tp,
               `2019→2021` = round(v(2021) - v(2019), 1),
               `2021→최종` = round(g$중앙[nrow(g)] - v(2021), 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))

  write_table(list(궤적 = tr, 격차_구군 = gap, 구간별 = seg,
                   정점 = pk, 코로나 = cv), "p4_궤적")
  cat("--- 정점과 그 이후:\n"); print(pk, row.names = FALSE)
  cat("--- 코로나 함몰과 회복(%p):\n"); print(cv, row.names = FALSE)
  cat(sprintf("--- 구−군 격차: %d년 %.1f%%p -> %d년 %.1f%%p (%+.1f%%p 벌어짐)\n",
              min(yrs), g0, max(yrs), g1, g1 - g0))
  print(seg, row.names = FALSE)

  ## ---- 그림: 논문 Figure 1 -------------------------------------------
  lab <- tr[tr$연도 == max(yrs), ]
  tr$type_en <- en_type(tr$유형); lab$type_en <- en_type(lab$유형)
  lab$type_ab <- abbr_type(lab$유형)
  p1 <- ggplot2::ggplot(tr, ggplot2::aes(연도, 중앙, colour = type_en, fill = type_en)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75),
                         alpha = 0.14, colour = NA) +
    ggplot2::geom_line(ggplot2::aes(linetype = type_en), linewidth = 1.1) +
    ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = type_ab),
                             hjust = 0, nudge_x = 0.5, direction = "y",
                             size = 3.6, seed = 1, show.legend = FALSE,
                             segment.colour = "grey70", family = KO_FAMILY) +
    scale_colour_hrm() + scale_fill_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8),
                                expand = ggplot2::expansion(mult = c(0.02, 0.045))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      title = "The national plateau is the average of three divergent trajectories",
      subtitle = sprintf("Median and interquartile range by administrative type; %d districts (balanced panel), expenditure basis",
                         length(unique(d$sgg))),
      x = NULL, y = "Localization index (%)", colour = NULL, fill = NULL, linetype = NULL,
      ## ⚠ 이 한 줄은 «제약 2»다 — 없으면 독자가 유형 «수준»을 비교한다. 뒤에 있던
      ##    「비교는 기울기이지 수준이 아니다」는 같은 말의 반복이라 뺐다.
      caption = paste0("Levels are not compared across types: urban districts are embedded ",
                       "in metropolitan areas, where crossing a boundary is trivial, and are ",
                       "structurally low.")) +
    theme_hrm()
  ## ⚠ 그림 번호는 «본문 첫 언급 순서»다 — 스크립트 번호가 아니다(2026-09-02 리드 변경).
  save_fig_journal(p1, "p_fig1_trajectory_by_type", height = FIG_W * 5.4 / 9)

  p2 <- ggplot2::ggplot(gap, ggplot2::aes(연도, 격차)) +
    ggplot2::geom_line(linewidth = 1.2, colour = ACCENT) +
    ggplot2::geom_point(size = 1.5, colour = ACCENT) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%p")) +
    ggplot2::labs(title = "When the gap between urban districts and rural counties widened",
                  subtitle = "Difference in the median localization index, gu minus gun",
                  x = NULL, y = "Gap (percentage points)",
                  caption = paste0("The vertical distance between the gu and gun medians ",
                                   "in Figure 1, reduced to a single series.")) +
    theme_hrm()
  save_fig_journal(p2, "p_fig2_gu_gun_gap", height = FIG_W * 4.4 / 8)

  save_step(list(traj = tr, gap = gap, seg = seg), name = "trajectory")
  invisible(tr)
}
