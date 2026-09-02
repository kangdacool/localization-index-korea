##################################################################
#####  17_view_benchmark.R - ⑤활용 · ⑥기준                  #####
##################################################################
##
## 이 두 영역이 비어 있는 동안 이 파이프라인은 **「많다/적다」를 말할 자격이 없었다.**
## ①~④는 「얼마나 있고 어디에 있나」까지만 답한다. 기준선이 있어야 그 양이 큰지 작은지가
## 정해지고, 활용이 있어야 「많은 것이 쓰이고 있나」를 물을 수 있다.
##
## 두 영역을 한 파일에 둔 이유: **짝이기 때문이다.** 병상이 OECD 최고인데(⑥) 가동률과
## 재원일수가 어떤가(⑤)는 한 질문이다. 따로 두면 둘을 나란히 볼 이유가 사라진다.
##
## ⛔ 되짚지 말 것
##  1. **여기 「의사」는 OECD의 «활동 의사» 정의**다. ①②③의 의사 수(DT_HIRA44,
##     «요양기관 근무 인력»)와 **다른 양**이다. 두 숫자를 한 문장에 넣지 말 것.
##  2. OECD 자료는 나라마다 **연도가 비어 있다**. 선이 끊기는 것은 결측이지 0이 아니다.
##  3. **「OECD」(평균 행)는 메타데이터에 있지만 값이 하나도 오지 않는다**(2026-09-01 실측).
##     그래서 이 절에 «OECD 평균»은 없다. 5개국 평균을 내서 그렇게 «부르지 말 것» —
##     OECD 평균은 38개국 기준이고 우리가 가진 것은 다섯이다. 다른 양이다.
##  4. **한국의 병상 가동률은 2000~2003 네 해뿐이다.** 최근 국제 가동률 비교에 한국을
##     넣을 수 없다. 이건 자료의 한계이지 우리 코드의 결함이 아니고, 숨기지 않고 말한다.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))

## 한국을 굵게, 나머지는 맥락으로. 두 «역할»이 다르므로 색만으로 가르지 않고
## 굵기로도 가른다 — 흑백으로 인쇄해도 어느 선이 한국인지는 남는다.
BM_FOCUS <- "대한민국"
BM_PEERS <- c("일본", "독일", "프랑스", "영국", "미국")

bm_series <- function(panel, key, item) {
  d <- panel_pick(panel, key, axis = list(국가 = "*"), item = item)
  d <- d[!is.na(d$value), c("year", "axis1_val", "value", "unit")]
  names(d)[2] <- "국가"
  d[order(d$국가, d$year), ]
}

bm_plot <- function(d, title, subtitle, ylab, caption) {
  d$역할 <- ifelse(d$국가 == BM_FOCUS, "한국", "다른 나라")
  lv <- c(BM_FOCUS, intersect(BM_PEERS, unique(d$국가)))
  d$국가 <- factor(d$국가, levels = lv)
  last <- do.call(rbind, lapply(split(d, d$국가), function(g)
    if (!nrow(g)) NULL else g[which.max(g$year), ]))

  ## ⚠ linetype 을 aes 에 두지 «않는다». 한때 두었다가 scale 만 지웠더니 ggplot 기본값이
  ## 붙어 **강조해야 할 한국 선이 점선**이 됐다(2026-09-01 실측) — 가장 약해 보였다.
  ## 여기서 색 말고 «무엇이 계열을 구분하는가»는 선 끝의 «직접 라벨»이다. 5개 계열에는
  ## 선종류보다 라벨이 낫고, 한국은 굵기로 따로 선다. 규칙은 「색에만 의존하지 말 것」이지
  ## 「반드시 lty」가 아니다.
  ggplot2::ggplot(d, ggplot2::aes(year, value, colour = 국가, linewidth = 역할)) +
    ggplot2::geom_line() +
    ggrepel::geom_text_repel(
      data = last, ggplot2::aes(label = 국가), hjust = 0, nudge_x = 0.6,
      direction = "y", size = 3.2, seed = 1, segment.colour = "grey70",
      min.segment.length = 0.25, show.legend = FALSE, family = KO_FAMILY) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      PAL_HRM[seq_along(lv)], lv)) +
    ## 한국은 굵게, 나머지는 얇게. 색이 안 보여도 «어느 선이 한국인지»는 남는다.
    ggplot2::scale_linewidth_manual(values = c(`한국` = 1.5, `다른 나라` = 0.6),
                                    guide = "none") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8),
                                expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = ylab,
                  caption = caption) +
    theme_hrm() + ggplot2::theme(legend.position = "none")
}

view_benchmark <- function(panel) {
  cat("=== 17_view_benchmark.R ===\n")

  ##################################################################
  #####  ⑥ 기준 — 국제적으로 많은가 적은가                    #####
  ##################################################################

  bed <- bm_series(panel, "oecd_bed", "병상 수")
  doc <- bm_series(panel, "oecd_doctor", "의사")
  ub <- unique(bed$unit)[1]; ud <- unique(doc$unit)[1]

  write_table(list(병상_국제 = bed, 의사_국제 = doc), "t16_기준_국제비교")

  save_fig(bm_plot(bed, "병상 수, 국제비교",
                   paste0("단위: ", ub),
                   paste0("병상 (", ub, ")"),
                   paste0("자료: KOSIS OECD 보건통계(DT_2OEHG020). ",
                          "선이 끊긴 곳은 그 나라의 «결측»이지 0이 아니다.")),
           "fig18_기준_병상_국제")

  save_fig(bm_plot(doc, "활동 의사 수, 국제비교",
                   paste0("단위: ", ud),
                   paste0("의사 (", ud, ")"),
                   paste0("자료: KOSIS OECD 보건통계(DT_2OEHG052). ",
                          "⚠ 이것은 OECD의 «활동 의사» 정의다 — ",
                          "이 문서의 다른 절에 나오는 «요양기관 근무 의사»와 다른 양이다.")),
           "fig19_기준_의사_국제")

  ## ⛔ 한국이 없으면 «조용히 넘어가지 않는다». 앞선 판이 그랬다 — 기준 계열이 비어
  ## sprintf 가 character(0) 을 인쇄해 «아무 말도 안 하는 것»으로 실패했고, 그림과 표는
  ## 멀쩡히 만들어져 통과한 것처럼 보였다.
  bm_say <- function(d, lab, unit, digits = 1) {
    cur <- d[d$국가 == BM_FOCUS, ]
    if (!nrow(cur)) stop("view_benchmark: ", lab, " 에 ", BM_FOCUS, " 가 없다")
    y <- max(cur$year)
    same <- d[d$year == y, ]
    same <- same[order(-same$value), ]
    rk <- which(same$국가 == BM_FOCUS)
    others <- same[same$국가 != BM_FOCUS, ]
    ## 「비교 N개국」은 «그 해에 값이 있는» 나라 수다. 미국처럼 마지막 보고가 이른
    ## 나라는 그 해 비교에서 빠진다 — 나라가 사라진 것이 아니라 그 해 값이 없는 것이다.
    cat(sprintf("--- %d년 %s: 한국 %.*f%s — 그 해 값이 있는 %d개국 중 %d위 (%s)\n",
                y, lab, digits, same$value[rk], unit, nrow(same), rk,
                paste(sprintf("%s %.*f", others$국가, digits, others$value),
                      collapse = " · ")))
    invisible(list(year = y, korea = same$value[rk], rank = rk, n = nrow(same)))
  }
  bm_say(bed, "병상 수", "")
  bm_say(doc, "활동 의사 수", "")

  ##################################################################
  #####  ⑤ 활용 — 그 많은 병상은 쓰이고 있나                  #####
  ##################################################################

  occ <- bm_series(panel, "bed_occupancy", "가동률")
  uo <- unique(occ$unit)[1]
  alos <- panel_pick(panel, "alos", axis = list(가상분류 = "데이터"),
                     item = "급성기 진료 평균재원일수")
  alos <- alos[!is.na(alos$value), c("year", "value", "unit")]
  disc <- panel_pick(panel, "alos", axis = list(가상분류 = "데이터"),
                     item = "급성기 진료 퇴원건수")
  disc <- disc[!is.na(disc$value), c("year", "value")]
  names(disc)[2] <- "퇴원건수"

  write_table(list(가동률_국제 = occ,
                   재원일수_한국 = merge(alos, disc, by = "year", all = TRUE)),
              "t17_활용")

  save_fig(bm_plot(occ, "급성기 병상 가동률, 국제비교",
                   paste0("단위: ", uo),
                   paste0("가동률 (", uo, ")"),
                   paste0("자료: KOSIS OECD 보건통계(DT_2OEHG021). ",
                          "병상이 많다는 것과 그 병상이 쓰인다는 것은 다른 사실이다. ",
                          "⚠ 한국은 2000~2003년만 보고돼 있어 최근 구간에 선이 없다 — ",
                          "자료의 한계이지 값이 0인 것이 아니다.")),
           "fig20_활용_가동률_국제")

  ## 한국 한 계열이므로 국제 그림과 형태를 달리한다 — 같은 틀로 그리면 비교로 오독된다.
  g <- ggplot2::ggplot(alos, ggplot2::aes(year, value)) +
    ggplot2::geom_line(linewidth = 1.2, colour = ACCENT) +
    ggplot2::geom_point(size = 1.6, colour = ACCENT) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(title = "급성기 진료 평균재원일수, 한국",
                  subtitle = paste0("단위: ", unique(alos$unit)[1],
                                    " · OECD 제출 기준"),
                  x = NULL, y = paste0("평균재원일수 (", unique(alos$unit)[1], ")"),
                  caption = paste0("자료: KOSIS 보건복지부 OECD 제출기준 급성기 진료",
                                   "(DT_117030_008). 이 표는 한국만 싣는다 — ",
                                   "국제 비교는 위 가동률 그림이 담당한다.")) +
    theme_hrm()
  save_fig(g, "fig21_활용_재원일수_한국", height = 4.6)

  ## ⛔ 한국의 가동률은 2000~2003 네 해뿐이다. 「최근 가동률은 한국이 …」라고 쓸 수 없다.
  ## 숨기지 않고, 마지막이 언제인지를 «말한다».
  ko_occ <- occ[occ$국가 == BM_FOCUS, ]
  oth <- occ[occ$국가 != BM_FOCUS, ]
  yo <- max(oth$year)
  cur <- oth[oth$year == yo, ]; cur <- cur[order(-cur$value), ]
  cat(sprintf("--- %d년 병상 가동률(한국 제외): %s\n", yo,
              paste(sprintf("%s %.1f%%", cur$국가, cur$value), collapse = " · ")))
  if (nrow(ko_occ)) {
    cat(sprintf("--- ⚠ 한국 가동률은 %d~%d년 %d개년뿐이다(마지막 %.1f%%) — ",
                min(ko_occ$year), max(ko_occ$year), nrow(ko_occ),
                ko_occ$value[which.max(ko_occ$year)]))
    cat("최근 국제 비교에 한국을 넣을 수 없다.\n")
  }
  cat(sprintf("--- 평균재원일수(한국) %d년 %.1f일 -> %d년 %.1f일\n",
              min(alos$year), alos$value[which.min(alos$year)],
              max(alos$year), alos$value[which.max(alos$year)]))

  save_step(list(bed = bed, doc = doc, occ = occ, alos = alos), name = "benchmark")
  invisible(list(bed = bed, doc = doc, occ = occ, alos = alos))
}
