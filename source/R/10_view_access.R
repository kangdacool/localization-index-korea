##################################################################
#####  10_view_access.R - domain 4: can people get there?    #####
##################################################################
##
## Density says how much capacity sits in a province. It does not say whether
## the people living there use it. Two ratios, from two tables that count
## different things, answer that:
##
##   자체충족률 (환자 거주지 기준) = 관내 / (관내 + 관외)
##       Of all the care this province's RESIDENTS received, how much happened
##       inside the province. Low = residents travel out.
##
##   환자유입률 (의료기관 소재지 기준) = 관외 / (관내 + 관외)
##       Of all the care this province's INSTITUTIONS delivered, how much went
##       to non-residents. High = the province draws patients in.
##
## They are not complements. 서울 2024: residents got 89% of their care at home
## while 35% of what 서울's institutions delivered went to people from elsewhere.
## Verified basis: TX_35003_A004 = A005 + A006 exactly (residence), while
## DT_35003_A0072 puts 서울's outside-region figure 4.6x higher (institution).
##
## ⚠ 2006 and 2011 are flagged: KOSIS's own 전체 table disagrees with 관내+관외
## in those two vintages (up to 7.2%), so the ratio is less trustworthy there.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

ACCESS_ITEM <- "진료비"   # 입내원일수도 같은 구조로 뽑힌다

view_access <- function(panel) {
  cat("=== 10_view_access.R (domain 4: 접근성) ===\n")

  grab <- function(key, levels = c("계", "입원", "외래")) {
    d <- panel_pick(panel, key,
                    axis = list(시도별 = "*", 입원및외래별 = levels),
                    item = ACCESS_ITEM)
    d <- d[!is.na(d$sido), ]
    data.frame(year = d$year, sido = d$sido, 구분 = d$입원및외래별,
               value = d$value, stringsAsFactors = FALSE)
  }

  ##################################################################
  #####  4a. 자체충족률 - residents staying in                 #####
  ##################################################################

  res <- merge(setNames(grab("care_in_res"),  c("year", "sido", "구분", "관내")),
               setNames(grab("care_out_res"), c("year", "sido", "구분", "관외")),
               by = c("year", "sido", "구분"))
  res$자체충족률 <- 100 * res$관내 / (res$관내 + res$관외)
  res$원자료_불일치 <- ifelse(res$year %in% ACCESS_MISMATCH_YEARS, "Y", "")
  res <- res[order(res$구분, res$sido, res$year), ]
  write_table(list(자체충족률 = res), "t10_접근성_자체충족률")

  ##################################################################
  #####  4b. 환자유입률 - institutions drawing outsiders in    #####
  ##################################################################

  ins <- merge(setNames(grab("care_in_inst"),  c("year", "sido", "구분", "관내")),
               setNames(grab("care_out_inst"), c("year", "sido", "구분", "관외")),
               by = c("year", "sido", "구분"))
  ins$환자유입률 <- 100 * ins$관외 / (ins$관내 + ins$관외)
  ins <- ins[order(ins$구분, ins$sido, ins$year), ]
  write_table(list(환자유입률 = ins), "t11_접근성_환자유입률")

  ##################################################################
  #####  fig11: the provincial picture, latest year            #####
  ##################################################################

  yr <- max(res$year)
  cur <- res[res$year == yr & res$구분 %in% c("입원", "외래"), ]
  ord <- cur$sido[cur$구분 == "입원"][order(cur$자체충족률[cur$구분 == "입원"])]
  cur$sido <- factor(cur$sido, levels = ord)
  cur$구분 <- factor(cur$구분, levels = c("입원", "외래"))

  ## 막대는 17개를 다 그린다(숨기지 않는다). 다만 세종만 톤을 낮춰, 본문의 「최저」가
  ## 그 막대를 가리키지 «않는다»는 것이 그림에서도 보이게 한다.
  cur$특수 <- cur$sido %in% HEADLINE_DROP_SIDO
  p11 <- ggplot2::ggplot(cur, ggplot2::aes(sido, 자체충족률, fill = 특수)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = ACCENT, `TRUE` = "#A9C4D6")) +
    ggplot2::geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.3) +
    ggplot2::facet_wrap(~구분, nrow = 1) +
    ggplot2::coord_flip(ylim = c(0, 100)) +
    ggplot2::labs(
      title = paste0("시도별 의료 자체충족률, ", yr, "년"),
      subtitle = "그 지역 주민이 받은 진료 중 지역 안에서 이루어진 비율 (진료비 기준)",
      x = NULL, y = "자체충족률 (%)",
      ## PNG 을 떼어 보는 사람에게도 «옅은 막대가 무엇인지»가 보여야 한다.
      ## 그림이 페이지 밖으로 나가면 본문 주석은 따라가지 않는다.
      caption = paste0("자료: KOSIS 국민건강보험공단 시도별 진료현황 관내/관외(환자 거주지 기준). ",
                       "입원 자체충족률 순으로 정렬했다. 옅은 막대 = ", SEJONG_NOTE)) +
    theme_hrm(11)
  save_fig(p11, "fig11_접근성_자체충족률", width = 9, height = 6.5)

  ##################################################################
  #####  fig12: outflow vs inflow - the referral map           #####
  ##################################################################
  ##
  ## One axis per counting basis. The quadrants are the story: bottom-left is a
  ## province whose residents leave and whose hospitals draw nobody.

  yr2 <- min(max(res$year), max(ins$year))
  sc <- merge(res[res$year == yr2 & res$구분 == "입원", c("sido", "자체충족률")],
              ins[ins$year == yr2 & ins$구분 == "입원", c("sido", "환자유입률")],
              by = "sido")
  p12 <- ggplot2::ggplot(sc, ggplot2::aes(자체충족률, 환자유입률)) +
    ggplot2::geom_point(size = 2.2, colour = ACCENT) +
    ## Repelled, not nudged: 대전 and 광주 land within a point radius of each
    ## other and a fixed vjust printed one Korean label on top of the other -
    ## legible in neither, and invisible to every check except looking at it.
    ggrepel::geom_text_repel(ggplot2::aes(label = sido), family = KO_FAMILY,
                             size = 3.3, colour = "grey25", seed = 1,
                             min.segment.length = 0.2, segment.colour = "grey65",
                             box.padding = 0.45, max.overlaps = Inf) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.08)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::labs(
      title = paste0("주민은 나가는가, 환자는 들어오는가 - 입원 기준, ", yr2, "년"),
      subtitle = "가로: 주민이 지역 안에서 해결한 비율 / 세로: 지역 기관 진료 중 외지 주민 몫",
      x = "자체충족률 (%, 환자 거주지 기준)",
      y = "환자유입률 (%, 의료기관 소재지 기준)",
      caption = paste0("두 축은 서로 다른 표에서 오고 서로의 여집합이 아니다. ",
                       "자료: KOSIS 국민건강보험공단 시도별/의료기관 시도별 진료현황.")) +
    theme_hrm()
  save_fig(p12, "fig12_접근성_유출입", width = 8.5, height = 6)

  ##################################################################
  #####  fig13: is self-sufficiency drifting?                  #####
  ##################################################################

  ## ⚠ 세종을 두면 2012년부터 「최저」 선이 사실상 «세종 한 곳의 선»이 된다 —
  ## 시도 간 «폭»을 보려는 그림인데 한 특수 사례의 궤적을 그리게 된다.
  ts <- drop_headline_sido(res[res$구분 == "입원" &
                               !res$year %in% ACCESS_MISMATCH_YEARS, ])
  span <- do.call(rbind, lapply(split(ts, ts$year), function(g) data.frame(
    year = g$year[1],
    최고 = max(g$자체충족률), 중앙 = stats::median(g$자체충족률),
    최저 = min(g$자체충족률))))
  spanL <- tidyr::pivot_longer(span, -year, names_to = "구분", values_to = "값")
  spanL$구분 <- factor(spanL$구분, levels = c("최고", "중앙", "최저"))
  p13 <- ggplot2::ggplot(spanL, ggplot2::aes(year, 값, colour = 구분, linetype = 구분)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.2) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(title = "입원 자체충족률의 시도 간 폭",
                  x = NULL, y = "자체충족률 (%)",
                  caption = paste0("시도별 최고·중앙·최저. ",
                                   paste(ACCESS_MISMATCH_YEARS, collapse = "년·"),
                                   "년은 원자료 내부 불일치로 제외했다. ", SEJONG_NOTE)) +
    theme_hrm()
  save_fig(p13, "fig13_접근성_추이")

  s <- drop_headline_sido(sc)[order(drop_headline_sido(sc)$자체충족률), ]
  cat(sprintf("--- %d년 입원 자체충족률: 최저 %s %.1f%% / 최고 %s %.1f%%\n", yr2,
              s$sido[1], s$자체충족률[1], s$sido[nrow(s)], s$자체충족률[nrow(s)]))
  i <- drop_headline_sido(sc)[order(-drop_headline_sido(sc)$환자유입률), ]
  cat(sprintf("--- %d년 입원 환자유입률: 최고 %s %.1f%% / 최저 %s %.1f%%\n", yr2,
              i$sido[1], i$환자유입률[1], i$sido[nrow(i)], i$환자유입률[nrow(i)]))
  invisible(list(residence = res, institution = ins, scatter = sc))
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  view_access(panel)
}
