##################################################################
#####  07_view_emergency.R - the emergency axis              #####
##################################################################
##
## Two sources that count DIFFERENT things, kept apart on purpose:
##
##  (a) 심평원 DT_HIRA49_2 - physical emergency-room beds a 요양기관 declares.
##      Quarterly since 2013Q2, broken down by institution tier.
##  (b) 중앙응급의료센터 DT_41104_411 - institutions holding a statutory
##      emergency designation (권역 / 전문 / 지역센터 / 지역기관 / 응급의료시설).
##      Annual 2014-2024, by province.
##
## A hospital can run an emergency room without holding a designation, and the
## designation tiers are a legal category rather than a capacity measure. Never
## divide one by the other.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

ER_TIERS  <- c("상급종합병원", "종합병원", "병원", "요양병원")
ER_LEVELS <- c("권역응급의료센터", "전문응급의료센터", "지역응급의료센터",
               "지역응급의료기관", "응급의료시설")

view_emergency <- function(panel) {
  cat("=== 07_view_emergency.R (응급) ===\n")

  ##################################################################
  #####  (a) emergency-room beds, quarterly                    #####
  ##################################################################

  er <- panel_pick(panel, "er_bed",
                   axis = list(특수진료실구분별 = "응급실",
                               요양기관종별 = c("계", ER_TIERS)),
                   item = c("병상수", "병실수"))
  er$종별 <- er$요양기관종별
  er$분기 <- er$year + (er$quarter - 1) / 4

  beds_w <- tidyr::pivot_wider(
    er[er$itm_nm == "병상수", c("prd_de", "year", "quarter", "종별", "value")],
    names_from = "종별", values_from = "value")
  rooms_w <- tidyr::pivot_wider(
    er[er$itm_nm == "병실수", c("prd_de", "year", "quarter", "종별", "value")],
    names_from = "종별", values_from = "value")
  write_table(list(응급실_병상수 = beds_w, 응급실_병실수 = rooms_w), "t7_응급실_병상")

  pd <- er[er$itm_nm == "병상수" & er$종별 %in% ER_TIERS, ]
  pd$종별 <- factor(pd$종별, levels = ER_TIERS)
  p7 <- ggplot2::ggplot(pd, ggplot2::aes(분기, value, colour = 종별, linetype = 종별)) +
    ggplot2::geom_line(linewidth = 0.85) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = "요양기관 종별 응급실 병상 수 (분기)",
                  x = NULL, y = "응급실 병상 수",
                  caption = "자료: KOSIS 심평원 요양기관 종별 특수진료실 현황, 2013년 2분기~.") +
    theme_hrm()
  save_fig(p7, "fig7_응급실_병상")

  ##################################################################
  #####  (b) statutory designations, annual                    #####
  ##################################################################

  ef <- panel_pick(panel, "er_facility",
                   axis = list(응급의료기관유형별 = c("계", ER_LEVELS),
                               지역별 = "*"), drop_na = FALSE)
  ef$유형 <- ef$응급의료기관유형별
  nat <- ef[ef$지역별 == "전체", ]
  nat_w <- tidyr::pivot_wider(nat[, c("year", "유형", "value")],
                              names_from = "유형", values_from = "value")

  ## An empty cell here is a real zero, not a gap: gate_region_sums confirms the
  ## 17 provinces add exactly to the national row with the blanks treated as 0.
  cur_year <- max(ef$year)
  sido_w <- ef[ef$year == cur_year & !is.na(ef$sido), ]
  sido_w$value[is.na(sido_w$value)] <- 0
  sido_w <- tidyr::pivot_wider(sido_w[, c("sido", "유형", "value")],
                               names_from = "유형", values_from = "value")
  sido_w <- sido_w[order(-sido_w$계), ]
  write_table(list(전국_연도별 = nat_w,
                   시도별 = sido_w), "t8_응급의료기관_지정")

  pl <- nat[nat$유형 %in% ER_LEVELS, ]
  pl$value[is.na(pl$value)] <- 0
  pl$유형 <- factor(pl$유형, levels = ER_LEVELS)
  p8 <- ggplot2::ggplot(pl, ggplot2::aes(year, value, colour = 유형, linetype = 유형)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.3) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
    ggplot2::labs(title = "응급의료기관 지정 현황, 유형별 운영기관 수",
                  x = NULL, y = "기관 수 (개소)",
                  caption = paste0("자료: KOSIS 국립중앙의료원 응급의료기관 및 응급의료시설 ",
                                   "운영기관 수. 법정 지정 등급이며 (a)의 병상 수와 다른 개념.")) +
    theme_hrm()
  save_fig(p8, "fig8_응급의료기관_지정")

  b0 <- er[er$itm_nm == "병상수" & er$종별 == "병원", ]
  cat(sprintf("--- 응급실 병상 %s->%s: 상급종합 %.0f->%.0f, 종합병원 %.0f->%.0f, 병원 %.0f->%.0f\n",
              min(er$prd_de), max(er$prd_de),
              beds_w$상급종합병원[1], beds_w$상급종합병원[nrow(beds_w)],
              beds_w$종합병원[1], beds_w$종합병원[nrow(beds_w)],
              b0$value[1], b0$value[nrow(b0)]))
  invisible(list(beds = beds_w, designation = nat_w, sido = sido_w))
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  view_emergency(panel)
}
