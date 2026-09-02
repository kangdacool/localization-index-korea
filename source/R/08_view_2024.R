##################################################################
#####  08_view_2024.R - the 2024 break, on a 15-year trend   #####
##################################################################
##
## The lab's manual HIRA exports hold two points, 2022Q4 and 2024Q4, so they can
## say tertiary-hospital doctor counts fell and nothing more. DT_HIRA44 carries
## 인턴 and 레지던트 as separate categories, by institution tier, every quarter
## back to 2009Q1 - which turns "it fell" into when, how far, whether the fall
## was in trainees or in specialists, and whether it has recovered.
##
## Panel A is the level. Panel B indexes capacity measures to 2023Q4 = 100 so a
## staffing shock and a bed change can be read on one scale; the index is a
## presentation device and every underlying count is in the table.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

BASE_PRD <- "202304"   # last full quarter before the 2024 resident walkout

view_2024 <- function(panel) {
  cat("=== 08_view_2024.R (2024 전환점) ===\n")

  hr <- panel_pick(panel, "hr_doctor",
                   axis = list(의료인력별 = c("의사", "전문의", "레지던트", "인턴"),
                               요양기관종별 = c("상급종합병원", "종합병원")),
                   item = "인력현황")
  bed <- panel_pick(panel, "bed_tier",
                    axis = list(입원실현황별 = "계", 요양기관종별 = "상급종합병원"),
                    item = "병상수")
  erb <- panel_pick(panel, "er_bed",
                    axis = list(특수진료실구분별 = "응급실", 요양기관종별 = "상급종합병원"),
                    item = "병상수")

  mk <- function(d, nm) data.frame(prd_de = d$prd_de, year = d$year, quarter = d$quarter,
                                   series = nm, value = d$value, stringsAsFactors = FALSE)
  sup <- hr[hr$요양기관종별 == "상급종합병원", ]
  long <- rbind(
    mk(sup[sup$의료인력별 == "전문의", ],   "상급종합 전문의"),
    mk(sup[sup$의료인력별 == "레지던트", ], "상급종합 레지던트"),
    mk(sup[sup$의료인력별 == "인턴", ],     "상급종합 인턴"),
    mk(bed, "상급종합 병상"),
    mk(erb, "상급종합 응급실 병상")
  )
  long$분기 <- long$year + (long$quarter - 1) / 4

  wide <- tidyr::pivot_wider(long[, c("prd_de", "year", "quarter", "series", "value")],
                             names_from = "series", values_from = "value")
  wide <- wide[order(wide$prd_de), ]

  ## Index to the last quarter before the break. Series that start later than
  ## the base would have no anchor; all five here begin well before it.
  base <- long[long$prd_de == BASE_PRD, c("series", "value")]
  names(base)[2] <- "base"
  if (nrow(base) != length(unique(long$series))) {
    stop("08_view_2024: base quarter ", BASE_PRD, " missing for ",
         paste(setdiff(unique(long$series), base$series), collapse = ", "))
  }
  idx <- merge(long, base, by = "series")
  idx$index <- 100 * idx$value / idx$base
  wide_idx <- tidyr::pivot_wider(
    idx[, c("prd_de", "series", "index")], names_from = "series", values_from = "index")
  wide_idx[-1] <- lapply(wide_idx[-1], function(x) round(x, 1))
  wide_idx <- wide_idx[order(wide_idx$prd_de), ]

  write_table(list(실수 = wide, 지수_2023Q4_100 = wide_idx), "t9_2024전환점")

  ##################################################################
  #####  panel A: levels                                       #####
  ##################################################################

  lv <- long[long$series %in% c("상급종합 전문의", "상급종합 레지던트", "상급종합 인턴"), ]
  lv$series <- factor(lv$series, levels = c("상급종합 전문의", "상급종합 레지던트",
                                            "상급종합 인턴"))
  pa <- ggplot2::ggplot(lv, ggplot2::aes(분기, value, colour = series, linetype = series)) +
    ggplot2::geom_vline(xintercept = 2024, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::annotate("text", x = 2024.1, y = Inf, hjust = 0, vjust = 1.6,
                      label = "2024", size = 3.4, colour = "grey35",
                      family = KO_FAMILY) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(9)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = "상급종합병원 의사 인력, 등급별 (분기)",
                  x = NULL, y = "인원 (명)",
                  caption = "자료: KOSIS 심평원 요양기관 종별 의료인력 현황, 2009년 1분기~.") +
    theme_hrm()
  save_fig(pa, "fig9_상급종합_인력등급")

  ##################################################################
  #####  panel B: indexed capacity                             #####
  ##################################################################

  ix <- idx[idx$prd_de >= "202001", ]
  ix$series <- factor(ix$series, levels = c("상급종합 레지던트", "상급종합 인턴",
                                            "상급종합 전문의", "상급종합 병상",
                                            "상급종합 응급실 병상"))
  pb <- ggplot2::ggplot(ix, ggplot2::aes(분기, index, colour = series, linetype = series)) +
    ggplot2::geom_hline(yintercept = 100, colour = "grey75", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 2024, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.9) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(7)) +
    ggplot2::labs(title = "상급종합병원 인력·병상, 2023년 4분기 = 100",
                  x = NULL, y = "지수 (2023Q4 = 100)",
                  caption = paste0("자료: KOSIS 심평원 의료인력·입원실·특수진료실 현황. ",
                                   "실수는 t9_2024전환점 표에 있다.")) +
    theme_hrm()
  save_fig(pb, "fig10_상급종합_지수")

  ## Report the trough and the latest value for each series, from the data.
  for (s in levels(ix$series)) {
    g <- idx[idx$series == s & idx$prd_de >= BASE_PRD, ]
    g <- g[order(g$prd_de), ]
    tr <- g[which.min(g$index), ]
    cat(sprintf("--- %-20s 저점 %s %.1f  최신 %s %.1f\n", s,
                tr$prd_de, tr$index, g$prd_de[nrow(g)], g$index[nrow(g)]))
  }
  invisible(list(levels = wide, index = wide_idx))
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  view_2024(panel)
}
