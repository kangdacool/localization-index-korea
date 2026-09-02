##################################################################
#####  04_view_volume.R - domain 1: how much per head        #####
##################################################################
##
## The first thing a resource description has to answer, and the one an
## absolute count cannot: how much capacity per person, and which way is it
## moving. Everything here is a national annual series so it lines up with the
## population denominator and with OECD conventions (per 1,000 population).

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

view_volume <- function(panel) {
  cat("=== 04_view_volume.R (domain 1: 양) ===\n")

  ## Quarterly stocks -> one value per year (Q4, or the newest quarter of a
  ## year still in progress; annualise() records which in q_used).
  beds <- annualise(panel_pick(panel, "bed_tier",
                         axis = list(입원실현황별 = "계", 요양기관종별 = "계"),
                         item = "병상수"))
  docs <- annualise(panel_pick(panel, "hr_doctor",
                         axis = list(의료인력별 = "의사", 요양기관종별 = "전체"),
                         item = "인력현황"))
  nurs <- annualise(panel_pick(panel, "hr_nurse",
                         axis = list(인력현황별 = "간호사", 요양기관종별 = "계"),
                         item = "인력현황"))
  pop  <- panel_pick(panel, "pop_sido", axis = list(행정구역별 = "전국"), item = "계")

  d <- Reduce(function(a, b) merge(a, b, by = "year", all = TRUE), list(
    data.frame(year = beds$year, 병상수 = beds$value, q_used = beds$q_used),
    data.frame(year = docs$year, 의사수 = docs$value),
    data.frame(year = nurs$year, 간호사수 = nurs$value),
    data.frame(year = pop$year,  인구 = pop$value)
  ))
  ## Density is only defined where the denominator exists; the population table
  ## ends a year before the resource tables, so those rows stay blank rather
  ## than being carried forward.
  d$인구천명당_병상 <- round(1000 * d$병상수   / d$인구, 2)
  d$인구천명당_의사 <- round(1000 * d$의사수   / d$인구, 2)
  d$인구천명당_간호사 <- round(1000 * d$간호사수 / d$인구, 2)
  d <- d[order(d$year), ]

  write_table(list(밀도 = d), "t1_밀도_전국")

  long <- tidyr::pivot_longer(
    d[!is.na(d$인구), c("year", "인구천명당_병상", "인구천명당_의사", "인구천명당_간호사")],
    -year, names_to = "지표", values_to = "값")
  long <- long[!is.na(long$값), ]
  long$지표 <- factor(long$지표,
                      levels = c("인구천명당_병상", "인구천명당_간호사", "인구천명당_의사"),
                      labels = c("병상", "간호사", "의사"))

  p <- ggplot2::ggplot(long, ggplot2::aes(year, 값, colour = 지표, linetype = 지표)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.4) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(
      title = "인구 1,000명당 의료자원, 전국",
      x = NULL, y = "인구 1,000명당",
      caption = paste0("자료: KOSIS 심평원 요양기관 종별 입원실·의료인력 현황(연 4분기 기준), ",
                       "행정안전부 주민등록인구. 분모가 있는 연도만 표시.")) +
    theme_hrm()
  save_fig(p, "fig1_밀도_전국")

  ## A one-line summary of the tension the whole project exists to describe.
  last <- d[!is.na(d$인구천명당_병상), ]
  last <- last[nrow(last), ]
  first <- d[!is.na(d$인구천명당_병상), ][1, ]
  cat(sprintf("--- %d -> %d: 병상 %.2f -> %.2f, 의사 %.2f -> %.2f (인구 1,000명당)\n",
              first$year, last$year, first$인구천명당_병상, last$인구천명당_병상,
              first$인구천명당_의사, last$인구천명당_의사))
  invisible(d)
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  view_volume(panel)
}
