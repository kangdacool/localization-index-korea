##################################################################
#####  05_view_mix.R - domain 2: what the capacity is made of #####
##################################################################
##
## A total hides the thing that actually moved. Korea's bed count rose for
## fifteen years, but the growth is not where an acute-care reading assumes;
## and a hospital's staffing mix is a different fact from its headcount.
## Three cuts: beds by institution tier, beds by function, doctors by grade.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

TIERS <- c("상급종합병원", "종합병원", "병원", "요양병원", "정신병원", "의원")

view_mix <- function(panel) {
  cat("=== 05_view_mix.R (domain 2: 구성) ===\n")

  ##################################################################
  #####  2a. beds by institution tier                          #####
  ##################################################################

  tier <- annualise(panel_pick(panel, "bed_tier",
                               axis = list(입원실현황별 = "계", 요양기관종별 = c("계", TIERS)),
                               item = "병상수"))
  tier$종별 <- tier$요양기관종별
  wide <- tidyr::pivot_wider(tier[, c("year", "종별", "value")],
                             names_from = "종별", values_from = "value")
  tot <- wide[["계"]]
  share <- wide
  for (tv in TIERS) share[[tv]] <- round(100 * wide[[tv]] / tot, 1)
  names(share)[names(share) == "계"] <- "전체병상"

  write_table(list(병상수 = wide, 점유율_퍼센트 = share), "t2_구성_병상종별")

  ## A tier that only starts part-way along is a reclassification, and the line
  ## it was carved out of drops by the same amount. Mark it on the figure rather
  ## than letting the drop read as closures (detect_series_breaks() logs the
  ## full list; here we just need the one that affects this chart).
  q_all <- sort(unique(panel$prd_de[panel$key == "bed_tier"]))
  brk <- NULL
  for (tv in TIERS) {
    g <- panel_pick(panel, "bed_tier",
                    axis = list(입원실현황별 = "계", 요양기관종별 = tv), item = "병상수")
    if (min(g$prd_de) > q_all[1]) {
      first <- min(g$prd_de)
      prev  <- q_all[which(q_all == first) - 1L]
      donors <- character(0)
      for (dv in setdiff(TIERS, tv)) {
        h <- panel_pick(panel, "bed_tier",
                        axis = list(입원실현황별 = "계", 요양기관종별 = dv), item = "병상수")
        a <- h$value[h$prd_de == prev]; b <- h$value[h$prd_de == first]
        if (length(a) == 1 && length(b) == 1 && a - b > 0.2 * g$value[g$prd_de == first]) {
          donors <- c(donors, sprintf("%s %s", dv,
                      format(-(a - b), big.mark = ",")))
        }
      }
      brk <- list(tier = tv, first = first,
                  year = as.integer(substr(first, 1, 4)) +
                         (as.integer(substr(first, 6, 6)) - 1) / 4,
                  value = g$value[g$prd_de == first], donors = donors)
    }
  }
  cap2 <- "자료: KOSIS 심평원 요양기관 종별 입원실 현황. 각 연도 4분기(2026년은 2분기) 기준."
  if (!is.null(brk)) {
    cap2 <- paste0(cap2, "\n", substr(brk$first, 1, 4), "년 ",
                   substr(brk$first, 6, 6), "분기에 ", brk$tier,
                   "이 별도 종별로 분리됐다(", format(brk$value, big.mark = ","),
                   "병상; 같은 분기 ", paste(brk$donors, collapse = ", "),
                   "). 분리 전후를 같은 계열로 읽지 말 것.")
  }

  plot_d <- tier[tier$종별 %in% TIERS, ]
  plot_d$종별 <- factor(plot_d$종별, levels = TIERS)
  p2 <- ggplot2::ggplot(plot_d, ggplot2::aes(year, value / 1000,
                                             colour = 종별, linetype = 종별)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.2) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(title = "요양기관 종별 병상 수",
                  x = NULL, y = "병상 수 (천 개)", caption = cap2) +
    theme_hrm()
  if (!is.null(brk)) {
    p2 <- p2 + ggplot2::geom_vline(xintercept = brk$year, colour = "grey55",
                                   linewidth = 0.4, linetype = "dotted")
  }
  save_fig(p2, "fig2_병상_종별")

  ##################################################################
  #####  2b. beds by function                                  #####
  ##################################################################

  FUNCS <- c("일반입원실", "중환자실", "격리병실", "정신과개방", "정신과폐쇄",
             "무균치료실", "임종실")
  fn <- annualise(panel_pick(panel, "bed_tier",
                             axis = list(입원실현황별 = FUNCS, 요양기관종별 = "계"),
                             item = "병상수"))
  fn$기능 <- fn$입원실현황별
  fn_wide <- tidyr::pivot_wider(fn[, c("year", "기능", "value")],
                                names_from = "기능", values_from = "value")
  write_table(list(기능별_병상수 = fn_wide), "t3_구성_병상기능")

  ## Intensive-care and isolation capacity are the policy-relevant slices and
  ## are two orders of magnitude below the general ward, so they get their own
  ## panel rather than being flattened against it.
  focus <- fn[fn$기능 %in% c("중환자실", "격리병실"), ]
  p3 <- ggplot2::ggplot(focus, ggplot2::aes(year, value, colour = 기능, linetype = 기능)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.3) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = "중환자실·격리병실 병상 수, 전체 요양기관",
                  x = NULL, y = "병상 수",
                  caption = "자료: KOSIS 심평원 요양기관 종별 입원실 현황.") +
    theme_hrm()
  save_fig(p3, "fig3_병상_기능별")

  ##################################################################
  #####  2c. doctors by grade                                  #####
  ##################################################################
  ##
  ## 전문의 : 전공의 is the staffing structure of a teaching hospital. The
  ## registry qualifies these as 의사>전문의 etc. because the bare names repeat
  ## under 치과의사 and 한의사 in the same axis.

  GRADES <- c("전문의", "레지던트", "인턴", "일반의")
  hr <- annualise(panel_pick(panel, "hr_doctor",
                             axis = list(의료인력별 = c("의사", GRADES),
                                      요양기관종별 = c("상급종합병원", "종합병원")),
                             item = "인력현황"))
  hr$등급 <- hr$의료인력별
  hr$종별 <- hr$요양기관종별

  hw <- tidyr::pivot_wider(hr[, c("year", "종별", "등급", "value")],
                           names_from = "등급", values_from = "value")
  hw$전공의 <- hw$인턴 + hw$레지던트
  hw$전문의_대_전공의 <- round(hw$전문의 / hw$전공의, 2)
  hw <- hw[order(hw$종별, hw$year), ]
  write_table(list(인력구성 = hw), "t4_구성_인력등급")

  p4 <- ggplot2::ggplot(hw, ggplot2::aes(year, 전문의_대_전공의,
                                         colour = 종별, linetype = 종별)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.3) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ## The ratio explodes when the denominator collapses - 2024 reaches ~30x and
    ## flattens fifteen years of history into a line at the bottom. A log axis is
    ## the honest fix for a ratio: it keeps the spike AND the earlier variation.
    ggplot2::scale_y_log10(breaks = c(1, 2, 3, 5, 10, 20, 30)) +
    ggplot2::labs(title = "전문의 대 전공의 비 (전공의 = 인턴 + 레지던트)",
                  x = NULL, y = "전문의 / 전공의 (로그 눈금)",
                  caption = "자료: KOSIS 심평원 요양기관 종별 의료인력 현황. 각 연도 4분기(2026년은 2분기) 기준.") +
    theme_hrm()
  save_fig(p4, "fig4_인력_전문의대전공의")

  invisible(list(tier = wide, share = share, func = fn_wide, grade = hw))
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  view_mix(panel)
}
