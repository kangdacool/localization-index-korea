##################################################################
#####  06_view_distribution.R - domain 3: where it sits      #####
##################################################################
##
## National density says nothing about whether a person can reach a bed. Two
## cuts: the current provincial picture, and whether concentration is getting
## worse over time (population-weighted Gini).
##
## LIMIT, stated here because it constrains every regional claim this project
## can make: KOSIS has no 시도 x 종별 x 병상 table. The bed table with a tier
## axis is national only (DT_HIRA45_1); the bed table with a region axis has no
## tier axis (DT_HIRA45_2). So provincial beds are ALL TIERS COMBINED, and the
## only thing available by province AND tier is the institution COUNT
## (DT_MIRE01). Do not write "상급종합병원 병상의 시도별 배치" from this data.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

## Population-weighted Gini: every province counts in proportion to the people
## living in it, so 세종 does not swing the index as hard as 경기.
gini_w <- function(y, w) {
  ok <- !is.na(y) & !is.na(w) & w > 0
  y <- y[ok]; w <- w[ok]
  if (length(y) < 2) return(NA_real_)
  o <- order(y); y <- y[o]; w <- w[o]
  ybar <- sum(w * y) / sum(w)
  num <- sum(outer(w, w) * abs(outer(y, y, "-")))
  num / (2 * sum(w)^2 * ybar)
}

view_distribution <- function(panel) {
  cat("=== 06_view_distribution.R (domain 3: 분포) ===\n")

  beds <- annualise(panel_pick(panel, "bed_sido",
                               axis = list(입원실현황별 = "계"), item = "병상수"))
  docs <- annualise(panel_pick(panel, "hr_sido",
                               axis = list(의료인력별 = c("의사", "전문의")),
                               item = "인력현황"))
  inst <- annualise(panel_pick(panel, "inst_sido",
                               axis = list(요양기관종별 = c("상급종합병원", "종합병원"))))
  pop  <- panel_pick(panel, "pop_sido", axis = list(행정구역별 = "전국"), item = "계",
                     drop_na = TRUE)
  ## build_panel() already canonicalised every province label into panel$sido,
  ## so nothing here re-parses 서울특별시 / 강원특별자치도 spellings.
  pop_all <- panel_pick(panel, "pop_sido", axis = list(행정구역별 = "*"), item = "계")

  mk <- function(d, val_name) {
    d <- d[!is.na(d$sido), ]
    data.frame(year = d$year, sido = d$sido, value = d$value,
               what = val_name, stringsAsFactors = FALSE)
  }
  parts <- list(
    mk(beds, "병상"),
    mk(docs[docs$의료인력별 == "의사", ], "의사"),
    mk(docs[docs$의료인력별 == "전문의", ], "전문의"),
    mk(inst[inst$요양기관종별 == "상급종합병원", ], "상급종합병원수"),
    mk(inst[inst$요양기관종별 == "종합병원", ], "종합병원수")
  )
  long <- do.call(rbind, parts)
  popd <- data.frame(year = pop_all$year, sido = pop_all$sido, 인구 = pop_all$value)
  popd <- popd[!is.na(popd$sido), ]
  long <- merge(long, popd, by = c("year", "sido"))
  long$per_1k <- 1000 * long$value / long$인구

  ##################################################################
  #####  3a. the current provincial picture                    #####
  ##################################################################

  yr <- max(long$year[long$what == "병상"])
  yr <- min(yr, max(long$year[long$what == "의사"]))
  cur <- long[long$year == yr, ]
  wide <- tidyr::pivot_wider(cur[, c("sido", "what", "value")],
                             names_from = "what", values_from = "value")
  dens <- tidyr::pivot_wider(cur[cur$what %in% c("병상", "의사", "전문의"),
                                 c("sido", "what", "per_1k")],
                             names_from = "what", values_from = "per_1k")
  names(dens)[-1] <- paste0("인구천명당_", names(dens)[-1])
  out <- merge(wide, dens, by = "sido")
  out <- merge(out, popd[popd$year == yr, c("sido", "인구")], by = "sido")
  out <- out[order(-out$인구천명당_병상), ]
  out[] <- lapply(out, function(x) if (is.numeric(x)) round(x, 2) else x)
  write_table(setNames(list(out), paste0("시도_", yr)), "t5_분포_시도")

  ## Beds and doctors are not the same map. Both panels below are ordered by
  ## bed density, so the doctor panel reading as unsorted IS the finding; the
  ## rank correlation puts a number on it.
  bd <- cur[cur$what == "병상", c("sido", "per_1k")]
  dc <- cur[cur$what == "의사", c("sido", "per_1k")]
  names(bd)[2] <- "bed_1k"; names(dc)[2] <- "doc_1k"
  bdc <- merge(bd, dc, by = "sido")
  rho <- suppressWarnings(stats::cor(bdc$bed_1k, bdc$doc_1k, method = "spearman"))

  bar <- cur[cur$what %in% c("병상", "의사"), ]
  bar$what <- factor(bar$what, levels = c("병상", "의사"),
                     labels = c("인구 1,000명당 병상", "인구 1,000명당 의사"))
  bar$sido <- factor(bar$sido, levels = out$sido)
  p5 <- ggplot2::ggplot(bar, ggplot2::aes(sido, per_1k)) +
    ggplot2::geom_col(fill = ACCENT, width = 0.72) +
    ggplot2::facet_wrap(~what, scales = "free_x", nrow = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = paste0("시도별 의료자원 밀도, ", yr, "년"),
                  x = NULL, y = "인구 1,000명당",
                  caption = paste0("두 패널 모두 병상 밀도 순으로 정렬했다. ",
                                   "시도 간 병상 밀도와 의사 밀도의 순위상관 rho = ",
                                   formatC(rho, format = "f", digits = 2), ".\n",
                                   "자료: KOSIS 심평원 시군구별 입원실·의료인력 현황(시도 집계), ",
                                   "행정안전부 주민등록인구. ",
                                   "병상은 요양기관 종별 구분 없는 합계 - KOSIS에 시도x종별 병상표가 없다.")) +
    theme_hrm(11)
  save_fig(p5, "fig5_분포_시도", width = 8, height = 8)

  ##################################################################
  #####  3b. is concentration widening?                        #####
  ##################################################################

  gin <- do.call(rbind, lapply(split(long, list(long$year, long$what), drop = TRUE),
                               function(g) data.frame(
                                 year = g$year[1], what = g$what[1],
                                 gini = gini_w(g$per_1k, g$인구))))
  gin <- gin[gin$what %in% c("병상", "의사", "전문의"), ]
  gin <- gin[order(gin$what, gin$year), ]
  gin$gini <- round(gin$gini, 4)
  write_table(list(인구가중_지니 = tidyr::pivot_wider(gin, names_from = "what",
                                                      values_from = "gini")),
              "t6_분포_지니")

  p6 <- ggplot2::ggplot(gin, ggplot2::aes(year, gini, colour = what, linetype = what)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.3) +
    scale_colour_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(title = "시도 간 자원 밀도의 불평등 (인구가중 지니계수)",
                  x = NULL, y = "지니계수 (0 = 완전 균등)",
                  caption = paste0("17개 시도의 인구 1,000명당 밀도에 대해 인구를 가중치로 계산. ",
                                   "병상 계열은 2018년부터 자료가 있다.")) +
    theme_hrm()
  save_fig(p6, "fig6_분포_지니")

  ## 표(t5)에는 17개 시도가 다 있다. 「최고·최저」라는 «문장»에서만 세종을 뺀다.
  oh <- drop_headline_sido(out)
  cat(sprintf("--- %d년 인구천명당 병상: 최고 %s %.1f / 최저 %s %.1f (%.1f배, 세종 제외)\n", yr,
              oh$sido[1], oh$인구천명당_병상[1],
              oh$sido[nrow(oh)], oh$인구천명당_병상[nrow(oh)],
              oh$인구천명당_병상[1] / oh$인구천명당_병상[nrow(oh)]))
  cat(sprintf("--- 병상 밀도 vs 의사 밀도 시도 순위상관 rho = %.2f (%d년)\n", rho, yr))
  invisible(list(current = out, gini = gin, rho = rho))
}

if (sys.nframe() == 0L) {
  panel <- load_step("^panel_")
  view_distribution(panel)
}
