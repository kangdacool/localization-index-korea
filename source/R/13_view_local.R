##################################################################
#####  13_view_local.R - 지역 단위로 내려가면 보이는 것       #####
##################################################################
##
## ④ 접근성을 시도에서 시군구·중진료권으로 내린다. KOSIS 기반 ④는 시도 17개 ×
## 진료비 하나였는데, 여기서는 서비스 10종 × 250 시군구가 된다.
##
## ⛔ 2023년 단면이다. 이 절의 어떤 수치도 «추세»로 읽지 않는다.
##
## ⚠ 관내의료이용률 0%는 «주민이 안 쓴다»가 아니라 대개 «그 지역에 그 시설이
## 없다»는 뜻이다. 상급종합병원은 250개 시군구 중 39곳에만 있으므로 나머지는
## 구조적으로 0이고, 그래서 중앙값이 0으로 나온다. 이 절을 «중증도에 따른 기울기»로
## 읽으면 틀린다 — 그것은 시설이 어디 있는지의 지도다. 그래서 0인 시군구 수를
## 그림에 함께 싣고, 분포는 0을 뺀 곳에서 따로 본다.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_healthmap")) source(file.path(r_dir, "12_healthmap.R"))

view_local <- function(hm) {
  cat("=== 13_view_local.R (지역 단위, 2023) ===\n")
  ind <- hm$indicators
  und <- hm$underserved

  ##################################################################
  #####  5a. 중증도에 따른 자체충족률 기울기                   #####
  ##################################################################

  loc <- ind[ind$measure == "관내의료이용률" & ind$level == "시군구" & !is.na(ind$value), ]

  ## Two numbers per service, because one cannot carry it: how many districts
  ## have none of this service at all, and - where it exists - how much of the
  ## demand stays home.
  zero <- stats::aggregate(cbind(n = value) ~ service, data = loc,
                           FUN = function(v) sum(v == 0))
  names(zero)[2] <- "n_zero"
  pos <- loc[loc$value > 0, ]
  med <- stats::aggregate(value ~ service, data = pos, FUN = stats::median)
  names(med)[2] <- "med_pos"
  sm <- merge(zero, med, by = "service", all = TRUE)
  sm <- sm[order(sm$n_zero, -sm$med_pos), ]

  wide <- tidyr::pivot_wider(
    ind[ind$measure == "관내의료이용률" & !is.na(ind$value),
        c("level", "label", "sido", "service", "value")],
    names_from = "service", values_from = "value")
  write_table(list(관내의료이용률 = wide, 서비스별_요약 = sm), "t12_지역_관내의료이용률")

  pos$service <- factor(pos$service, levels = sm$service)
  lab <- sm
  lab$service <- factor(lab$service, levels = sm$service)
  lab$txt <- paste0("시설 없는 시군구 ", lab$n_zero, "곳")

  p14 <- ggplot2::ggplot(pos, ggplot2::aes(service, value)) +
    ggplot2::geom_boxplot(outlier.size = 0.7, outlier.colour = "grey55",
                          fill = ACCENT_FILL, colour = ACCENT, width = 0.62,
                          linewidth = 0.4) +
    ggplot2::geom_text(data = lab, ggplot2::aes(y = 101, label = txt),
                       hjust = 0, size = 3.1, colour = "grey40", family = KO_FAMILY) +
    ggplot2::coord_flip(ylim = c(0, 100), clip = "off") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5)) +
    ggplot2::labs(
      title = "중증으로 갈수록, 지역에 시설 자체가 없다",
      subtitle = "관내의료이용률이 0%가 아닌 시군구에서의 분포, 2023년 (오른쪽은 0%인 곳의 수)",
      x = NULL, y = "관내의료이용률 (%)",
      caption = "자료: 국립중앙의료원 헬스맵. 0%는 대개 그 지역에 해당 시설이 없다는 뜻이므로 분포에서 빼고 따로 셌다.") +
    theme_hrm(11.5) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 132, 6, 6))
  save_fig(p14, "fig14_지역_자체충족_기울기", width = 10, height = 6)

  ##################################################################
  #####  5b. 기준시간 내에 닿는가                              #####
  ##################################################################

  tm <- ind[ind$measure == "기준시간내의료이용률" & ind$level == "시군구" &
            !is.na(ind$value), ]
  tm$lab <- paste0(tm$service, " (", tm$mins, "분)")
  tmed <- stats::aggregate(value ~ lab, data = tm, FUN = stats::median)
  tm$lab <- factor(tm$lab, levels = tmed$lab[order(-tmed$value)])

  p15 <- ggplot2::ggplot(tm, ggplot2::aes(lab, value)) +
    ggplot2::geom_boxplot(outlier.size = 0.7, outlier.colour = "grey55",
                          fill = ACCENT_FILL, colour = ACCENT, width = 0.62,
                          linewidth = 0.4) +
    ggplot2::coord_flip(ylim = c(0, 100)) +
    ggplot2::labs(
      title = "기준시간 안에 닿는 비율",
      subtitle = "250개 시군구, 2023년. 괄호 안은 그 서비스에 적용된 기준시간",
      x = NULL, y = "기준시간내의료이용률 (%)",
      caption = "자료: 국립중앙의료원 헬스맵. 응급실 30분·권역응급의료센터 60분·상급종합병원 180분처럼 서비스마다 기준이 다르므로 서로 직접 비교하지 않는다.") +
    theme_hrm(11.5)
  save_fig(p15, "fig15_지역_기준시간내", width = 9, height = 5.5)

  ##################################################################
  #####  5c. 의료취약지                                        #####
  ##################################################################

  TY <- c("분만", "소아청소년과", "인공신장실", "응급")
  und$n_type <- rowSums(und[, TY])
  long <- tidyr::pivot_longer(und[, c("sido", "sgg_nm", TY)], all_of(TY),
                              names_to = "유형", values_to = "flag")
  agg <- stats::aggregate(flag ~ sido + 유형, data = long, FUN = sum)
  ord <- stats::aggregate(flag ~ sido, data = agg, FUN = sum)
  agg$sido <- factor(agg$sido, levels = ord$sido[order(ord$flag)])
  agg$유형 <- factor(agg$유형, levels = TY)

  write_table(list(취약지_시군구 = und[order(-und$n_type, und$sido), ],
                   시도별_집계 = tidyr::pivot_wider(agg, names_from = "유형",
                                                    values_from = "flag")),
              "t13_의료취약지")

  p16 <- ggplot2::ggplot(agg, ggplot2::aes(sido, flag)) +
    ggplot2::geom_col(fill = ACCENT, width = 0.72) +
    ggplot2::facet_wrap(~유형, nrow = 1) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(4)) +
    ggplot2::labs(
      title = "의료취약지로 판정된 시군구, 2023년",
      subtitle = paste0("전국 250개 시군구 중 분만 ", sum(und$분만), " · 소아청소년과 ",
                        sum(und$소아청소년과), " · 인공신장실 ", sum(und$인공신장실),
                        " · 응급 ", sum(und$응급)),
      x = NULL, y = "시군구 수",
      caption = "자료: 국립중앙의료원 헬스맵 의료취약지 선정 결과.") +
    theme_hrm(11)
  save_fig(p16, "fig16_의료취약지", width = 9.5, height = 5.5)

  ## 무엇이 가장 값나가는 한 줄인가: 중복 취약
  multi <- und[und$n_type >= 3, ]
  g <- function(sv, col) sm[[col]][sm$service == sv]
  cat(sprintf("--- 시설 없는(관내 0%%) 시군구: 전체 병원 %d곳 / 상급종합 %d곳 / 권역응급 %d곳 (250개 중)\n",
              g("전체 병원", "n_zero"), g("상급종합병원", "n_zero"),
              g("권역응급의료센터", "n_zero")))
  cat(sprintf("--- 있는 곳에서의 관내이용률 중앙값: 전체 병원 %.1f%% / 상급종합 %.1f%% / 권역응급 %.1f%%\n",
              g("전체 병원", "med_pos"), g("상급종합병원", "med_pos"),
              g("권역응급의료센터", "med_pos")))
  cat(sprintf("--- 3종 이상 동시 취약 시군구 %d개: %s\n", nrow(multi),
              paste0(multi$sido, " ", multi$sgg_nm, collapse = ", ")))
  invisible(list(local = wide, underserved = und, summary = sm))
}

if (sys.nframe() == 0L) {
  hm <- load_step("^healthmap_")
  view_local(hm)
}
