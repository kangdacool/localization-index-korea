##################################################################
#####  07_sensitivity.R - 심사자가 반드시 묻는 셋            #####
##################################################################
##
## `lit-scan`(2026-09-01)이 국제 형평 저널을 노릴 때 «가장 확실히 지적당한다»고 짚은
## 셋을 선제 처리한다. 결론이 뒤집히지 않음을 보이는 것이 목적이지, 새 주장을 만드는
## 것이 아니다.
##
##   ① **인구 가중** — 230곳을 «각 1표»로 놓으면 옹진군과 수원시가 같은 무게다.
##      선행연구(Kim & Song 2023)는 명시적으로 인구를 가중했고, 형평 저널은 그걸
##      기본으로 본다. 가중·비가중을 «둘 다» 낸다.
##   ② **유계 비율에 타일을 쓴 것** — 타일/GE 계열은 원래 비유계 척도(소득)용이다.
##      0–100% 에 쓰면 상한 근처에서 거동이 왜곡되고, 「90%인 곳은 더 오를 여지가
##      없다」는 천장효과가 불평등 «감소»로 잘못 읽힐 수 있다.
##      → **로짓 변환 후 분산 분해**를 나란히 낸다. 방향이 같으면 방어된다.
##   ③ **균형패널 선택** — 230곳만 쓴 것이 결과를 만들었는지. 불균형(264)으로도 본다.
##
## ⛔ 인구는 1998–2023 이다(우리 패널은 2024까지). **2024는 가중 분석에서 «뺀다»** —
##    2023 인구를 끌어다 쓰면 그 해만 다른 규칙이 되고, 그건 숨은 가정이다.

POP_CSV <- file.path(DATA_ONLY, "kor_pop", "data", "pop_sgg_age_sex_1997_2023.csv")

## 인구 파일의 시군구 이름을 통계연보 형식으로 맞춘다.
## 5자리 코드에서 «부모 시»는 code[:4]+"0" 이다(31010 수원시 -> 31011 장안구).
## 통계연보는 「수원시장안구」로 붙여 쓰므로 같은 형식을 만든다.
load_population <- function() {
  if (!file.exists(POP_CSV))
    stop("07_sensitivity: 시군구 인구가 없다: ", POP_CSV)
  p <- readr::read_csv(POP_CSV, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE)
  p <- p[p$sex_code == "0" & p$age_code == "000", ]
  sido_map <- c("서울특별시"="서울","부산광역시"="부산","대구광역시"="대구","인천광역시"="인천",
                "광주광역시"="광주","대전광역시"="대전","울산광역시"="울산","세종특별자치시"="세종",
                "경기도"="경기","강원도"="강원","강원특별자치도"="강원","충청북도"="충북",
                "충청남도"="충남","전라북도"="전북","전북특별자치도"="전북","전라남도"="전남",
                "경상북도"="경북","경상남도"="경남","제주특별자치도"="제주","제주도"="제주")
  two <- p[nchar(p$sgg_code) == 2 & p$sgg != "전국", ]
  code2sido <- stats::setNames(unname(sido_map[two$sgg]), substr(two$sgg_code, 1, 2))
  code2sido <- code2sido[!is.na(code2sido)]
  code2sido <- code2sido[!duplicated(names(code2sido))]

  f <- p[nchar(p$sgg_code) == 5, ]
  nm5 <- stats::setNames(f$sgg, f$sgg_code)
  parent <- nm5[paste0(substr(f$sgg_code, 1, 4), "0")]
  is_sub <- substr(f$sgg_code, 5, 5) != "0" & !is.na(parent)
  f$지역 <- ifelse(is_sub, paste0(parent, f$sgg), f$sgg)
  f$시도 <- unname(code2sido[substr(f$sgg_code, 1, 2)])
  f$연도 <- as.integer(f$year)
  f$인구 <- as.numeric(f$pop)
  f <- f[!is.na(f$시도) & !is.na(f$인구), c("연도", "시도", "지역", "인구")]
  f$sgg <- paste(f$시도, f$지역)
  f[!duplicated(paste(f$연도, f$sgg)), ]
}

## 가중 타일과 그 분해. 가중치가 전부 같으면 06 의 비가중 결과와 «수치가 일치»해야 한다.
theil_w <- function(x, w, g) {
  w <- w / sum(w)
  mu <- sum(w * x)
  Tt <- sum(w * (x / mu) * log(x / mu))
  Tw <- 0; Tb <- 0
  for (lv in unique(g)) {
    i <- g == lv
    Wg <- sum(w[i]); mug <- sum(w[i] * x[i]) / Wg
    sg <- Wg * mug / mu
    Tw <- Tw + sg * sum((w[i] / Wg) * (x[i] / mug) * log(x[i] / mug))
    Tb <- Tb + sg * log(mug / mu)
  }
  c(총 = Tt, 집단내 = Tw, 집단간 = Tb, 집단간_몫 = 100 * Tb / Tt)
}

sensitivity <- function(d, all_d) {
  cat("=== 07_sensitivity.R ===\n")
  d <- d[d$유형 %in% TYPES, ]

  ##################################################################
  #####  ① 인구 가중                                          #####
  ##################################################################
  pp <- load_population()
  m <- merge(d, pp[, c("연도", "sgg", "인구")], by = c("연도", "sgg"), all.x = TRUE)
  yrs_w <- sort(unique(m$연도[!is.na(m$인구)]))
  cov <- vapply(yrs_w, function(y) {
    g <- m[m$연도 == y, ]; 100 * mean(!is.na(g$인구))
  }, numeric(1))
  cat(sprintf("--- 인구 매칭: %d~%d년, 연도별 최저 %.1f%% · 최고 %.1f%%\n",
              min(yrs_w), max(yrs_w), min(cov), max(cov)))
  ## ⛔ 게이트: 매칭이 낮으면 가중 결과가 «표본이 다른» 분석이 된다.
  if (min(cov) < 95)
    stop(sprintf("07_sensitivity: 인구 매칭이 %.1f%% 로 낮다(%d년) — 이름 대조를 고칠 것",
                 min(cov), yrs_w[which.min(cov)]))
  drop_y <- setdiff(sort(unique(d$연도)), yrs_w)
  if (length(drop_y))
    cat(sprintf("--- ⚠ 인구가 없어 가중 분석에서 뺀 연도: %s (2023 인구를 끌어다 쓰지 않는다)\n",
                paste(drop_y, collapse = ", ")))

  wt <- do.call(rbind, lapply(yrs_w, function(y) {
    g <- m[m$연도 == y & !is.na(m$인구), ]
    a <- theil_w(g$자체충족, rep(1, nrow(g)), g$유형)     # 비가중(06 재현)
    b <- theil_w(g$자체충족, g$인구, g$유형)              # 인구가중
    data.frame(연도 = y, n = nrow(g),
               비가중_총 = a[["총"]], 비가중_집단간몫 = a[["집단간_몫"]],
               가중_총 = b[["총"]], 가중_집단간몫 = b[["집단간_몫"]])
  }))

  ## 게이트: 비가중 재현값이 06 과 같아야 한다(같은 양을 두 번 계산했으므로).
  ## ⛔ 2026-09-02 감사: 여기가 parent_step 이었다. inequality_*.rds 는 «이 프로젝트»가
  ##    output/processed/ 에 쓰므로 부모 폴더엔 없다 → tryCatch 가 삼켜 게이트가 «영구히»
  ##    죽어 있었다(로그에 그 OK 줄이 아예 없었다). 00_setup.R 이 「둘을 섞지 말라」고
  ##    적어 둔 바로 그 실수다.
  iq <- tryCatch(paper_step("^inequality_", "06 산출"), error = function(e) NULL)
  if (!is.null(iq)) {
    cmp <- merge(wt[, c("연도", "비가중_총")], iq$ineq[, c("연도", "타일")], by = "연도")
    gap <- max(abs(cmp$비가중_총 - cmp$타일))
    if (gap > 1e-9) stop("07_sensitivity: 비가중 재현이 06 과 어긋난다(", gap, ")")
    cat(sprintf("--- gate 비가중 재현 OK (06 과 일치, 오차 %.1e)\n", gap))
  }

  a <- wt[which.min(wt$연도), ]; b <- wt[which.max(wt$연도), ]
  cat(sprintf("--- 집단간 몫  비가중 %.0f%% -> %.0f%%  |  인구가중 %.0f%% -> %.0f%%\n",
              a$비가중_집단간몫, b$비가중_집단간몫, a$가중_집단간몫, b$가중_집단간몫))
  cat(sprintf("--- 총 타일    비가중 %.4f -> %.4f  |  인구가중 %.4f -> %.4f\n",
              a$비가중_총, b$비가중_총, a$가중_총, b$가중_총))

  ##################################################################
  #####  ② 로짓 변환 — 유계 비율 문제                         #####
  ##################################################################
  ## 타일은 비유계 척도용이다. 0–100% 에 쓰면 천장효과가 불평등 «감소»로 읽힐 수 있다.
  ## 로짓으로 펴고 «분산»을 집단간/집단내로 가른다(ANOVA 항등식). 방향이 같으면 방어된다.
  lg <- do.call(rbind, lapply(sort(unique(d$연도)), function(y) {
    g <- d[d$연도 == y, ]
    p <- pmin(pmax(g$자체충족 / 100, 1e-4), 1 - 1e-4)
    z <- log(p / (1 - p))
    gm <- tapply(z, g$유형, mean); nn <- table(g$유형)
    tot <- mean((z - mean(z))^2)
    btw <- sum(nn * (gm - mean(z))^2) / length(z)
    data.frame(연도 = y, 분산 = tot, 집단간 = btw, 집단내 = tot - btw,
               집단간_몫 = 100 * btw / tot)
  }))
  la <- lg[which.min(lg$연도), ]; lb <- lg[which.max(lg$연도), ]
  cat(sprintf("--- 로짓 분산   %.4f -> %.4f · 집단간 몫 %.0f%% -> %.0f%%\n",
              la$분산, lb$분산, la$집단간_몫, lb$집단간_몫))

  ##################################################################
  #####  ③ 불균형패널                                         #####
  ##################################################################
  ub <- all_d
  ub$유형 <- ifelse(substr(ub$지역, nchar(ub$지역), nchar(ub$지역)) == "군", "군",
             ifelse(substr(ub$지역, nchar(ub$지역), nchar(ub$지역)) == "구", "구",
             ifelse(substr(ub$지역, nchar(ub$지역), nchar(ub$지역)) == "시", "시", "기타")))
  ub <- ub[ub$유형 %in% TYPES, ]
  ub$자체충족 <- 100 * ub$관내 / (ub$관내 + ub$관외)
  un <- do.call(rbind, lapply(sort(unique(ub$연도)), function(y) {
    g <- ub[ub$연도 == y, ]
    v <- theil_w(g$자체충족, rep(1, nrow(g)), g$유형)
    data.frame(연도 = y, n = nrow(g), 총 = v[["총"]], 집단간_몫 = v[["집단간_몫"]])
  }))
  ua <- un[which.min(un$연도), ]; ubb <- un[which.max(un$연도), ]
  cat(sprintf("--- 불균형패널(%d~%d곳) 집단간 몫 %.0f%% -> %.0f%%\n",
              min(un$n), max(un$n), ua$집단간_몫, ubb$집단간_몫))

  ##################################################################
  #####  ④ 인공물인가 — 인구 극소 시군구를 잘라 본다         #####
  ##################################################################
  ## 리뷰어의 첫 의심: 「비가중 불평등 상승은 옹진·울릉급 «인구 극소» 군이 만든 것 아닌가.」
  ## 잘라도 방향이 남으면 인공물이 아니다. 5% 와 10% 를 둘 다 본다 — 5%만 보면
  ## 「하필 5%」라는 되물음이 남는다.
  trim <- do.call(rbind, lapply(c(0, 5, 10), function(pct) {
    do.call(rbind, lapply(yrs_w, function(y) {
      g <- m[m$연도 == y & !is.na(m$인구), ]
      if (pct > 0) g <- g[g$인구 > stats::quantile(g$인구, pct / 100), ]
      v <- theil_w(g$자체충족, rep(1, nrow(g)), g$유형)
      data.frame(절사 = paste0(pct, "%"), 연도 = y, n = nrow(g),
                 총 = v[["총"]], 집단간_몫 = v[["집단간_몫"]])
    }))
  }))
  cat("--- 인구 극소 절사(비가중):\n")
  for (pc in unique(trim$절사)) {
    a <- trim[trim$절사 == pc & trim$연도 == min(yrs_w), ]
    b <- trim[trim$절사 == pc & trim$연도 == max(yrs_w), ]
    cat(sprintf("      절사 %-4s n=%d  총 타일 %.4f -> %.4f (%+.0f%%) · 집단간 몫 %.0f%% -> %.0f%%\n",
                pc, b$n, a$총, b$총, 100 * (b$총 / a$총 - 1), a$집단간_몫, b$집단간_몫))
  }

  ##################################################################
  #####  ⑤ 항등식 분해가 «인구가중에서도» 서는가             #####
  ##################################################################
  ## 문헌 스캔(2026-09-01) 결과 살아 있는 «발견»은 관내·관외 동반증가 분해 하나다.
  ## 그것이 가중 논쟁과 «무관하게» 서면 논문의 중심이 흔들리지 않는다 — 확인한다.
  ## ⚠ 인구가 있는 마지막 해까지만 본다(2024 제외).
  y0 <- min(m$연도); y1 <- max(m$연도[!is.na(m$인구)])
  b0 <- m[m$연도 == y0, c("sgg", "관내", "관외")]
  names(b0)[2:3] <- c("기0_관내", "기0_관외")
  e1 <- merge(m[m$연도 == y1 & !is.na(m$인구), c("sgg", "관내", "관외", "인구", "유형")],
              b0, by = "sgg")
  e1$배수_관내 <- e1$관내 / e1$기0_관내
  e1$배수_관외 <- e1$관외 / e1$기0_관외
  w <- e1$인구 / sum(e1$인구)
  ## 가중 «중앙값»은 정의가 갈리므로 가중평균과 비가중중앙값을 나란히 낸다.
  dec <- data.frame(
    지표 = c("관내 성장배수", "관외 성장배수", "관내가 줄어든 시군구",
             "그 시군구에 사는 인구 비중"),
    비가중 = c(sprintf("%.2f배(중앙값)", stats::median(e1$배수_관내)),
               sprintf("%.2f배(중앙값)", stats::median(e1$배수_관외)),
               sprintf("%d곳 / %d", sum(e1$배수_관내 < 1), nrow(e1)), "—"),
    인구가중 = c(sprintf("%.2f배(가중평균)", sum(w * e1$배수_관내)),
                 sprintf("%.2f배(가중평균)", sum(w * e1$배수_관외)), "—",
                 sprintf("%.2f%%", 100 * sum(w[e1$배수_관내 < 1]))),
    stringsAsFactors = FALSE)
  cat(sprintf("--- 항등식 분해(%d->%d): 관내 %.2f배 vs 관외 %.2f배 (비가중 중앙값)\n",
              y0, y1, stats::median(e1$배수_관내), stats::median(e1$배수_관외)))
  cat(sprintf("---                     관내 %.2f배 vs 관외 %.2f배 (인구가중 평균)\n",
              sum(w * e1$배수_관내), sum(w * e1$배수_관외)))
  cat(sprintf("--- 관내가 줄어든 시군구 %d곳(인구의 %.2f%%)\n",
              sum(e1$배수_관내 < 1), 100 * sum(w[e1$배수_관내 < 1])))

  write_table(list(인구가중 = wt, 로짓분산 = lg, 불균형패널 = un, 절사 = trim, 항등식_가중 = dec),
              "p6_민감도")
  ## 절사(trim)도 담는다 — 10_supplement.R 이 이걸 읽는다(전엔 xlsx 로만 나가 step 에 없었다)
  save_step(list(w = wt, logit = lg, unbal = un, trim = trim, dec = dec), name = "sensitivity")
  invisible(list(w = wt, logit = lg, unbal = un, dec = dec))
}
