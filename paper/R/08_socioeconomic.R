##################################################################
#####  08_socioeconomic.R - 사회경제 축으로 한 번 더 분해      #####
##################################################################
##
## 지금까지의 집단축은 **행정유형(구·시·군)** 하나뿐이다. 그것은 «행정 위계»이지
## 사회경제적 위치가 아니다. 형평 저널(IJEqH 급)은 사회경제 축을 기대한다.
##
## **재정자립도**를 두 번째 집단축으로 쓴다. 근거는 둘이다.
##   · 국내 선행연구가 이미 그것을 타일 분해의 집단축으로 썼다(박영수 2025, 응급의료자원).
##     같은 축을 쓰면 그 논문과의 대조가 «자연스러운 비교»가 된다.
##   · 시군구 박탈지수가 없다. 재정자립도는 공개돼 있고 자치단체 단위로 정의가 분명하다.
##
## ⛔ 되짚지 말 것
##  1. **재정자립도는 «자치단체» 지표다.** 수원시는 있고 장안구는 «없다» — 행정구는
##     자치단체가 아니기 때문이다. 그래서 **행정구는 부모 시의 값을 물려받는다.**
##     이것은 근사가 아니라 «정의상 옳다» — 장안구는 수원시 예산 안에 있다.
##  2. **코드로 다룬다.** 「동구」가 여섯이다. 그리고 **부모는 언제나 `code[:-3]`** 이다
##     (광주 1224·전남 1236 만 4자리 시도코드를 쓴다).
##  3. 항목이 **「세입과목개편 전/후」 둘로 갈린다.** 어느 해에 어느 쪽이 있는지 확인하고,
##     이어 붙였으면 **그 사실을 적는다.**
##  4. ⛔ 재정자립도를 «원인»으로 말하지 말 것. 여기서도 분해이지 인과가 아니다.

## 재정자립도를 «시도 수준»과 «시군구 수준» 둘 다 돌려준다 — 자치단체가 아닌 단위는
## 시도 값으로 내려가야 하기 때문이다(제주시·서귀포시).
fiscal_panel <- function() {
  p <- parent_step("^panel_", "패널(03_build_panel.R)")
  f <- p[p$key == "fiscal_indep" & !is.na(p$value), ]
  if (!"axis1_cd" %in% names(f))
    stop("08: 패널에 axis1_cd 가 없다 — 자료원의 read_raw 가 코드를 안 실어 온다")

  ## 시도 이름표: 2자리(대개) 또는 4자리(광주 1224·전남 1236) 코드가 시도 행이다.
  hd <- f[nchar(f$axis1_cd) %in% c(2L, 4L) & f$axis1_val != "전국", ]
  code2sido <- stats::setNames(canon_sido(hd$axis1_val), hd$axis1_cd)
  code2sido <- code2sido[!duplicated(names(code2sido))]

  ## 항목 둘 중 어느 해에 어느 쪽이 있는지 «보고»한다.
  tb <- table(f$year, f$itm_nm)
  yo <- as.integer(rownames(tb)[tb[, "재정자립도(세입과목개편전)"] > 0])
  yn <- as.integer(rownames(tb)[tb[, "재정자립도(세입과목개편후)"] > 0])
  cat(sprintf("--- 재정자립도 항목: 개편전 %d~%d · 개편후 %d~%d (겹치는 해 %d개, 겹치면 개편후를 쓴다)\n",
              min(yo), max(yo), min(yn), max(yn), length(intersect(yo, yn))))

  mk <- function(g, lvl) {
    g$계열 <- ifelse(g$itm_nm == "재정자립도(세입과목개편후)", "개편후", "개편전")
    g <- g[order(g$.k, g$year, g$계열 != "개편후"), ]
    g <- g[!duplicated(paste(g$.k, g$year)), ]
    data.frame(연도 = g$year, 시도 = g$시도, 지역 = g$지역, 수준 = lvl,
               계열 = g$계열, 재정자립도 = g$value, stringsAsFactors = FALSE)
  }

  sd <- hd
  sd$시도 <- canon_sido(sd$axis1_val); sd$지역 <- NA_character_; sd$.k <- sd$시도
  sg <- f[nchar(f$axis1_cd) %in% c(5L, 7L), ]
  sg$시도 <- unname(code2sido[substr(sg$axis1_cd, 1, nchar(sg$axis1_cd) - 3L)])
  sg <- sg[!is.na(sg$시도), ]
  sg$지역 <- sg$axis1_val; sg$.k <- paste(sg$시도, sg$지역)

  rbind(mk(sg, "시군구"), mk(sd, "시도"))
}

## 우리 시군구에 재정자립도를 붙인다. **세 단계로 내려간다** — 자치단체가 아닌 단위는
## 자기 예산이 없으므로 «자기를 품은 자치단체»의 값이 정의상 옳다.
##   ① 직접(자치구·시·군)  ② 시 안의 «행정구» → 부모 시  ③ 그래도 없으면 «시도»
## ⚠ ③ 을 쓴 곳은 «세어서 보고»한다 — 조용히 쓰면 자치단체 값으로 오해한다.
attach_fiscal <- function(d, fis) {
  sgg <- fis[fis$수준 == "시군구", ]
  sdo <- fis[fis$수준 == "시도", ]
  key <- unique(d[, c("시도", "지역", "sgg")])
  hit <- unique(paste(sgg$시도, sgg$지역))
  key$대응 <- key$지역
  key$경로 <- "① 직접"

  for (i in which(!paste(key$시도, key$지역) %in% hit)) {
    ## ⚠ 조건들의 «길이»를 맞춰야 한다. 부분집합에 전체 길이 논리벡터를 섞으면 R 이
    ## 재활용해 엉뚱한 것을 고른다(2026-09-01: 수원·전주 행정구가 그래서 안 붙었다).
    sis <- unique(sgg$지역[sgg$시도 == key$시도[i] & endsWith(sgg$지역, "시")])
    cand <- sis[startsWith(key$지역[i], sis)]
    if (length(cand)) {
      key$대응[i] <- cand[which.max(nchar(cand))]; key$경로[i] <- "② 부모 시"
    } else {
      key$대응[i] <- NA_character_;              key$경로[i] <- "③ 시도"
    }
  }
  n2 <- sum(key$경로 == "② 부모 시"); n3 <- sum(key$경로 == "③ 시도")
  cat(sprintf("--- 대응 경로: ① 직접 %d · ② 부모 시 %d · ③ 시도 %d\n",
              sum(key$경로 == "① 직접"), n2, n3))
  if (n3)
    cat(sprintf("--- ⚠ 자치단체가 아니라 «시도» 값을 쓴 곳: %s\n",
                paste(key$sgg[key$경로 == "③ 시도"], collapse = ", ")))

  m <- merge(d, key[, c("sgg", "대응", "경로")], by = "sgg", all.x = TRUE)
  a <- merge(m[!is.na(m$대응), ], sgg[, c("연도", "시도", "지역", "재정자립도", "계열")],
             by.x = c("연도", "시도", "대응"), by.y = c("연도", "시도", "지역"), all.x = TRUE)
  b <- merge(m[is.na(m$대응), ], sdo[, c("연도", "시도", "재정자립도", "계열")],
             by = c("연도", "시도"), all.x = TRUE)
  rbind(a[, intersect(names(a), names(b))], b[, intersect(names(a), names(b))])
}

socioeconomic <- function(d) {
  cat("=== 08_socioeconomic.R ===\n")
  d <- d[d$유형 %in% TYPES, ]
  fis <- fiscal_panel()
  m <- attach_fiscal(d, fis)

  yrs <- sort(unique(m$연도[!is.na(m$재정자립도)]))
  cov <- vapply(yrs, function(y) 100 * mean(!is.na(m$재정자립도[m$연도 == y])), numeric(1))
  cat(sprintf("--- 재정자립도 매칭: %d~%d년, 연도별 최저 %.1f%% · 최고 %.1f%%\n",
              min(yrs), max(yrs), min(cov), max(cov)))
  ## ⛔ 게이트: 매칭이 낮으면 «표본이 다른» 분석이 된다.
  if (min(cov) < 95)
    stop(sprintf("08: 재정자립도 매칭이 %.1f%% 로 낮다(%d년) — 이름 대조를 고칠 것",
                 min(cov), yrs[which.min(cov)]))
  miss_y <- setdiff(sort(unique(d$연도)), yrs)
  if (length(miss_y))
    cat(sprintf("--- ⚠ 재정자립도가 없어 뺀 연도: %s\n", paste(miss_y, collapse = ", ")))

  ## 5분위를 «두 가지»로 만든다. ⚠ 행정구가 부모 시 값을 물려받아 동점이 많다 — rank 로 가른다.
  ##
  ## ⛔ **연도별 재계산 분위는 «같은 지역의 궤적»이 아니다.** 실측(2026-09-01): 230곳 중
  ##    19년 내내 같은 분위에 머문 곳은 **65곳(28%)** 뿐이고 165곳이 분위를 오간다.
  ##    그래서 「Q1 이 이렇게 움직였다」는 문장은 연도별 분위로 쓰면 «지역이 바뀌는 집단»의
  ##    이야기가 된다. **기준연도 고정 분위**를 함께 낸다 — 그쪽이 궤적의 정본이다.
  cut5 <- function(v) paste0("Q", cut(rank(v, ties.method = "first"),
                                      breaks = 5, labels = FALSE))
  q <- do.call(rbind, lapply(yrs, function(y) {
    g <- m[m$연도 == y & !is.na(m$재정자립도), ]
    g$재정5분위 <- cut5(g$재정자립도)          # 연도별 재계산(분해용)
    g
  }))
  base <- q[q$연도 == min(yrs), c("sgg", "재정자립도")]
  base$고정5분위 <- cut5(base$재정자립도)
  q <- merge(q, base[, c("sgg", "고정5분위")], by = "sgg", all.x = TRUE)
  n_move <- length(unique(q$sgg[q$재정5분위 != q$고정5분위]))
  cat(sprintf("--- ⚠ 기준연도 분위와 «다른 해가 한 번이라도 있는» 시군구 %d곳/%d — 궤적은 «고정 분위»로 그린다\n",
              n_move, length(unique(q$sgg))))

  ## 두 집단축의 분해를 «나란히» 낸다 — 이것이 이 절의 요점이다.
  both <- do.call(rbind, lapply(yrs, function(y) {
    g <- q[q$연도 == y, ]
    a <- theil_w(g$자체충족, rep(1, nrow(g)), g$유형)
    b <- theil_w(g$자체충족, rep(1, nrow(g)), g$재정5분위)
    data.frame(연도 = y, n = nrow(g), 총 = a[["총"]],
               행정유형_집단간몫 = a[["집단간_몫"]],
               재정5분위_집단간몫 = b[["집단간_몫"]])
  }))
  a <- both[which.min(both$연도), ]; b <- both[which.max(both$연도), ]
  cat(sprintf("--- 집단간 몫  행정유형 %.0f%% -> %.0f%%  |  재정자립도 5분위 %.0f%% -> %.0f%%\n",
              a$행정유형_집단간몫, b$행정유형_집단간몫,
              a$재정5분위_집단간몫, b$재정5분위_집단간몫))

  ## 재정 5분위별 자체충족률 궤적 — 사회경제 기울기가 벌어졌는가
  ## ⭐ 궤적은 «기준연도 고정 분위»로 그린다 — 같은 지역을 따라가야 궤적이다.
  tr <- do.call(rbind, lapply(split(q, list(q$고정5분위, q$연도), drop = TRUE), function(g)
    data.frame(재정5분위 = g$고정5분위[1], 연도 = g$연도[1], n = nrow(g),
               중앙 = stats::median(g$자체충족))))
  tr <- tr[order(tr$재정5분위, tr$연도), ]
  g0 <- tr$중앙[tr$재정5분위 == "Q5" & tr$연도 == min(yrs)] -
        tr$중앙[tr$재정5분위 == "Q1" & tr$연도 == min(yrs)]
  g1 <- tr$중앙[tr$재정5분위 == "Q5" & tr$연도 == max(yrs)] -
        tr$중앙[tr$재정5분위 == "Q1" & tr$연도 == max(yrs)]
  cat(sprintf("--- 재정 최상위(Q5) − 최하위(Q1) 격차: %.1f%%p -> %.1f%%p (%+.1f%%p)\n",
              g0, g1, g1 - g0))

  write_table(list(두_집단축 = both, 재정5분위_궤적 = tr,
                   재정자립도 = fis[fis$연도 %in% yrs & fis$수준 == "시군구", ]),
              "p7_사회경제축")

  lab <- tr[tr$연도 == max(yrs), ]
  ## ⚠ PAL_HRM 의 뒤 세 색은 배경 대비 3:1 미만이다(프로젝트 CLAUDE.md) — 5계열에서
  ##    선종류는 「선택」이 아니라 필수다.
  gg <- ggplot2::ggplot(tr, ggplot2::aes(연도, 중앙, colour = 재정5분위,
                                         linetype = 재정5분위)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = 재정5분위),
                             hjust = 0, nudge_x = 0.5, direction = "y", size = 3.3,
                             seed = 1, show.legend = FALSE, segment.colour = "grey70",
                             family = KO_FAMILY) +
    scale_colour_hrm() +
    ggplot2::scale_linetype_manual(values = c("solid", "22", "42", "1343", "73")) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8),
                                expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(title = "Localization index by quintile of local fiscal independence",
                  subtitle = sprintf("Quintiles fixed at %d; Q1 = weakest fiscal capacity; %d–%d districts",
                                     min(yrs), min(both$n), max(both$n)),
                  x = NULL, y = "Localization index (%)", colour = NULL, linetype = NULL,
                  ## ⚠ 「매년 재계산하면 궤적이 아니다」는 Methods 의 선택 근거다 — 범례에서
                  ##    뺐다. 비단조는 남긴다(없으면 독자가 등급 순서를 읽는다).
                  caption = paste0("The fiscal independence ratio is a local-government ",
                                   "statistic, so administrative districts nested within a ",
                                   "city take their parent city's value. Quintiles are fixed ",
                                   "at the baseline year. The ordering across quintiles is not ",
                                   "monotonic: Q4 lies above Q5 throughout. No causal reading ",
                                   "is intended.")) +
    theme_hrm()
  save_fig_journal(gg, "p_fig5_fiscal_quintile_trajectory", height = FIG_W * 5 / 9)

  save_step(list(both = both, traj = tr, fiscal = fis), name = "socioeconomic")
  invisible(both)
}
