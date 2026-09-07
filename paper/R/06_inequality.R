##################################################################
#####  06_inequality.R - 불평등은 «어디»에서 커졌나           #####
##################################################################
##
## 05 는 유형별 중앙값이 갈라졌음을 보였다. 그런데 「구 −군 격차가 벌어졌다」는
## **집단 간** 이야기이고, 같은 유형 «안»에서도 벌어졌을 수 있다. 둘은 다른 문제다.
##   · 집단 간이 크면  → 지역 «유형»이 문제다(도농 구조)
##   · 집단 내가 크면  → 같은 군끼리도 갈렸다는 뜻이라 처방이 달라진다
##
## **타일 지수(Theil)** 는 이 둘로 «가법 분해»된다 — 그래서 쓴다.
##   T = Σ s_g·T_g  (집단 내)  +  Σ s_g·ln(μ_g/μ)  (집단 간),   s_g = n_g·μ_g /(N·μ)
## 지니는 그렇게 안 갈라진다(겹치는 집단에서 잔차가 남는다). 여기 목적이 «분해»이므로
## 타일이 맞고, 지니는 «수준»을 읽기 쉬워 함께 낸다.
##
## ⛔ 이것은 «자체충족률 분포»의 불평등이지 «건강 형평»이 아니다. 소득·박탈 순위를
##    쓰는 집중지수(CI)·SII 와 다른 양이다 — 그 지표들은 순위변수가 있어야 하는데
##    이 자료에는 없다(있으려면 시군구 박탈지수가 필요하다). **CI/SII 라고 부르지 말 것.**

theil <- function(x) {
  x <- x[!is.na(x) & x > 0]
  mu <- mean(x)
  mean((x / mu) * log(x / mu))
}

gini <- function(x) {
  x <- sort(x[!is.na(x) & x >= 0]); n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

inequality <- function(d) {
  cat("=== 06_inequality.R ===\n")
  d <- d[d$유형 %in% TYPES, ]
  yrs <- sort(unique(d$연도))

  per_year <- do.call(rbind, lapply(yrs, function(y) {
    g <- d[d$연도 == y, ]
    x <- g$자체충족; mu <- mean(x); N <- nrow(g)
    ## 집단별 몫과 타일
    parts <- do.call(rbind, lapply(TYPES, function(tp) {
      s <- g$자체충족[g$유형 == tp]
      data.frame(유형 = tp, n = length(s), mu_g = mean(s), T_g = theil(s))
    }))
    parts$s_g <- parts$n * parts$mu_g / (N * mu)
    Tw <- sum(parts$s_g * parts$T_g)
    Tb <- sum(parts$s_g * log(parts$mu_g / mu))
    data.frame(연도 = y, 지니 = gini(x), 타일 = theil(x),
               집단내 = Tw, 집단간 = Tb,
               집단간_몫 = 100 * Tb / (Tw + Tb),
               p90p10 = stats::quantile(x, .9) / stats::quantile(x, .1))
  }))

  ## 게이트: 분해가 «합»과 맞아야 한다. 안 맞으면 가중치를 잘못 쓴 것이다.
  gap <- max(abs(per_year$타일 - (per_year$집단내 + per_year$집단간)))
  if (gap > 1e-9)
    stop("06_inequality: 타일 분해가 총합과 어긋난다(최대 ", gap, ")")
  cat(sprintf("--- gate theil-decomposition OK (집단내+집단간 = 총 타일, 오차 %.1e)\n", gap))

  a <- per_year[per_year$연도 == min(yrs), ]
  b <- per_year[per_year$연도 == max(yrs), ]
  cat(sprintf("--- 타일: %d년 %.4f -> %d년 %.4f (%+.0f%%)\n",
              a$연도, a$타일, b$연도, b$타일, 100 * (b$타일 / a$타일 - 1)))
  cat(sprintf("--- 그중 «집단간»(구/시/군 사이) 몫: %.0f%% -> %.0f%%\n",
              a$집단간_몫, b$집단간_몫))
  cat(sprintf("--- 지니 %.3f -> %.3f · p90/p10 %.2f -> %.2f\n",
              a$지니, b$지니, a$p90p10, b$p90p10))

  write_table(list(연도별 = per_year), "p5_불평등")

  ## 그림: 총 불평등을 «두 층으로 쌓아» 보여준다 — 어느 쪽이 커졌는지가 곧 답이다.
  st <- rbind(
    data.frame(연도 = per_year$연도, 층 = "Between administrative types", 값 = per_year$집단간),
    data.frame(연도 = per_year$연도, 층 = "Within administrative types", 값 = per_year$집단내))
  st$층 <- factor(st$층, levels = c("Between administrative types",
                                    "Within administrative types"))
  g <- ggplot2::ggplot(st, ggplot2::aes(연도, 값, fill = 층)) +
    ggplot2::geom_area(alpha = 0.9) +
    scale_fill_hrm() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::labs(
      ## ⛔ 제약 9: 「총 불평등이 커졌다」를 «단독»으로 말하지 않는다 — 인구가중에서 뒤집힌다.
      ##    성분의 «움직임»만 말하고 총량 주장은 하지 않는다.
      title = "Between-type inequality nearly doubled; within-type inequality did not",
      subtitle = sprintf("Theil index of the district localization index and its additive decomposition; %d districts",
                         length(unique(d$sgg))),
      x = NULL, y = "Theil index", fill = NULL,
      ## ⚠ 마지막 문장은 «제약 9»다 — 총량의 부호가 가중에 따라 뒤집히므로 그림 옆에
      ##    반드시 붙는다. ⛔ 「집중지수가 아니다」류 변론은 뺐다(Methods 가 이미 말한다).
      caption = paste0("The Theil index decomposes additively into between-group and ",
                       "within-group components. Districts are weighted equally here; ",
                       "population-weighted results, which move in the opposite direction, ",
                       "are reported separately.")) +
    theme_hrm()
  save_fig_journal(g, "p_fig4_inequality_decomposition", height = FIG_W * 5 / 9)

  save_step(list(ineq = per_year), name = "inequality")
  invisible(per_year)
}
