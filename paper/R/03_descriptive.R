##################################################################
#####  03_descriptive.R - Table 1                            #####
##################################################################
##
## 「표본이 무엇인가」를 한 표로. 해석은 하지 않는다 — 여기는 Results 의 첫머리다.

describe_cohort <- function(d) {
  cat("=== 03_descriptive.R ===\n")
  y0 <- min(d$연도); y1 <- max(d$연도)

  fmt <- function(v) sprintf("%.1f (%.1f–%.1f)", stats::median(v),
                             stats::quantile(v, .25), stats::quantile(v, .75))
  row <- function(lab, g0, g1, n) data.frame(
    구분 = lab, 시군구 = n,
    `기준연도` = fmt(g0), `최종연도` = fmt(g1),
    `중앙값의 차` = sprintf("%+.1f%%p", stats::median(g1) - stats::median(g0)),
    check.names = FALSE, stringsAsFactors = FALSE)

  a0 <- d[d$연도 == y0, ]; a1 <- d[d$연도 == y1, ]
  out <- row("전체", a0$자체충족, a1$자체충족, nrow(a0))
  for (tp in c("구", "시", "군")) {
    s0 <- a0[a0$유형 == tp, ]; s1 <- a1[a1$유형 == tp, ]
    if (!nrow(s0)) next
    out <- rbind(out, row(paste0("  ", tp), s0$자체충족, s1$자체충족, nrow(s0)))
  }

  ## 진료량도 같이 낸다 — 자체충족률만 보면 「그 지역이 커졌는가」가 안 보인다.
  vol <- data.frame(
    구분 = "총진료비 지수(명목, 기준=100)", 시군구 = nrow(a1),
    `기준연도` = "100.0", `최종연도` = sprintf("%.1f", stats::median(100 * a1$총진료량 / a1$기준_총진료량)),
    `중앙값의 차` = "", check.names = FALSE, stringsAsFactors = FALSE)
  out <- rbind(out, vol)

  names(out)[3:4] <- c(paste0(y0, "년"), paste0(y1, "년"))
  write_table(list(Table1 = out), "p2_table1")
  print(out, row.names = FALSE)

  ## 「정체」가 전체 중앙값의 성질인지, 개별 시군구도 안 움직였는지는 다른 질문이다.
  ch <- a1$변화_자체충족
  cat(sprintf("--- 개별 시군구의 %d→%d 변화: 중앙값 %+.1f%%p, 사분위 %+.1f ~ %+.1f, 범위 %+.1f ~ %+.1f\n",
              y0, y1, stats::median(ch), stats::quantile(ch, .25),
              stats::quantile(ch, .75), min(ch), max(ch)))
  cat(sprintf("--- 오른 곳 %d · 내린 곳 %d (전체 %d)\n",
              sum(ch > 0), sum(ch < 0), length(ch)))
  invisible(out)
}
