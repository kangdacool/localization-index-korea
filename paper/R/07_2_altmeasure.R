##################################################################
#####  07_2_altmeasure.R - 척도를 바꿔도 결과가 서는가        #####
##################################################################
##
## 이 논문의 가장 약한 지점은 「명목 진료비 기준」이라는 것이다. 리뷰어의 첫 반박은
## 「그 하락은 물가·수가·고령화가 만든 것 아니냐」이고, 지금 원고는 「비율이라 약분된다」로만
## 답한다. 그것은 «공통» 가격요인에만 참이다 — 관내와 관외의 «단가가 다르게» 움직이면
## 약분되지 않는다.
##
## ⭐ 그런데 통계연보가 같은 축으로 사람 수와 일수를 준다(2026-09-02 실측: 5개 지표 ×
##    관내/관외 × 계/입원/외래가 19년 결번 없이 있다). 사람 수로 재면 가격이 «통째로» 빠진다.
##
## ⛔ 이것은 정본을 «대체»하는 것이 아니라 대조하는 것이다. 원고의 주 분석은 진료비 기준이고,
##    여기서 나오는 것은 「그 결론이 척도에 얼마나 의존하는가」다.
##
## 판정 기준을 «미리» 적어 둔다(결과를 보고 정하지 않기 위해):
##   ① 유형별 «갈라짐»(구·시 대 군)이 척도를 바꿔도 같은 방향인가  ← 논문의 핵심 주장
##   ② 집단간 몫의 «상승»이 척도를 바꿔도 같은 방향인가            ← 두 번째 주장
##   ③ 전국 중앙값의 방향은 «부수적»이다 — 그것은 이 논문의 주장이 아니다

altmeasure <- function(cohort) {
  cat("=== 07_2_altmeasure.R ===\n")

  yb <- parent_step("^yearbook_", "통계연보 조립본")
  if (is.null(yb$alt))
    stop("07_2: 자료원에 대조 척도가 없다 - 보건자원감시 R/16_yearbook.R 을 먼저 돌릴 것")

  keep <- unique(cohort$balanced$sgg)          # 같은 230곳 위에서만 비교한다
  MEAS <- c("진료비_계" = "Expenditure, all care (primary)",
            "진료실인원수_계" = "Persons treated, all care",
            "입내원일수_계"  = "Visit-days, all care",
            "진료비_입원"    = "Expenditure, inpatient",
            "진료비_외래"    = "Expenditure, outpatient")

  frames <- c(list(진료비_계 = yb$wide), yb$alt)
  out <- list(); typ <- list()

  for (nm in names(MEAS)) {
    w <- frames[[nm]]
    w$sgg <- paste(w$시도, w$지역)
    w <- w[w$sgg %in% keep, ]
    ## ⚠ 균형패널로 «다시» 자른다 — 척도마다 결측 시군구가 다를 수 있다
    n_yr <- table(w$sgg)
    w <- w[w$sgg %in% names(n_yr)[n_yr == max(n_yr)], ]
    d <- add_variables(w)
    d <- d[d$유형 %in% TYPES, ]
    y0 <- min(d$연도); y1 <- max(d$연도)

    med <- function(v) stats::median(v, na.rm = TRUE)
    a <- d[d$연도 == y0, ]; b <- d[d$연도 == y1, ]

    ## 집단간 몫 — 06 과 «같은» 정의를 쓴다(두 벌로 두면 갈라진다)
    ## ⚠ 타일은 0 에서 정의되지 않는다(log 0). 입원 기준에는 «그 지역에 입원 시설이
    ##    아예 없어» 0% 인 시군구가 있다 - 자료원 제약 7 이 원고로 들어온 자리다.
    ##    0 을 뺀 «부분집합»에서 계산하고 «몇 개를 뺐는지»를 함께 낸다(조용히 넘기지 않는다).
    n_zero <- 0L
    share <- function(x) {
      x <- x[!is.na(x$자체충족), ]
      n_zero <<- sum(x$자체충족 <= 0)
      x <- x[x$자체충족 > 0, ]
      g <- x$유형; v <- x$자체충족
      mu <- mean(v); tot <- mean(v / mu * log(v / mu))
      betw <- sum(vapply(split(seq_along(v), g), function(i) {
        s <- length(i) * mean(v[i]) / (length(v) * mu); s * log(mean(v[i]) / mu) }, 0))
      100 * betw / tot
    }

    out[[nm]] <- data.frame(
      척도 = MEAS[[nm]], 시군구 = length(unique(d$sgg)),
      중앙_첫해 = round(med(a$자체충족), 1), 중앙_끝해 = round(med(b$자체충족), 1),
      집단간몫_첫해 = round(share(a), 0), 영_첫해 = n_zero,
      집단간몫_끝해 = round(share(b), 0), 영_끝해 = n_zero,
      stringsAsFactors = FALSE, check.names = FALSE)

    typ[[nm]] <- do.call(rbind, lapply(TYPES, function(t) data.frame(
      척도 = MEAS[[nm]], 유형 = t,
      첫해 = round(med(a$자체충족[a$유형 == t]), 1),
      끝해 = round(med(b$자체충족[b$유형 == t]), 1),
      변화 = round(med(b$자체충족[b$유형 == t]) - med(a$자체충족[a$유형 == t]), 1),
      stringsAsFactors = FALSE, check.names = FALSE)))
  }

  sm <- do.call(rbind, out); ty <- do.call(rbind, typ)
  rownames(sm) <- NULL; rownames(ty) <- NULL

  cat("--- 척도별 요약:\n"); print(sm, row.names = FALSE)
  cat("--- 유형별 변화(%p):\n")
  print(reshape(ty[, c("척도", "유형", "변화")], idvar = "척도", timevar = "유형",
                direction = "wide"), row.names = FALSE)

  ## ── 게이트: «핵심 주장»이 척도를 넘어 서는가
  gap <- ty$변화[ty$유형 == "구"] - ty$변화[ty$유형 == "군"]
  cat(sprintf("--- 구-군 변화 차이: %s\n",
              paste(sprintf("%s %+.1f%%p", sub(",.*", "", unique(ty$척도)), gap), collapse = " · ")))
  if (any(gap <= 0))
    cat(" ⚠  구-군 갈라짐이 «뒤집히는» 척도가 있다 - 원고 주장을 그 척도로 한정할 것\n")
  else
    cat(" ok  구-군 갈라짐이 다섯 척도 «전부»에서 같은 방향이다\n")

  up <- sm$집단간몫_끝해 - sm$집단간몫_첫해
  if (any(up <= 0))
    cat(" ⚠  집단간 몫 상승이 뒤집히는 척도가 있다\n")
  else
    cat(" ok  집단간 몫 상승이 다섯 척도 전부에서 같은 방향이다\n")

  write_table(list(척도별_요약 = sm, 유형별 = ty), "p8_대조척도")
  save_step(list(summary = sm, bytype = ty), name = "altmeasure")
  invisible(list(summary = sm, bytype = ty))
}
