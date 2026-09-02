##################################################################
#####  09_manuscript_check.R - 원고와 파이프라인이 갈라졌나   #####
##################################################################
##
## 원고에 적힌 수치가 «지금 파이프라인이 내는 값»과 같은지 본다. 자료원의 T8
## (브리핑↔페이지 공유 숫자)과 같은 장치다.
##
## 왜 필요한가. 원고는 사람이 쓰고 표는 코드가 낸다. 재분석하면 표는 바뀌는데 원고는
## 안 바뀐다 — 그 순간 원고의 숫자는 «출처가 없는 숫자»가 된다. 이 프로젝트의 규율은
## 「숫자를 손으로 옮기지 않는다」인데, 옮기지 «않았는지»를 확인하는 것이 이 파일이다.
##
## ⛔ 이 검사는 «값이 참인가»를 묻지 않는다. **원고와 코드가 같은 말을 하는가**를 묻는다.
##    값 자체의 타당성은 자료원의 게이트(KOSIS 대조 303칸 0.00%)가 담당한다.

manuscript_check <- function() {
  cat("=== 09_manuscript_check.R ===\n")
  md_dir <- file.path(PAPER_ROOT, "manuscript")
  files <- list.files(md_dir, pattern = "^0[0-5].*\\.md$", full.names = TRUE)
  if (!length(files)) { cat("--- 원고 없음, 건너뜀\n"); return(invisible(NULL)) }
  ## ⚠ 편집 지침 블록(구분선 아래)은 «금지 문장»을 일부러 담고 있다. 본문만 본다.
  txt <- paste(unlist(lapply(files, function(f) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    cut <- which(trimws(x) == "---" & c(FALSE, trimws(utils::head(x, -1)) == "---"))
    if (length(cut)) x <- x[seq_len(cut[1] - 2)]
    x
  })), collapse = "\n")

  co <- paper_step("^cohort_", "01 코호트")
  iq <- paper_step("^inequality_", "06 불평등")
  se <- paper_step("^sensitivity_", "07 민감도")
  tj <- paper_step("^trajectory_", "05 궤적")
  so <- paper_step("^socioeconomic_", "08 사회경제축")
  dc <- paper_step("^decomp_", "04 분해")
  am <- paper_step("^altmeasure_",    "07_2 대조척도")
  .am <- function(meas, col) am$summary[[col]][am$summary$척도 == meas]
  ## 정점은 05 가 xlsx 로만 내보내고 step 에는 안 담는다 → 궤적에서 직접 구한다(같은 정의).
  .peak <- function(ty) {
    g <- tj$traj[tj$traj$유형 == ty, ]; g <- g[order(g$연도), ]
    i <- which.max(g$중앙); list(값 = g$중앙[i], 연도 = g$연도[i], g = g, i = i)
  }
  pk <- stats::setNames(lapply(TYPES, .peak), TYPES)
  d  <- add_variables(co$balanced); d <- d[d$유형 %in% TYPES, ]

  y0 <- min(d$연도); y1 <- max(d$연도)
  a <- d[d$연도 == y0, ]; b <- d[d$연도 == y1, ]
  i0 <- iq$ineq[iq$ineq$연도 == y0, ]; i1 <- iq$ineq[iq$ineq$연도 == y1, ]
  w0 <- se$w[which.min(se$w$연도), ]; w1 <- se$w[which.max(se$w$연도), ]

  claims <- list(
    list("균형패널 시군구 수",      sprintf("%d", length(unique(d$sgg)))),
    list("균형패널 행 수",          sprintf("%s", format(nrow(d), big.mark = ","))),
    list("중앙값 첫해",             sprintf("%.1f", stats::median(a$자체충족))),
    list("중앙값 끝해",             sprintf("%.1f", stats::median(b$자체충족))),
    list("오른 곳",                 sprintf("%d", sum(b$변화_자체충족 > 0))),
    list("내린 곳",                 sprintf("%d", sum(b$변화_자체충족 < 0))),
    list("타일 첫해",               sprintf("%.4f", i0$타일)),
    list("타일 끝해",               sprintf("%.4f", i1$타일)),
    list("집단간 몫 첫해",          sprintf("%.0f", i0$집단간_몫)),
    list("집단간 몫 끝해",          sprintf("%.0f", i1$집단간_몫)),
    list("지니 첫해",               sprintf("%.3f", i0$지니)),
    list("지니 끝해",               sprintf("%.3f", i1$지니)),
    list("가중 타일 첫해",          sprintf("%.4f", w0$가중_총)),
    list("가중 타일 끝해",          sprintf("%.4f", w1$가중_총)),
    list("군 구간시작값",           sprintf("%.1f", tj$seg$시작[tj$seg$유형 == "군"][1])),
    ## ⛔ 2026-09-02: 위 항목은 라벨이 「정점값」이었으나 실제로는 «구간 시작값»을 잡고 있었다.
    ##    정점 36.4 는 한 번도 검사되지 않았고, 그래서 「13년 단조 하락」(실제 12/13)이
    ##    이 게이트를 그냥 통과했다. 정점 세 개를 «정점 시트»에서 직접 잡는다.
    list("구 정점값",               sprintf("%.1f", pk[["구"]]$값)),
    list("시 정점값",               sprintf("%.1f", pk[["시"]]$값)),
    list("군 정점값",               sprintf("%.1f", pk[["군"]]$값)),
    list("군 정점연도",             sprintf("%d",   as.integer(pk[["군"]]$연도))),
    ## ⭐ 「단조 하락」이 통과한 자리. 정점 이후 «하락한 구간 수»를 원고가 말한 대로 지킨다.
    ## ⛔ 2026-09-02 감사: 이 claim 이 「12」였는데 원고는 그 값을 **twelve 라고 영어 단어로**
    ##    쓴다. 「12」는 12.6 beds · Japan (12.5) · 2012 · Mannion 2012 에 우연히 걸려
    ##    **아무것도 지키지 않았다** — 「thirteen」으로 되돌려도 통과했다.
    ##    → 수치가 아니라 «그 문장»을 건다. 이것이 이 게이트가 지켜야 할 형태다.
    list("군 하락구간 서술", local({
      g <- pk[["군"]]$g; v <- g$중앙[pk[["군"]]$i:nrow(g)]
      n <- sum(diff(v) < 0); tot <- length(v) - 1L
      w <- c("twelve", "thirteen", "fourteen")[c(12L, 13L, 14L) == n]
      wt <- c("twelve", "thirteen", "fourteen")[c(12L, 13L, 14L) == tot]
      sprintf("%s of the following %s year-steps", w, wt) })),
    ## 항등식 분해의 성장배수 — 원고 결론이 이 세 숫자 위에 서 있는데 안 지키고 있었다
    list("관내 성장배수",           sprintf("%.2f", stats::median(dc$last$배수_관내))),
    list("관외 성장배수",           sprintf("%.2f", stats::median(dc$last$배수_관외))),
    list("상대성장",                sprintf("%.3f", stats::median(dc$last$상대성장))),
    ## 07_2 대조척도 — 「명목 진료비 아니냐」에 대한 답이 이 숫자들 위에 서 있다
    list("인원기준 첫해",           sprintf("%.1f", .am("Persons treated, all care", "중앙_첫해"))),
    list("인원기준 끝해",           sprintf("%.1f", .am("Persons treated, all care", "중앙_끝해"))),
    list("일수기준 첫해",           sprintf("%.1f", .am("Visit-days, all care", "중앙_첫해"))),
    list("일수기준 끝해",           sprintf("%.1f", .am("Visit-days, all care", "중앙_끝해"))),
    list("인원기준 집단간몫 끝해",  sprintf("%.0f", .am("Persons treated, all care", "집단간몫_끝해"))),
    ## ⭐ 이 논문의 «핵심 주장»이 척도를 넘어 서는가 — 최솟값이 양수여야 한다
    list("구-군 갈라짐 최소",       sprintf("%.1f", local({
      g <- am$bytype
      min(g$변화[g$유형 == "구"] - g$변화[g$유형 == "군"]) }))),
    list("구-군 격차 끝해",         sprintf("%.1f", tj$gap$격차[which.max(tj$gap$연도)])),
    ## 08 사회경제축 — 원고에 반영된 뒤 이 셋도 지킨다
    list("재정 집단간몫 첫해",      sprintf("%.0f", so$both$재정5분위_집단간몫[which.min(so$both$연도)])),
    list("재정 집단간몫 끝해",      sprintf("%.0f", so$both$재정5분위_집단간몫[which.max(so$both$연도)])),
    list("재정 Q5-Q1 끝해",         sprintf("%.1f", local({
      t1 <- so$traj[so$traj$연도 == max(so$traj$연도), ]
      t1$중앙[t1$재정5분위 == "Q5"] - t1$중앙[t1$재정5분위 == "Q1"] })))
  )

  ## ⭐ 메타 검사 — 「값이 어딘가 있으면 통과」는 짧은 수치에서 «공허»해진다.
  ##    2026-09-02 감사 실측: 33개 claim 중 「12」는 6회 걸리는데 전부 decoy 였고,
  ##    「24」는 28회(2024 등), 「10」은 20회(2010·100%·P90/P10) 걸린다.
  ##    → 걸린 «횟수»를 세어, 짧은 수치가 여러 번 걸리면 그 claim 은 신뢰할 수 없다고 «말한다».
  ##    ⛔ 실패로 만들지 않는 이유: 여러 번 걸려도 그중 하나가 진짜일 수 있다.
  ##       게이트가 거짓말하지 않게 «약하다는 사실»을 드러내는 것이 여기서 할 일이다.
  .n_hits <- function(v) length(gregexpr(v, txt, fixed = TRUE)[[1]][
                                gregexpr(v, txt, fixed = TRUE)[[1]] > 0])
  weak <- character(0)

  bad <- 0L
  for (c1 in claims) {
    hit <- grepl(c1[[2]], txt, fixed = TRUE)
    if (hit && nchar(c1[[2]]) <= 3L && .n_hits(c1[[2]]) > 2L)
      weak <- c(weak, sprintf("%s(=%s, %d회)", c1[[1]], c1[[2]], .n_hits(c1[[2]])))
    cat(sprintf("%s %-22s %-10s %s\n", if (hit) "  ok  " else " FAIL ",
                c1[[1]], c1[[2]], if (hit) "원고에 있음" else "원고에서 못 찾음"))
    if (!hit) bad <- bad + 1L
  }

  ## ⭐ 본문 표의 «모든 숫자»가 원고에 있는가 — 33개 claim 이 못 덮던 자리(2026-09-02 감사)
  if (exists("gate_table_numbers")) gate_table_numbers(txt)

  ## 표·그림 참조가 실제 파일과 대응하는가 (개수만 — 이름 규칙이 아직 논리번호다)
  ## ⛔ 2026-09-02: 예전엔 전 stamp 를 다 세어 14 가 됐다. 참조 6 < 14 라 «항상» 통과.
  ##    최신 stamp 한 벌만 센다.
  .latest_set <- function(dir, ext) {
    f <- list.files(dir, pattern = paste0("\\.", ext, "$"))
    if (!length(f)) return(character(0))
    ## ⛔ 2026-09-02 감사: 「전역 최신 stamp」였다. 같은 날 collect_surfaces() 에서 고친
    ##    바로 그 버그가 여기 남아 있었다 — 표 하나만 재실행하면 n_tab=1 이 되어 엉뚱한
    ##    이유로 FAIL 한다. 옳은 의미는 «이름별» 최신이다.
    st <- regmatches(f, regexpr("[0-9]{6}(?=\\.[a-z]+$)", f, perl = TRUE))
    if (length(st) != length(f)) return(f)
    base <- sub("_[0-9]{6}\\.[a-z]+$", "", f)
    f[vapply(seq_along(f), function(i) st[i] == max(st[base == base[i]]), logical(1))]
  }
  tabs  <- .latest_set(tables_dir,  "xlsx")
  figs  <- .latest_set(figures_dir, "png")
  n_tab <- length(tabs)
  n_fig <- length(figs)
  ## ══════════════════════════════════════════════════════════════════════════
  ## ⛔ 금지 표현 — 「있어야 할 것이 있는가」만 보면 「없어야 할 것」이 되살아난다.
  ##    2026-09-02 실측: Results 에서 고친 「monotonically for thirteen years」가
  ##    Discussion 에 «더 강한 말»로 되살아났고, 「하락구간수 12」 claim 은 12 가 원고
  ##    어딘가에 있으면 통과하므로 그것을 못 봤다. 두 검사는 «다른 질문»이다.
  ##
  ## ⭐ 그리고 «원고만» 보면 또 좁다. 같은 날 두 번째로 겪었다 — 원고에서 지운
  ##    「urban-rural gap」이 **그림 범례 파일**에 살아 있었고, 표의 «열 제목」이 다른 양을
  ##    가리키는 것을 어떤 게이트도 안 봤다(사람이 정독해서야 나왔다).
  ##    → `gate_surfaces()`(공용 00_config.R)로 **산문 · 범례 · 표의 시트명/열 제목**을 함께 본다.
  ##
  ## ⚠ 규칙에 «문체 취향»을 넣지 말 것. 넣을 것은 「사실로 틀린 표현」과 「제약이 금지한
  ##    표현」뿐이다. 패턴은 «그 오류의 형태»로 좁힌다 — "monotonic" 단독으로 걸었더니
  ##    재정 5분위의 «정당한» 「not monotonic」까지 잡혔다(같은 날).
  FORBIDDEN <- list(
    list("declined monotonically", "군은 13구간 중 12구간 하락이다(2021->2022 상승)"),
    list("monotonically for",      "같은 것"),
    ## ⚠ 「urban-rural」 단독으로 걸면 «정당한» 용례까지 잡힌다 — Liang 2017 의 층화 서술과,
    ##    그 표현을 «금지하는» Methods 문장 자체가 걸렸다(2026-09-02, 같은 실수 두 번째).
    ##    금지되는 것은 «우리 격차를 그렇게 부르는 것»이다. 패턴을 그 형태로 좁힌다.
    list("thirteen years to 29.2", "같은 것 - twelve of the following thirteen year-steps"),
    list("urban-rural gap",        "Methods 가 「행정 위계이지 도시-농촌 분류가 아니다」라고 못박았다"),
    list("urban\u2013rural gap",       "같은 것(en-dash 판)"),
    list("localisation index",     "MEDLINE 0건인 철자다 - 이 단어만 -z"),
    list("no small-area deprivation", "박탈지수는 «존재한다»(Kim AM 2019 · Kim I 2017)"),
    list("within-area",            "in-area 로 통일했다"),
    list("outside-area",           "out-of-area 로 통일했다"),
    list("변화 중앙값",             "「중앙값의 차」와 「변화의 중앙값」은 «다른 양»이다(+1.5 vs +0.5)")
  )
  bad <- bad + gate_surfaces(
    FORBIDDEN,
    ## ⭐ 투고 패키지도 «독자가 보는 표면»이다 — 커버레터는 편집자가 읽는다.
    ##    원고와 같은 주장을 담으므로 갈라질 수 있고, 갈라지면 그것을 편집자가 먼저 본다.
    md_globs      = c(file.path(md_dir, c("0[0-5]*.md", "9[0-9]*.md")),
                      file.path(PAPER_ROOT, "submission", "*.md")),
    xlsx_dir      = tables_dir,
    figures_dir_  = figures_dir)

  ## ⛔ 2026-09-02: 예전엔 «문자열 종류»를 셌다 — "[Figure 5]" 와 ", Figure 5]" 가 서로 다른
  ##    참조로 잡혀 같은 그림이 둘로 세어졌다. 세어야 하는 것은 «번호»다.
  .nums <- function(pat) sort(unique(as.integer(gsub("\\D", "",
             regmatches(txt, gregexpr(pat, txt))[[1]]))))
  nums_t <- .nums("\\[Table [0-9]+")
  nums_f <- .nums("Figure [0-9]+")
  ref_t <- length(nums_t); ref_f <- length(nums_f)
  cat(sprintf("--- 산출물 표 %d · 그림 %d  |  원고 참조 Table %d · Figure %d\n",
              n_tab, n_fig, ref_t, ref_f))
  if (ref_t > n_tab || ref_f > n_fig) {
    cat(" FAIL  원고가 «없는» 표·그림을 참조한다\n"); bad <- bad + 1L
  }
  ## ⭐ 역방향 — 「맞는가」뿐 아니라 「다 인용되는가」. 번호가 «비면» 제작에서 밀린다.
  for (nm in list(list("Table", nums_t), list("Figure", nums_f))) {
    v <- nm[[2]]
    if (length(v) && !identical(v, seq_len(max(v)))) {
      cat(sprintf(" FAIL  %s 번호가 연속이 아니다: %s (빠진 것 %s)\n", nm[[1]],
                  paste(v, collapse = ","),
                  paste(setdiff(seq_len(max(v)), v), collapse = ",")))
      bad <- bad + 1L
    }
  }
  ## ⚠ 영문 저널 제출본인가 — 그림 라벨에 한글이 남아 있으면 «투고 불가»다 (감사 0-2)
  ko <- figs[grepl("[가-힣]", figs)]
  if (length(ko)) cat(sprintf("  주의  그림 파일명에 한글: %d개 — 라벨 영문화 여부를 눈으로 확인할 것\n",
                              length(ko)))

  if (bad > 0L)
    stop(sprintf("manuscript_check FAILED: %d건이 원고와 어긋난다 — ",
                 bad), "원고를 고치거나, 파이프라인이 바뀌었으면 원고를 갱신할 것")
  if (length(weak))
    cat(sprintf("  주의  «공허할 수 있는» claim %d개 - 짧은 값이 여러 번 걸린다: %s\n",
                length(weak), paste(weak, collapse = " · ")))
  cat(sprintf("--- gate manuscript OK (%d개 수치가 원고와 일치)\n", length(claims)))
  invisible(TRUE)
}
