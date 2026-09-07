##################################################################
#####  12_assemble.R - 절 파일을 «통합본» 한 장으로 조립한다   #####
##################################################################
##
## ⛔ 2026-09-02 pipeline-audit: `manuscript/` 에 절 파일 11개뿐이고 통합본도 조립기도 없었다.
##    `manuscript/README.md` 자신의 규칙(「절 파일을 고쳤으면 통합본을 «같은 턴에» 다시
##    조립한다」)과 전역 CLAUDE.md 「Manuscript Version Control」 ②·⑤ 를 «지킬 수단이 없었다».
##
## ⭐ 그리고 그 규칙이 존재하는 이유가 실무적이다 — 공저자에게 15개 파일을 골라 보내게 하면
##    «다른 폴더의 다른 판»이 섞인다. 한 장을 보낸다.
##
## 이 조립기가 지키는 것 셋:
##   ① 「⚠ 편집 지침」 블록(`---\n---` 아래)은 «본문이 아니다» — 뺀다
##   ② 본문 표는 `11_tables.R` 이 만든 것을 «그 자리에» 넣는다 — 손으로 옮긴 것이 아니다
##   ③ 그림은 파일 경로와 «자동 생성된 범례»를 함께 건다

ASSEMBLE_ORDER <- c(
  "_00_titlepage.md", "00_abstract.md", "01_introduction.md", "02_methods.md",
  "03_results.md", "04_discussion.md", "05_conclusions.md",
  "94_abbreviations.md", "95_declarations.md", "99_references.md",
  "96_supplement.md", "97_record_checklist.md")

## 본문이 아닌 파일 — 통합본에는 «참고 자료»로 뒤에 붙인다
NOT_BODY <- c("_00_titlepage.md", "94_abbreviations.md", "95_declarations.md",
              "96_supplement.md", "97_record_checklist.md")

## ⛔ 2026-09-07: 절 순서가 BMC 규정과 달랐다. 통합본은 Conclusions → «References» →
##    표 → 범례 → abbreviations → Declarations 로 나가는데, IJEqH 는
##    Conclusions → List of abbreviations → Declarations → References 를 요구한다
##    (ijeqh_research_articletype.txt 「Preparing your manuscript」).
##    Additional file 둘은 «원고 뒤»에 따로 붙는 물건이므로 맨 끝에 둔다.
BACK_BEFORE_REFS <- c("94_abbreviations.md", "95_declarations.md")
ADDITIONAL_FILES <- c("96_supplement.md", "97_record_checklist.md")

## 참고문헌에서 «목록»만 뽑는다 — 작업 주석은 원본에 남긴다.
## ⛔ 2026-09-02: 99_references.md 의 한국어 작업 주석 196줄이 통합본에 통째로 들어가
##    본문 5,600단어짜리 원고의 조립본이 11,295단어가 됐다. 그 주석은 «우리가» 읽는 것이다.
##    지우지 않고 뽑아낸다 — 원본은 정본으로 남고, 조립본은 저널이 받는 형태를 받는다.
clean_references <- function(md_dir) {
  f <- file.path(md_dir, "99_references.md")
  if (!file.exists(f)) return(character(0))
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")

  MARK <- "[\u26d4\u26a0\u2b50\U0001F534\U0001F7E1\u2705\U0001F381]"
  KEY  <- "인용키|채울 것|정정|미확인|Pass 2|전문 확인|초록만|되살리지|쓰지 말"
  is_item <- grepl("^\\s*[0-9]+\\. ", x)

  keep <- character(0); n_note <- 0L; n_dead <- 0L
  in_item <- FALSE; in_note <- FALSE
  for (i in seq_along(x)) {
    if (is_item[i]) {
      ## 취소선 항목은 통째로 뺀다 — 철회한 문헌이다
      if (grepl("~~", x[i])) { in_item <- FALSE; n_dead <- n_dead + 1L; next }
      in_item <- TRUE; in_note <- FALSE
      keep <- c(keep, "")                     # 항목 사이 한 줄
    } else if (!in_item) next
    ## ⛔ 2026-09-07: 항목 사이의 «국문 소제목»(`### 상급종합 지정과 쏠림`)이 앞 항목의
    ##    이어쓰기로 잡혀 통합본에 실렸다. 제목은 항목이 아니다 — 거기서 끊는다.
    if (!is_item[i] && grepl("^\\s*#", x[i])) { in_item <- FALSE; next }
    if (!in_item) next

    ## ⭐ 서지가 먼저, 주석이 뒤. 한 번 주석이 시작되면 그 항목 끝까지 주석이다.
    if (in_note) { n_note <- n_note + 1L; next }

    ## ⛔ 2026-09-07: KEY 검사가 MARK 검사 «뒤»에 있었다. `PMID …. 인용키 [Yun 2025]. ⭐ …`
    ##    처럼 마커가 인용키 «뒤»에 오면 앞부분이 통째로 남아 「인용키」가 통합본에 실렸다.
    ##    작업 어휘가 먼저다.
    ## ⛔ 2026-09-07(2): KEY 는 줄을 «통째로» 버렸다. 그런데 서지가 같은 줄 앞부분에 있는
    ##    항목이 있다 — `**2021**;36(45):e289. doi:… PMID … 인용키 [JKMS 2021].`
    ##    그래서 통합본의 6번 항목에 «연도·권·호·DOI 가 통째로 없었다». MARK 처럼 앞을 남긴다.
    m <- regexpr(paste0(MARK, "|", KEY), x[i], perl = TRUE)
    if (m > 0L) {                              # 표시가 줄 «중간»이면 그 앞까지만 남긴다
      head_txt <- sub("[\\s,·—-]+$", "", sub("\\s+$", "", substr(x[i], 1L, m - 1L)))
      in_note <- TRUE; n_note <- n_note + 1L
      if (nzchar(trimws(head_txt))) keep <- c(keep, head_txt)
      next
    }
    if (!nzchar(trimws(x[i]))) next            # 항목 내부 빈 줄은 버린다
    keep <- c(keep, x[i])
  }
  keep <- keep[!(!nzchar(trimws(keep)) & c(TRUE, !nzchar(trimws(utils::head(keep, -1)))))]

  ## ⭐ 2026-09-07: 밴쿠버 번호는 «본문 첫 등장 순»이라 파일의 물리적 순서와 다르다.
  ##    파일은 사람이 읽는 묶음(검증 상태·주제)으로 정렬돼 있으므로 그대로 두고,
  ##    조립본에서만 번호순으로 낸다. ⛔ 파일을 재정렬하지 말 것 — 묶음이 사라진다.
  st <- which(grepl("^\\s*[0-9]+\\. ", keep))
  if (length(st)) {
    en  <- c(utils::tail(st, -1) - 1L, length(keep))
    num <- as.integer(sub("^\\s*([0-9]+)\\..*$", "\\1", keep[st]))
    if (anyDuplicated(num) || !identical(sort(num), seq_along(num)))
      stop(sprintf("참고문헌 번호가 1..%d 연속이 아니다: %s",
                   length(num), paste(sort(num), collapse = ",")))
    keep <- unlist(lapply(order(num), function(i) c(keep[st[i]:en[i]], "")))
  }

  cat(sprintf("--- 참고문헌 추출: 항목 %d개 (주석 %d줄 제외 · 철회 %d건 제외)\n",
              sum(grepl("^\\s*[0-9]+\\. ", keep)), n_note, n_dead))
  c("# References", keep)
}

assemble <- function() {
  cat("=== 12_assemble.R ===\n")
  md_dir <- file.path(PAPER_ROOT, "manuscript")

  ## 편집 지침 블록을 벗긴다 — 09 게이트가 쓰는 것과 «같은» 규칙이어야 한다
  strip <- function(f) {
    x <- readLines(file.path(md_dir, f), warn = FALSE, encoding = "UTF-8")
    cut <- which(trimws(x) == "---" & c(FALSE, trimws(utils::head(x, -1)) == "---"))
    if (length(cut)) x <- x[seq_len(cut[1] - 2)]
    x
  }

  have <- ASSEMBLE_ORDER[file.exists(file.path(md_dir, ASSEMBLE_ORDER))]
  miss <- setdiff(ASSEMBLE_ORDER, have)
  if (length(miss)) cat(sprintf("  주의  없는 절 파일: %s\n", paste(miss, collapse = ", ")))

  body <- have[!(have %in% NOT_BODY)]
  body_txt <- body[body != "99_references.md"]   # 참고문헌은 «뽑아서» 따로 넣는다
  pre_ref  <- have[have %in% BACK_BEFORE_REFS]
  addl     <- have[have %in% ADDITIONAL_FILES]

  out <- c(
    "<!-- 자동 조립본. ⛔ 이 파일을 고치지 말 것 — 절 파일을 고치고 run_paper.R 을 다시 돌린다. -->",
    sprintf("<!-- generated %s by R/12_assemble.R -->", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "")
  for (f in body_txt) out <- c(out, strip(f), "")
  ## BMC 순서: … Conclusions → List of abbreviations → Declarations → References
  for (f in pre_ref) out <- c(out, strip(f), "")
  out <- c(out, clean_references(md_dir), "")

  ## ② 본문 표를 «그 자리에» — 11_tables.R 산출을 그대로 싣는다
  tf <- file.path(tables_dir, paste0("manuscript_tables_", stamp, ".md"))
  if (file.exists(tf)) {
    out <- c(out, "---", "", readLines(tf, warn = FALSE, encoding = "UTF-8"), "")
  } else {
    ## ⛔⛔ 2026-09-07: 여기가 «경고만 하고 초안을 만들었다». 그런데 13_docx.py 는
    ##    «가장 최신» 초안을 집으므로, 그 초안이 그대로 표 없는 totale 이 된다.
    ##    실제로 그날 표 48줄이 통째로 빠진 초안이 만들어졌다(스탬프가 오늘인데
    ##    output/tables 에는 260903 까지뿐이었다).
    ## ⭐ 낡은 표로 «대신 채우지도» 않는다 — 본문과 표가 다른 실행에서 나오면
    ##    전역 CLAUDE.md 「Manuscript Version Control」 ④ 위반이다. **멈춘다.**
    stop(sprintf(paste0("본문 표가 없다: %s
",
                        "  11_tables.R 을 먼저 돌려 오늘 스탬프의 표를 만들 것.
",
                        "  (표를 낡은 판으로 채우지 않는다 - 본문과 표는 같은 실행이어야 한다)"), tf))
  }

  ## ③ 그림 — 경로와 «자동 생성된» 범례를 함께
  lf <- file.path(figures_dir, "figure_legends.md")
  if (file.exists(lf)) {
    figs <- basename(list.files(figures_dir, pattern = paste0("_", stamp, "\\.png$")))
    out <- c(out, "---", "", readLines(lf, warn = FALSE, encoding = "UTF-8"), "",
             "### Figure files", "",
             sprintf("- `output/figures/%s`", sort(figs)), "")
  }

  for (f in addl) out <- c(out, "---", "", strip(f), "")

  ## ⭐ 영문 저널에 가는 통합본에 «한국어»가 남아 있으면 안 된다. 실측(2026-09-02):
  ##    99_references.md 의 작업 주석(Pass 2 상태·경고)이 통째로 들어가 본문이 5,600단어인데
  ##    통합본이 11,295단어가 됐다. 그 주석은 우리가 읽는 것이지 공저자·편집자가 읽는 것이 아니다.
  ##    ⛔ 자동으로 «지우지» 않는다 — 기계가 지우면 진짜 내용도 조용히 사라진다. 세어서 «말한다».
  ## ⛔ 2026-09-07: 이 셈이 body_txt «만» 봤다. 그래서 94·95·96·97 의 한국어 작업 주석
  ##    103줄이 매번 통합본에 실리면서 아무 경고도 나지 않았다. 뒷부분도 센다.
  ## ⚠ 그리고 `[가-힣]` 는 로케일에 따라 «—»·«−» 를 잡는다(2026-09-07 Git Bash 실측).
  ##    코드포인트로 본다.
  chk <- c(body_txt, pre_ref, addl)
  ko <- vapply(chk, function(f) sum(grepl("\\p{Hangul}", strip(f), perl = TRUE)), 0L)
  if (any(ko > 0L)) {
    cat(sprintf("  주의  통합본에 한국어 줄이 남아 있다 - %s\n",
                paste(sprintf("%s %d줄", chk[ko > 0L], ko[ko > 0L]), collapse = " · ")))
    out <- c(utils::head(out, 2),
             "> ⚠ **이 조립본에는 한국어 작업 주석이 남아 있다.** 공저자·저널에 보내기 전에",
             "> 그 절을 정리할 것 — 어디인지는 run.log 의 「통합본에 한국어」 줄이 말한다.",
             "", utils::tail(out, -2))
  }

  f_out <- file.path(md_dir, sprintf("manuscript_draft_%s.md", stamp))
  writeLines(out, f_out, useBytes = TRUE)
  n_w <- length(unlist(strsplit(paste(unlist(lapply(body_txt, strip)), collapse = " "), "\\s+")))
  cat(sprintf("--- 통합본: %s (본문 %d절 · %s 단어 · 참고 %d절)\n",
              basename(f_out), length(body), format(n_w, big.mark = ","),
              length(pre_ref) + length(addl)))
  invisible(f_out)
}
