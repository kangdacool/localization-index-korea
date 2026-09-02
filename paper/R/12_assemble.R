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
    if (!in_item) next

    ## ⭐ 서지가 먼저, 주석이 뒤. 한 번 주석이 시작되면 그 항목 끝까지 주석이다.
    if (in_note) { n_note <- n_note + 1L; next }

    m <- regexpr(MARK, x[i], perl = TRUE)
    if (m > 0L) {                              # 마커가 줄 «중간»이면 그 앞까지만 남긴다
      head_txt <- sub("\\s+$", "", substr(x[i], 1L, m - 1L))
      in_note <- TRUE; n_note <- n_note + 1L
      if (nzchar(trimws(head_txt))) keep <- c(keep, head_txt)
      next
    }
    if (grepl(KEY, x[i])) { in_note <- TRUE; n_note <- n_note + 1L; next }
    if (!nzchar(trimws(x[i]))) next            # 항목 내부 빈 줄은 버린다
    keep <- c(keep, x[i])
  }
  keep <- keep[!(!nzchar(trimws(keep)) & c(TRUE, !nzchar(trimws(utils::head(keep, -1)))))]
  cat(sprintf("--- 참고문헌 추출: 항목 %d개 (주석 %d줄 제외 · 철회 %d건 제외)\n",
              sum(grepl("^\\s*[0-9]+\\. ", keep)), n_note, n_dead))
  c("## References", keep)
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
  back <- have[have %in% NOT_BODY & have != "_00_titlepage.md"]

  out <- c(
    "<!-- 자동 조립본. ⛔ 이 파일을 고치지 말 것 — 절 파일을 고치고 run_paper.R 을 다시 돌린다. -->",
    sprintf("<!-- generated %s by R/12_assemble.R -->", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "")
  for (f in body_txt) out <- c(out, strip(f), "")
  out <- c(out, clean_references(md_dir), "")

  ## ② 본문 표를 «그 자리에» — 11_tables.R 산출을 그대로 싣는다
  tf <- file.path(tables_dir, paste0("manuscript_tables_", stamp, ".md"))
  if (file.exists(tf)) {
    out <- c(out, "---", "", readLines(tf, warn = FALSE, encoding = "UTF-8"), "")
  } else {
    out <- c(out, "---", "", "> ⚠ 본문 표가 없다 — `11_tables.R` 을 먼저 돌릴 것.", "")
  }

  ## ③ 그림 — 경로와 «자동 생성된» 범례를 함께
  lf <- file.path(figures_dir, "figure_legends.md")
  if (file.exists(lf)) {
    figs <- basename(list.files(figures_dir, pattern = paste0("_", stamp, "\\.png$")))
    out <- c(out, "---", "", readLines(lf, warn = FALSE, encoding = "UTF-8"), "",
             "### Figure files", "",
             sprintf("- `output/figures/%s`", sort(figs)), "")
  }

  for (f in back) out <- c(out, "---", "", strip(f), "")

  ## ⭐ 영문 저널에 가는 통합본에 «한국어»가 남아 있으면 안 된다. 실측(2026-09-02):
  ##    99_references.md 의 작업 주석(Pass 2 상태·경고)이 통째로 들어가 본문이 5,600단어인데
  ##    통합본이 11,295단어가 됐다. 그 주석은 우리가 읽는 것이지 공저자·편집자가 읽는 것이 아니다.
  ##    ⛔ 자동으로 «지우지» 않는다 — 기계가 지우면 진짜 내용도 조용히 사라진다. 세어서 «말한다».
  ko <- vapply(body_txt, function(f) sum(grepl("[가-힣]", strip(f))), 0L)
  if (any(ko > 0L)) {
    cat(sprintf("  주의  통합본에 한국어 줄이 남아 있다 - %s\n",
                paste(sprintf("%s %d줄", body_txt[ko > 0L], ko[ko > 0L]), collapse = " · ")))
    out <- c(utils::head(out, 2),
             "> ⚠ **이 조립본에는 한국어 작업 주석이 남아 있다.** 공저자·저널에 보내기 전에",
             "> 그 절을 정리할 것 — 어디인지는 run.log 의 「통합본에 한국어」 줄이 말한다.",
             "", utils::tail(out, -2))
  }

  f_out <- file.path(md_dir, sprintf("manuscript_draft_%s.md", stamp))
  writeLines(out, f_out, useBytes = TRUE)
  n_w <- length(unlist(strsplit(paste(unlist(lapply(body_txt, strip)), collapse = " "), "\\s+")))
  cat(sprintf("--- 통합본: %s (본문 %d절 · %s 단어 · 참고 %d절)\n",
              basename(f_out), length(body), format(n_w, big.mark = ","), length(back)))
  invisible(f_out)
}
