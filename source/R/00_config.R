##################################################################
#####  00_config.R - paths, packages, secrets, shared helpers  #####
##################################################################
##
## Sourced by every other script. Defines PROJ_ROOT by marker file, so the
## pipeline runs on any machine and from any working directory.
##
## NOTE (encoding): this file is UTF-8 without BOM. Do not add a BOM - R will
## choke on the first line. Diagnostic messages use plain ASCII dashes only;
## em-dashes in cat() can crash the parser on Windows R.

##################################################################
#####  1. PROJECT ROOT                                       #####
##################################################################

.find_proj_root <- function() {
  candidates <- c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."))
  for (d in candidates) {
    d <- normalizePath(d, mustWork = FALSE)
    if (length(list.files(d, pattern = "\\.Rproj$")) > 0 ||
        file.exists(file.path(d, "CLAUDE.md"))) {
      return(d)
    }
  }
  stop("PROJ_ROOT not found: no .Rproj or CLAUDE.md marker above ", getwd())
}

PROJ_ROOT   <- .find_proj_root()

r_dir       <- file.path(PROJ_ROOT, "R")
data_dir    <- file.path(PROJ_ROOT, "data")
raw_dir     <- file.path(data_dir, "raw")
proc_dir    <- file.path(data_dir, "processed")
output_dir  <- file.path(PROJ_ROOT, "output")
tables_dir  <- file.path(output_dir, "tables")
figures_dir <- file.path(output_dir, "figures")
logs_dir    <- file.path(PROJ_ROOT, "logs")
notes_dir   <- file.path(PROJ_ROOT, "notes")

for (d in c(raw_dir, proc_dir, tables_dir, figures_dir, logs_dir, notes_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

## Shared raw data lives in 2nd/dataonly/, one copy, read directly - no copies
## inside analysis projects (lab convention). Defined once here so a path cannot
## drift across scripts.
DATA_ONLY <- normalizePath(file.path(PROJ_ROOT, "..", "..", "2nd", "dataonly"),
                           mustWork = FALSE)

stamp <- format(Sys.Date(), "%y%m%d")

##################################################################
#####  2. PACKAGES                                           #####
##################################################################

packages <- c("httr2", "jsonlite", "dplyr", "tidyr", "readr",
              "ggplot2", "writexl", "scales", "stringr", "ggrepel")

.missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(.missing) > 0) {
  cat("--- installing missing packages:", paste(.missing, collapse = ", "), "\n")
  install.packages(.missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE))

##################################################################
#####  3. SECRETS - the only route to the API key            #####
##################################################################
##
## Lab convention: a plain "KEY=value" file named .secrets at a project root,
## gitignored. The master store for Korean government APIs is the sibling
## project air_lone_seoul; several projects already read it from there.
## Never hardcode a key, never echo one into a log or an output file.

SECRETS_FALLBACK <- file.path(PROJ_ROOT, "..", "..", "2nd", "env_lone",
                              "air_lone_seoul", ".secrets")

read_secret <- function(name,
                        paths = c(file.path(PROJ_ROOT, ".secrets"), SECRETS_FALLBACK)) {
  env_val <- Sys.getenv(name, unset = "")
  if (nzchar(env_val)) return(env_val)
  for (p in paths) {
    p <- normalizePath(p, mustWork = FALSE)
    if (!file.exists(p)) next
    for (line in readLines(p, warn = FALSE)) {
      if (startsWith(line, paste0(name, "="))) {
        return(trimws(sub(paste0("^", name, "="), "", line)))
      }
    }
  }
  stop(name, " not found. Set the environment variable or add it to a .secrets file.\n",
       "  looked in: ", paste(normalizePath(paths, mustWork = FALSE), collapse = ", "))
}

##################################################################
#####  4. FILE HELPERS                                       #####
##################################################################

excel_name <- function(file) paste0(file, "_", stamp, ".xlsx")
png_name   <- function(file) paste0(file, "_", stamp, ".png")
rds_name   <- function(file) paste0(file, "_", stamp, ".rds")

latest_file <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matching '", pattern, "' in ", dir)
  files[which.max(file.mtime(files))]
}

## Intermediates live in data/processed only. output/ holds tables + figures,
## i.e. things run_pipeline.R can recreate after a delete.
save_step <- function(x, name) {
  f <- file.path(proc_dir, paste0(name, "_", stamp, ".rds"))
  saveRDS(x, f)
  message("Saved: ", f)
  invisible(f)
}
load_step <- function(pattern) readRDS(latest_file(proc_dir, pattern))

write_table <- function(x, name) {
  f <- file.path(tables_dir, excel_name(name))
  writexl::write_xlsx(x, f)
  message("Table: ", basename(f))
  invisible(f)
}

##################################################################
#####  5. FIGURES - Korean text on Windows R                 #####
##################################################################
##
## ggsave on Windows silently drops Korean glyphs (exit code 0, no warning);
## the failure only shows in the saved PNG. Setting a CJK-capable family for
## every device up front is the fix. Always open the PNG afterwards.

KO_FAMILY <- if (.Platform$OS.type == "windows") "Malgun Gothic" else "sans"
if (.Platform$OS.type == "windows") {
  try(windowsFonts(ko = windowsFont(KO_FAMILY)), silent = TRUE)
}

##################################################################
#####  「최저·최고」를 말할 때 세종을 뺀다                   #####
##################################################################
##
## 세종은 2012년 충청남도에서 «떼어 만든» 행정도시다. 상급종합병원이 없고, 인구가
## 적고, 생활권이 대전에 붙어 있다. 그래서 시도 순위에서 거의 언제나 극단값이 되는데,
## **그 극단은 「의료자원이 부족하다」가 아니라 「행정구역이 특수하다」를 뜻한다.**
## 「최저 세종」을 반복해 말하면 읽는 사람이 매번 같은 각주를 붙여야 한다.
##
## ⛔ **자료에서 빼는 것이 «아니다».** 17개 시도 합계 게이트도, 지니도, 그림의 막대도
##    그대로 둔다 — 빼면 전국 합이 안 맞고, 숨긴 것이 된다.
##    빼는 것은 **「최저는 어디인가」라는 «문장»뿐**이고, 그 문장에는 「세종 제외」를 적는다.
HEADLINE_DROP_SIDO <- "세종"

## 순위 문장을 만들기 «직전»에만 쓴다. 그림·표·게이트에는 쓰지 않는다.
drop_headline_sido <- function(d, col = "sido") {
  d[!(d[[col]] %in% HEADLINE_DROP_SIDO), , drop = FALSE]
}
SEJONG_NOTE <- "세종은 2012년 신설된 특수 행정구역이라 최저·최고 집계에서 뺐다(자료에는 있다)."

##################################################################
#####  색 — 색으로 볼 수 있는 사람은 색으로 보게 한다        #####
##################################################################
##
## 랩 규칙(figure_design_defaults.md)은 «흑백으로 그려라»가 아니라
## **「색상 + 선종류(lty) 중복 인코딩」**이다. 색을 빼는 것이 아니라 «색에만 의존하지»
## 않는 것이 요점이고, 그 파일은 Okabe-Ito 쌍(#0072B2 / #D55E00)을 직접 권한다.
## (2026-09-01 정정: 그 전까지 scale_colour_grey() 로 색을 아예 빼고 있었다.)
##
## 이 순서는 «검증된 것»이다 — dataviz 스킬의 validate_palette.js 실측:
##   명도대·채도 하한·CVD 인접쌍 분리(최악 ΔE 9.6 deutan)·정상시야 하한 전부 PASS.
## ⚠ 순서를 바꾸지 말 것. #CC79A7 과 #009E73 이 «이웃»이면 ΔE 7.6 으로 떨어진다
##    (원래 순서가 그랬고, 그래서 넷째·다섯째를 맞바꿨다).
## ⚠ 뒤 세 색(#E69F00·#CC79A7·#56B4E9)은 배경 대비가 3:1 미만이다. 그래서 **범례와
##    선종류가 «선택»이 아니다** — 그 둘이 이 팔레트를 합법으로 만드는 구제책이다.
PAL_HRM <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7", "#56B4E9")

## 계열이 하나뿐인 그림(막대·상자·산점)은 색이 «정보를 나르지 않는다». 그래도 회색보다
## 한 색이 낫다 — 랩 규칙의 「색은 정보를 나를 때만」은 «무지개 금지»이지 «무채색 강제»가 아니다.
ACCENT <- PAL_HRM[1]
ACCENT_FILL <- "#CFE3F2"   # 상자·띠의 옅은 면. ACCENT 와 같은 색상, 훨씬 밝게.

scale_colour_hrm <- function(...) ggplot2::scale_colour_manual(values = PAL_HRM, ...)
scale_fill_hrm   <- function(...) ggplot2::scale_fill_manual(values = PAL_HRM, ...)

theme_hrm <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size, base_family = KO_FAMILY) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey92", colour = NA),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.caption     = ggplot2::element_text(colour = "grey35", hjust = 0)
    )
}

## Caption wrapping. ggplot does not wrap a caption, it just runs it off the
## right edge - and that is invisible in every structural check, only in the
## rendered PNG (a 2021-reclassification note was clipped mid-sentence this way).
## Korean glyphs occupy two display columns, so wrapping on character count
## alone breaks far too late.
.disp_width <- function(s) {
  cp <- utf8ToInt(s)
  if (length(cp) == 0) return(0L)
  sum(ifelse(cp >= 0x1100 & (cp <= 0x115F | (cp >= 0x2E80 & cp <= 0xA4CF) |
             (cp >= 0xAC00 & cp <= 0xD7A3) | (cp >= 0xF900 & cp <= 0xFAFF) |
             (cp >= 0xFE30 & cp <= 0xFE6F) | (cp >= 0xFF00 & cp <= 0xFF60) |
             (cp >= 0xFFE0 & cp <= 0xFFE6)), 2L, 1L))
}

wrap_cap <- function(x, cols = 117) {
  if (is.null(x) || !is.character(x) || length(x) != 1) return(x)
  out <- character(0)
  for (para in strsplit(x, "\n", fixed = TRUE)[[1]]) {
    words <- strsplit(para, " ", fixed = TRUE)[[1]]
    line <- ""
    for (w in words) {
      cand <- if (nzchar(line)) paste(line, w) else w
      if (.disp_width(cand) > cols && nzchar(line)) {
        out <- c(out, line); line <- w
      } else line <- cand
    }
    out <- c(out, line)
  }
  paste(out, collapse = "\n")
}

save_fig <- function(plot, name, width = 9, height = 5.5, dpi = 300) {
  f <- file.path(figures_dir, png_name(name))
  ## Wrap here, once, so no individual figure has to remember to.
  if (!is.null(plot$labels$caption)) {
    plot$labels$caption <- wrap_cap(plot$labels$caption,
                                    cols = round(width * 13))
  }
  ggplot2::ggsave(f, plot, width = width, height = height, dpi = dpi,
                  bg = "white")
  message("Figure: ", basename(f))
  invisible(f)
}

##################################################################
#####  6. CANONICAL LABELS                                   #####
##################################################################
##
## KOSIS spells the same province differently across tables and vintages
## (e.g. 강원도 vs 강원특별자치도). Analysis joins on the canonical name.

SIDO_CANON <- c(
  "서울특별시" = "서울", "서울" = "서울",
  "부산광역시" = "부산", "부산" = "부산",
  "대구광역시" = "대구", "대구" = "대구",
  "인천광역시" = "인천", "인천" = "인천",
  "광주광역시" = "광주", "광주" = "광주",
  "대전광역시" = "대전", "대전" = "대전",
  "울산광역시" = "울산", "울산" = "울산",
  "세종특별자치시" = "세종", "세종" = "세종",
  "경기도" = "경기", "경기" = "경기",
  "강원도" = "강원", "강원특별자치도" = "강원", "강원" = "강원",
  "충청북도" = "충북", "충북" = "충북",
  "충청남도" = "충남", "충남" = "충남",
  "전라북도" = "전북", "전북특별자치도" = "전북", "전북" = "전북",
  "전라남도" = "전남", "전남" = "전남",
  "경상북도" = "경북", "경북" = "경북",
  "경상남도" = "경남", "경남" = "경남",
  "제주특별자치도" = "제주", "제주도" = "제주", "제주" = "제주"
)
SIDO_17 <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종",
             "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

canon_sido <- function(x) {
  out <- unname(SIDO_CANON[trimws(x)])
  out[is.na(out)] <- NA_character_
  out
}

## Institution tiers, ordered from most to least acute.
JONGBYEOL_ORDER <- c("상급종합병원", "종합병원", "병원", "요양병원",
                     "정신병원", "의원")

##################################################################
#####  7. PERIOD LABELS (shared by fetch and panel)          #####
##################################################################
##
## KOSIS writes a quarter as "201102" in data but "2011 2/4" in metadata,
## and a month as "2026.07". Both the fetcher and the continuity gate need
## the same parsing, so it lives here rather than in either one.

PRD_SE_KO <- c(Q = "분기", Y = "년", M = "월")

## "2026 2/4" -> "202602"   "2025" -> "2025"   "2026.07" -> "202607"
parse_prd <- function(x, prd_se) {
  x <- trimws(x)
  if (prd_se == "Q") {
    m <- regmatches(x, regexec("^(\\d{4})\\s*(\\d)/4$", x))[[1]]
    if (length(m) == 3) return(paste0(m[2], "0", m[3]))
  } else if (prd_se == "M") {
    return(gsub("[.]", "", x))
  }
  gsub("\\D", "", x)
}

## Every period label between two bounds, so a big pull can be split.
enum_periods <- function(start, end, prd_se) {
  if (prd_se == "Y") return(as.character(seq(as.integer(start), as.integer(end))))
  if (prd_se == "Q") {
    f <- function(p) as.integer(substr(p, 1, 4)) * 4L + as.integer(substr(p, 6, 6)) - 1L
    g <- function(i) sprintf("%d0%d", i %/% 4L, i %% 4L + 1L)
    return(vapply(seq(f(start), f(end)), g, character(1)))
  }
  f <- function(p) as.integer(substr(p, 1, 4)) * 12L + as.integer(substr(p, 5, 6)) - 1L
  g <- function(i) sprintf("%d%02d", i %/% 12L, i %% 12L + 1L)
  vapply(seq(f(start), f(end)), g, character(1))
}

cat("=== 00_config.R loaded ===\n")
cat("--- PROJ_ROOT:", PROJ_ROOT, "\n")
cat("--- stamp    :", stamp, "\n")

##################################################################
#####  표면 게이트 — 독자가 보는 «모든» 곳을 검사한다         #####
##################################################################
##
## ⭐ 게이트가 보는 범위가 독자가 보는 범위보다 좁으면, 좁은 그 틈으로 결함이 되살아난다.
##    2026-09-02 에 두 번 그랬다: 원고에서 지운 금지 표현이 «그림 범례»에 살아 있었고,
##    표의 «열 제목»이 다른 양을 가리키는 것을 어떤 게이트도 안 봤다(사람이 찾았다).
##
## 표면은 넷이다:
##   ① 산문  — .md / .html (원고 · 브리핑 · 페이지)
##   ② 그림 범례 — figure_legends.md (그림 «안»의 글자를 벗겨 낸 것)
##   ③ 표    — xlsx 의 **시트명과 열 제목**  ← 여기가 가장 자주 빠진다
##   ④ 그림 라벨 — PNG 안이라 직접 못 읽는다. ②가 그 대리다.
##
## ⚠ 규칙에 «문체 취향»을 넣지 말 것. 넣을 것은 「사실로 틀린 표현」과 「제약이 금지한 표현」뿐이다.
##    게이트가 소음이 되면 아무도 안 본다 — 실제로 한 번 그렇게 만들었다(2026-09-02: "monotonic"
##    단독으로 걸었더니 정당한 「not monotonic」까지 잡혔다. 패턴은 «그 오류의 형태»로 좁힌다).

## 표면 텍스트를 모은다. 반환: data.frame(surface, kind, text)
collect_surfaces <- function(md_globs = character(0),
                             xlsx_dir = NULL, figures_dir_ = NULL,
                             latest_only = TRUE) {
  out <- list()
  add <- function(sf, kind, txt) if (length(txt) && any(nzchar(txt)))
    out[[length(out) + 1L]] <<- data.frame(surface = sf, kind = kind,
                                           text = paste(txt, collapse = "\n"),
                                           stringsAsFactors = FALSE)

  ## ① 산문
  for (g in md_globs) for (f in Sys.glob(g))
    add(basename(f), "prose", readLines(f, warn = FALSE, encoding = "UTF-8"))

  ## ② 그림 범례
  if (!is.null(figures_dir_)) {
    fl <- file.path(figures_dir_, "figure_legends.md")
    if (file.exists(fl)) add("figure_legends.md", "legend",
                             readLines(fl, warn = FALSE, encoding = "UTF-8"))
  }

  ## ③ 표 — 시트명 + 열 제목. ⛔ «값»은 안 본다(숫자 게이트의 몫이다).
  if (!is.null(xlsx_dir) && requireNamespace("readxl", quietly = TRUE)) {
    fs <- list.files(xlsx_dir, pattern = "\\.xlsx$", full.names = TRUE)
    ## ⚠ 「전역 최신 stamp」로 고르면 «부분 재실행»한 프로젝트에서 표 하나만 남는다
    ##    (2026-09-02 실측: 보건자원감시 33개 중 1개만 검사됐다 — 그날 표 하나만 다시
    ##    돌렸기 때문이다). 옳은 의미는 «표 이름별» 최신이다.
    if (length(fs) && latest_only) {
      bn <- basename(fs)
      base <- sub("_[0-9]{6}\\.xlsx$", "", bn)
      st   <- sub("^.*_([0-9]{6})\\.xlsx$", "\\1", bn)
      ok   <- grepl("^[0-9]{6}$", st)
      keep <- ok & vapply(seq_along(fs), function(i)
        !ok[i] || st[i] == max(st[ok & base == base[i]]), logical(1))
      fs <- fs[keep | !ok]
    }
    for (f in fs) {
      sh <- tryCatch(readxl::excel_sheets(f), error = function(e) character(0))
      hdr <- unlist(lapply(sh, function(x) tryCatch(
        names(readxl::read_excel(f, sheet = x, n_max = 0)),
        error = function(e) character(0))))
      add(basename(f), "table", c(sh, hdr))
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

## 금지 표현을 «모든» 표면에 건다. rules = list(list(pattern, reason), ...)
## 반환: 위반 건수(0 이면 통과). 호출자가 stop() 여부를 정한다.
gate_surfaces <- function(rules, ..., quiet = FALSE) {
  sur <- collect_surfaces(...)
  if (is.null(sur)) { cat(" 주의  검사할 표면이 없다 - 경로를 확인할 것\n"); return(0L) }
  bad <- 0L
  for (r in rules) for (i in seq_len(nrow(sur))) {
    if (grepl(r[[1]], sur$text[i], fixed = TRUE)) {
      cat(sprintf(" FAIL  [%s %s] 금지 표현 \"%s\" - %s\n",
                  sur$kind[i], sur$surface[i], r[[1]], r[[2]]))
      bad <- bad + 1L
    }
  }
  if (!quiet)
    cat(sprintf("--- 표면 게이트: %d개 표면(%s) x 규칙 %d개 -> 위반 %d\n",
                nrow(sur), paste(names(table(sur$kind)), table(sur$kind),
                                 sep = "=", collapse = " "),
                length(rules), bad))
  invisible(bad)
}
