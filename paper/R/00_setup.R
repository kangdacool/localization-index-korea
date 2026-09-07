##################################################################
#####  00_setup.R - 자료원(보건자원감시)을 빌려 쓴다          #####
##################################################################
##
## 이 프로젝트는 **관측 기틀을 «자료원»으로 삼는다.** `2nd/dataonly` 가 보건자원감시의
## 자료원인 것과 같은 관계다 — 그래서 형제 폴더로 갈라져 있다.
##
## 상속하는 것: 폰트·팔레트·save_fig·write_table·panel_pick (두 벌로 두면 갈라진다)
## 갈라 두는 것: 출력 경로 (`tables_dir`·`figures_dir`·`proc_dir`)
##
## ⛔ API 를 치지 않는다. 자료원이 저장해 둔 `data/processed/` 만 읽는다.

## 루트는 «이 파일의 위치»에서 찾는다. getwd() 로 찾으면 다른 폴더에서 실행할 때 깨진다.
.find_root <- function() {
  if (exists(".here", envir = globalenv())) {
    d <- dirname(get(".here", envir = globalenv()))
    if (file.exists(file.path(d, "R", "04_decomposition.R"))) return(d)
  }
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) {
    d <- dirname(dirname(normalizePath(sub("^--file=", "", a[1]))))
    if (file.exists(file.path(d, "R", "04_decomposition.R"))) return(d)
  }
  d <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:5) {
    if (file.exists(file.path(d, "R", "04_decomposition.R"))) return(d)
    d <- dirname(d)
  }
  stop("자체충족률논문 루트를 찾지 못했다. 프로젝트 안에서 실행할 것")
}
PAPER_ROOT <- normalizePath(.find_root(), mustWork = TRUE)

## 자료원은 «형제»다. 이름을 박지 않고 «표지로» 찾는다 — 폴더 이름이 바뀌어도,
## 다른 기계에서 위치가 달라도 돌아야 한다(랩 규칙: 기계 고정 경로 금지).
.find_source <- function() {
  agents <- dirname(PAPER_ROOT)
  for (d in list.dirs(agents, recursive = FALSE)) {
    if (file.exists(file.path(d, "R", "00_config.R")) &&
        file.exists(file.path(d, "R", "indicator_registry.csv"))) return(d)
  }
  stop("자료원(보건자원감시)을 ", agents, " 아래에서 찾지 못했다.\n",
       "  표지: R/00_config.R + R/indicator_registry.csv 를 가진 형제 폴더")
}
SOURCE_ROOT <- normalizePath(.find_source(), mustWork = TRUE)

## ⚠ 자료원의 `00_config.R` 은 **getwd() 로 PROJ_ROOT 를 잡는다.** 논문 폴더에도
## `CLAUDE.md` 가 있어서 그냥 source 하면 **논문 폴더를 자료원으로 오인**하고,
## 곧바로 없는 `01_registry.R` 을 찾다 죽는다(2026-09-01 실측).
## → 자료원 폴더에서 «서서» 읽고 제자리로 돌아온다. 자료원 코드는 고치지 않는다.
local({
  old <- setwd(SOURCE_ROOT)
  on.exit(setwd(old), add = TRUE)
  source(file.path(SOURCE_ROOT, "R", "00_config.R"), local = FALSE)
  source(file.path(SOURCE_ROOT, "R", "03_build_panel.R"), local = FALSE)
})
if (!identical(normalizePath(PROJ_ROOT), SOURCE_ROOT))
  stop("00_setup: 자료원 인식 실패 — PROJ_ROOT=", PROJ_ROOT)

## 그 «다음»에 출력 경로를 갈아 끼운다. 순서가 중요하다 — 자료원을 나중에 읽으면
## 그쪽 경로가 이 값을 덮어써서 산출물이 자료원 폴더로 샌다(실제로 한 번 그랬다).
tables_dir  <- file.path(PAPER_ROOT, "output", "tables")
figures_dir <- file.path(PAPER_ROOT, "output", "figures")
notes_dir   <- file.path(PAPER_ROOT, "notes")
parent_proc <- proc_dir                                        # 읽기 전용(자료원)
proc_dir    <- file.path(PAPER_ROOT, "output", "processed")    # 쓰기(여기)
for (d in c(tables_dir, figures_dir, notes_dir, proc_dir))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

## 자료원의 중간산출을 읽는 통로. 자료원이 한 번도 안 돌았으면 «여기서» 멈춘다 —
## 뒤에서 「행이 0개」로 조용히 실패하는 것보다 낫다.
parent_step <- function(pattern, what) {
  f <- tryCatch(latest_file(parent_proc, pattern), error = function(e) NULL)
  if (is.null(f))
    stop("자료원의 ", what, " 이 없다(", pattern, ").\n",
         "  먼저 `Rscript ", file.path(SOURCE_ROOT, "R", "run_pipeline.R"), "` 를 돌릴 것")
  ## ⚠ 자료원의 save_step 은 `.rds` 를 쓴다(전역 템플릿의 `.RData` 가 아니다).
  ## load() 로 읽으면 "bad restore file magic number" 로 죽는다 — 실제로 한 번 그랬다.
  obj <- readRDS(f)
  message("자료원에서 읽음: ", basename(f))
  obj
}

## 이 프로젝트가 «자기» 단계에서 낸 것을 읽는다(parent_step 은 자료원 쪽이다).
## 둘을 섞으면 「없다」는 오류가 엉뚱한 곳을 가리킨다 — 실제로 한 번 그랬다.
paper_step <- function(pattern, what) {
  f <- tryCatch(latest_file(proc_dir, pattern), error = function(e) NULL)
  if (is.null(f))
    stop("이 프로젝트의 ", what, " 이 없다(", pattern, "). run_paper.R 를 처음부터 돌릴 것")
  readRDS(f)
}

cat("=== 자체충족률논문/00_setup.R ===\n")
cat("--- 논문 :", PAPER_ROOT, "\n")
cat("--- 자료원:", SOURCE_ROOT, "\n")

## ── 그림은 «영문 저널 제출본»이다 (2026-09-02 감사 0-2) ────────────────────────
## 표와 콘솔은 한국어를 유지한다. 그림만 영문 — 그림이 원고에 «실리는» 것이기 때문이다.
## ⚠ 라벨을 각 스크립트에 흩어 두면 하나만 한글로 남는다. 여기 한 곳에 둔다.
TYPE_EN <- c("구" = "Gu (urban district)", "시" = "Si (city)", "군" = "Gun (rural county)")
en_type <- function(x) factor(unname(TYPE_EN[as.character(x)]), levels = unname(TYPE_EN))
## ⚠ 선 «옆»에 붙이는 직접 라벨은 짧아야 한다 — 전체 이름을 쓰면 패널 밖으로 흘러 선을 덮는다
## (2026-09-02 렌더해서 눈으로 확인). 전체 이름은 범례가 진다.
TYPE_ABBR <- c("구" = "Gu", "시" = "Si", "군" = "Gun")
abbr_type <- function(x) unname(TYPE_ABBR[as.character(x)])

## ── 그림 규격: 저널이 «인쇄할 크기»로 만든다 (2026-09-02) ────────────────────
## IJEqH: 전폭 170 mm · 반폭 85 mm · 캡션 포함 최대 높이 225 mm · 최종 크기에서 ~300 dpi ·
##        모든 선 >0.25 pt · 그림 제목 <=15 단어 · 범례 <=300 단어.
## ⚠ 이 값을 안 쓰고 자료원 기본값(9인치=229 mm)으로 두면, 저널이 170 mm 로 줄이면서
##    글자가 25% 작아진다 — «설계한 크기로 찍히지 않는다». 실측으로 그랬다.
FIG_W      <- 170 / 25.4      # 6.69 in — 전폭
FIG_W_HALF <-  85 / 25.4      # 3.35 in — 반폭
FIG_H_MAX  <- 225 / 25.4      # 8.86 in — 캡션 포함 상한

## ── 저널용 그림 저장 ────────────────────────────────────────────────────────
## IJEqH 는 **그림 제목·범례를 그래픽이 아니라 «본문»에** 두라고 규정한다.
## 그래서 `labs()` 의 title/subtitle/caption 을 «벗겨서» 저장하고, 벗긴 문구를
## figure_legends.md 로 내보낸다. 그림과 범례가 한 곳에서 나오므로 갈라질 수 없다.
## ⚠ labs() 는 여전히 정본이다 — 코드를 읽는 사람은 그림이 무슨 말을 하는지 거기서 본다.
.FIG_LEGENDS <- new.env(parent = emptyenv())

save_fig_journal <- function(plot, name, width = FIG_W, height = NULL) {
  if (is.null(height)) height <- width * 0.6
  stopifnot(width <= FIG_W + 1e-9, height <= FIG_H_MAX)
  L <- plot$labels
  n <- as.integer(sub("^p_fig([0-9]+).*$", "\\1", name))
  assign(sprintf("%02d", n), list(
    n = n, title = L$title, subtitle = L$subtitle, caption = L$caption),
    envir = .FIG_LEGENDS)
  ## 벗긴다 — 저널이 본문에서 받는다
  plot <- plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  f <- file.path(figures_dir, png_name(name))
  ggplot2::ggsave(f, plot, width = width, height = height, dpi = 300, bg = "white")
  message("Figure: ", basename(f), sprintf("  (%.0f x %.0f mm)", width * 25.4, height * 25.4))
  invisible(f)
}

## 파이프라인 끝에서 부른다 — 원고가 이 파일을 그대로 옮긴다
write_figure_legends <- function() {
  ## ⛔ 2026-09-02 본문/보충 배분: 파일 번호와 «원고 번호»가 다르다.
  ##    본문 = 궤적(1) · 산점도(2) · 타일분해(3) / 보충 = 구군격차(S1) · 재정5분위(S2)
  ##    실측 IJEqH 중앙값이 표+그림 합 6 이라 12 → 7 로 줄인 결과다.
  FIG_LABEL <- c("1" = "1", "2" = "S1", "3" = "2", "4" = "3", "5" = "S2")
  ks <- sort(ls(.FIG_LEGENDS))
  if (!length(ks)) return(invisible(NULL))
  ## ⛔ 2026-09-07: 여기 있던 머리말이 «한국어 작업 지시»였고 통합본에 그대로 실렸다.
  ##    이 파일은 원고에 스플라이스되는 «독자가 보는 표면»이다 — 빌더 이름도 규정 인용도
  ##    여기 두지 않는다(IJEqH 가 범례를 본문에 두라고 한다는 사실은 이 주석이 갖는다).
  ## ⛔ 손으로 고치지 말 것 — 정본은 각 스크립트의 `labs()` 다.
  out <- c("# Figure legends", "")
  for (k in ks) {
    g <- get(k, envir = .FIG_LEGENDS)
    lab <- FIG_LABEL[[as.character(g$n)]]
    out <- c(out, sprintf("**%s %s** %s",
                          ## ⛔ 본문은 「Figure 1」이라고 쓴다. 범례가 「Fig. 1」이면 한 문서가 같은 것을 두 이름으로
  ##    부르고, display_items 검사는 그 둘을 잇지 못해 「유령 그림」으로 신고한다(2026-09-07).
                          if (startsWith(lab, "S")) "Supplementary Figure" else "Figure",
                          lab, g$title))
    ## ⛔ 2026-09-07 렌더에서 발견: 부제와 캡션이 «한 문단으로 이어 붙어» 문장이 끊기지
    ##    않았다 — 「…expenditure basis Levels are not compared…」. 조립기는 빈 줄이
    ##    없으면 이어지는 줄을 한 문단으로 «합친다»(하드랩 원고라 그래야 한다).
    ##    → 마침표를 붙이고, 캡션은 «빈 줄»로 갈라 다른 문단이 되게 한다.
    .stop <- function(s) if (grepl("[.!?]$", s)) s else paste0(s, ".")
    if (!is.null(g$subtitle)) out <- c(out, .stop(g$subtitle))
    ## ⚠ 빈 줄로 «가르지» 않는다 — 조립기가 범례 블록을 「빈 줄까지」로 세기 때문에
    ##    중간에 빈 줄이 있으면 보충 그림 범례를 건너뛰다가 캡션만 본문에 남는다.
    if (!is.null(g$caption))  out <- c(out, .stop(gsub("\n", " ", g$caption)))
    out <- c(out, "")
  }
  f <- file.path(figures_dir, "figure_legends.md")
  writeLines(out, f, useBytes = TRUE)
  message("Figure legends: ", basename(f))
  invisible(f)
}
