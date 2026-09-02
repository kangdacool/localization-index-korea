##################################################################
#####  run_paper.R - 논문 파이프라인 전체                     #####
##################################################################
##
##   Rscript 자체충족률논문/R/run_paper.R
##
## ⛔ 자료원(보건자원감시)을 «먼저» 한 번 돌려야 한다. 이 파이프라인은 API 를 치지 않고
##    자료원의 data/processed/ 만 읽는다. 없으면 00_setup.R 의 parent_step() 이 멈춘다.

## 스크립트가 «자기 위치»를 찾는다. Rscript 는 `--file=` 로 넘겨주므로 그것이 정답이고,
## source() 나 REPL 이면 후보를 훑는다.
## ⚠ `sys.frame(1)$ofile` 만 믿으면 호출 방식에 따라 NULL 이 되어 엉뚱한 경로를 잡는다
##    (2026-09-01: 폴더를 옮긴 뒤 그것 때문에 죽었다).
.here <- local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) return(dirname(normalizePath(sub("^--file=", "", a[1]))))
  for (d in c(file.path(getwd(), "R"), getwd(), file.path(getwd(), "..", "R")))
    if (file.exists(file.path(d, "00_setup.R"))) return(normalizePath(d))
  stop("run_paper.R: 스크립트 위치를 찾지 못했다")
})

source(file.path(.here, "00_setup.R"))
source(file.path(.here, "01_cohort.R"))
source(file.path(.here, "02_variables.R"))
source(file.path(.here, "03_descriptive.R"))
source(file.path(.here, "04_decomposition.R"))
source(file.path(.here, "05_trajectory.R"))
source(file.path(.here, "06_inequality.R"))
source(file.path(.here, "07_sensitivity.R"))
source(file.path(.here, "07_2_altmeasure.R"))
source(file.path(.here, "08_socioeconomic.R"))
source(file.path(.here, "10_supplement.R"))
source(file.path(.here, "11_tables.R"))
source(file.path(.here, "12_assemble.R"))
source(file.path(.here, "09_manuscript_check.R"))

t0 <- Sys.time()
co <- build_cohort()
d  <- add_variables(co$balanced)
describe_cohort(d)
decompose(d)
trajectory(d)
inequality(d)
sensitivity(d, co$all)
altmeasure(co)
socioeconomic(d)
build_tables()
write_figure_legends()
supplement()
manuscript_check()
assemble()

## ⛔ 2026-09-02 감사: 이 파이프라인이 sessionInfo() 를 «안 남기고» 있었다(랩 규칙 위반).
##    그리고 논문 숫자가 «부모의 어느 빈티지» 위에 섰는지 남는 곳이 덮어써지는 run.log 한 줄뿐이었다.
##    둘을 함께 남긴다 — 재현은 「같은 코드」가 아니라 「같은 코드 + 같은 입력」이다.
local({
  d <- file.path(PAPER_ROOT, "logs"); dir.create(d, showWarnings = FALSE)
  f <- file.path(d, sprintf("sessionInfo_%s.txt", stamp))
  src <- list.files(parent_proc, pattern = "\\.rds$", full.names = TRUE)
  keep <- src[order(file.mtime(src), decreasing = TRUE)]
  keep <- keep[!duplicated(sub("_[0-9]{6}\\.rds$", "", basename(keep)))]
  writeLines(c(
    "=== 부모(자료원) 입력 빈티지 — 이 숫자들이 «무엇 위에» 섰는가 ===",
    sprintf("%-28s %s", basename(keep), format(file.mtime(keep), "%Y-%m-%d %H:%M")),
    "", "=== sessionInfo() ===",
    capture.output(utils::sessionInfo())), f, useBytes = TRUE)
  message("Log: ", basename(f))
})

cat(sprintf("\n=== 자체충족률논문 done in %.1f min | 표 %d · 그림 %d ===\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")),
            length(list.files(tables_dir, pattern = paste0("_", stamp, "\\.xlsx$"))),
            length(list.files(figures_dir, pattern = paste0("_", stamp, "\\.png$")))))
cat("설계와 제약: CLAUDE.md\n")
