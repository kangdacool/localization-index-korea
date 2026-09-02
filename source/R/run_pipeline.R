##################################################################
#####  run_pipeline.R - the whole thing, from nothing        #####
##################################################################
##
##   Rscript R/run_pipeline.R            # reuse today's raw pull if present
##   Rscript R/run_pipeline.R --refetch  # force a fresh pull from KOSIS
##
## Deleting output/ and re-running must reproduce output/ exactly. Anything
## that would not come back that way does not belong in output/.

t_start <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
REFETCH <- "--refetch" %in% args

source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
source(file.path(r_dir, "01_registry.R"))
source(file.path(r_dir, "02_fetch_kosis.R"))
source(file.path(r_dir, "03_build_panel.R"))
source(file.path(r_dir, "04_view_volume.R"))
source(file.path(r_dir, "05_view_mix.R"))
source(file.path(r_dir, "06_view_distribution.R"))
source(file.path(r_dir, "07_view_emergency.R"))
source(file.path(r_dir, "08_view_2024.R"))
source(file.path(r_dir, "10_view_access.R"))
source(file.path(r_dir, "09_brief.R"))
source(file.path(r_dir, "12_healthmap.R"))
source(file.path(r_dir, "13_view_local.R"))
source(file.path(r_dir, "15_build_facility.R"))
source(file.path(r_dir, "16_yearbook.R"))
source(file.path(r_dir, "17_view_benchmark.R"))
source(file.path(r_dir, "11_build_page.R"))
source(file.path(r_dir, "99_acceptance.R"))

step <- function(label, expr) {
  cat("\n##########  ", label, "  ##########\n")
  tryCatch(force(expr), error = function(e) {
    cat("!!! FAILED at ", label, ": ", conditionMessage(e), "\n", sep = "")
    stop(e)
  })
}

step("1/5 collect",  fetch_all(force = REFETCH))
step("2/5 panel",    { panel <<- build_panel(); run_gates(panel, kosis_rows(load_registry())); save_step(panel, "panel") })
step("3/5 views",    {
  view_volume(panel)
  view_mix(panel)
  view_distribution(panel)
  view_emergency(panel)
  view_2024(panel)
  view_access(panel)
  hm <<- build_healthmap()
  view_local(hm)
  view_benchmark(panel)
  ## 기관 단위 병상은 «받아 둔 것이 있을 때만» 조립한다. 수집(14)은 3,365콜이라
  ## 파이프라인에 넣지 않는다 — 사람이 결정해서 따로 돌린다.
  if (length(list.files(raw_dir, pattern = "^hira_facility_.*\\.jsonl$")) > 0) {
    fac <- read_facility()
    gate_facility_vs_kosis(fac, panel)
    view_facility(fac)
  } else {
    cat("--- 기관 단위 병상: 수집본 없음, 건너뜀 (Rscript R/14_fetch_hira_facility.R)\n")
  }
  ## 통계연보도 «파싱본이 있을 때만». 파싱(python)은 파이프라인 밖이다.
  if (file.exists(YB_CSV)) {
    yb <- read_yearbook()
    gate_yearbook_span(yb)          # 「다 있는가」를 먼저 — 「맞는가」보다 앞이다
    gate_yearbook_vs_kosis(yb, panel)
    view_yearbook(yb)
  } else {
    cat("--- 통계연보: 파싱본 없음, 건너뜀 (python R/_parse_yearbook.py)\n")
  }
})
step("4/5 brief",    { write_brief(panel); build_page(panel) })
## Acceptance runs LAST on purpose: T8 reads the brief and the page off disk,
## so both have to be written before it can compare them.
step("5/5 accept",   { n_fail <<- run_acceptance(panel) })

## Provenance: which package versions produced these numbers. lmtp-style
## version drift is not a risk here, but "which run made this table" is.
writeLines(utils::capture.output(utils::sessionInfo()),
           file.path(logs_dir, paste0("sessionInfo_", stamp, ".txt")))

cat(sprintf("\n=== done in %.1f min | tables %d | figures %d | acceptance failures %d ===\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins")),
            length(list.files(tables_dir, pattern = paste0(stamp, "\\.xlsx$"))),
            length(list.files(figures_dir, pattern = paste0(stamp, "\\.png$"))),
            n_fail))
if (n_fail > 0L) quit(status = 1L)
