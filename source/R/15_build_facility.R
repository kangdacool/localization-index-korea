##################################################################
#####  15_build_facility.R - 기관 단위 병상 -> 시도 x 종별   #####
##################################################################
##
## 14 가 받아 둔 목록(JSON)과 시설정보(JSONL)를 붙여, KOSIS 가 구조적으로 못 주는
## 「시도 × 종별 × 병상」을 만든다. ⛔ 스냅숏이다 — 추세로 쓰지 않는다.
##
## 필드 해석은 실측 표본(가톨릭대 성빈센트병원, 허가병상 796 ≈ 공표값)에서 읽었고,
## 전국 종별 합계를 KOSIS 와 대조하는 게이트로 확인한다. 두 값은 «정확히» 같을
## 수 없다 — 심평원 신고 기준(KOSIS)과 허가 기준(이 API)은 다른 양이다. 게이트는
## 동일성이 아니라 «자릿수가 맞는가»를 본다.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("build_panel")) source(file.path(r_dir, "03_build_panel.R"))

## 시설정보 응답의 병상 관련 필드. 이름이 축약이라 여기 한 번만 적고 전부 경유한다.
FAC_FIELDS <- c(
  permSbdCnt           = "허가병상",
  stdSickbdCnt         = "일반병상",
  hghrSickbdCnt        = "상급병상",
  aduChldSprmCnt       = "중환자실_성인소아",
  chldSprmCnt          = "중환자실_소아",
  nbySprmCnt           = "중환자실_신생아",
  isnrSbdCnt           = "격리병상",
  anvirTrrmSbdCnt      = "음압격리병상",
  dtrmSbdCnt           = "임종실",
  psydeptClsGnlSbdCnt  = "정신과폐쇄_일반",
  psydeptClsHigSbdCnt  = "정신과폐쇄_상급",
  psydeptOpenGnlSbdCnt = "정신과개방_일반",
  psydeptOpenHigSbdCnt = "정신과개방_상급",
  emymCnt              = "응급실",
  soprmCnt             = "수술실",
  partumCnt            = "분만실",
  ptrmCnt              = "물리치료실"
)

read_facility <- function() {
  lf <- latest_file(raw_dir, "^hira_inst_list_.*\\.json$")
  ff <- latest_file(raw_dir, "^hira_facility_.*\\.jsonl$")
  lst <- jsonlite::fromJSON(lf)

  ln <- readLines(ff, warn = FALSE); ln <- ln[nzchar(ln)]
  fac <- dplyr::bind_rows(lapply(ln, function(l)
    tryCatch(as.data.frame(jsonlite::fromJSON(l), stringsAsFactors = FALSE),
             error = function(e) NULL)))
  cat(sprintf("--- 목록 %d개 · 시설정보 %d개 (%.0f%%)\n",
              nrow(lst), nrow(fac), 100 * nrow(fac) / nrow(lst)))

  keep <- intersect(c("ykiho", names(FAC_FIELDS)), names(fac))
  fac <- fac[, keep, drop = FALSE]
  for (k in setdiff(names(FAC_FIELDS), names(fac))) fac[[k]] <- NA
  for (k in names(FAC_FIELDS)) fac[[k]] <- suppressWarnings(as.numeric(fac[[k]]))

  d <- merge(lst[, c("ykiho", "yadmNm", "clCd", "clCdNm2", "sidoCdNm", "sgguCdNm",
                     "XPos", "YPos", "drTotCnt", "mdeptSdrCnt", "mdeptResdntCnt",
                     "mdeptIntnCnt", "mdeptGdrCnt")],
             fac, by = "ykiho", all.x = TRUE)
  names(d)[match(names(FAC_FIELDS), names(d))] <- unname(FAC_FIELDS)
  d$sido <- canon_sido(d$sidoCdNm)
  ## 캐시된 목록이 옛 라벨("상급종합")로 저장돼 있어도 여기서 KOSIS 표기로 맞춘다.
  TIER_CANON <- c("상급종합" = "상급종합병원")
  d$종별 <- ifelse(d$clCdNm2 %in% names(TIER_CANON),
                   unname(TIER_CANON[d$clCdNm2]), d$clCdNm2)
  for (k in c("drTotCnt", "mdeptSdrCnt", "mdeptResdntCnt", "mdeptIntnCnt", "mdeptGdrCnt")) {
    d[[k]] <- suppressWarnings(as.numeric(d[[k]]))
  }
  d
}

##################################################################
#####  GATE - 자릿수가 맞는가 (동일성이 아니다)              #####
##################################################################

gate_facility_vs_kosis <- function(d, panel, lo = 0.85, hi = 1.15) {
  k <- panel_pick(panel, "bed_tier",
                  axis = list(입원실현황별 = "계", 요양기관종별 = "*"), item = "병상수")
  k <- k[k$prd_de == max(k$prd_de), c("요양기관종별", "value")]
  names(k) <- c("종별", "KOSIS")

  a <- stats::aggregate(허가병상 ~ 종별, data = d, FUN = sum, na.rm = TRUE)
  cmp <- merge(a, k, by = "종별")
  ## merge 는 inner join 이라 라벨이 한 글자만 달라도 그 종별이 «조용히» 빠지고,
  ## 게이트는 남은 것만 보고 OK 라고 말한다. 실제로 "상급종합" vs "상급종합병원"
  ## 때문에 5개 중 4개만 대조하고 통과한 적이 있다. 대조하지 «못한» 것을 먼저 센다.
  missed <- setdiff(a$종별, cmp$종별)
  if (length(missed) > 0) {
    stop("gate_facility_vs_kosis: 대조하지 못한 종별 ", paste(missed, collapse = ", "),
         " — KOSIS 라벨(", paste(utils::head(k$종별, 12), collapse = ", "),
         ")과 표기가 다르다")
  }
  cmp$비율 <- cmp$허가병상 / cmp$KOSIS
  cmp <- cmp[order(-cmp$KOSIS), ]
  print(cmp, row.names = FALSE)

  bad <- cmp[cmp$비율 < lo | cmp$비율 > hi, , drop = FALSE]
  if (nrow(bad) > 0) {
    stop("gate_facility_vs_kosis FAILED: ",
         paste(sprintf("%s %.2f배", bad$종별, bad$비율), collapse = ", "),
         " — 필드 해석(허가병상=permSbdCnt)이 틀렸거나 수집이 덜 끝났다")
  }
  cat(sprintf("--- gate facility-vs-KOSIS OK (%d개 종별, 비율 %.2f~%.2f)\n",
              nrow(cmp), min(cmp$비율), max(cmp$비율)))
  invisible(cmp)
}

##################################################################
#####  뷰 — KOSIS 가 못 주는 교차                            #####
##################################################################

view_facility <- function(d) {
  cat("=== 15_build_facility.R ===\n")

  ## ★ 시도 × 종별 × 병상. CLAUDE.md 제약 1 이 막고 있던 바로 그 표다.
  w <- stats::aggregate(cbind(허가병상, 중환자실_성인소아, 응급실) ~ sido + 종별,
                        data = d[!is.na(d$sido), ], FUN = sum, na.rm = TRUE)
  bed <- tidyr::pivot_wider(w[, c("sido", "종별", "허가병상")],
                            names_from = "종별", values_from = "허가병상")
  icu <- tidyr::pivot_wider(w[, c("sido", "종별", "중환자실_성인소아")],
                            names_from = "종별", values_from = "중환자실_성인소아")
  n <- as.data.frame(table(d$sido[!is.na(d$sido)], d$종별[!is.na(d$sido)]))
  names(n) <- c("sido", "종별", "기관수")
  cnt <- tidyr::pivot_wider(n, names_from = "종별", values_from = "기관수")

  write_table(list(시도x종별_허가병상 = bed,
                   시도x종별_중환자실 = icu,
                   시도x종별_기관수 = cnt,
                   기관단위 = d[order(-d$허가병상),
                                c("종별", "sido", "sgguCdNm", "yadmNm", "허가병상",
                                  "일반병상", "상급병상", "중환자실_성인소아",
                                  "중환자실_신생아", "격리병상", "음압격리병상",
                                  "응급실", "수술실", "분만실",
                                  "drTotCnt", "mdeptSdrCnt", "XPos", "YPos")]),
              "t14_기관단위_병상")

  top <- d[order(-d$허가병상), ][1:10, ]
  cat("--- 허가병상 상위 10개 기관:\n")
  for (i in seq_len(nrow(top))) {
    cat(sprintf("    %-28s %-8s %5.0f병상\n",
                substr(top$yadmNm[i], 1, 28), top$종별[i], top$허가병상[i]))
  }
  invisible(list(bed = bed, inst = d))
}

if (sys.nframe() == 0L) {
  d <- read_facility()
  panel <- load_step("^panel_")
  gate_facility_vs_kosis(d, panel)
  view_facility(d)
}
