##################################################################
#####  12_healthmap.R - 국립중앙의료원 헬스맵 (2023 단면)      #####
##################################################################
##
## KOSIS는 시간을 준다(2003~2026, 분기). 헬스맵은 그 대신 «2023년 한 해의 깊이»를
## 준다 — 시군구·중진료권·시도 세 단위로, 서비스별 관내의료이용률·지역환자구성비·
## 기준시간내의료이용률, 그리고 의료취약지 판정.
##
## ⛔ 두 자료를 이어 붙여 «추세»라고 말하지 않는다. 헬스맵은 단면이다.
##
## 착수 전 실측으로 확인한 세 가지:
##
##  (1) 유출입 행렬의 축은 지역1 = 의료기관 소재지, 지역2 = 환자 거주지다.
##      종로구를 지역1로 놓으면 상급종합 입원 192,844건(서울대병원 등이 받은 전국
##      환자), 지역2로 놓으면 13,545건(종로구 주민 이용). 인구 14만인 구에 19만 건
##      유입은 기관 축일 때만 성립한다. KOSIS의 관내/관외와 똑같은 함정이다.
##
##  (2) 기관정보의 상급종합병원은 48개소인데, 시도별로 뜯으면 충남을 뺀 모든 시도가
##      KOSIS 2024Q4(47개소)와 일치하고 충남만 1개소 많다. 파일명은 20231231이지만
##      지정 플래그는 5기(2024 시행) 기준의 «혼합 빈티지»다.
##      → 개소수의 정본은 KOSIS다. 헬스맵의 지정 플래그를 연도 지표로 쓰지 않는다.
##
##  (3) 중진료권 70개의 시군구 배정표는 이 자료에 없다(코드 B00001~B00070, 명칭은
##      시도명뿐). 그래서 유출입을 중진료권으로 «집계할 수 없다» — 대신 지표 파일이
##      중진료권 단위로 이미 계산해 둔 값을 쓴다.

if (!exists("DATA_ONLY")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))

## 원자료는 프로젝트가 아니라 dataonly 에 한 벌 있다. 2023 판은 다음 판이 나오면
## 포털에서 내려갈 수 있어 «다시 못 받을» 자료이고, 그래서 지워도 되는 캐시가 아니다.
HM_DIR <- file.path(DATA_ONLY, "healthmap", "csv")

## 필수의료 축으로 고른 8개 서비스. 298개 열을 전부 들이지 않는 이유는, 축이 늘수록
## 읽는 사람이 무엇을 봐야 하는지 잃기 때문이다. 더 필요하면 여기에 한 줄 더한다.
## 「전체 병원」과 「종합병원」은 대조 기준선으로 넣는다 — 이 둘 없이는 중증으로
## 갈수록 자체충족률이 떨어진다는 «기울기»를 보여줄 수 없다.
HM_SERVICES <- c("전체 병원", "종합병원", "상급종합병원",
                 "응급실", "권역응급의료센터", "중환자실",
                 "고위험분만", "소아청소년과", "심장질환", "뇌혈관 질환")
HM_MEASURES <- c("관내의료이용률", "지역환자구성비", "기준시간내의료이용률")

##################################################################
#####  1. LOAD                                               #####
##################################################################

.hm_read <- function(f) {
  readr::read_csv(file.path(HM_DIR, f), col_types = readr::cols(.default = readr::col_character()),
                  progress = FALSE, name_repair = "minimal")
}

## "관내의료이용률_상급종합병원(백분율)"            -> 관내의료이용률 / 상급종합병원 / NA
## "기준시간내의료이용률_응급실(30분)(백분율)"      -> 기준시간내의료이용률 / 응급실 / 30분
.parse_col <- function(x) {
  m <- regmatches(x, regexec(
    "^(관내의료이용률|지역환자구성비|기준시간내의료이용률|접근성취약인구율)_(.+?)(\\(([0-9]+)분\\))?\\(백분율\\)$", x))
  do.call(rbind, lapply(m, function(g) {
    if (length(g) == 0) return(data.frame(measure = NA_character_, service = NA_character_,
                                          mins = NA_character_))
    data.frame(measure = g[2], service = g[3], mins = if (nzchar(g[5])) g[5] else NA_character_)
  }))
}

hm_indicators <- function() {
  d <- .hm_read("지역별 모니터링 지표.csv")
  meta <- d[, c("지역코드", "지역코드_구분", "기준연도", "명칭", "지표명")]
  names(meta) <- c("code", "level", "year", "sido_nm", "label")

  cols <- setdiff(names(d), names(meta))
  parsed <- .parse_col(cols)
  keep <- which(!is.na(parsed$measure) &
                parsed$measure %in% HM_MEASURES &
                parsed$service %in% HM_SERVICES)
  if (length(keep) == 0) stop("headers changed: no 관내의료이용률 columns matched")

  out <- list()
  for (i in keep) {
    out[[length(out) + 1L]] <- data.frame(
      meta,
      measure = parsed$measure[i], service = parsed$service[i], mins = parsed$mins[i],
      value = suppressWarnings(as.numeric(d[[cols[i]]])),
      stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, out)
  res$sido <- canon_sido(res$sido_nm)
  res
}

hm_underserved <- function() {
  d <- .hm_read("의료취약지.csv")
  keep <- c("기준연도", "시도명", "시군구코드", "시군구명",
            "분만_A취약지", "소아청소년과_취약지", "인공신장실_취약지", "응급_취약지")
  d <- d[, keep]
  names(d) <- c("year", "sido_nm", "sgg_code", "sgg_nm",
                "분만", "소아청소년과", "인공신장실", "응급")
  ## An empty cell means "not designated", and readr reads it as NA. Without the
  ## is.na() guard every flag becomes NA, rowSums goes NA, and `df[NA >= 3, ]`
  ## silently returns 250 NA-filled rows instead of erroring.
  for (k in c("분만", "소아청소년과", "인공신장실", "응급")) {
    d[[k]] <- ifelse(!is.na(d[[k]]) & trimws(d[[k]]) == "Y", 1L, 0L)
  }
  d$sido <- canon_sido(d$sido_nm)
  d
}

## Origin-destination matrix. 지역1 = 기관 소재지, 지역2 = 환자 거주지 (see header).
hm_flow <- function(file = "유출입_요양종별.csv", middle = NULL) {
  d <- .hm_read(file)
  names(d)[names(d) == "지역1"] <- "inst_sgg"
  names(d)[names(d) == "지역2"] <- "resi_sgg"
  d$n <- suppressWarnings(as.numeric(d$입원건수))
  if (!is.null(middle)) d <- d[d$중분류 %in% middle, ]
  d[, c("기준연도", "대분류", "중분류", "inst_sgg", "resi_sgg", "n")]
}

##################################################################
#####  2. GATE - the flow matrix must reproduce the indicator #####
##################################################################
##
## 관내의료이용률 is published, and the flow matrix is published, and nothing says
## they were built the same way. Computing one from the other is the only check
## available here - and it simultaneously confirms which axis is the residence.
##
## Self-sufficiency for a district = (its residents treated in it) / (its
## residents treated anywhere) = diagonal / column-sum over 거주지.

gate_healthmap_flow <- function(ind, tol_pp = 1.0) {
  fl <- hm_flow(middle = "상급종합병원")
  denom <- stats::aggregate(n ~ resi_sgg, data = fl, FUN = sum)
  diagv <- fl[fl$inst_sgg == fl$resi_sgg, c("resi_sgg", "n")]
  names(diagv)[2] <- "same"
  cmp <- merge(denom, diagv, by = "resi_sgg", all.x = TRUE)
  cmp$same[is.na(cmp$same)] <- 0
  cmp$computed <- 100 * cmp$same / cmp$n

  pub <- ind[ind$level == "시군구" & ind$measure == "관내의료이용률" &
             ind$service == "상급종합병원", c("code", "value")]
  pub$resi_sgg <- sub("^C", "", pub$code)
  cmp <- merge(cmp, pub[, c("resi_sgg", "value")], by = "resi_sgg")
  cmp <- cmp[!is.na(cmp$value) & !is.na(cmp$computed), ]
  if (nrow(cmp) < 200) stop("gate_healthmap_flow: only ", nrow(cmp), " districts comparable")

  cmp$gap <- abs(cmp$computed - cmp$value)
  bad <- cmp[cmp$gap > tol_pp, , drop = FALSE]
  if (nrow(bad) > 0) {
    w <- bad[which.max(bad$gap), ]
    stop(sprintf("gate_healthmap_flow FAILED: %d of %d 시군구 differ by >%.1f%%p. worst %s: 유출입=%.1f vs 지표=%.1f",
                 nrow(bad), nrow(cmp), tol_pp, w$resi_sgg, w$computed, w$value))
  }
  cat(sprintf("--- gate healthmap-flow OK (%d 시군구, 유출입으로 계산한 상급종합 관내이용률이 지표값과 %.2f%%p 이내)\n",
              nrow(cmp), max(cmp$gap)))
  invisible(cmp)
}

gate_healthmap_shape <- function(ind, und) {
  n <- table(unique(ind[, c("code", "level")])$level)
  want <- c(시도 = 17L, 중진료권 = 70L, 시군구 = 250L)
  for (k in names(want)) {
    if (is.na(n[k]) || n[k] != want[[k]]) {
      stop("gate_healthmap_shape: ", k, " = ", n[k], ", expected ", want[[k]])
    }
  }
  v <- ind$value[!is.na(ind$value)]
  if (any(v < 0 | v > 100)) stop("gate_healthmap_shape: 백분율 밖의 값이 있다")
  if (nrow(und) != 250L) stop("gate_healthmap_shape: 취약지 ", nrow(und), "행, 250 기대")
  cat(sprintf("--- gate healthmap-shape OK (시도 17 / 중진료권 70 / 시군구 250, 값 %d개)\n",
              length(v)))
}

##################################################################
#####  3. RUN                                                #####
##################################################################

build_healthmap <- function() {
  cat("=== 12_healthmap.R (2023 단면) ===\n")
  ind <- hm_indicators()
  und <- hm_underserved()
  cat(sprintf("--- 지표 %d행 (%d개 서비스 x %d개 측도), 취약지 %d행\n",
              nrow(ind), length(unique(ind$service)), length(unique(ind$measure)), nrow(und)))
  gate_healthmap_shape(ind, und)
  gate_healthmap_flow(ind)
  save_step(list(indicators = ind, underserved = und), "healthmap")
  invisible(list(indicators = ind, underserved = und))
}

if (sys.nframe() == 0L) build_healthmap()
