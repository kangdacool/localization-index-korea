##################################################################
#####  14_fetch_hira_facility.R - 기관 단위 병상 (심평원 API) #####
##################################################################
##
## KOSIS 로는 「시도 × 종별 × 병상」이 안 된다(CLAUDE.md 제약 1). 그 벽을 기관 단위로
## 넘는 유일한 경로다. 대신 «스냅숏»이므로 시계열은 여전히 KOSIS 가 담당한다.
##
## 자료원 두 개:
##   목록  hospInfoServicev2/getHospBasisList   종별당 1콜. ykiho·종별·시군구·좌표
##                                              + 의사 수(전문의/레지던트/인턴/일반의)
##   시설  MadmDtlInfoService2.8/getEqpInfo2.8  기관당 1콜. 허가병상·중환자실·격리·
##                                              정신과·응급실·수술실·분만실 + 설립구분
##
## ⚠ 시설정보는 ykiho 없이 페이징되지 않는다(실측: total=0). 기관당 1콜이 강제된다.
##
## 콜 예산 (2026-08-31 실측 totalCount):
##   상급종합 47 · 종합병원 338 · 병원 1,434 · 요양병원 1,280 · 정신병원 261 = 3,360
##   + 목록 7콜 = 약 3,367콜. 개발계정 일 한도 10,000 의 34%.
##   의원 37,841 은 제외한다 — 병상이 거의 없는데 한도의 40% 를 쓴다.
##
## 중단되어도 이어받는다: 기관 하나를 받을 때마다 JSONL 에 한 줄씩 흘려 쓰고,
## 다시 돌리면 이미 받은 ykiho 를 건너뛴다.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))

HIRA_KEY   <- read_secret("DATA_GO_KR_KEY")
## 라벨은 KOSIS 종별과 «글자까지» 같아야 한다 — API 는 "상급종합", KOSIS 는
## "상급종합병원"이라 그대로 두면 대조 merge 에서 조용히 빠진다(실제로 그랬다).
HIRA_CLCD  <- c("01" = "상급종합병원", "11" = "종합병원", "21" = "병원",
                "28" = "요양병원", "29" = "정신병원")
HIRA_CAP   <- 4000L      # 하드캡. 예산 3,367 보다 조금 위, 한도 10,000 보다 한참 아래
HIRA_SLEEP <- 0.05

LIST_URL <- "https://apis.data.go.kr/B551182/hospInfoServicev2/getHospBasisList"
FAC_URL  <- "https://apis.data.go.kr/B551182/MadmDtlInfoService2.8/getEqpInfo2.8"

.calls <- new.env(parent = emptyenv()); .calls$n <- 0L

##################################################################
#####  1. XML -> data.frame                                  #####
##################################################################

.hira_get <- function(url, q) {
  .calls$n <- .calls$n + 1L
  if (.calls$n > HIRA_CAP) stop("HIRA 호출 하드캡 ", HIRA_CAP, " 초과 — 무언가 돌고 있다")
  r <- httr2::request(url) |>
    httr2::req_url_query(!!!c(list(serviceKey = HIRA_KEY), q)) |>
    httr2::req_timeout(40) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 2 * i) |>
    httr2::req_error(is_error = function(x) FALSE) |>
    httr2::req_perform()
  x <- httr2::resp_body_string(r)
  ## HTTP 200 은 성공을 뜻하지 않는다 — 권한·한도 문제도 200 + XML 본문으로 온다.
  if (grepl("<returnAuthMsg>", x)) {
    stop("data.go.kr: ", sub(".*<returnAuthMsg>([^<]*)<.*", "\\1", x))
  }
  x
}

.items <- function(x) regmatches(x, gregexpr("<item>.*?</item>", x))[[1]]

.parse <- function(it) {
  tags <- regmatches(it, gregexpr("<[a-zA-Z0-9_]+>[^<]*</[a-zA-Z0-9_]+>", it))[[1]]
  k <- sub("^<([a-zA-Z0-9_]+)>.*", "\\1", tags)
  v <- sub("^<[a-zA-Z0-9_]+>([^<]*)</.*", "\\1", tags)
  stats::setNames(as.list(v), k)
}

##################################################################
#####  2. 목록                                               #####
##################################################################

fetch_inst_list <- function() {
  f <- file.path(raw_dir, paste0("hira_inst_list_", stamp, ".json"))
  if (file.exists(f)) {
    cat("--- 목록 캐시 사용:", basename(f), "\n")
    return(jsonlite::fromJSON(f))
  }
  out <- list()
  for (cd in names(HIRA_CLCD)) {
    x <- .hira_get(LIST_URL, list(pageNo = 1, numOfRows = 3000, clCd = cd))
    tot <- as.integer(sub(".*<totalCount>([0-9]+)</totalCount>.*", "\\1", x))
    its <- .items(x)
    if (length(its) < tot) {
      stop("목록이 잘렸다: clCd=", cd, " total=", tot, " 받은 것=", length(its))
    }
    d <- dplyr::bind_rows(lapply(its, .parse))
    d$clCdNm2 <- HIRA_CLCD[[cd]]
    out[[cd]] <- d
    cat(sprintf("--- 목록 %-8s %5d개\n", HIRA_CLCD[[cd]], nrow(d)))
    Sys.sleep(HIRA_SLEEP)
  }
  res <- dplyr::bind_rows(out)
  jsonlite::write_json(res, f, auto_unbox = TRUE)
  cat(sprintf("--- 목록 합계 %d개 -> %s\n", nrow(res), basename(f)))
  res
}

##################################################################
#####  3. 시설정보 (기관당 1콜, 이어받기)                    #####
##################################################################

fetch_facility <- function(lst) {
  f <- file.path(raw_dir, paste0("hira_facility_", stamp, ".jsonl"))
  done <- character(0)
  if (file.exists(f)) {
    ln <- readLines(f, warn = FALSE)
    ln <- ln[nzchar(ln)]
    done <- vapply(ln, function(l) tryCatch(jsonlite::fromJSON(l)$ykiho,
                                            error = function(e) NA_character_), character(1))
    done <- unname(stats::na.omit(done))
    cat("--- 이어받기: 이미", length(done), "개 받음\n")
  }
  todo <- setdiff(lst$ykiho, done)
  cat(sprintf("--- 남은 기관 %d개 (예상 호출 %d, 하드캡 %d)\n",
              length(todo), length(todo), HIRA_CAP))
  if (length(todo) == 0) return(invisible(f))

  con <- file(f, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  t0 <- Sys.time(); ok <- 0L; empty <- 0L
  for (i in seq_along(todo)) {
    yk <- todo[i]
    x <- tryCatch(.hira_get(FAC_URL, list(ykiho = yk, pageNo = 1, numOfRows = 5)),
                  error = function(e) { message("  ! ", conditionMessage(e)); NULL })
    if (is.null(x)) break
    its <- .items(x)
    rec <- if (length(its)) .parse(its[1]) else list()
    rec$ykiho <- yk
    writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE), con)
    if (length(its)) ok <- ok + 1L else empty <- empty + 1L
    if (i %% 200 == 0) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("    %5d/%d  (%.1f분, 값 있음 %d / 없음 %d)\n",
                  i, length(todo), el, ok, empty))
      flush(con)
    }
    Sys.sleep(HIRA_SLEEP)
  }
  cat(sprintf("--- 시설정보 완료: 값 있음 %d · 없음 %d · 총 호출 %d · %.1f분\n",
              ok, empty, .calls$n, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  invisible(f)
}

##################################################################
#####  4. RUN                                                #####
##################################################################

fetch_hira_facility <- function() {
  cat("=== 14_fetch_hira_facility.R ===\n")
  lst <- fetch_inst_list()
  fetch_facility(lst)
  cat("--- 이 실행의 총 호출:", .calls$n, "\n")
  invisible(NULL)
}

if (sys.nframe() == 0L) fetch_hira_facility()
