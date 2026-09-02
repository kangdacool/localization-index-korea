##################################################################
#####  01_registry.R - registry -> KOSIS codes (name based)  #####
##################################################################
##
## The registry names axes and items in Korean; this script turns those names
## into the ITM_ID / objL codes KOSIS wants, by reading the table's own
## metadata. Nobody hand-writes a code, so a table can be re-coded upstream and
## the pipeline still runs as long as the Korean names survive.
##
## Two traps this exists to handle, both found by calling the API:
##
##  (1) AXIS ORDER DIFFERS PER TABLE. DT_HIRA45_1 is 입원실현황별=L1 /
##      요양기관종별=L2, but DT_MIRE01 is 요양기관종별=L1 / 시도별=L2. Swapping
##      them returns {"err":"21","errMsg":"잘못된 요청 변수를 호출 하였습니다."}.
##      The slot is read from OBJ_ID_SN, never assumed.
##
##  (2) ITEM NAMES REPEAT WITHIN AN AXIS. In DT_HIRA44, 일반의/인턴/레지던트/
##      전문의 each appear three times - under 의사, 치과의사 and 한의사. A bare
##      name is ambiguous, so the registry writes 의사>전문의 and we match on the
##      parent. Where a name is both a parent and its own child (간호사 in
##      DT_HIRA4A) the parent wins. Anything still ambiguous is an error, not a
##      guess.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))

##################################################################
#####  1. LOW LEVEL API                                      #####
##################################################################

KOSIS_KEY_ENC <- utils::URLencode(read_secret("KOSIS_API_KEY"), reserved = TRUE)

.api_calls <- new.env(parent = emptyenv())
.api_calls$log <- list()

kosis_get <- function(url, what = "") {
  t0 <- Sys.time()
  resp <- httr2::request(url) |>
    httr2::req_timeout(90) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 2 * i) |>
    httr2::req_perform()
  txt <- httr2::resp_body_string(resp)
  out <- jsonlite::fromJSON(txt)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  ## KOSIS returns a LIST on success and a DICT on failure. HTTP is 200 either
  ## way, so the status code tells you nothing - the body has to be judged.
  err <- NULL
  if (!is.data.frame(out) && is.list(out) && !is.null(out$err)) {
    err <- paste0("err=", out$err, " ", out$errMsg)
  }
  n <- if (is.data.frame(out)) nrow(out) else 0L

  .api_calls$log[[length(.api_calls$log) + 1L]] <- data.frame(
    time = format(t0, "%Y-%m-%d %H:%M:%S"), what = what,
    rows = n, secs = round(secs, 2), error = err %||% "", stringsAsFactors = FALSE
  )
  if (length(.api_calls$log) > 60L) {
    stop("API call cap (60) exceeded - something is looping. Inspect logs/api_calls_*.csv")
  }
  if (!is.null(err)) stop("KOSIS ", what, ": ", err)
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

write_call_log <- function() {
  if (length(.api_calls$log) == 0) return(invisible(NULL))
  df <- do.call(rbind, .api_calls$log)
  f <- file.path(logs_dir, paste0("api_calls_", stamp, ".csv"))
  readr::write_excel_csv(df, f)
  cat("--- API calls:", nrow(df), "total, log ->", basename(f), "\n")
  invisible(f)
}

##################################################################
#####  2. METADATA (cached on disk; costs 1 call per table)  #####
##################################################################

kosis_meta <- function(org_id, tbl_id, type = "ITM") {
  f <- file.path(raw_dir, paste0("meta_", tbl_id, "_", type, ".json"))
  if (file.exists(f)) {
    out <- jsonlite::fromJSON(f)
    if (is.data.frame(out)) return(out)
  }
  url <- sprintf(paste0("https://kosis.kr/openapi/statisticsData.do?method=getMeta",
                        "&apiKey=%s&orgId=%s&tblId=%s&type=%s&format=json&jsonVD=Y"),
                 KOSIS_KEY_ENC, org_id, tbl_id, type)
  out <- kosis_get(url, what = paste0("meta:", tbl_id, ":", type))
  if (!is.data.frame(out)) stop("meta ", tbl_id, " ", type, ": unexpected shape")
  jsonlite::write_json(out, f, auto_unbox = TRUE)
  out
}

## Ensure the optional columns exist so downstream code never indexes NULL.
.norm_meta <- function(m) {
  for (col in c("OBJ_NM", "OBJ_ID_SN", "ITM_ID", "ITM_NM", "UP_ITM_ID", "UNIT_NM")) {
    if (is.null(m[[col]])) m[[col]] <- NA_character_
  }
  m$UP_ITM_ID <- ifelse(is.na(m$UP_ITM_ID), "", as.character(m$UP_ITM_ID))
  m
}

##################################################################
#####  3. NAME -> CODE RESOLUTION                            #####
##################################################################

## spec value forms:  "ALL"  "TOPLEVEL"  "이름"  "부모>자식"
resolve_values <- function(meta_axis, values, axis_nm, tbl_id) {
  if (identical(values, "ALL"))      return(meta_axis$ITM_ID)
  if (identical(values, "TOPLEVEL")) return(meta_axis$ITM_ID[meta_axis$UP_ITM_ID == ""])

  out <- character(0)
  for (v in values) {
    ## TOTAL: tables disagree on what the grand total is called - DT_HIRA44 says
    ## 전체 where DT_HIRA4A says 계, and the population table says 전국. Spelling
    ## it out in the registry means editing 9 rows every time one table renames.
    if (identical(v, "TOTAL")) {
      tot <- meta_axis[meta_axis$UP_ITM_ID == "" &
                       meta_axis$ITM_NM %in% c("계", "전체", "전국", "합계"), , drop = FALSE]
      if (nrow(tot) != 1) {
        stop(tbl_id, " / ", axis_nm, ": TOTAL matched ", nrow(tot),
             " items. Name it explicitly. top level: ",
             paste(meta_axis$ITM_NM[meta_axis$UP_ITM_ID == ""], collapse = " | "))
      }
      out <- c(out, tot$ITM_ID)
      next
    }
    parent <- NA_character_
    nm <- v
    if (grepl(">", v, fixed = TRUE)) {
      parts  <- strsplit(v, ">", fixed = TRUE)[[1]]
      parent <- trimws(parts[1])
      nm     <- trimws(parts[2])
    }
    hit <- meta_axis[meta_axis$ITM_NM == nm, , drop = FALSE]
    if (nrow(hit) == 0) {
      stop(tbl_id, " / ", axis_nm, ": no item named '", nm, "'.\n",
           "  available: ", paste(head(unique(meta_axis$ITM_NM), 40), collapse = " | "))
    }
    if (nrow(hit) > 1 && !is.na(parent)) {
      pid <- meta_axis$ITM_ID[meta_axis$ITM_NM == parent & meta_axis$UP_ITM_ID == ""]
      if (length(pid) != 1) {
        pid <- meta_axis$ITM_ID[meta_axis$ITM_NM == parent]
      }
      hit <- hit[hit$UP_ITM_ID %in% pid, , drop = FALSE]
    }
    if (nrow(hit) > 1) {
      ## a name that is both a parent and its own child: the parent is the total
      top <- hit[hit$UP_ITM_ID == "", , drop = FALSE]
      if (nrow(top) == 1) hit <- top
    }
    if (nrow(hit) != 1) {
      stop(tbl_id, " / ", axis_nm, ": '", v, "' is ambiguous (", nrow(hit),
           " matches). Qualify it as 부모>자식.")
    }
    out <- c(out, hit$ITM_ID)
  }
  out
}

parse_axis_spec <- function(spec) {
  if (is.na(spec) || !nzchar(trimws(spec)) || identical(trimws(spec), "ALL")) return(list())
  parts <- strsplit(trimws(spec), ";", fixed = TRUE)[[1]]
  out <- list()
  for (p in parts) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    if (length(kv) != 2) stop("bad axis_spec fragment: ", p)
    out[[trimws(kv[1])]] <- trimws(strsplit(kv[2], "|", fixed = TRUE)[[1]])
  }
  out
}

## Returns everything 02_fetch needs: item ids and objL1..objL8 slots.
resolve_row <- function(row) {
  meta <- .norm_meta(kosis_meta(row$org_id, row$tbl_id, "ITM"))

  item_axis <- meta[meta$OBJ_NM == "항목", , drop = FALSE]
  if (nrow(item_axis) == 0) stop(row$tbl_id, ": metadata has no 항목 axis")
  item_ids <- if (identical(trimws(row$item_names), "ALL")) {
    item_axis$ITM_ID
  } else {
    resolve_values(item_axis, trimws(strsplit(row$item_names, "|", fixed = TRUE)[[1]]),
                   "항목", row$tbl_id)
  }

  spec <- parse_axis_spec(row$axis_spec)
  cls  <- meta[meta$OBJ_NM != "항목" & !is.na(meta$OBJ_ID_SN), , drop = FALSE]
  slots <- list()
  if (length(spec) == 0 && nrow(cls) > 0) {
    for (sn in sort(unique(cls$OBJ_ID_SN))) {
      slots[[as.character(sn)]] <- cls$ITM_ID[cls$OBJ_ID_SN == sn]
    }
  }
  for (axis_nm in names(spec)) {
    ax <- cls[cls$OBJ_NM == axis_nm, , drop = FALSE]
    if (nrow(ax) == 0) {
      stop(row$tbl_id, ": no axis named '", axis_nm, "'. available: ",
           paste(unique(cls$OBJ_NM), collapse = " | "))
    }
    sn <- unique(ax$OBJ_ID_SN)
    if (length(sn) != 1) stop(row$tbl_id, ": axis '", axis_nm, "' spans several slots")
    ## OBJ_ID_SN is the authority on which objL slot this axis occupies.
    slots[[as.character(sn)]] <- resolve_values(ax, spec[[axis_nm]], axis_nm, row$tbl_id)
  }

  axis_names <- setNames(
    vapply(sort(unique(cls$OBJ_ID_SN)), function(sn) unique(cls$OBJ_NM[cls$OBJ_ID_SN == sn])[1],
           character(1)),
    as.character(sort(unique(cls$OBJ_ID_SN)))
  )

  list(item_ids = item_ids, slots = slots, axis_names = axis_names, meta = meta)
}

##################################################################
#####  4. LOAD + VALIDATE THE REGISTRY                       #####
##################################################################

load_registry <- function(active_only = TRUE) {
  f <- file.path(r_dir, "indicator_registry.csv")
  reg <- readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()),
                         progress = FALSE)
  reg$active <- as.integer(reg$active)
  if (any(duplicated(reg$key))) stop("duplicate key in registry: ",
                                     paste(reg$key[duplicated(reg$key)], collapse = ", "))
  if (active_only) reg <- reg[reg$active == 1L, , drop = FALSE]
  if (is.null(reg$kind)) reg$kind <- "kosis"
  reg
}

## The registry is the single manifest for BOTH kinds of source. Only the KOSIS
## rows are fetched over the network; file rows are already on disk and are
## loaded by their own script. Keeping them in one table is what makes
## "add an axis = add a row" true of the project rather than of KOSIS alone.
kosis_rows <- function(reg) reg[reg$kind == "kosis", , drop = FALSE]

if (sys.nframe() == 0L) {
  cat("=== 01_registry.R self-check ===\n")
  reg <- load_registry()
  for (i in seq_len(nrow(reg))) {
    r <- as.list(reg[i, ])
    res <- resolve_row(r)
    cat(sprintf("--- %-14s %-14s items=%d slots=%s\n", r$key, r$tbl_id,
                length(res$item_ids),
                paste(sprintf("%s:%d", res$axis_names[names(res$slots)],
                              lengths(res$slots)), collapse = " ")))
  }
  write_call_log()
}
