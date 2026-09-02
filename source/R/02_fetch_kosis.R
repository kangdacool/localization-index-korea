##################################################################
#####  02_fetch_kosis.R - pull each registry row to raw JSON  #####
##################################################################
##
## Walks the active registry, resolves names to codes (01_registry.R), asks
## KOSIS for the full recorded period of each table, and writes the raw JSON to
## data/raw/. Nothing downstream touches the API again - all aggregation runs
## off these files, so a re-run of 03..08 costs zero calls.
##
## The recorded period comes from the table's own PRD metadata rather than from
## a date we compute, because "latest" drifts: DT_HIRA45_1 ends 2026 2/4 while
## the calendar already says Q3, and asking for a quarter that does not exist
## is a silent empty result, not an error.

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("resolve_row")) source(file.path(r_dir, "01_registry.R"))

##################################################################
#####  1. PERIOD HANDLING                                    #####
##################################################################

## Recorded period for one table+frequency, straight from KOSIS.
table_period <- function(org_id, tbl_id, prd_se) {
  prd <- kosis_meta(org_id, tbl_id, "PRD")
  ko  <- PRD_SE_KO[[prd_se]]
  row <- prd[prd$PRD_SE == ko, , drop = FALSE]
  if (nrow(row) == 0) {
    stop(tbl_id, ": no '", ko, "' frequency. available: ",
         paste(prd$PRD_SE, collapse = ", "))
  }
  list(start = parse_prd(row$STRT_PRD_DE[1], prd_se),
       end   = parse_prd(row$END_PRD_DE[1],  prd_se))
}


##################################################################
#####  2. ONE REQUEST                                        #####
##################################################################

MAX_CELLS <- 30000L   # stay well under the KOSIS per-request cell ceiling

build_url <- function(row, res, start_prd, end_prd) {
  parts <- c(
    "https://kosis.kr/openapi/Param/statisticsParameterData.do?method=getList",
    paste0("apiKey=", KOSIS_KEY_ENC),
    paste0("orgId=", row$org_id),
    paste0("tblId=", row$tbl_id),
    paste0("itmId=", paste(res$item_ids, collapse = "+")),
    paste0("prdSe=", row$prd_se),
    paste0("startPrdDe=", start_prd),
    paste0("endPrdDe=", end_prd),
    "format=json", "jsonVD=Y"
  )
  ## objL1..objLn: the slot number IS OBJ_ID_SN, never the order we wrote them.
  for (sn in sort(as.integer(names(res$slots)))) {
    parts <- append(parts,
                    paste0("objL", sn, "=", paste(res$slots[[as.character(sn)]], collapse = "+")),
                    after = 5L)
  }
  paste0(parts[1], "&", paste(parts[-1], collapse = "&"))
}

fetch_row <- function(row) {
  res <- resolve_row(row)
  per <- table_period(row$org_id, row$tbl_id, row$prd_se)

  start_prd <- if (!is.na(row$start_prd) && nzchar(row$start_prd)) {
    max(row$start_prd, per$start)
  } else per$start
  end_prd <- if (!is.na(row$end_prd) && nzchar(row$end_prd)) {
    min(row$end_prd, per$end)
  } else per$end

  periods  <- enum_periods(start_prd, end_prd, row$prd_se)
  per_cell <- length(res$item_ids) * prod(lengths(res$slots))
  n_chunk  <- max(1L, ceiling(per_cell * length(periods) / MAX_CELLS))
  groups   <- split(periods, ceiling(seq_along(periods) / ceiling(length(periods) / n_chunk)))

  cat(sprintf("--- %-12s %-14s %s..%s  %d periods x %d cells  -> %d call(s)\n",
              row$key, row$tbl_id, start_prd, end_prd, length(periods),
              per_cell, length(groups)))

  out <- list()
  for (g in groups) {
    url <- build_url(row, res, g[1], g[length(g)])
    d <- kosis_get(url, what = paste0("data:", row$key, ":", g[1], "-", g[length(g)]))
    out[[length(out) + 1L]] <- d
  }
  df <- dplyr::bind_rows(out)
  if (nrow(df) == 0) stop(row$key, ": returned 0 rows - codes resolved but no data")

  f <- file.path(raw_dir, paste0(row$key, "_", row$tbl_id, "_", stamp, ".json"))
  jsonlite::write_json(df, f, auto_unbox = TRUE)
  cat(sprintf("    rows=%d -> %s\n", nrow(df), basename(f)))
  invisible(df)
}

##################################################################
#####  3. RUN                                                #####
##################################################################

fetch_all <- function(force = FALSE) {
  reg <- kosis_rows(load_registry())
  cat("=== 02_fetch_kosis.R:", nrow(reg), "active KOSIS indicators ===\n")
  for (i in seq_len(nrow(reg))) {
    r <- as.list(reg[i, ])
    existing <- list.files(raw_dir, pattern = paste0("^", r$key, "_.*\\.json$"),
                           full.names = TRUE)
    if (!force && length(existing) > 0 &&
        any(grepl(paste0("_", stamp, "\\.json$"), existing))) {
      cat(sprintf("--- %-12s cached (today), skipping\n", r$key))
      next
    }
    fetch_row(r)
  }
  write_call_log()
  invisible(NULL)
}

if (sys.nframe() == 0L) fetch_all()
