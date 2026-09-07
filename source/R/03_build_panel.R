##################################################################
#####  03_build_panel.R - raw JSON -> one long panel + gates  #####
##################################################################
##
## Reads every raw file written by 02, flattens KOSIS's C1/C2 layout into one
## long table, and refuses to hand it downstream unless the internal identities
## hold. Zero API calls.
##
## Panel grain: one row per (key, period, axis1 value, axis2 value, item).

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))
if (!exists("load_registry")) source(file.path(r_dir, "01_registry.R"))

##################################################################
#####  1. FLATTEN                                            #####
##################################################################

.blank_to_na <- function(x) { x <- as.character(x); x[!nzchar(trimws(x))] <- NA; x }

read_raw <- function(key) {
  f <- latest_file(raw_dir, paste0("^", key, "_.*\\.json$"))
  d <- jsonlite::fromJSON(f)
  for (col in c("C1_OBJ_NM", "C1_NM", "C1", "C2_OBJ_NM", "C2_NM", "C2", "UNIT_NM")) {
    if (is.null(d[[col]])) d[[col]] <- NA_character_
  }
  tibble::tibble(
    key       = key,
    org_id    = as.character(d$ORG_ID),
    tbl_id    = as.character(d$TBL_ID),
    tbl_nm    = as.character(d$TBL_NM),
    prd_de    = as.character(d$PRD_DE),
    axis1_nm  = .blank_to_na(d$C1_OBJ_NM),
    axis1_val = .blank_to_na(d$C1_NM),
    ## ⭐ 축의 «코드»도 실어 나른다. 이름만으로는 못 가르는 것이 있다 —
    ## 재정자립도의 「동구」는 여섯 개이고, KOSIS 는 C1 에 코드를 준다.
    ## (2026-09-01: 코드를 안 들고 오는 바람에 시도를 되찾을 수 없었다.)
    ## ⚠ 코드 체계는 표마다 다르다. DT_1YL20921 은 광주(1224)·전남(1236)만 4자리
    ##    시도코드를 쓰고 나머지는 2자리다 — 그래서 «부모는 언제나 code[:-3]» 이다.
    axis1_cd  = .blank_to_na(d$C1),
    axis2_nm  = .blank_to_na(d$C2_OBJ_NM),
    axis2_val = .blank_to_na(d$C2_NM),
    axis2_cd  = .blank_to_na(d$C2),
    itm_nm    = as.character(d$ITM_NM),
    unit      = .blank_to_na(d$UNIT_NM),
    value     = suppressWarnings(as.numeric(d$DT))
  )
}

## PRD_DE is "201102" for a quarter, "2014" for a year. Split it once, here.
add_time <- function(df, prd_se) {
  if (prd_se == "Q") {
    df$year    <- as.integer(substr(df$prd_de, 1, 4))
    df$quarter <- as.integer(substr(df$prd_de, 6, 6))
  } else {
    df$year    <- as.integer(substr(df$prd_de, 1, 4))
    df$quarter <- NA_integer_
  }
  df$prd_se <- prd_se
  df
}

build_panel <- function() {
  reg <- kosis_rows(load_registry())
  parts <- list()
  for (i in seq_len(nrow(reg))) {
    r <- as.list(reg[i, ])
    d <- add_time(read_raw(r$key), r$prd_se)
    d$domain <- r$domain
    d$label_ko <- r$label_ko
    parts[[r$key]] <- d
    cat(sprintf("--- %-12s %6d rows  %s..%s\n", r$key, nrow(d),
                min(d$prd_de), max(d$prd_de)))
  }
  panel <- dplyr::bind_rows(parts)

  ## Canonical province label. KOSIS spells 강원도 / 강원특별자치도 / 강원 across
  ## tables and vintages; joins downstream must not depend on the spelling.
  panel$sido <- NA_character_
  for (col in c("axis1_val", "axis2_val")) {
    hit <- !is.na(panel[[col]]) & panel[[col]] %in% names(SIDO_CANON)
    panel$sido[hit] <- canon_sido(panel[[col]][hit])
  }
  panel
}

##################################################################
#####  1b. PANEL ACCESSOR                                    #####
##################################################################
##
## Slice by AXIS NAME, never by slot. Which of C1/C2 holds 요양기관종별 differs
## per table (see gate_region_sums), so every view that hard-codes axis1_val is
## one upstream re-slot away from silently reading the wrong column.
##
##   panel_pick(panel, "bed_tier",
##        axis = list(입원실현황별 = "계", 요양기관종별 = "상급종합병원"),
##        item = "병상수")

panel_pick <- function(panel, key, axis = list(), item = NULL, drop_na = TRUE) {
  d <- panel[panel$key == key, , drop = FALSE]
  if (nrow(d) == 0) stop("panel_pick: no rows for key '", key, "'")
  axis <- as.list(axis)
  ## c(요양기관종별 = c("계", "병원")) silently becomes 요양기관종별1/2 - a named
  ## vector cannot hold a vector. Say so rather than reporting a missing axis.
  known <- unique(stats::na.omit(c(d$axis1_nm, d$axis2_nm)))
  strayed <- names(axis)[!names(axis) %in% known &
                         sub("\\d+$", "", names(axis)) %in% known]
  if (length(strayed) > 0) {
    stop("panel_pick: axis name(s) ", paste(strayed, collapse = ", "),
         " look like c() flattening. Use axis = list(", sub("\\d+$", "", strayed[1]),
         " = c(...)) for multi-value axes.")
  }
  for (nm in names(axis)) {
    want <- axis[[nm]]
    col <- if (any(d$axis1_nm == nm, na.rm = TRUE)) "axis1_val"
           else if (any(d$axis2_nm == nm, na.rm = TRUE)) "axis2_val"
           else stop("panel_pick: ", key, " has no axis '", nm, "'. available: ",
                     paste(unique(stats::na.omit(c(d$axis1_nm, d$axis2_nm))), collapse = " | "))
    if (!identical(want, "*")) {
      miss <- setdiff(want, unique(d[[col]]))
      if (length(miss) > 0) {
        stop("panel_pick: ", key, " / ", nm, " has no value(s) ", paste(miss, collapse = ", "),
             ". available: ", paste(utils::head(unique(d[[col]]), 25), collapse = " | "))
      }
      d <- d[d[[col]] %in% want, , drop = FALSE]
    }
    d$.axis <- d[[col]]
    names(d)[names(d) == ".axis"] <- nm
  }
  if (!is.null(item)) {
    miss <- setdiff(item, unique(d$itm_nm))
    if (length(miss) > 0) {
      stop("panel_pick: ", key, " has no item(s) ", paste(miss, collapse = ", "),
           ". available: ", paste(unique(d$itm_nm), collapse = " | "))
    }
    d <- d[d$itm_nm %in% item, , drop = FALSE]
  }
  if (drop_na) d <- d[!is.na(d$value), , drop = FALSE]
  if (nrow(d) == 0) stop("panel_pick: ", key, " matched 0 rows after filtering")
  d
}

## One observation per year from a quarterly series. Q4 is the convention (it is
## what the manual HIRA exports in the lab use), but the newest year has no Q4
## yet, so the latest available quarter stands in and the column says which.
annualise <- function(d) {
  if (all(is.na(d$quarter))) { d$q_used <- NA_integer_; return(d) }
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  ## Group on the identity columns as a pasted string: interaction() on a
  ## data-frame subset errors, and a tibble subset is still a data frame.
  id_cols <- intersect(c("key", "axis1_val", "axis2_val", "itm_nm"), names(d))
  grp <- do.call(paste, c(d[id_cols], list(d$year, sep = "\r")))
  out <- do.call(rbind, lapply(split(seq_len(nrow(d)), grp), function(ix) {
    g <- d[ix, , drop = FALSE]
    g[which.max(g$quarter), , drop = FALSE]
  }))
  out$q_used <- out$quarter
  rownames(out) <- NULL
  out[order(out$year), , drop = FALSE]
}

##################################################################
#####  2. GATES - identities that must hold, or we stop      #####
##################################################################
##
## Anything asserted here is something a later script would otherwise trust
## silently. A gate that never fires is untested, so 99_acceptance.R feeds a
## deliberately broken row through this file.

## helper: pull one slice as a named vector keyed by period
.slice <- function(panel, k, filt) {
  d <- panel[panel$key == k, , drop = FALSE]
  d <- d[filt(d), , drop = FALSE]
  d
}

gate_region_sums <- function(panel, tol = 0) {
  ## For every table carrying a region axis, the 17 provinces must add up to the
  ## national row. This is the strongest internal check available: a wrong code,
  ## a dropped province or a mis-slotted axis all break it.
  ##
  ## The region axis is DETECTED, not declared. Which slot holds the region
  ## differs by table - DT_HIRA45_2 returns 입원실현황별 as C1 and 시군구별 as C2,
  ## while DT_HIRA4T is the other way round - and hand-listing the slot per table
  ## reintroduces exactly the brittleness the name-based registry removed.
  TOTAL_LABELS <- c("계", "전체", "전국", "합계")
  checked <- 0L
  skipped_ratio <- character(0)
  unchecked     <- character(0)   # 게이트가 «아무 말도 못 한» 지표
  for (k in unique(panel$key)) {
    d <- panel[panel$key == k, , drop = FALSE]

    ## ⛔ 이 게이트는 «합산 가능한 양»(개수·병상·진료비)을 전제한다. **비율은 안 더해진다** —
    ## 재정자립도의 시도 합은 8254%, 전국은 58% 다(2026-09-01 실측). 그래서 단위가 %인
    ## 계열은 «건너뛴다». 느슨하게 만드는 것이 아니라 **적용 대상을 바로잡는 것**이다.
    ## ⚠ 건너뛴 것을 조용히 넘기지 않고 세어서 보고한다 — 안 그러면 「검사됐다」고 오해한다.
    ## ⛔ 2026-09-03: 예전엔 «계열 통째로» 건너뛰었다. 그런데 한 계열이 「건수」와 「생존율」을
    ##    둘 다 갖는 경우가 있고(OHCA), 그러면 검사 가능한 건수까지 빠졌다.
    ##    → 비율 «항목»만 빼고 나머지로 검사한다. 무엇을 뺐는지도 계속 보고한다.
    ## ⛔ 「더할 수 있는 양」인가로 가른다 - 이름이 아니라 «단위»로.
    ##    2026-09-03: 연령표준화 사망률(`십만명당`)이 이 목록에 없어 게이트가
    ##    0칸을 검사하고 OK 라고 말했다. 17시도 합 4,967 vs 전국 300(≈17배).
    ## ⛔ 「인구당·평균」만 뺀다. 입내원일수(단위 일)는 «세는 양»이라 더해진다 -
    ##    단위만 보고 그것까지 빼면 오래 통과하던 검사 다섯이 조용히 사라진다(실측).
    is_ratio <- d$unit %in% c("%", "％", "십만명당", "10만명당", "만명당", "천명당") |
                grepl("^평균", d$itm_nm)
    if (any(is_ratio, na.rm = TRUE)) {
      skipped_ratio <- c(skipped_ratio,
                         sprintf("%s[%s]", k, paste(unique(d$itm_nm[is_ratio]), collapse = "/")))
      d <- d[!is_ratio, , drop = FALSE]
      if (!nrow(d)) next
    }

    region_col <- NULL
    for (col in c("axis1_val", "axis2_val")) {
      vals <- unique(stats::na.omit(d[[col]]))
      if (length(vals) == 0) next
      if (sum(canon_sido(vals) %in% SIDO_17) >= 15) { region_col <- col; break }
    }
    if (is.null(region_col)) next

    by_col <- setdiff(c("axis1_val", "axis2_val"), region_col)
    d$.region <- d[[region_col]]
    d$.by <- if (all(is.na(d[[by_col]]))) d$itm_nm else paste(d[[by_col]], d$itm_nm)

    tot_lab <- intersect(unique(d$.region), TOTAL_LABELS)
    tot_lab <- setdiff(tot_lab, names(SIDO_CANON))  # 전체/전국 are never provinces
    if (length(tot_lab) == 0) {
      ## Legitimate table shape, not a defect: the 관내/관외 tables (org 350) list
      ## the 17 provinces with no national row at all. Nothing to check here -
      ## gate_access_identity() covers those by a cross-table identity instead.
      cat(sprintf("--- gate region-sum  %-14s skipped (no national row)\n", k))
      next
    }
    if (length(tot_lab) > 1) {
      stop("gate_region_sums: ", k, " has ", length(tot_lab),
           " candidate national rows (", paste(tot_lab, collapse = ", "), ")")
    }

    tot <- d[d$.region == tot_lab, c("prd_de", ".by", "value")]
    names(tot)[3] <- "total"
    ## Sum EVERY non-total row, not just the 17 provinces: TX_35003_A004 carries
    ## a 기타 row (residence unknown / abroad) that belongs to the national total,
    ## so a provinces-only sum falls short by exactly that amount. Summing all
    ## non-total values is also the more general statement of the identity.
    ## (If a region axis ever mixes parents and children, this over-counts - and
    ## the gate failing is the right way to find that out.)
    prt <- d[!is.na(d$.region) & d$.region != tot_lab, ]
    ## A cell whose parts include an unpublished value cannot be summed: KOSIS
    ## leaves TX_35003_A004's 기타 row empty before 2010, so the parts fall short
    ## by exactly that unknown amount. Skip those rather than call them errors.
    prt$.na <- as.integer(is.na(prt$value))
    agg <- stats::aggregate(cbind(value, .na) ~ prd_de + .by, data = prt,
                            FUN = sum, na.rm = TRUE, na.action = stats::na.pass)
    cmp <- merge(tot, agg, by = c("prd_de", ".by"))
    if (nrow(cmp) == 0) stop("gate_region_sums: ", k, " produced no comparable rows")
    cmp$diff <- abs(cmp$value - cmp$total)
    ## Counts must match exactly; money aggregated in 천원 carries rounding (서울
    ## 2024 진료비 is off by 1 of 128,075,117,581). A relative floor keeps the
    ## count tables strict - 1e-6 of 722,671 beds is 0.72, so one stray bed still
    ## fails - while not reporting a rounding unit as a defect.
    cmp$tol <- pmax(tol, 1e-6 * abs(cmp$total))
    ## An empty cell means two different things and only the arithmetic can tell
    ## them apart. In DT_41104_411 a blank is a STRUCTURAL ZERO - 세종 has no
    ## 권역응급의료센터 - and treating blanks as 0 makes the provinces add to the
    ## national row exactly, which is itself the proof they are zeros. In
    ## TX_35003_A004 the 기타 row is simply UNPUBLISHED before 2010 and the parts
    ## fall short by an unknown amount.
    ## So: sum with blanks as 0 first. If it balances, the blanks were zeros and
    ## the cell is checked. If it does not and blanks are present, the sum is not
    ## computable - skip rather than report a defect that is not there.
    ok  <- cmp$diff <= cmp$tol & !is.na(cmp$diff)
    bad <- cmp[!ok & cmp$.na == 0, , drop = FALSE]
    n_skip <- sum(!ok & cmp$.na > 0)
    cmp <- cmp[ok, , drop = FALSE]
    if (nrow(bad) > 0) {
      worst <- bad[order(-bad$diff), ][1, ]
      stop(sprintf("gate_region_sums FAILED for %s: %d of %d cells differ. worst %s / %s: parts=%.0f national=%.0f",
                   k, nrow(bad), nrow(cmp), worst$prd_de, worst$.by, worst$value, worst$total))
    }
    ## ⭐ 「맞는가」와 「검사했는가」는 다른 질문이다. 0칸을 OK 라 부르면 그 지표는
    ##    검사된 적이 없는데 검사된 것처럼 보인다(2026-09-03 mort_sgg 실측).
    if (nrow(cmp) == 0L) {
      cat(sprintf("--- gate region-sum  %-12s ⚠ 미검사 (대조 가능한 칸 0, %d칸 부분값 미공개)\n",
                  k, n_skip))
      unchecked <- c(unchecked, k)
    } else {
      cat(sprintf("--- gate region-sum  %-12s OK (%s, %d cells%s)\n", k, region_col, nrow(cmp),
                  if (n_skip > 0) sprintf(", %d skipped: 부분값 미공개", n_skip) else ""))
      checked <- checked + 1L
    }
  }
  if (length(skipped_ratio))
    cat(sprintf("--- gate region-sum  비율 항목 제외(합산 불가): %s\n",
                paste(unique(skipped_ratio), collapse = ", ")))
  if (length(unchecked))
    cat(sprintf("--- gate region-sum  ⚠ 이 지표들은 «검사되지 않았다»: %s\n",
                paste(unchecked, collapse = ", ")))
  if (checked == 0L) stop("gate_region_sums: no table carried a detectable region axis")
}

gate_period_continuity <- function(panel) {
  for (k in unique(panel$key)) {
    d <- panel[panel$key == k, ]
    prd_se <- d$prd_se[1]
    got <- sort(unique(d$prd_de))
    want <- enum_periods(min(got), max(got), if (prd_se == "Q") "Q" else "Y")
    miss <- setdiff(want, got)
    if (length(miss) > 0) {
      stop("gate_period_continuity FAILED for ", k, ": missing ",
           length(miss), " period(s): ", paste(utils::head(miss, 8), collapse = ", "))
    }
    cat(sprintf("--- gate continuity  %-12s OK (%d periods)\n", k, length(got)))
  }
}

gate_no_dead_category <- function(panel) {
  ## Header present is not the same as value present: a category that resolved
  ## to a real code but came back entirely empty means we asked for the wrong
  ## thing, and no downstream check would notice.
  ##
  ## Scoped to whole CATEGORIES, not single cells. An individual cell can be
  ## legitimately empty - 세종 has no 권역응급의료센터, and only a couple of
  ## 전문응급의료센터 exist nationwide - so requiring every cell to be populated
  ## would flag real structural zeros as bugs. A category empty in EVERY period
  ## and every combination is the failure this is looking for.
  bad <- character(0)
  for (k in unique(panel$key)) {
    d <- panel[panel$key == k, ]
    for (col in c("axis1_val", "axis2_val", "itm_nm")) {
      v <- d[[col]]
      if (all(is.na(v))) next
      for (lev in unique(stats::na.omit(v))) {
        if (all(is.na(d$value[v == lev & !is.na(v)]))) {
          bad <- c(bad, paste0(k, " / ", col, " = ", lev))
        }
      }
    }
  }
  if (length(bad) > 0) {
    stop("gate_no_dead_category FAILED: ", length(bad),
         " requested categories are empty everywhere:\n  ",
         paste(utils::head(bad, 10), collapse = "\n  "))
  }
  n_cell <- sum(!is.na(panel$value))
  cat(sprintf("--- gate dead-category OK (%d categories populated, %d values)\n",
              length(unique(paste(panel$key, panel$axis1_val, panel$axis2_val))), n_cell))
}

##################################################################
#####  gate: 요청했는데 «안 온» 축 값                        #####
##################################################################
##
## 죽은 범주 게이트는 «패널에 있는» 범주가 비었는지를 본다. 그런데 KOSIS가 그 범주의
## 행을 «아예 안 보내면» 패널에 없으므로 그 게이트의 눈에 안 띈다 —
## 요청한 것과 받은 것을 직접 대조해야만 보인다.
##
## 실측(2026-09-01): OECD 국제비교 3표에서 「OECD」(평균 행)를 요청했는데 값이 하나도
## 오지 않았다. 그런데 그림도 표도 «조용히» 만들어졌고, 「한국 vs OECD 평균」을 찍는
## cat() 이 `character(0)` 을 인쇄해 **아무 말도 안 하는 것으로** 실패했다.
## 아무 말도 안 하는 실패가 가장 나쁘다 — 통과한 것처럼 보인다.
##
## 경고로 둔다(멈추지 않는다). 나라가 그 해에 보고를 안 한 것은 «정상»이고, 우리가
## 고칠 수 있는 것이 아니다. 다만 «보이게» 만든다.
gate_requested_vs_delivered <- function(panel, reg) {
  miss <- list()
  for (i in seq_len(nrow(reg))) {
    r <- reg[i, ]
    if (!nzchar(r$axis_spec) || identical(r$axis_spec, "ALL")) next
    got <- panel[panel$key == r$key & !is.na(panel$value), ]
    if (!nrow(got)) next
    for (part in strsplit(r$axis_spec, ";", fixed = TRUE)[[1]]) {
      kv <- strsplit(part, "=", fixed = TRUE)[[1]]
      if (length(kv) != 2 || kv[2] %in% c("ALL", "*", "TOPLEVEL")) next
      want <- strsplit(kv[2], "|", fixed = TRUE)[[1]]
      have <- unique(c(got$axis1_val[got$axis1_nm == kv[1]],
                       got$axis2_val[got$axis2_nm == kv[1]]))
      ## ⚠ 레지스트리는 «사람이 쓰는 문법»으로 적혀 있다. 날문자열로 대조하면
      ## 10건 중 9건이 거짓 양성이 된다(2026-09-01 실측). 시끄러운 게이트는 안 보게 되고,
      ## 안 보는 게이트는 없는 것만 못하다 — 풀고 나서 비교한다.
      ##   · TOTAL      = 합계 행의 «별칭»(표마다 계/전체/전국/합계로 다르다)
      ##   · 부모>자식  = 같은 축에 같은 이름이 여럿일 때의 «경로». 오는 값은 자식뿐.
      TOTALS <- c("계", "전체", "전국", "합계")
      norm <- function(v) ifelse(v == "TOTAL", NA_character_, sub("^.*>", "", v))
      wn <- norm(want)
      gone <- character(0)
      if (any(want == "TOTAL") && !any(TOTALS %in% have)) gone <- c(gone, "TOTAL")
      gone <- c(gone, want[!is.na(wn) & !(wn %in% have)])
      if (length(gone))
        miss[[length(miss) + 1L]] <- data.frame(
          key = r$key, tbl_id = r$tbl_id, axis = kv[1],
          missing = paste(gone, collapse = "|"), stringsAsFactors = FALSE)
    }
  }
  if (!length(miss)) {
    cat("--- gate requested-vs-delivered OK (요청한 축 값이 전부 왔다)\n")
    return(invisible(NULL))
  }
  m <- do.call(rbind, miss)
  f <- file.path(logs_dir, paste0("missing_axis_values_", stamp, ".csv"))
  utils::write.csv(m, f, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("!!! 요청했는데 «안 온» 축 값 %d건 — %s\n", nrow(m), basename(f)))
  for (i in seq_len(nrow(m)))
    cat(sprintf("    %-14s %-14s [%s] %s\n", m$key[i], m$tbl_id[i], m$axis[i], m$missing[i]))
  invisible(m)
}

detect_series_breaks <- function(panel, drop_pct = 3) {
  ## A category that appears part-way through a series is usually a
  ## reclassification, not a new thing coming into existence - and it makes the
  ## sibling it was carved out of look like it collapsed.
  ##
  ## Found in this data: 정신병원 first appears 2021Q1 with 44,661 beds while
  ## 병원 falls 165,107 -> 149,794 the same quarter. Reading the 병원 line as a
  ## 15,000-bed closure would be wrong - 병원 + 정신병원 actually ROSE. The other
  ## hit is real: 세종특별자치시 was carved out of 충청남도 in 2012.
  ##
  ## The comparison runs WITHIN A STRATUM - one level of the other axis and one
  ## item - because a category's value is otherwise several numbers per period
  ## and there is nothing to difference.
  ##
  ## This WARNS rather than stops: a break is a property of the source, not a
  ## defect in the pull. It writes what it found so the figures and the brief
  ## can say so instead of leaving a reader to misread the line.
  ## Signature of a carve-out: at the quarter a new category appears, some
  ## sibling drops by an amount COMPARABLE TO THE NEW CATEGORY'S OWN VALUE.
  ## Without that second condition every ordinary dip next to a new category
  ## gets reported and the warning becomes noise nobody reads.
  out <- list()
  for (k in unique(panel$key)) {
    d <- panel[panel$key == k, , drop = FALSE]
    all_prd <- sort(unique(d$prd_de))
    for (col in c("axis1_val", "axis2_val")) {
      if (all(is.na(d[[col]]))) next
      other <- setdiff(c("axis1_val", "axis2_val"), col)
      d$.strat <- paste(ifelse(is.na(d[[other]]), "", d[[other]]), d$itm_nm, sep = "\r")
      for (st in unique(d$.strat)) {
        ds <- d[d$.strat == st, , drop = FALSE]
        levs <- unique(stats::na.omit(ds[[col]]))
        for (lev in levs) {
          g <- ds[ds[[col]] == lev & !is.na(ds$value), , drop = FALSE]
          if (nrow(g) == 0) next
          first <- min(g$prd_de)
          if (first <= all_prd[1]) next        # present from the start: fine
          new_val <- g$value[g$prd_de == first]
          if (length(new_val) != 1 || is.na(new_val) || new_val <= 0) next
          prev <- all_prd[which(all_prd == first) - 1L]
          if (length(prev) == 0) next

          for (sl in setdiff(levs, lev)) {
            a <- ds$value[ds[[col]] == sl & ds$prd_de == prev]
            b <- ds$value[ds[[col]] == sl & ds$prd_de == first]
            if (length(a) != 1 || length(b) != 1 || is.na(a) || is.na(b) || a == 0) next
            drop <- a - b
            if (drop <= 0) next
            if (100 * drop / a < drop_pct) next
            if (drop < 0.2 * new_val) next     # too small to be the source
            ## Keep EVERY qualifying sibling, not just the biggest: 정신병원 was
            ## carved out of 요양병원 AND 병원 at once (-32,309 and -15,313 against
            ## its own 44,661), and reporting only the larger one hides a parent.
            out[[length(out) + 1L]] <- data.frame(
              key = k, 층 = sub("\r", " / ", st, fixed = TRUE),
              신규범주 = lev, 등장시점 = first, 신규값 = new_val,
              감소범주 = sl, 직전값 = a, 등장시점값 = b,
              감소량 = drop, 변화율_퍼센트 = round(-100 * drop / a, 1),
              stringsAsFactors = FALSE)
          }
        }
      }
    }
  }
  if (length(out) == 0) {
    cat("--- gate series-break OK (no mid-series category appearances)\n")
    return(invisible(NULL))
  }
  br <- do.call(rbind, out)
  br <- br[order(-br$감소량), ]
  f <- file.path(logs_dir, paste0("series_breaks_", stamp, ".csv"))
  readr::write_excel_csv(br, f)
  ## one line per (key, new category, sibling) for the console
  head_br <- br[!duplicated(paste(br$key, br$신규범주, br$감소범주)), ]
  warning("series break(s) detected - see ", basename(f), call. = FALSE)
  cat("--- gate series-break WARNING:", nrow(br), "row(s),",
      nrow(head_br), "distinct ->", basename(f), "\n")
  for (i in seq_len(min(nrow(head_br), 6))) {
    cat(sprintf("      %s: %s 등장(%s) <-> %s %.0f -> %.0f (%.1f%%)\n",
                head_br$key[i], head_br$신규범주[i], head_br$등장시점[i],
                head_br$감소범주[i], head_br$직전값[i], head_br$등장시점값[i],
                head_br$변화율_퍼센트[i]))
  }
  invisible(br)
}

## Years where KOSIS's own 전체 table disagrees with 관내 + 관외. Found by
## measurement, not assumption - see gate_access_identity(). Views flag these.
ACCESS_MISMATCH_YEARS <- c(2006L, 2011L)

gate_access_identity <- function(panel) {
  ## 관내 + 관외 must equal 전체, province by province and year by year. All three
  ## come from separate KOSIS tables, so this is a genuine cross-table identity -
  ## the strongest check available for the access domain, and the reason
  ## care_total_res is collected at all (nothing else uses it).
  ##
  ## It also pins down WHOSE region "관내" means. Verified 2026-08-31 on 서울 2024
  ## 진료비: TX_35003_A004 = 22,250,709,108 = A005 (19,903,023,387) + A006
  ## (2,347,685,721) exactly, so the TX series is keyed by PATIENT RESIDENCE. The
  ## institution-keyed pair says something else entirely - DT_35003_A0072 puts
  ## 서울's outside-region care at 10,805,486,048, 4.6x larger, because that is
  ## non-residents treated in 서울 rather than residents treated elsewhere.
  need <- c("care_in_res", "care_out_res", "care_total_res")
  if (!all(need %in% panel$key)) {
    cat("--- gate access-identity skipped (access rows not active)\n")
    return(invisible(NULL))
  }
  gr <- function(k, lev) {
    d <- panel[panel$key == k & !is.na(panel$sido) & !is.na(panel$value), ]
    d <- d[d$axis2_val == lev, ]
    stats::aggregate(value ~ year + sido + itm_nm, data = d, FUN = sum)
  }
  a <- gr("care_in_res", "계");  names(a)[4] <- "관내"
  b <- gr("care_out_res", "계"); names(b)[4] <- "관외"
  t <- gr("care_total_res", "합계"); names(t)[4] <- "전체"
  cmp <- merge(merge(a, b, by = c("year", "sido", "itm_nm")), t,
               by = c("year", "sido", "itm_nm"))
  if (nrow(cmp) == 0) stop("gate_access_identity: nothing comparable")
  cmp$diff <- abs(cmp$관내 + cmp$관외 - cmp$전체)
  bad <- cmp[cmp$diff > pmax(1, 1e-6 * abs(cmp$전체)), , drop = FALSE]  # 천원 반올림
  ## Measured 2026-08-31: the identity holds exactly in 17 of 19 years. It fails
  ## in 2006 and 2011 - every province, both items - so those two vintages of the
  ## 전체 table were compiled differently from the 관내/관외 pair. That is a
  ## property of the source, not of this code, and 자체충족률 is computed as
  ## 관내/(관내+관외) so it never divides by the disagreeing total.
  ## The exception is PINNED rather than tolerated: a third year drifting is a
  ## new fact and must stop the run.
  bad_years <- sort(unique(bad$year))
  new_years <- setdiff(bad_years, ACCESS_MISMATCH_YEARS)
  if (length(new_years) > 0) {
    w <- bad[bad$year %in% new_years, ][which.max(bad$diff[bad$year %in% new_years]), ]
    stop(sprintf("gate_access_identity FAILED in unexpected year(s) %s. worst %d %s %s: 관내+관외=%.0f vs 전체=%.0f",
                 paste(new_years, collapse = ", "), w$year, w$sido, w$itm_nm,
                 w$관내 + w$관외, w$전체))
  }
  if (nrow(bad) > 0) {
    worst <- max(100 * bad$diff / bad$전체)
    cat(sprintf("--- gate access-identity OK (%d of %d cells; %s 제외 = 원자료 불일치, 최대 %.1f%%)\n",
                nrow(cmp) - nrow(bad), nrow(cmp),
                paste(bad_years, collapse = "/"), worst))
  } else {
    cat(sprintf("--- gate access-identity OK (%d cells: 관내+관외 = 전체)\n", nrow(cmp)))
  }
}

gate_denominator <- function(panel, tol_pct = 1.5) {
  ## Cross-check the population denominator against the lab's shared copy in
  ## 2nd/dataonly/kor_pop. NOT an identity: that file is 연앙인구 (mid-year
  ## average, hence the .5 in 1997) from 통계청, while DT_1YL20651E is 연말
  ## 주민등록인구 from 행정안전부. They should track within about a percent; a
  ## bigger gap means we grabbed the wrong table or the wrong unit.
  f <- normalizePath(file.path(PROJ_ROOT, "..", "..", "2nd", "dataonly", "kor_pop",
                               "data", "pop_sgg_age_sex_1997_2023.csv"), mustWork = FALSE)
  if (!file.exists(f)) {
    warning("gate_denominator SKIPPED: shared kor_pop not found at ", f)
    return(invisible(NULL))
  }
  ref <- readr::read_csv(f, col_types = readr::cols_only(
    year = readr::col_integer(), sgg_code = readr::col_character(),
    sex = readr::col_character(), age = readr::col_character(),
    pop = readr::col_double()), progress = FALSE)
  ref <- ref[ref$sgg_code == "00" & ref$sex == "계" & ref$age == "계", c("year", "pop")]

  ours <- panel[panel$key == "pop_sido" & panel$axis1_val == "전국", c("year", "value")]
  cmp <- merge(ours, ref, by = "year")
  cmp$pct <- 100 * abs(cmp$value - cmp$pop) / cmp$pop
  if (nrow(cmp) == 0) stop("gate_denominator: no overlapping years to compare")
  worst <- cmp[which.max(cmp$pct), ]
  if (worst$pct > tol_pct) {
    stop(sprintf("gate_denominator FAILED: %d differs by %.2f%% (ours %.0f vs kor_pop %.0f)",
                 worst$year, worst$pct, worst$value, worst$pop))
  }
  cat(sprintf("--- gate denominator OK (%d overlapping years, worst gap %.2f%% in %d)\n",
              nrow(cmp), worst$pct, worst$year))
}

run_gates <- function(panel, reg = NULL) {
  cat("=== gates ===\n")
  gate_period_continuity(panel)
  gate_no_dead_category(panel)
  ## 죽은 범주 게이트가 «못 보는» 것을 본다: 요청했는데 «행 자체가 안 온» 축 값.
  if (!is.null(reg)) gate_requested_vs_delivered(panel, reg)
  gate_region_sums(panel)
  gate_denominator(panel)
  gate_access_identity(panel)
  detect_series_breaks(panel)
  invisible(TRUE)
}

##################################################################
#####  3. RUN                                                #####
##################################################################

if (sys.nframe() == 0L) {
  cat("=== 03_build_panel.R ===\n")
  panel <- build_panel()
  run_gates(panel, kosis_rows(load_registry()))
  save_step(panel, "panel")
  cat(sprintf("=== panel: %d rows, %d indicators ===\n", nrow(panel),
              length(unique(panel$key))))
}
