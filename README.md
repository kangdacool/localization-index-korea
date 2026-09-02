# Localization index in Korean districts, 2006–2024

Analysis code for **"Divergence beneath a stable indicator: the localization index in 230 Korean
districts, 2006–2024."**

The **localization index** — among the care received by an area's residents, the share delivered
within that area — is published annually for every Korean district by the National Health
Insurance Service under the name *jache-chungjok-ryul* (self-sufficiency rate), and appears in the
Korean literature also as the *relevance index*. This repository holds the code that builds a
nineteen-year district panel from the published yearbooks and produces every table and figure in
the manuscript.

---

## Reproduce

Run these in order. **The first step is a Python script that no R runner calls** — skipping it
leaves the pipeline in a state where step ③ fails and points you back to step ②, which cannot
help you.

```bash
# ① Parse nineteen editions of the NHIS regional utilisation yearbook
cd source
python R/_parse_yearbook.py
#    → parsed/진료현황_시군구.csv  (438,076 rows)
#    Passing years as arguments makes a PARTIAL run that writes elsewhere and
#    does not overwrite the canonical file.

# ② Source pipeline — collection, panel assembly, gates, yearbook assembly
Rscript R/run_pipeline.R
#    → data/processed/{panel,yearbook,...}_<stamp>.rds

# ③ Paper — reads only ②'s processed/ output. Makes no API calls.
cd ../paper
Rscript R/run_paper.R
#    → output/{tables,figures}/ and eight gates
```

**Requirements.** R 4.4.1 with `tidyr` `readr` `ggplot2` `ggrepel` `writexl` `readxl` `openxlsx`
`scales`; Python 3 with `openpyxl`. An API key is needed only for `--refetch`, and is read from a
local `.secrets` file or an environment variable — no key appears in this repository.

## Data

All source data are **publicly published** and are not redistributed here:

| Source | What |
|---|---|
| NHIS *Regional Statistical Yearbook of Medical Care Utilization*, 2006–2024 | District-level expenditure by residents, split by whether care was delivered inside or outside the district |
| Korean Statistical Information Service (KOSIS) | Population, local fiscal independence ratio, OECD comparison series, and the provincial series used to validate the parsed yearbook |

## What the code checks

The pipeline stops rather than reporting a number it cannot stand behind. Eight gates run on every
build:

| Gate | What it catches |
|---|---|
| Cohort | Balanced panel is 230 districts × 19 years with no gaps |
| Trajectory | Endpoint medians agree with the descriptive table |
| Theil decomposition | Between + within = total, to machine precision |
| Unweighted reproduction | The sensitivity script's Theil agrees with the main one |
| Body-table coverage | Every number in the manuscript's tables appears in its text |
| Surface | Prose, figure legends, **and spreadsheet sheet names and column headers** are checked against a list of statements known to be wrong |
| Forbidden strings | Claims that were corrected once cannot return |
| Manuscript | Key reported values match the script's output; table and figure numbers are contiguous |

Two of these exist because a specific error occurred and was fixed: the forbidden-string check
because a corrected sentence reappeared in a different section, and the surface gate because a
corrected phrase survived in a figure legend that no check was reading.

## Layout

```
source/R/    Monitoring pipeline (00_config … 17_view_benchmark, 99_acceptance)
             + _parse_yearbook.py — the yearbook parser
             + indicator_registry.csv — one row per indicator; add a row, not code
paper/R/     Paper pipeline (00_setup … 12_assemble)
```

The paper reads the source project's processed output and never writes to it.

## Notes on the yearbook parser

Three features of the source required explicit handling, and each is a place where a naive parser
gets a wrong answer:

1. **The same table title appears twice in every edition** under two different bases — once by
   patient residence and once by institution location. Combining them inflates provincial totals
   by a factor of 2.4. Tables are assigned by whether the chapter title contains "의료기관"
   (medical institution).
2. **Column positions are not stable.** The 2006 and 2007 editions carry an extra payment-count
   column and label the expenditure column differently, so measures are located by header text.
3. **Sheet names and chapter numbering vary by edition**, so target tables are identified from
   cell content rather than sheet names.

## License

Code: MIT (see `LICENSE`). The underlying data are published by the National Health Insurance
Service and Statistics Korea under their own terms.
