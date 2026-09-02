"""✅ 검증 통과 (2026-08-31) — 산출물을 시도로 합치면 KOSIS `care_total_res` 진료비와
   **303칸(2006~2024년) 전부 오차 0.00%**(1칸은 원자료 결함으로 면제). 게이트는 R/16_yearbook.R.
   ⚠ 제목이 «두 칸에 쪼개져» 들어오는 해가 있다(2006~2010: A1="시ㆍ군ㆍ구별 급여형태별"
   + H1="진료실적 현황(관내-계)"). 1~3행을 이어 붙여 찾고, 그 «틈»은 MID_OK 로 통제한다.

지역별 의료이용 통계연보 19년치에서 「시ㆍ군ㆍ구별 진료현황」 표를 뽑는다.

이 표가 목표인 이유:
  · 시군구 × 연도 × 급여형태 × 5개 지표(진료실인원수·입내원일수·요양급여일수·진료비·급여비)
  · **(관내)/(관외) 판이 따로 있다** → 시군구 단위 «자체충족률 시계열». KOSIS 는 시도까지,
    헬스맵은 2023 한 해뿐이라 이 조합은 여기서만 나온다.

읽는 규칙 (실측으로 세운 것, 2026-08-31):
  1행     제목.  "시ㆍ군ㆍ구별 진료현황(계)" / "(건강보험)" / "(의료급여 1종)" /
          "(관내)" / "(관외)" … 뒤에 "<계속>" 이 붙으면 앞 시트의 연속이다.
  5~9행   한영 병기 다중 헤더. 병합이라 한 칸으로는 못 읽는다 → «열별로 세로로 이어 붙여»
          지표를 찾는다(map_columns). ⛔ **위치로 읽지 말 것** — 2006·2007 에는
          「지급건수(건)」 열이 하나 더 있어 위치가 한 칸씩 밀린다. 그렇게 읽으면
          진료비 자리에 «진료일수»가 들어오고(서울 2007 = 206,145,862) 게이트에서
          95.8% 오차로 잡힌다. 열 이름도 흔들린다(2007 「총진료비」 / 2008+ 「진료비」).
  10행~   1열 = 지역명, 그 뒤는 map_columns 가 찾아 준 열. 파생지표(「…당」)는 버린다.

⚠ 시트 «이름»은 연도 간에 안 이어진다(2024=`128p`, 2006=`3`). 그래서 이름이 아니라
   **매 시트의 1행을 직접 읽어** 판별한다. sheet_index.csv 의 제목은 후보 탐색용이고
   여기서는 쓰지 않는다 — 그 제목 추출이 열 머리글을 집기도 하기 때문이다.

⚠⚠ **장(章)을 반드시 함께 봐야 한다.** 제목만으로 자르면 서로 다른 기준의 표가 섞인다:
   제2장 진료실적/급여실적 · 제3장 관내 및 관외 = **환자 거주지 기준**
   「의료기관 시군구별」 장                     = **의료기관 소재지 기준**
   ⚠ 장 «번호»는 연도마다 다르다(2024=7장, 2015=4장) — **이름**으로 갈라야 한다.
   둘 다 「시ㆍ군ㆍ구별 … 진료현황(계/관내/관외)」 라는 같은 제목을 쓴다. 합치면 시도
   합계가 2.4배가 된다(실측: 2024 서울 52.96조 vs KOSIS 22.25조). KOSIS 의 관내/관외가
   거주지·기관 두 기준으로 갈리는 것과 «같은 함정»이 이 자료에도 있다.
   워크북에 `제N장 …` 이름의 구분 시트가 실제로 있으므로 그것으로 자른다.

  python R/_parse_yearbook.py            # 전체 -> 정본 진료현황_시군구.csv 갱신
  python R/_parse_yearbook.py 2023 2024  # 특정 연도 -> **별도 파일** _부분_*.csv

⛔ 연도를 지정하면 정본을 «덮어쓰지 않는다». 2026-09-01 실측 사고: 경로 수정을 시험하려고
   `... 2024` 를 돌렸더니 정본이 한 해로 줄었는데, 대조 게이트는 «있는 연도»만 보므로
   0.00% 로 통과했고 브리핑·페이지·논문 코호트가 조용히 1년짜리가 됐다.
   게이트가 「맞는가」는 봤지만 「다 있는가」는 안 봤다 — 둘은 다른 질문이다.
   (그래서 16_yearbook.R 에 `gate_yearbook_span` 도 함께 생겼다.)

출력: dataonly/nhis_region_stats/parsed/진료현황_시군구.csv (long)
"""
import zipfile, io, os, re, sys, csv, tempfile, shutil, warnings

warnings.filterwarnings("ignore")
## ⛔ 기계 고정 경로를 적지 않는다(랩 규칙). 00_config.R 의 DATA_ONLY 와 같은 셈:
## 프로젝트 루트에서 두 단계 올라가 2nd/dataonly.
_R = os.path.dirname(os.path.abspath(__file__))
PROJ = os.path.dirname(_R)
DATA_ONLY = os.path.abspath(os.path.join(PROJ, "..", "..", "2nd", "dataonly"))
D = os.path.join(DATA_ONLY, "nhis_region_stats")
OUTDIR = os.path.join(D, "parsed")
OUT_FULL = os.path.join(OUTDIR, "진료현황_시군구.csv")


def out_path(years):
    """부분 실행은 정본을 건드리지 않는다(위 ⛔ 참조)."""
    if not years:
        return OUT_FULL
    return os.path.join(OUTDIR, "_부분_%s.csv" % "-".join(sorted(map(str, years))))

MEASURES = ["진료실인원수", "입내원일수", "요양급여일수", "진료비", "급여비"]

## 헤더를 못 읽어 «담지 않은» 시트. 조용히 넘기지 않고 끝에 보고한다.
SKIPPED = []

SIDO = ["서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종",
        "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주"]
SIDO_ALIAS = {"서울특별시": "서울", "부산광역시": "부산", "대구광역시": "대구",
              "인천광역시": "인천", "광주광역시": "광주", "대전광역시": "대전",
              "울산광역시": "울산", "세종특별자치시": "세종", "세종시": "세종",
              "경기도": "경기", "강원도": "강원", "강원특별자치도": "강원",
              "충청북도": "충북", "충청남도": "충남", "전라북도": "전북",
              "전북특별자치도": "전북", "전라남도": "전남", "경상북도": "경북",
              "경상남도": "경남", "제주도": "제주", "제주특별자치도": "제주"}


def zname(i):
    if i.flag_bits & 0x800:
        return i.filename
    try:
        return i.filename.encode("cp437").decode("cp949")
    except Exception:
        return i.filename


def year_of(fn):
    m = re.search(r"(19|20)\d{2}", fn)
    return m.group(0) if m else ""


def clean(x):
    return re.sub(r"\s+", "", str(x)) if x is not None else ""


## 제목은 «한 셀에 있지 않을 수 있다» — 2024 는 A1 에 통째로 있지만 2023 은
## A1="시ㆍ군ㆍ구별" 과 다른 셀 "진료실적 현황(계)" 로 쪼개져 있다. 1~3행을 이어 붙여 찾는다.
## 표현도 연도마다 다르다(진료현황 / 진료실적현황), 구분자도 ㆍ 와 · 가 섞인다.
##
## ⚠ 「시군구별」과 「진료현황」 사이의 «틈»을 느슨하게 두면(`.{0,8}`) 다른 장(章)의
##    표가 통째로 딸려 온다 — 「시ㆍ군ㆍ구별 **요양기관종별** 진료현황」,
##    「… **연령별성별** …」. 실제로 한 권에서 같은 시군구가 7번 나오고 값이 전부
##    달랐다. 그래서 «틈을 잡아내서» MID_OK 에 있는 것만 받는다(아래).
TITLE_RE = re.compile(r"시.?군.?구별([^(]{0,14}?)진료(?:실적)?현황\(([^)]+)\)")


## 「시군구별」과 「진료현황」 사이에 올 수 있는 «허용된» 말. 목록으로 두는 이유:
## 비워 두면 다른 표(「요양기관종별」·「연령별성별」)가 딸려 오고, 아무것이나 받으면
## 3.3배가 된다. 2006~2010 은 제목이 두 칸에 쪼개져 있어 이 틈에 「급여형태별」이 온다
## (A1="시ㆍ군ㆍ구별 급여형태별" + H1="진료실적 현황(관내-계)"). 그 다섯 해를 살리는 열쇠다.
MID_OK = ("", "급여형태별")


## 지표 이름의 «변종». 왼쪽에서 처음 맞는 열을 그 지표로 삼는다.
MEASURE_PAT = [
    ("진료실인원수", ("진료실인원",)),
    ("입내원일수",   ("내원일수",)),
    ("요양급여일수", ("급여일수", "진료일수")),
    ("진료비",       ("총진료비", "진료비")),
    ("급여비",       ("급여비",)),
]


def map_columns(grid):
    """헤더를 열별로 세로로 이어 붙여 {지표: 열번호} 를 만든다. 못 찾으면 None.

    파생지표(「건당」·「진료실인원당」·「내원1일당」)는 원지표와 «같은 이름»을 쓰므로
    「당」이 처음 나오는 열을 경계로 삼고 그 왼쪽에서만 찾는다.
    """
    ## 헤더 = «첫 데이터 행» 앞의 모든 행. 데이터 행은 1열이 지역명이고 그 뒤에 «수»가
    ## 있는 행이다. 1열이 한글이라는 것만으로 끊으면 제목행("시ㆍ군ㆍ구별 …")에서 멈춘다.
    head_rows = []
    for row in grid[:14]:
        first = clean(row[0]) if row else ""
        has_num = any(num(row[i]) is not None
                      for i in range(1, min(len(row), 8)))
        if first and first != "구분" and re.search(r"[가-힣]", first) and has_num:
            break              # 첫 데이터 행(지역명 + 값)을 만났다
        head_rows.append(row)
    if not head_rows:
        return None
    ncol = max(len(r) for r in head_rows)
    col = ["".join(clean(r[c]) if len(r) > c else "" for r in head_rows)
           for c in range(ncol)]
    bound = next((c for c in range(1, ncol) if "당" in col[c]), ncol)
    out = {}
    for name, pats in MEASURE_PAT:
        for c in range(1, bound):
            if c in out.values():
                continue
            if any(pt in col[c] for pt in pats):
                out[name] = c
                break
    return out or None


def parse_title(grid):
    """(구분, 계속여부) 또는 None. 틈이 MID_OK 가 아니면 «다른 표»이므로 받지 않는다."""
    joined = ""
    for row in grid[:3]:
        joined += "".join(clean(c) for c in row)
    m = TITLE_RE.search(joined)
    if not m:
        return None
    if m.group(1) not in MID_OK:   # 요양기관종별 / 연령별성별 / … → 다른 표
        return None
    return m.group(2), ("계속" in joined)


## 권 이름에서 그 책이 담은 시도를 읽는다: "02_서울 인천 경기 강원_최종.xlsx"
def sidos_of_volume(vol):
    found = []
    for full, short in SIDO_ALIAS.items():
        if full in vol and short not in found:
            found.append(short)
    for short in SIDO:
        if short in vol and short not in found:
            found.append(short)
    return found


def num(x):
    if x is None:
        return None
    if isinstance(x, (int, float)):
        return float(x)
    s = re.sub(r"[,\s]", "", str(x))
    if not s or s in ("-", "‐", "–"):
        return None
    try:
        return float(s)
    except ValueError:
        return None


def rows_of(path, is_xlsx):
    """(시트명, 1행, [데이터행...]) 를 흘려 준다."""
    if is_xlsx:
        import openpyxl
        wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
        try:
            for sn in wb.sheetnames:
                ws = wb[sn]
                grid = [r for r in ws.iter_rows(min_row=1, max_row=ws.max_row or 1,
                                                max_col=8, values_only=True)]
                if grid:
                    yield sn, grid
        finally:
            wb.close()
    else:
        import xlrd
        wb = xlrd.open_workbook(path, on_demand=True)
        try:
            for sn in wb.sheet_names():
                try:
                    ws = wb.sheet_by_name(sn)
                except Exception:
                    continue
                grid = [[ws.cell_value(r, c) for c in range(min(8, ws.ncols))]
                        for r in range(ws.nrows)]
                yield sn, grid
                wb.unload_sheet(sn)
        finally:
            wb.release_resources()


CHAP_RE = re.compile(r"^제\s*(\d+)\s*장\s*(.*)$")


def parse_book(path, is_xlsx, year, vol, out):
    """시도 상태는 «표» 단위로 유지한다 — 표가 여러 시트에 걸쳐(<계속>) 이어지므로
    시트마다 초기화하면 이어지는 쪽의 시도를 잃는다. 새 표가 시작되면(계속 아님) 초기화."""
    vol_sidos = sidos_of_volume(vol)
    cur_sido = None
    chap_no, chap_nm = "", ""
    for sn, grid in rows_of(path, is_xlsx):
        m = CHAP_RE.match(clean(sn).replace("장", "장 ", 1).strip()) or \
            CHAP_RE.match(str(sn).strip())
        if m:
            chap_no, chap_nm = m.group(1), m.group(2).strip()
            cur_sido = None
            continue
        if not grid:
            continue
        t = parse_title(grid)
        if t is None:
            continue
        gubun, cont = t
        if not cont:
            cur_sido = None          # 새 표 → 첫 시도 행을 만날 때까지 미정
        cmap = map_columns(grid)
        if cmap is None or "진료비" not in cmap:
            ## 헤더를 못 읽으면 «위치로 넘겨짚지 않는다» — 그 추측이 2006·2007 을
            ## 통째로 틀리게 만들었다. 세어서 보고하고 넘어간다.
            SKIPPED.append((year, vol, sn))
            continue
        cols = [(m, cmap[m]) for m, _ in MEASURE_PAT if m in cmap]
        for row in grid[1:]:
            name = clean(row[0]) if row else ""
            if not name or not re.search(r"[가-힣]", name):
                continue
            if name in ("구분", "계"):
                continue
            vals = [(m, num(row[i]) if len(row) > i else None) for m, i in cols]
            if all(v is None for _, v in vals):
                continue
            canon = SIDO_ALIAS.get(name, name)
            if canon in SIDO:
                cur_sido = canon
                region, level = canon, "시도"
            else:
                if cur_sido is None:
                    # 시도를 모르는 채로 시군구를 담으면 «동구» 가 여러 시도에 걸쳐
                    # 뭉쳐진다. 담지 않고 세어서 보고한다.
                    out.append(dict(연도=year, 권=vol, 시트=sn, 장=chap_no, 장명=chap_nm,
                                    구분=gubun, 시도="", 지역=name,
                                    단위="시도미상", 지표="", 값=None))
                    continue
                region, level = name, "시군구"
            for m, v in vals:
                if v is None:
                    continue
                out.append(dict(연도=year, 권=vol, 시트=sn, 장=chap_no, 장명=chap_nm,
                                구분=gubun, 시도=cur_sido or "", 지역=region,
                                단위=level, 지표=m, 값=v))
    return len(vol_sidos)


def main(years=None):
    os.makedirs(OUTDIR, exist_ok=True)
    zips = sorted(f for f in os.listdir(D) if f.lower().endswith(".zip"))
    if years:
        zips = [z for z in zips if year_of(z) in years]
    out = []
    tmp = tempfile.mkdtemp(prefix="yb_parse_")
    try:
        for zf in zips:
            yr = year_of(zf)
            try:
                z = zipfile.ZipFile(os.path.join(D, zf))
            except Exception as e:
                print(f"  ! {zf}: {e}"); continue
            books = [i for i in z.infolist()
                     if not i.is_dir() and zname(i).lower().endswith((".xls", ".xlsx"))]
            before = len(out)
            for i in books:
                nm = zname(i)
                p = os.path.join(tmp, re.sub(r"[^\w.\-가-힣]", "_", nm))
                with open(p, "wb") as fh:
                    fh.write(z.read(i))
                try:
                    parse_book(p, nm.lower().endswith(".xlsx"), yr, nm, out)
                except Exception as e:
                    print(f"    ! {yr} {nm[:36]}: {type(e).__name__} {e}")
                finally:
                    try:
                        os.remove(p)
                    except OSError:
                        pass
            got = len(out) - before
            unk = sum(1 for r in out[before:] if r["단위"] == "시도미상")
            regions = len({r["지역"] for r in out[before:] if r["단위"] == "시군구"})
            kinds = sorted({r["구분"] for r in out[before:]})
            chaps = sorted({r["장"] for r in out[before:] if r["장"]})
            print(f"{yr}  행 {got:>7,}  시군구 {regions:>3}  시도미상 {unk:>4}  "
                  f"장 [{','.join(chaps) or '없음'}]  구분 {len(kinds)}: "
                  + ", ".join(kinds[:5]))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    dst = out_path(years)
    with io.open(dst, "w", encoding="utf-8-sig", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["연도", "권", "시트", "장", "장명", "구분",
                                           "시도", "지역", "단위", "지표", "값"])
        w.writeheader()
        w.writerows([r for r in out if r["단위"] != "시도미상"])
    n_unk = sum(1 for r in out if r["단위"] == "시도미상")
    if SKIPPED:
        yrs = sorted({y for y, _, _ in SKIPPED})
        print(f"⚠ 헤더를 못 읽어 «건너뛴» 시트 {len(SKIPPED)}개 "
              f"(연도 {yrs[0]}~{yrs[-1]})")
    if n_unk:
        print(f"⚠ 시도 미상으로 «버린» 행 {n_unk:,}개 — 표 첫머리에 시도 행이 없는 시트")
    print(f"\n총 {len(out):,}행 -> {dst}")
    if years:
        print("※ 부분 실행이라 정본(진료현황_시군구.csv)은 «건드리지 않았다».")


if __name__ == "__main__":
    main(set(sys.argv[1:]) or None)
