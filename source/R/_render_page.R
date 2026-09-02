##################################################################
#####  _render_page.R - 페이지를 «눈으로» 보기 위한 도구      #####
##################################################################
##
## 파이프라인이 아니라 검사 도구다(`_` 접두사). HTML은 구조 검사로는 아무것도
## 안 보인다 — 캡션이 잘리는지, 다크모드에서 글자가 사라지는지, 드릴다운 차트가
## 실제로 그려지는지는 렌더한 픽셀에만 있다.
##
##   Rscript R/_render_page.R          # 라이트 + 다크, 전체 페이지 + 조각
##
## 결과는 logs/render/ 에 남는다(산출물이 아니므로 output/ 이 아니다).

if (!exists("PROJ_ROOT")) source(file.path(if (basename(getwd()) == "R") "." else "R", "00_config.R"))

RENDER_DIR <- file.path(logs_dir, "render")
if (!dir.exists(RENDER_DIR)) dir.create(RENDER_DIR, recursive = TRUE)

page <- latest_file(output_dir, "^보건자원감시_.*\\.html$")
cat("--- 대상:", basename(page), "\n")

## ⚠ headless Chrome 은 prefers-color-scheme: dark 를 «기본값»으로 쓴다. 아무것도
## 안 하고 찍으면 다크가 나오는데 그걸 라이트로 착각한다(2026-08-31 실제로 그랬다).
## 두 테마 모두 data-theme 로 «명시적으로» 강제해야 판정이 된다.
themed_copy <- function(src, theme) {
  txt <- paste(readLines(src, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- paste0(txt, sprintf(
    "\n<script>document.documentElement.setAttribute('data-theme','%s');</script>\n", theme))
  f <- file.path(RENDER_DIR, paste0("page_", theme, ".html"))
  con <- file(f, open = "wb"); writeBin(charToRaw(enc2utf8(txt)), con); close(con)
  f
}

shoot <- function(src, out, w = 1400) {
  webshot2::webshot(paste0("file:///", normalizePath(src, winslash = "/")),
                    file = out, vwidth = w, vheight = 1000, delay = 2.5,
                    zoom = 1, cliprect = NULL)
  d <- dim(png::readPNG(out))
  cat(sprintf("--- %s : %d x %d px\n", basename(out), d[2], d[1]))
  out
}

## 한 장이 너무 길어 그대로는 못 본다. 세로로 잘라 각각을 열어 볼 수 있게 한다.
slice <- function(img, prefix, n = 6) {
  im <- magick::image_read(img)
  inf <- magick::image_info(im)
  h <- as.integer(inf$height); w <- as.integer(inf$width)
  step <- ceiling(h / n)
  for (i in seq_len(n)) {
    y <- (i - 1) * step
    hh <- min(step, h - y)
    if (hh <= 0) next
    magick::image_write(
      magick::image_crop(im, magick::geometry_area(w, hh, 0, y)),
      file.path(RENDER_DIR, sprintf("%s_%02d.png", prefix, i)))
  }
  cat(sprintf("--- %s: %d조각 (각 %dpx)\n", prefix, n, step))
}

light <- shoot(themed_copy(page, "light"), file.path(RENDER_DIR, "page_light.png"))
slice(light, "light", n = 7)

dark <- shoot(themed_copy(page, "dark"), file.path(RENDER_DIR, "page_dark.png"))
slice(dark, "dark", n = 4)

cat("--- 완료:", RENDER_DIR, "\n")
