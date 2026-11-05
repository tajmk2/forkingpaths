# A tiny pixel-art engine in base R -----------------------------------------
#
# A canvas is an h x w x 3 numeric array (0-255), row 1 at the top. Sprites
# are character matrices; each character is a palette key and "." is
# transparent. Nothing here touches the global RNG, so drawing a scene never
# changes the game.

PALETTE <- c(
  K = "#07070d",  # near black
  N = "#10142a",  # night navy
  n = "#1b2340",  # navy
  B = "#27345a",  # wall blue
  b = "#34467a",  # light wall blue
  P = "#2b2138",  # purple shadow
  p = "#4a3a5c",  # purple sweater
  q = "#66507a",  # sweater highlight
  G = "#0d2a1e",  # CRT dark green
  g = "#2f6b47",  # CRT mid green
  L = "#8ff0a4",  # CRT phosphor
  A = "#c07a26",  # amber
  a = "#f2c46d",  # light amber
  Y = "#fff3c4",  # glint
  W = "#d6d2c4",  # lab coat
  w = "#8f8b80",  # lab coat shadow
  S = "#d29b76",  # skin
  s = "#9a6550",  # skin shadow
  H = "#3a2519",  # dark hair
  h = "#6e6a66",  # grey hair
  E = "#151515",  # glasses frame
  D = "#3b2a1f",  # dark wood
  d = "#5e4330",  # wood
  e = "#7a5a40",  # wood highlight
  C = "#5d7196",  # rain
  c = "#c9c3ad",  # paper
  R = "#9c2f3a",  # red
  r = "#5e1c26",  # dark red
  M = "#9a968c",  # monitor casing
  m = "#5f5c56"   # monitor shadow
)

palette_rgb <- function() {
  m <- grDevices::col2rgb(PALETTE)
  colnames(m) <- names(PALETTE)
  m
}

#' Build a sprite from strings
#'
#' Rows are padded on the right with transparency to the widest row.
#' @keywords internal
#' @noRd
sprite <- function(...) {
  rows <- c(...)
  w <- max(nchar(rows))
  rows <- formatC(rows, width = -w, flag = " ")
  rows <- gsub(" ", ".", rows, fixed = TRUE)
  m <- do.call(rbind, strsplit(rows, "", fixed = TRUE))
  bad <- setdiff(unique(as.vector(m)), c(".", names(PALETTE)))
  if (length(bad)) {
    stop("Unknown palette keys in sprite: ", paste(bad, collapse = " "),
         call. = FALSE)
  }
  m
}

new_canvas <- function(w, h, key = "K") {
  rgb <- palette_rgb()[, key]
  cv <- array(0, dim = c(h, w, 3))
  for (k in 1:3) cv[, , k] <- rgb[k]
  cv
}

clip_range <- function(from, to, max) {
  from <- max(1, round(from))
  to <- min(max, round(to))
  if (from > to) integer() else from:to
}

fill_rect <- function(cv, x0, y0, x1, y1, key) {
  ys <- clip_range(y0, y1, dim(cv)[1])
  xs <- clip_range(x0, x1, dim(cv)[2])
  if (!length(ys) || !length(xs)) return(cv)
  rgb <- palette_rgb()[, key]
  for (k in 1:3) cv[ys, xs, k] <- rgb[k]
  cv
}

#' Paint a sprite with its top-left pixel at (x, y)
#'
#' `recolor` maps every opaque pixel to one key (for silhouettes), except
#' keys listed in `keep`.
#' @keywords internal
#' @noRd
put_sprite <- function(cv, spr, x, y, recolor = NULL, keep = character(),
                       rows = NULL) {
  if (!is.null(rows)) spr <- spr[rows, , drop = FALSE]
  pal <- palette_rgb()
  h <- dim(cv)[1]
  w <- dim(cv)[2]
  for (key in setdiff(unique(as.vector(spr)), ".")) {
    idx <- which(spr == key, arr.ind = TRUE)
    yy <- y + idx[, 1] - 1
    xx <- x + idx[, 2] - 1
    ok <- yy >= 1 & yy <= h & xx >= 1 & xx <= w
    if (!any(ok)) next
    use <- if (!is.null(recolor) && !key %in% keep) recolor else key
    for (k in 1:3) cv[cbind(yy[ok], xx[ok], k)] <- pal[k, use]
  }
  cv
}

#' Blend the canvas toward a colour with per-pixel weights
#' @keywords internal
#' @noRd
blend <- function(cv, weight, key) {
  weight[] <- pmin(pmax(weight, 0), 1)
  rgb <- palette_rgb()[, key]
  for (k in 1:3) cv[, , k] <- cv[, , k] * (1 - weight) + rgb[k] * weight
  cv
}

# 4x4 ordered-dither thresholds, for that VGA gradient look.
BAYER <- matrix(c(0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5),
                4, byrow = TRUE) / 16 + 1 / 32

#' Dithered vertical gradient between two palette keys
#' @keywords internal
#' @noRd
dither_rect <- function(cv, x0, y0, x1, y1, top, bottom) {
  ys <- clip_range(y0, y1, dim(cv)[1])
  xs <- clip_range(x0, x1, dim(cv)[2])
  if (!length(ys) || !length(xs)) return(cv)
  t <- (ys - min(ys)) / max(1, length(ys) - 1)
  thresh <- BAYER[cbind(rep((ys - 1) %% 4 + 1, length(xs)),
                        rep((xs - 1) %% 4 + 1, each = length(ys)))]
  use_bottom <- matrix(rep(t, length(xs)) > thresh, length(ys))
  pal <- palette_rgb()
  for (k in 1:3) {
    cv[ys, xs, k] <- ifelse(use_bottom, pal[k, bottom], pal[k, top])
  }
  cv
}

# Pixel font, 3x5 (the "." is one pixel wide).
FONT <- list(
  "0" = c("###", "#.#", "#.#", "#.#", "###"),
  "1" = c(".#.", "##.", ".#.", ".#.", "###"),
  "2" = c("###", "..#", "###", "#..", "###"),
  "3" = c("###", "..#", ".##", "..#", "###"),
  "4" = c("#.#", "#.#", "###", "..#", "..#"),
  "5" = c("###", "#..", "###", "..#", "###"),
  "6" = c("###", "#..", "###", "#.#", "###"),
  "7" = c("###", "..#", "..#", ".#.", ".#."),
  "8" = c("###", "#.#", "###", "#.#", "###"),
  "9" = c("###", "#.#", "###", "..#", "###"),
  "." = c(".", ".", ".", ".", "#"),
  "p" = c("...", "##.", "#.#", "##.", "#.."),
  "=" = c("...", "###", "...", "###", "..."),
  "<" = c("..#", ".#.", "#..", ".#.", "..#"),
  ">" = c("#..", ".#.", "..#", ".#.", "#.."),
  "?" = c("###", "..#", ".##", "...", ".#."),
  " " = c("..", "..", "..", "..", "..")
)

text_width <- function(txt) {
  chars <- strsplit(txt, "")[[1]]
  sum(vapply(chars, function(ch) nchar(FONT[[ch]][1]), 1)) +
    length(chars) - 1
}

#' Write pixel text with its top-left corner at (x, y)
#' @keywords internal
#' @noRd
put_text <- function(cv, txt, x, y, key) {
  for (ch in strsplit(txt, "")[[1]]) {
    glyph <- FONT[[ch]]
    if (is.null(glyph)) stop("No glyph for '", ch, "'", call. = FALSE)
    m <- gsub("#", key, glyph, fixed = TRUE)
    cv <- put_sprite(cv, sprite(m), x, y)
    x <- x + nchar(glyph[1]) + 1
  }
  cv
}

canvas_raster <- function(cv) {
  cv <- pmin(pmax(cv, 0), 255)
  cols <- grDevices::rgb(cv[, , 1], cv[, , 2], cv[, , 3],
                         maxColorValue = 255)
  grDevices::as.raster(matrix(cols, nrow = dim(cv)[1]))
}

#' Draw a canvas filling the current plot region with square pixels
#' @keywords internal
#' @noRd
draw_canvas <- function(cv) {
  h <- dim(cv)[1]
  w <- dim(cv)[2]
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h), xaxs = "i",
                        yaxs = "i", asp = 1)
  graphics::rasterImage(canvas_raster(cv), 0, 0, w, h, interpolate = FALSE)
  invisible(NULL)
}
