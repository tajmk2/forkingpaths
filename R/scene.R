# The lab at night -----------------------------------------------------------

SCENE_W <- 192L
SCENE_H <- 108L
FLOOR_Y <- 80L

# Reviewer 2: grey hair, round glasses, lab coat, clipboard. 20 x 46.
REVIEWER <- sprite(
  "......hhhhhhh",
  ".....hhhhhhhhh",
  "....hhhhhhhhhhh",
  "....hhSSSSSSShh",
  "....hSSSSSSSSSh",
  "....EEEESSSEEEE",
  "....EYYESSSEYYE",
  "....EEEESsSEEEE",
  "....sSSSSsSSSSs",
  ".....SSSSSSSSS",
  ".....SSsKKKsSS",
  ".....sSSSSSSSs",
  "......sSSSSSs",
  "........sSs",
  "....wWWWcRcWWWw",
  "...wWWWWcRcWWWWw",
  "..wWWWWWWRWWWWWWw",
  "..WWWwWWWRWWWwWWW",
  "..WWWwWWWrWWWwWWW",
  "..WWWwWWWWWWWwWWW",
  "..WWWwWWWWWWWwWWddd",
  "..WWWwWWWWWWWwWWdcd",
  "..WWWwWWWWWWWwWWdcd",
  "..WWWwWWWWWWWwWSdcd",
  "..WWWwWWWWWWWwWSdcd",
  "..WWWwWWWWWWWwWWddd",
  "..WWWwWWWWWWWwWW",
  "..SWWwWWWWWWWwWW",
  "..SWWwWWWWWWWwWW",
  "...WWwWWWWWWWwWW",
  "...WWwWWWWWWWwWW",
  "...WWwWWWWWWWwWW",
  "...WWwwwwwwwwwWW",
  "....WWKKKKKKKWW",
  ".....KKKKKKKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  ".....KKKK.KKKK",
  "....EEEEE.EEEEE",
  "....EEEEE.EEEEE"
)

# You, from behind, slumped in front of the monitor. 24 x 30.
RESEARCHER <- sprite(
  "........HHHHHHH",
  ".......HHHHHHHHH",
  "......HHHHHHHHHHH",
  "......HHHHHHHHHHH",
  "......HHHHHHHHHHH",
  "......HHHHHHHHHHH",
  ".......HHHHHHHHH",
  "........sSSSSSs",
  ".....ppppppppppppp",
  "...pppppppppppppppppp",
  "..ppqpppppppppppppqppp",
  ".ppqqpppppppppppppqqpp",
  ".ppqppppppppppppppqqpp",
  "pppqppppppppppppppqqppp",
  "pppqppppppppppppppqqppp",
  "ppKKKKKKKKKKKKKKKKKKKpp",
  "ppKNNNNNNNNNNNNNNNNNKpp",
  "ppKNNNNNNNNNNNNNNNNNKpp",
  "..KNNNNNNNNNNNNNNNNNK",
  "..KNNNNNNNNNNNNNNNNNK",
  "..KNNNNNNNNNNNNNNNNNK",
  "..KKKKKKKKKKKKKKKKKKK",
  ".........KKK",
  ".........KKK",
  ".........KKK",
  "....KKKKKKKKKKKKK",
  "....K...........K"
)

LAMP <- sprite(
  "...rrrrrr",
  "..rRRRRRRr",
  ".rRRRRRRRRr",
  "rRRRRRRRRRRr",
  "....KK",
  ".....KK",
  "......KK",
  ".......KK",
  "......KK",
  ".....KK",
  "....KK",
  "...KKKKKK",
  "..KKKKKKKK"
)

#' Paint the lab
#'
#' @param suspicion Current suspicion; Reviewer 2 gets closer as it rises.
#' @param p,alpha Shown on the CRT, which turns amber when p < alpha.
#' @param filed Studies in the file drawer so far; the paper pile grows.
#' @param stage Override the reviewer stage (0-3); computed from suspicion
#'   when `NULL`.
#' @return A canvas.
#' @keywords internal
#' @noRd
lab_scene <- function(suspicion = 0, p = 0.5, alpha = 0.05, filed = 0,
                      stage = NULL) {
  if (is.null(stage)) stage <- reviewer_stage(suspicion)
  significant <- p < alpha
  W <- SCENE_W
  H <- SCENE_H
  cv <- new_canvas(W, H, "K")

  # Walls: dithered upper wall, darker wainscot, wooden floor.
  cv <- dither_rect(cv, 1, 1, W, 58, "n", "B")
  cv <- fill_rect(cv, 1, 59, W, 60, "N")
  cv <- dither_rect(cv, 1, 61, W, FLOOR_Y - 1, "P", "N")
  cv <- dither_rect(cv, 1, FLOOR_Y, W, H, "D", "P")
  for (x in seq(9, W, by = 23)) cv <- fill_rect(cv, x, FLOOR_Y, x, H, "K")

  # Window with rain.
  cv <- fill_rect(cv, 10, 8, 50, 50, "K")
  cv <- dither_rect(cv, 12, 10, 48, 48, "K", "N")
  cv <- rain(cv, 12, 10, 48, 48)
  cv <- fill_rect(cv, 29, 10, 30, 48, "K")
  cv <- fill_rect(cv, 12, 28, 48, 29, "K")
  cv <- fill_rect(cv, 8, 50, 52, 52, "D")
  # Distant city lights.
  for (xy in list(c(15, 44), c(20, 46), c(35, 43), c(41, 45), c(45, 42))) {
    cv <- fill_rect(cv, xy[1], xy[2], xy[1], xy[2], "A")
  }

  # Motivational poster.
  cv <- fill_rect(cv, 60, 12, 86, 34, "D")
  cv <- fill_rect(cv, 62, 14, 84, 32, "c")
  cv <- put_text(cv, "p<.05", 64, 20, "R")
  cv <- fill_rect(cv, 64, 27, 82, 27, "w")

  # Door, with a window onto the corridor.
  cv <- fill_rect(cv, 152, 16, 182, FLOOR_Y - 1, "K")
  cv <- fill_rect(cv, 154, 18, 180, FLOOR_Y - 1, "P")
  cv <- fill_rect(cv, 155, 19, 179, FLOOR_Y - 1, "p")
  cv <- fill_rect(cv, 158, 24, 176, 42, "K")
  cv <- dither_rect(cv, 159, 25, 175, 41, "b", "B")
  cv <- fill_rect(cv, 158, 50, 176, 70, "P")
  cv <- fill_rect(cv, 175, 52, 177, 54, "A")
  cv <- reviewer_in_door(cv, stage)

  # Desk.
  cv <- fill_rect(cv, 46, 70, 146, 72, "e")
  cv <- fill_rect(cv, 46, 73, 146, 74, "d")
  cv <- fill_rect(cv, 48, 75, 144, 96, "D")
  cv <- fill_rect(cv, 52, 78, 140, 78, "K")
  cv <- fill_rect(cv, 96, 75, 96, 96, "K")
  cv <- fill_rect(cv, 116, 84, 124, 85, "A")

  # File-drawer pile: one sheet layer per abandoned study.
  for (i in seq_len(min(filed, 9))) {
    y <- 69 - (i - 1) * 2
    shift <- c(0, 1, -1)[(i - 1) %% 3 + 1]
    cv <- fill_rect(cv, 128 + shift, y - 1, 141 + shift, y, "c")
    cv <- fill_rect(cv, 128 + shift, y, 141 + shift, y, "w")
  }

  # CRT monitor.
  cv <- fill_rect(cv, 92, 42, 126, 69, "m")
  cv <- fill_rect(cv, 93, 43, 125, 67, "M")
  cv <- fill_rect(cv, 96, 46, 122, 63, "K")
  cv <- fill_rect(cv, 97, 47, 121, 62, "G")
  screen_key <- if (significant) "a" else "L"
  txt <- crt_text(p)
  cv <- put_text(cv, txt, 109 - text_width(txt) %/% 2, 52, screen_key)
  for (y in seq(48, 62, by = 2)) {
    cv[y, 97:121, ] <- cv[y, 97:121, ] * 0.7
  }
  cv <- fill_rect(cv, 104, 68, 114, 69, "m")

  # Desk lamp.
  cv <- put_sprite(cv, LAMP, 50, 57)

  # You.
  cv <- put_sprite(cv, RESEARCHER, 64, 61)

  # Reviewer 2 has entered the room.
  if (stage >= 3) cv <- put_sprite(cv, REVIEWER, 146, FLOOR_Y - 45)

  # Lighting: lamp pool, CRT glow, then everything else falls into gloom.
  cv <- light(cv, cx = 56, cy = 70, radius = 34, key = "a", strength = 0.5,
              below = 61)
  cv <- light(cv, cx = 109, cy = 55, radius = 34,
              key = if (significant) "A" else "g", strength = 0.3)
  cv <- vignette(cv, 0.55)
  cv
}

reviewer_stage <- function(suspicion) {
  if (suspicion <= 0) 0L else if (suspicion <= 2) 1L else
    if (suspicion <= 7) 2L else 3L
}

crt_text <- function(p) {
  if (p < 0.001) "p<.001" else paste0("p=", sub("^0", "", sprintf("%.3f", p)))
}

# Slanted streaks from a fixed hash, so the rain is the same every frame.
rain <- function(cv, x0, y0, x1, y1) {
  for (x in x0:x1) for (y in y0:y1) {
    if (((x * 7 + y * 13) %% 37) == 0) {
      for (k in 0:3) {
        xx <- x - k %/% 2
        yy <- y + k
        if (xx >= x0 && yy <= y1) {
          cv <- fill_rect(cv, xx, yy, xx, yy, if (k == 0) "b" else "C")
        }
      }
    }
  }
  cv
}

reviewer_in_door <- function(cv, stage) {
  if (stage == 1L) {
    # A silhouette, glasses catching the light.
    cv <- put_sprite(cv, REVIEWER, 158, 26, rows = 1:16, recolor = "K",
                     keep = "Y")
  } else if (stage == 2L) {
    cv <- put_sprite(cv, REVIEWER, 158, 26, rows = 1:16)
    cv <- blend(cv, door_window_mask(cv), "N")
  }
  cv
}

door_window_mask <- function(cv) {
  m <- matrix(0, dim(cv)[1], dim(cv)[2])
  m[25:41, 159:175] <- 0.35
  m
}

light <- function(cv, cx, cy, radius, key, strength, below = NULL) {
  h <- dim(cv)[1]
  w <- dim(cv)[2]
  d <- sqrt(outer((seq_len(h) - cy)^2, (seq_len(w) - cx)^2, `+`))
  wt <- d
  wt[] <- strength * pmax(0, 1 - d / radius)^1.5
  if (!is.null(below)) wt[seq_len(min(h, below - 1)), ] <- 0
  blend(cv, wt, key)
}

vignette <- function(cv, strength) {
  h <- dim(cv)[1]
  w <- dim(cv)[2]
  yy <- (seq_len(h) - h / 2) / (h / 2)
  xx <- (seq_len(w) - w / 2) / (w / 2)
  d <- sqrt(outer(yy^2, xx^2, `+`)) / sqrt(2)
  blend(cv, strength * d^2, "K")
}

scene_caption <- function(stage, significant) {
  base <- c(
    "The lab is quiet. Rain taps on the window.",
    "Footsteps in the corridor. Then nothing.",
    "Someone is watching you through the door.",
    "Reviewer 2 has entered the room."
  )[stage + 1]
  if (significant) paste(base, "The monitor glows.") else base
}
