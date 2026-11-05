pixel <- function(cv, x, y) cv[y, x, ]
rgb_of <- function(key) as.vector(grDevices::col2rgb(PALETTE[[key]]))

test_that("sprite pads ragged rows with transparency", {
  s <- sprite("KK", "K", "KKK")
  expect_equal(dim(s), c(3, 3))
  expect_equal(s[2, ], c("K", ".", "."))
})

test_that("sprite rejects unknown palette keys", {
  expect_error(sprite("KZ"), "Unknown palette keys in sprite: Z")
})

test_that("all built-in sprites use only palette keys", {
  for (s in list(REVIEWER, RESEARCHER, LAMP)) {
    expect_true(all(s %in% c(".", names(PALETTE))))
  }
})

test_that("put_sprite paints opaque pixels and skips transparent ones", {
  cv <- new_canvas(5, 5, "K")
  cv <- put_sprite(cv, sprite("W.", ".R"), 2, 3)
  expect_equal(pixel(cv, 2, 3), rgb_of("W"))
  expect_equal(pixel(cv, 3, 3), rgb_of("K"))
  expect_equal(pixel(cv, 3, 4), rgb_of("R"))
})

test_that("put_sprite clips at the edges instead of erroring", {
  cv <- new_canvas(4, 4, "K")
  cv <- put_sprite(cv, sprite("WWW", "WWW"), 3, 4)
  expect_equal(pixel(cv, 4, 4), rgb_of("W"))
  expect_silent(put_sprite(cv, sprite("W"), 50, 50))
})

test_that("silhouettes recolor everything except kept keys", {
  cv <- new_canvas(3, 1, "B")
  cv <- put_sprite(cv, sprite("WYS"), 1, 1, recolor = "K", keep = "Y")
  expect_equal(pixel(cv, 1, 1), rgb_of("K"))
  expect_equal(pixel(cv, 2, 1), rgb_of("Y"))
  expect_equal(pixel(cv, 3, 1), rgb_of("K"))
})

test_that("fill_rect and dither_rect stay in bounds and in palette", {
  cv <- fill_rect(new_canvas(6, 6), -3, -3, 2, 2, "W")
  expect_equal(pixel(cv, 1, 1), rgb_of("W"))
  expect_equal(pixel(cv, 3, 3), rgb_of("K"))

  cv <- dither_rect(new_canvas(8, 8), 1, 1, 8, 8, "n", "B")
  cols <- unique(apply(cv, c(1, 2), function(v) paste(v, collapse = ",")))
  expect_setequal(as.vector(cols),
                  c(paste(rgb_of("n"), collapse = ","),
                    paste(rgb_of("B"), collapse = ",")))
})

test_that("blend moves pixels toward the target by the weight", {
  cv <- new_canvas(2, 1, "K")
  cv <- blend(cv, matrix(c(0, 1), 1), "W")
  expect_equal(pixel(cv, 1, 1), rgb_of("K"))
  expect_equal(pixel(cv, 2, 1), rgb_of("W"))
})

test_that("pixel text is the advertised width", {
  expect_equal(text_width("p=.043"), 3 + 3 + 1 + 3 + 3 + 3 + 5)
  cv <- put_text(new_canvas(30, 6), "p<.05", 1, 1, "R")
  lit <- which(apply(cv[, , 1] == rgb_of("R")[1], 2, any))
  expect_equal(max(lit), text_width("p<.05"))
  expect_error(put_text(cv, "Z", 1, 1, "R"), "No glyph")
})

test_that("crt_text formats p for the monitor", {
  expect_equal(crt_text(0.0432), "p=.043")
  expect_equal(crt_text(0.00001), "p<.001")
  expect_equal(crt_text(1), "p=1.000")
  expect_silent(text_width(crt_text(0.5)))
})

test_that("reviewer stages follow suspicion", {
  expect_equal(reviewer_stage(0), 0L)
  expect_equal(reviewer_stage(1), 1L)
  expect_equal(reviewer_stage(2), 1L)
  expect_equal(reviewer_stage(3), 2L)
  expect_equal(reviewer_stage(7), 2L)
  expect_equal(reviewer_stage(8), 3L)
})

test_that("the lab scene is deterministic and leaves the RNG alone", {
  set.seed(1)
  before <- .Random.seed
  a <- lab_scene(suspicion = 4, p = 0.08, filed = 2)
  b <- lab_scene(suspicion = 4, p = 0.08, filed = 2)
  expect_identical(.Random.seed, before)
  expect_identical(a, b)
  expect_equal(dim(a), c(SCENE_H, SCENE_W, 3))
  expect_true(all(a >= 0 & a <= 255))
})

region_differs <- function(a, b, xs, ys) {
  any(abs(a[ys, xs, ] - b[ys, xs, ]) > 1)
}

test_that("the scene reacts to the game state", {
  base <- lab_scene(0, p = 0.5)
  # Reviewer at the door window, then in the room.
  expect_true(region_differs(base, lab_scene(5, p = 0.5), 159:175, 25:41))
  expect_true(region_differs(base, lab_scene(9, p = 0.5), 146:165, 40:78))
  expect_false(region_differs(base, lab_scene(0, p = 0.5), 1:SCENE_W,
                              1:SCENE_H))
  # The file-drawer pile grows on the desk.
  expect_true(region_differs(base, lab_scene(0, p = 0.5, filed = 5),
                             127:142, 58:69))
  # The CRT turns amber when significant.
  sig <- lab_scene(0, p = 0.01, alpha = 0.05)
  nonsig <- lab_scene(0, p = 0.5, alpha = 0.05)
  screen <- function(cv) cv[52:56, 97:121, ]
  red_minus_green <- function(cv) mean(screen(cv)[, , 1] - screen(cv)[, , 2])
  expect_gt(red_minus_green(sig), red_minus_green(nonsig))
})

test_that("captions match the stage", {
  expect_match(scene_caption(0, FALSE), "quiet")
  expect_match(scene_caption(3, FALSE), "Reviewer 2")
  expect_match(scene_caption(1, TRUE), "monitor glows")
})

test_that("plot_study draws the scene and restores graphics state", {
  set.seed(2)
  run <- start_next_study(new_run())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  before <- graphics::par("mfrow", "bg", "mar")
  expect_silent(plot_study(run$study, run, scene = TRUE))
  expect_equal(graphics::par("mfrow", "bg", "mar"), before)
  expect_silent(plot_study(run$study, scene = FALSE))
  run$study$suspicion <- 9
  expect_silent(plot_study(run$study, run))
})

test_that("draw_canvas renders a canvas", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(draw_canvas(new_canvas(4, 3, "W")))
  r <- canvas_raster(new_canvas(4, 3, "W"))
  expect_equal(dim(r), c(3, 4))
})
