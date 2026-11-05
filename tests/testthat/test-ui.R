# A scripted player: picks the first choice matching `pattern`, else 1.
scripted <- function(pattern) {
  function(title, choices) {
    i <- grep(pattern, choices)
    if (length(i)) i[1] else 1L
  }
}

test_that("play() refuses to start non-interactively without `ask`", {
  skip_if(interactive())
  expect_error(play(), "interactive")
})

test_that("a full career of file-drawering plays to the end", {
  out <- capture.output(career <- play(seed = 1, plot = FALSE,
                                       ask = scripted("file drawer")))
  expect_equal(nrow(career$history), 9)
  expect_equal(career$reason, "denied")
  expect_true(any(grepl("TENURE DENIED", out)))
  expect_length(career$perks, 2)
})

test_that("a submit-when-possible hacker produces a coherent history", {
  # Hack (first category), submit when allowed; file drawer when broke.
  ask <- function(title, choices) {
    for (pat in c("^SUBMIT", "^Exclude|^Add or drop|^Switch|^Analyse",
                  "file drawer")) {
      i <- grep(pat, choices)
      if (length(i)) return(i[1])
    }
    1L
  }
  out <- capture.output(career <- play(seed = 3, plot = FALSE, ask = ask))
  h <- career$history
  expect_true(career$over)
  expect_true(career$reason %in% c("tenure", "denied", "fired"))
  expect_true(all(h$outcome %in% c("published", "caught by Reviewer 2",
                                   "file drawer")))
  expect_equal(career$points, sum(h$points))
  expect_true(any(grepl("The truth", out)) || all(h$outcome == "file drawer"))
})

test_that("quitting returns immediately", {
  out <- capture.output(career <- play(seed = 1, plot = FALSE,
                                       ask = function(title, choices) 0L))
  expect_equal(career$reason, "quit")
  expect_equal(nrow(career$history), 0)
})

test_that("Back in a sub-menu changes nothing", {
  calls <- 0L
  ask <- function(title, choices) {
    calls <<- calls + 1L
    if (calls == 1L) return(grep("^Exclude", choices))
    if (calls == 2L) return(length(choices))  # "Back"
    0L
  }
  out <- capture.output(career <- play(seed = 1, plot = FALSE, ask = ask))
  expect_equal(calls, 3L)
})

test_that("p_bar marks alpha and fills with smaller p", {
  b1 <- p_bar(0.5, 0.05)
  b2 <- p_bar(0.001, 0.05)
  expect_equal(nchar(b1), 32)
  expect_match(b1, "|", fixed = TRUE)
  expect_gt(lengths(regmatches(b2, gregexpr("=", b2))),
            lengths(regmatches(b1, gregexpr("=", b1))))
})

test_that("format_p is APA-ish", {
  expect_equal(format_p(0.0432), ".043")
  expect_equal(format_p(0.0001), "< .001")
})

test_that("plot_study draws without error", {
  set.seed(1)
  s <- new_study(1L)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot_study(s))
  s$spec$subgroup <- "women"
  s$spec$outlier_sd <- 2
  expect_silent(plot_study(s))
})

test_that("glyphs fall back to ASCII outside UTF-8 locales", {
  withr_opt <- options(forkingpaths.utf8 = FALSE)
  on.exit(options(withr_opt))
  expect_equal(run_hearts(2, 3), "<3 <3 --")
  expect_equal(meter(2, 4, glyph("dot"), glyph("empty_dot")), "**..")
  options(forkingpaths.utf8 = TRUE)
  expect_equal(run_hearts(1, 2), "\u2665\u2661")
})
