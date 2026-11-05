# Console front end ----------------------------------------------------------

GLYPHS <- list(
  utf8 = list(heart = "\u2665", empty_heart = "\u2661", dot = "\u25cf",
              empty_dot = "\u25cb", rule = "\u2500"),
  ascii = list(heart = "<3 ", empty_heart = "-- ", dot = "*",
               empty_dot = ".", rule = "-")
)

use_utf8 <- function() {
  getOption("forkingpaths.utf8", isTRUE(l10n_info()[["UTF-8"]]))
}

glyph <- function(name) {
  GLYPHS[[if (use_utf8()) "utf8" else "ascii"]][[name]]
}

#' Play forkingpaths
#'
#' A p-hacking roguelike. You run nine studies over three years. Each one
#' starts with a dataset and a preregistered analysis; spend researcher
#' degrees of freedom to push p below the journal's alpha, then survive
#' Reviewer 2 and the replication attempt. Lose all your credibility and
#' you're fired. Finish with enough points and you get tenure.
#'
#' Choose options by number at the prompt; `0` quits. Plots of the current
#' analysis and your p-value trail are drawn in the active graphics device.
#'
#' @param seed Optional random seed, for replaying a career.
#' @param plot Draw plots as you play? Defaults to `interactive()`.
#' @param ask Function `function(title, choices)` returning the chosen index
#'   (0 to quit). Defaults to [utils::menu()]; replace it to script a game.
#' @return The final career state, invisibly. `$history` has one row per
#'   study, including the true effect, so you can analyse your career.
#' @export
#' @examples
#' if (interactive()) {
#'   career <- play()
#'   career$history
#' }
play <- function(seed = NULL, plot = interactive(), ask = NULL) {
  if (is.null(ask)) {
    if (!interactive()) {
      stop("play() needs an interactive R session, or an `ask` function.",
           call. = FALSE)
    }
    ask <- function(title, choices) utils::menu(choices, title = title)
  }
  if (!is.null(seed)) set.seed(seed)

  run <- new_run()
  show_intro()

  while (!run$over) {
    run <- start_next_study(run)
    run <- play_study(run, ask, plot)
    if (run$over) break
    if (!is.null(run$perk_offer)) run <- offer_perks(run, ask)
  }

  show_ending(run)
  invisible(run)
}

play_study <- function(run, ask, plot) {
  repeat {
    show_study(run)
    if (plot) plot_study(run$study, run)

    moves <- affordable_moves(run$study, run$perks)
    types <- unique(vapply(moves, `[[`, "", "type"))
    cats <- MOVE_TYPES[match(types, MOVE_TYPES$type), ]
    cat_labels <- sprintf("%s  [%s]", cats$category,
      vapply(cats$type, function(t) dof_label(move_cost(t, run$perks)), ""))

    submittable <- can_submit(run)
    choices <- c(cat_labels,
      if (submittable) sprintf("SUBMIT to %s", run$study$journal$name),
      "Put it in the file drawer and move on",
      "Help")
    i <- ask("What now?", choices)

    if (i == 0L) {
      run$over <- TRUE
      run$reason <- "quit"
      run$study <- NULL
      return(run)
    }
    pick <- choices[i]
    if (i <= nrow(cats)) {
      opts <- Filter(function(m) m$type == cats$type[i], moves)
      if (length(opts) == 1L) {
        run$study <- apply_move(run$study, opts[[1]], run$perks)
      } else {
        j <- ask(cats$category[i],
                 c(vapply(opts, `[[`, "", "label"), "Back"))
        if (j >= 1L && j <= length(opts)) {
          run$study <- apply_move(run$study, opts[[j]], run$perks)
        }
      }
    } else if (startsWith(pick, "SUBMIT")) {
      s <- run$study
      run <- submit_study(run)
      show_submission(run, s)
      return(run)
    } else if (startsWith(pick, "Put it in the file drawer")) {
      run <- file_drawer(run)
      say("Into the file drawer it goes. Nobody will ever know.")
      return(run)
    } else {
      show_help()
    }
  }
}

offer_perks <- function(run, ask) {
  year <- run$index %/% STUDIES_PER_YEAR
  say(sprintf("\nEnd of year %d. The department offers you a perk:", year))
  offer <- PERKS[match(run$perk_offer, PERKS$id), ]
  i <- ask("Choose a perk", sprintf("%s: %s", offer$name, offer$description))
  if (i == 0L) i <- 1L
  run <- choose_perk(run, offer$id[i])
  say(sprintf("You got %s.", offer$name[i]))
  run
}

# Printing --------------------------------------------------------------------

say <- function(...) cat(..., "\n", sep = "")

beat <- function() {
  if (interactive()) Sys.sleep(getOption("forkingpaths.delay", 0.7))
}

dof_label <- function(cost) {
  if (cost == 0) "free" else sprintf("%d DoF", as.integer(cost))
}

meter <- function(n, total, on, off) {
  n <- max(0, n)
  paste0(strrep(on, n), strrep(off, max(0, total - n)))
}

format_p <- function(p) {
  if (p < 0.001) "< .001" else sub("^0", "", sprintf("%.3f", p))
}

# A bar from p = 1 (left) to p = .0005 (right), log scale, with "|" at alpha.
p_bar <- function(p, alpha, width = 30) {
  pos <- function(x) {
    x <- min(max(x, 5e-4), 1)
    round(width * log10(x) / log10(5e-4))
  }
  filled <- pos(p)
  bar <- c(rep("=", filled), rep("-", width - filled))
  a <- min(max(pos(alpha), 1), width)
  bar[a] <- "|"
  paste0("[", paste(bar, collapse = ""), "]")
}

show_intro <- function() {
  say(strrep(glyph("rule"), 72))
  say("  FORKING PATHS  -  a p-hacking roguelike")
  say(strrep(glyph("rule"), 72))
  say("You are a new assistant professor. You have three years, nine")
  say("studies and ", run_hearts(3, 3), " credibility. Publish or perish.")
  say("Tenure needs ", TENURE_BAR, " points. Choose by number; 0 quits.")
  beat()
}

run_hearts <- function(cred, max_cred) {
  trimws(meter(cred, max_cred, glyph("heart"), glyph("empty_heart")))
}

show_study <- function(run) {
  s <- run$study
  res <- analyse(s$data, s$spec)
  j <- s$journal
  year <- ceiling(s$index / STUDIES_PER_YEAR)
  say("")
  say(strrep(glyph("rule"), 72))
  say(sprintf(" Study %d/%d  |  Year %d  |  %s (alpha = %s)",
              s$index, run$n_studies, year, j$name, format(j$alpha)))
  say(strrep(glyph("rule"), 72))
  say(sprintf(" \"%s\"", s$title))
  say(sprintf(" Credibility %s   Points %d   DoF %s   Suspicion %d",
              run_hearts(run$credibility, run$max_credibility),
              as.integer(run$points),
              meter(s$dof, s$dof_start, glyph("dot"), glyph("empty_dot")),
              as.integer(s$suspicion)))
  if (length(run$perks)) {
    say(" Perks: ", paste(perk_name(run$perks), collapse = ", "))
  }
  say(" Analysis: ", describe_spec(s$spec, s$labels))
  verdict <- if (res$ok && res$p < j$alpha) "  SIGNIFICANT!" else ""
  say(sprintf(" n = %d   estimate = %+.2f   p = %s  %s%s",
              res$n, res$estimate, format_p(res$p),
              p_bar(res$p, j$alpha), verdict))
  say("")
}

show_submission <- function(run, study) {
  r <- run$last_report
  say("")
  say("You submit to ", study$journal$name, "...")
  beat()
  if (r$caught) {
    say("Reviewer 2: \"Was this analysis preregistered? It was not.\"")
    say("Rejected. Your reputation takes a hit. ",
        run_hearts(run$credibility, run$max_credibility))
  } else {
    say("ACCEPTED! +", r$points, " points. Press release drafted.")
    beat()
    if (is.na(r$replicated)) {
      say("Nobody tries to replicate it. Phew.")
    } else {
      say("A replication team re-runs your exact analysis on 2.5x the data...")
      beat()
      if (r$replicated) {
        say(sprintf("It REPLICATES (p = %s). Credibility restored. %s",
                    format_p(r$replication_p),
                    run_hearts(run$credibility, run$max_credibility)))
      } else {
        say(sprintf("It FAILS to replicate (p = %s). Awkward. %s",
                    format_p(r$replication_p),
                    run_hearts(run$credibility, run$max_credibility)))
      }
    }
  }
  beat()
  truth <- if (study$effect == 0) "there was no effect at all" else
    sprintf("the true effect was d = %s", format(study$effect))
  say("(The truth: ", truth, ".)")
}

show_help <- function() {
  say("")
  say("Each study starts with a preregistered analysis. Every change you")
  say("make costs degrees of freedom (DoF) and adds suspicion. When p is")
  say("below the journal's alpha you can submit:")
  say("  * Reviewer 2 may catch you. The more suspicion, the likelier.")
  say("  * Published papers may be replicated on 2.5x the data with your")
  say("    exact analysis. Replications restore 1 credibility; failures")
  say("    cost 1. Returning to the preregistered analysis is free, but the")
  say("    paths you explored still count as suspicion.")
  say("Journals get stricter every year. A perk is offered after each year.")
  say("Lose all credibility and you're fired. Need ", TENURE_BAR,
      " points for tenure.")
}

show_ending <- function(run) {
  say("")
  say(strrep(glyph("rule"), 72))
  msg <- switch(run$reason,
    tenure = "TENURE GRANTED. Your h-index is a monument to the forking paths.",
    denied = "TENURE DENIED. Not enough papers. Perhaps industry?",
    fired = "FIRED. Your credibility is gone. A methods blog wrote a post about you.",
    quit = "You quit academia. Honestly, fair."
  )
  say(" ", msg)
  say(strrep(glyph("rule"), 72))
  h <- run$history
  if (nrow(h)) {
    say(sprintf(" Final points: %d", as.integer(run$points)))
    pub <- h[h$outcome == "published", ]
    if (nrow(pub)) {
      fp <- sum(pub$true_effect == 0)
      say(sprintf(" You published %d paper%s. %d of them %s false positives.",
                  nrow(pub), if (nrow(pub) == 1) "" else "s", fp,
                  if (fp == 1) "was" else "were"))
    } else {
      say(" You published nothing.")
    }
    honest_null <- sum(h$outcome == "file drawer" & h$true_effect > 0)
    if (honest_null) {
      say(sprintf(" You file-drawered %d stud%s with a real effect.",
                  honest_null, if (honest_null == 1) "y" else "ies"))
    }
    say(" Your career is returned invisibly. Try: career$history")
  }
}

# Plotting --------------------------------------------------------------------

THEMES <- list(
  light = list(bg = "white", fg = "#333333", main = "black",
               control = "#6b7a8f", treatment = "#d1495b",
               trail = "#333333", hit = "#d1495b", alpha = "#d1495b"),
  gloom = list(bg = "#07070d", fg = "#8a93a6", main = "#d6d2c4",
               control = "#7f93ba", treatment = "#f2c46d",
               trail = "#8ff0a4", hit = "#f2c46d", alpha = "#d1495b")
)

#' Plot the current analysis and the p-value trail
#'
#' With `scene = TRUE` (the default, see `options(forkingpaths.scene)`) the
#' lab is drawn above the statistics in pixel art, in a dark theme.
#' @keywords internal
#' @noRd
plot_study <- function(study, run = NULL,
                       scene = getOption("forkingpaths.scene", TRUE)) {
  res <- analyse(study$data, study$spec, keep_data = TRUE)
  alpha <- study$journal$alpha
  th <- THEMES[[if (scene) "gloom" else "light"]]

  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1)
    graphics::par(op)
  })
  graphics::par(bg = th$bg, fg = th$fg, col.axis = th$fg, col.lab = th$fg,
                col.main = th$main)

  if (scene) {
    filed <- if (is.null(run)) 0 else
      sum(run$history$outcome == "file drawer")
    stage <- reviewer_stage(study$suspicion)
    graphics::layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE),
                     heights = c(1.3, 1))
    graphics::par(mar = c(1.6, 0.5, 0.5, 0.5))
    draw_canvas(lab_scene(study$suspicion, res$p, alpha, filed, stage))
    graphics::mtext(scene_caption(stage, res$ok && res$p < alpha), side = 1,
                    line = 0.4, family = "mono", cex = 0.85, col = "#f2c46d")
  } else {
    graphics::par(mfrow = c(1, 2))
  }
  graphics::par(mar = c(4, 4, 2.5, 1))

  y_lab <- study$labels[[study$spec$outcome]]
  if (isTRUE(study$spec$log)) y_lab <- sprintf("log(%s)", y_lab)
  kept <- res$kept
  cols <- c(control = th$control, treatment = th$treatment)
  if (nrow(kept)) {
    graphics::stripchart(.y ~ group, data = kept, vertical = TRUE,
      method = "jitter", jitter = 0.15, pch = 16,
      col = grDevices_adjust(cols, 0.6), ylab = y_lab, xlab = "",
      main = sprintf("n = %d, p = %s", res$n, format_p(res$p)))
    means <- tapply(kept$.y, kept$group, mean)
    graphics::segments(seq_along(means) - 0.25, means,
                       seq_along(means) + 0.25, means, lwd = 3,
                       col = cols[names(means)])
  } else {
    graphics::plot.new()
    graphics::title("No data left. Impressive.")
  }

  trail <- study$trail
  p <- pmax(trail$p, 1e-4)
  graphics::plot(trail$step, p, type = "b", log = "y", pch = 16,
    ylim = c(min(c(p, alpha / 4)), 1), xlab = "Forking path step",
    xaxt = "n", ylab = "p-value", main = "Your garden of forking paths",
    col = ifelse(p < alpha, th$hit, th$trail))
  graphics::axis(1, at = trail$step)
  graphics::abline(h = alpha, lty = 2, col = th$alpha)
  graphics::text(graphics::par("usr")[2], alpha,
                 sprintf("alpha = %s", format(alpha)), adj = c(1.05, -0.5),
                 cex = 0.8, col = th$alpha)
  invisible(NULL)
}

grDevices_adjust <- function(cols, alpha) {
  m <- grDevices::col2rgb(cols) / 255
  grDevices::rgb(m[1, ], m[2, ], m[3, ], alpha)
}
