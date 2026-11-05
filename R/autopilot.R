# Autopilot: let a strategy function play thousands of careers -------------

#' Build the state a strategy sees
#'
#' @return A `forkingpaths_state`: a list with the current `p`, `estimate`,
#'   `n`, `alpha`, `can_submit`, `dof`, `suspicion`, `credibility`,
#'   `max_credibility`, `points`, `study`, `year`, `journal`, `perks`,
#'   `spec`, `trail` and `moves` (a data frame of affordable moves with
#'   `id`, `type`, `value`, `label`, `cost` and `suspicion`).
#' @keywords internal
#' @noRd
make_state <- function(run) {
  s <- run$study
  res <- analyse(s$data, s$spec)
  moves <- affordable_moves(s, run$perks)
  types <- vapply(moves, `[[`, "", "type")
  move_df <- data.frame(
    id = seq_along(moves),
    type = types,
    value = vapply(moves, function(m) {
      if (is.null(m$value)) NA_character_ else as.character(m$value)
    }, ""),
    label = vapply(moves, `[[`, "", "label"),
    cost = vapply(types, move_cost, 1, perks = run$perks, USE.NAMES = FALSE),
    suspicion = vapply(types, move_suspicion, 1, USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      p = res$p, estimate = res$estimate, n = res$n,
      alpha = s$journal$alpha, can_submit = res$ok && res$p < s$journal$alpha,
      dof = s$dof, suspicion = s$suspicion,
      credibility = run$credibility, max_credibility = run$max_credibility,
      points = run$points, study = s$index,
      year = as.integer(ceiling(s$index / STUDIES_PER_YEAR)),
      journal = s$journal$name, perks = run$perks,
      spec = s$spec, trail = s$trail, moves = move_df
    ),
    class = "forkingpaths_state",
    study = s, move_list = moves, perk_ids = run$perks
  )
}

#' @export
print.forkingpaths_state <- function(x, ...) {
  cat(sprintf("<forkingpaths state> study %d (year %d), %s\n",
              x$study, x$year, x$journal))
  cat(sprintf("  p = %s (alpha %s)%s  estimate %+.2f  n = %d\n",
              format_p(x$p), format(x$alpha),
              if (x$can_submit) " SIGNIFICANT" else "", x$estimate, x$n))
  cat(sprintf("  DoF %d  suspicion %d  credibility %d/%d  points %d\n",
              as.integer(x$dof), as.integer(x$suspicion),
              as.integer(x$credibility), as.integer(x$max_credibility),
              as.integer(x$points)))
  cat(sprintf("  %d moves available; see $moves or peek()\n", nrow(x$moves)))
  invisible(x)
}

#' Peek at where each move would take you
#'
#' Returns `state$moves` with a `p_after` column: the p-value you would get
#' from each move, without spending anything. Recruiting more participants
#' gives `NA`, because those participants don't exist yet.
#'
#' @param state The state passed to your strategy.
#' @return A data frame.
#' @export
#' @examples
#' my_strategy <- function(state) {
#'   if (state$can_submit) return("submit")
#'   options <- peek(state)
#'   options <- options[!is.na(options$p_after) & options$type != "reset", ]
#'   if (nrow(options) == 0) return("file_drawer")
#'   options$id[which.min(options$p_after)]
#' }
peek <- function(state) {
  stopifnot(inherits(state, "forkingpaths_state"))
  study <- attr(state, "study")
  out <- state$moves
  out$p_after <- vapply(attr(state, "move_list"), function(m) {
    if (m$type == "more_data") return(NA_real_)
    analyse(study$data, spec_after(study$spec, m))$p
  }, numeric(1))
  out
}

#' Find a move by type (and optionally value)
#'
#' @param state The state passed to your strategy.
#' @param type A move type: `"outliers"`, `"covariate"`, `"outcome"`,
#'   `"subgroup"`, `"log"`, `"more_data"`, `"one_tailed"` or `"reset"`.
#' @param value Optional value, compared as text, e.g. `"2.5"`, `"age"`,
#'   `"y2"`, `"women"`.
#' @return The move's `id`, or `NA` if it isn't available or affordable.
#' @export
pick_move <- function(state, type, value = NULL) {
  m <- state$moves
  hit <- m$type == type
  if (!is.null(value)) hit <- hit & m$value %in% as.character(value)
  if (any(hit)) m$id[which(hit)[1]] else NA_integer_
}

resolve_action <- function(action, state) {
  moves <- attr(state, "move_list")
  if (is.numeric(action) && length(action) == 1 && !is.na(action)) {
    if (action < 1 || action > length(moves) || action != round(action)) {
      stop(sprintf("Strategy chose move %s, but only moves 1-%d exist.",
                   format(action), length(moves)), call. = FALSE)
    }
    return(list(kind = "move", move = moves[[action]]))
  }
  if (is.character(action) && length(action) == 1) {
    if (action == "submit") {
      if (!state$can_submit) {
        stop(sprintf(paste0("Strategy tried to submit with p = %s, but ",
                            "alpha is %s. Check state$can_submit first."),
                     format_p(state$p), format(state$alpha)), call. = FALSE)
      }
      return(list(kind = "submit"))
    }
    if (action == "file_drawer") return(list(kind = "file_drawer"))
    id <- pick_move(state, action)
    if (!is.na(id)) return(list(kind = "move", move = moves[[id]]))
    if (action %in% MOVE_TYPES$type) {
      stop(sprintf("Strategy chose '%s', which isn't available or affordable.",
                   action), call. = FALSE)
    }
  }
  stop(paste0("A strategy must return \"submit\", \"file_drawer\", a move ",
              "id from state$moves, or a move type."), call. = FALSE)
}

autopilot_career <- function(strategy, perk_strategy, max_steps) {
  run <- new_run()
  run$stalled <- 0L
  while (!run$over) {
    run <- start_next_study(run)
    steps <- 0L
    repeat {
      state <- make_state(run)
      act <- resolve_action(strategy(state), state)
      if (act$kind == "submit") {
        run <- submit_study(run)
        break
      }
      if (act$kind == "file_drawer") {
        run <- file_drawer(run)
        break
      }
      run$study <- apply_move(run$study, act$move, run$perks)
      steps <- steps + 1L
      if (steps >= max_steps) {
        run$stalled <- run$stalled + 1L
        run <- file_drawer(run)
        break
      }
    }
    if (!is.null(run$perk_offer)) {
      perk <- perk_strategy(run$perk_offer, run$perks)
      run <- choose_perk(run, perk)
    }
  }
  run
}

#' Let a strategy play many careers
#'
#' Your strategy is a function that takes a `state` and returns one of:
#' `"submit"` (only when `state$can_submit`), `"file_drawer"`, a move `id`
#' from `state$moves`, or a move type such as `"more_data"` (the first
#' available move of that type). It is called again after every move.
#' Use [peek()] to see where each move would take p, and [pick_move()] to
#' find a specific move.
#'
#' @param strategy A strategy function, e.g. [strat_greedy].
#' @param n_careers Number of careers to simulate.
#' @param seed Optional random seed.
#' @param perk_strategy `function(offer, perks)` returning one perk id from
#'   `offer`. The default takes the first, which is random. See
#'   [perk_list()] for the ids.
#' @param max_steps Safety valve: moves allowed per study before the study
#'   is file-drawered automatically.
#' @param progress Show a progress bar?
#' @return A `forkingpaths_autopilot` object with `careers` (one row per
#'   career) and `studies` (one row per study), plus print and plot methods.
#' @export
#' @examples
#' res <- autopilot(strat_honest, n_careers = 20, seed = 1)
#' res
#' head(res$studies)
autopilot <- function(strategy, n_careers = 200, seed = NULL,
                      perk_strategy = NULL, max_steps = 50,
                      progress = interactive()) {
  name <- paste(deparse(substitute(strategy)), collapse = "")
  strategy <- match.fun(strategy)
  if (is.null(perk_strategy)) perk_strategy <- function(offer, perks) offer[1]
  if (!is.null(seed)) set.seed(seed)

  pb <- if (progress) utils::txtProgressBar(max = n_careers, style = 3)
  runs <- vector("list", n_careers)
  for (i in seq_len(n_careers)) {
    runs[[i]] <- autopilot_career(strategy, perk_strategy, max_steps)
    if (progress) utils::setTxtProgressBar(pb, i)
  }
  if (progress) close(pb)
  summarise_runs(runs, name)
}

summarise_runs <- function(runs, name) {
  studies <- do.call(rbind, lapply(seq_along(runs), function(i) {
    h <- runs[[i]]$history
    if (nrow(h)) cbind(career = i, h) else NULL
  }))
  careers <- data.frame(
    career = seq_along(runs),
    reason = vapply(runs, `[[`, "", "reason"),
    points = vapply(runs, `[[`, 1, "points"),
    studies = vapply(runs, function(r) nrow(r$history), 1L),
    published = vapply(runs, function(r) {
      sum(r$history$outcome == "published")
    }, 1L),
    false_positives = vapply(runs, function(r) {
      sum(r$history$outcome == "published" & r$history$true_effect == 0)
    }, 1L),
    caught = vapply(runs, function(r) {
      sum(r$history$outcome == "caught by Reviewer 2")
    }, 1L),
    perks = vapply(runs, function(r) paste(r$perks, collapse = ","), ""),
    stalled = vapply(runs, function(r) as.integer(r$stalled), 1L),
    stringsAsFactors = FALSE
  )
  structure(list(strategy = name, careers = careers, studies = studies),
            class = "forkingpaths_autopilot")
}

autopilot_stats <- function(x) {
  cr <- x$careers
  s <- x$studies
  pub <- s[s$outcome == "published", , drop = FALSE]
  tried <- pub[pub$replication %in% c("replicated", "failed"), , drop = FALSE]
  data.frame(
    strategy = x$strategy,
    careers = nrow(cr),
    tenure = mean(cr$reason == "tenure"),
    denied = mean(cr$reason == "denied"),
    fired = mean(cr$reason == "fired"),
    mean_points = mean(cr$points),
    papers = mean(cr$published),
    fdr = if (nrow(pub)) mean(pub$true_effect == 0) else NA_real_,
    replication_rate = if (nrow(tried)) {
      mean(tried$replication == "replicated")
    } else {
      NA_real_
    },
    caught = mean(cr$caught),
    stringsAsFactors = FALSE
  )
}

pct <- function(x) ifelse(is.na(x), "  -", sprintf("%3.0f%%", 100 * x))

#' @export
print.forkingpaths_autopilot <- function(x, ...) {
  st <- autopilot_stats(x)
  cat(sprintf("Strategy: %s  (%d careers)\n", st$strategy, st$careers))
  cat(sprintf("Tenure %s   Denied %s   Fired %s\n",
              pct(st$tenure), pct(st$denied), pct(st$fired)))
  cat(sprintf("Mean points %.0f   Papers per career %.1f   Caught %.2f\n",
              st$mean_points, st$papers, st$caught))
  cat(sprintf("False-positive share of published papers: %s\n", pct(st$fdr)))
  cat(sprintf("Replication success rate: %s\n", pct(st$replication_rate)))
  if (sum(x$careers$stalled)) {
    cat(sprintf("(%d studies hit max_steps and were file-drawered.)\n",
                sum(x$careers$stalled)))
  }
  invisible(x)
}

#' @export
summary.forkingpaths_autopilot <- function(object, ...) {
  autopilot_stats(object)
}

#' @export
plot.forkingpaths_autopilot <- function(x, ...) {
  op <- graphics::par(mfrow = c(1, 3), mar = c(4.5, 4, 3, 1))
  on.exit(graphics::par(op))
  cr <- x$careers
  s <- x$studies

  reasons <- table(factor(cr$reason, levels = c("tenure", "denied", "fired")))
  graphics::barplot(reasons / sum(reasons), ylim = c(0, 1),
    col = c("#2a9d8f", "#9aa5b1", "#d1495b"), ylab = "Share of careers",
    main = "How careers end")

  graphics::hist(cr$points, breaks = 20, col = "#9aa5b1", border = "white",
    main = "Career points", xlab = "Points")
  graphics::abline(v = TENURE_BAR, lty = 2, lwd = 2, col = "#d1495b")
  graphics::mtext("tenure bar", side = 3, at = TENURE_BAR, cex = 0.7,
                  col = "#d1495b")

  pub <- s[s$outcome == "published", , drop = FALSE]
  if (nrow(pub)) {
    tier <- factor(pub$journal, levels = JOURNALS$name,
                   labels = sprintf("alpha %s", format(JOURNALS$alpha)))
    truth <- factor(ifelse(pub$true_effect == 0, "false positive", "real"),
                    levels = c("real", "false positive"))
    graphics::barplot(table(truth, tier), col = c("#2a9d8f", "#d1495b"),
      main = "What got published", ylab = "Papers",
      legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
  } else {
    graphics::plot.new()
    graphics::title("Nothing got published")
  }
  invisible(x)
}

#' Compare strategies head to head
#'
#' Runs [autopilot()] for each strategy with the same settings.
#'
#' @param ... Named strategy functions.
#' @inheritParams autopilot
#' @return A `forkingpaths_comparison` data frame, one row per strategy,
#'   with the full autopilot results in `attr(, "results")`.
#' @export
#' @examples
#' cmp <- compare_strategies(honest = strat_honest,
#'                           stopping = strat_optional_stopping,
#'                           n_careers = 20, seed = 1)
#' cmp
compare_strategies <- function(..., n_careers = 200, seed = 1,
                               perk_strategy = NULL, max_steps = 50,
                               progress = interactive()) {
  strategies <- list(...)
  nms <- names(strategies)
  if (is.null(nms)) nms <- rep("", length(strategies))
  fallback <- vapply(as.list(substitute(list(...)))[-1], function(e) {
    paste(deparse(e), collapse = "")
  }, "")
  nms[nms == ""] <- fallback[nms == ""]

  results <- lapply(seq_along(strategies), function(i) {
    if (progress) message("Running ", nms[i])
    res <- autopilot(strategies[[i]], n_careers = n_careers, seed = seed,
                     perk_strategy = perk_strategy, max_steps = max_steps,
                     progress = progress)
    res$strategy <- nms[i]
    res
  })
  names(results) <- nms
  out <- do.call(rbind, lapply(results, autopilot_stats))
  rownames(out) <- NULL
  structure(out, class = c("forkingpaths_comparison", "data.frame"),
            results = results)
}

#' @export
print.forkingpaths_comparison <- function(x, ...) {
  shown <- data.frame(
    strategy = x$strategy,
    tenure = pct(x$tenure), fired = pct(x$fired),
    points = sprintf("%.0f", x$mean_points),
    papers = sprintf("%.1f", x$papers),
    FDR = pct(x$fdr), replicates = pct(x$replication_rate),
    stringsAsFactors = FALSE
  )
  cat(sprintf("%d careers per strategy\n", x$careers[1]))
  print(shown, row.names = FALSE, right = FALSE)
  invisible(x)
}

#' @export
plot.forkingpaths_comparison <- function(x, ...) {
  fdr <- ifelse(is.na(x$fdr), 0, x$fdr)
  graphics::plot(fdr, x$tenure, xlim = c(0, 1), ylim = c(0, 1), pch = 19,
    cex = 1.6, col = "#d1495b", xlab = "False-positive share of papers",
    ylab = "Tenure rate", main = "The incentive landscape")
  # Alternate labels above/below (in order of FDR) so neighbours don't clash.
  pos <- integer(length(fdr))
  pos[order(fdr)] <- rep_len(c(3L, 1L), length(fdr))
  graphics::text(fdr, x$tenure, x$strategy, pos = pos, cex = 0.9)
  graphics::abline(h = 0.5, v = 0.5, lty = 3, col = "#9aa5b1")
  invisible(x)
}

#' The perks you can be offered
#'
#' @return A data frame of perk `id`, `name` and `description`, for writing
#'   a `perk_strategy`.
#' @export
perk_list <- function() PERKS
