fresh_state <- function(seed = 1, perks = character()) {
  set.seed(seed)
  run <- new_run()
  run$perks <- perks
  make_state(start_next_study(run))
}

test_that("make_state exposes what a strategy needs", {
  st <- fresh_state()
  expect_s3_class(st, "forkingpaths_state")
  expect_equal(st$alpha, 0.05)
  expect_equal(st$dof, BASE_DOF)
  expect_equal(st$suspicion, 0)
  expect_equal(st$year, 1L)
  expect_named(st$moves, c("id", "type", "value", "label", "cost",
                           "suspicion"))
  expect_equal(st$moves$id, seq_len(nrow(st$moves)))
  expect_false("reset" %in% st$moves$type)
  expect_output(print(st), "forkingpaths state")
})

test_that("state$moves lists only affordable moves", {
  set.seed(2)
  run <- start_next_study(new_run())
  run$study$dof <- 1
  st <- make_state(run)
  expect_true(all(st$moves$cost <= 1))
})

test_that("peek predicts each move's p without spending or using RNG", {
  st <- fresh_state(3)
  seed_before <- .Random.seed
  pk <- peek(st)
  expect_identical(.Random.seed, seed_before)
  expect_true(is.na(pk$p_after[pk$type == "more_data"]))

  i <- which(pk$type == "outcome")[1]
  s <- apply_move(attr(st, "study"), attr(st, "move_list")[[i]])
  expect_equal(pk$p_after[i], analyse(s$data, s$spec)$p)
  expect_equal(st$dof, BASE_DOF)
})

test_that("pick_move finds moves by type and value", {
  st <- fresh_state()
  expect_equal(st$moves$type[pick_move(st, "covariate", "sleep")],
               "covariate")
  expect_equal(st$moves$value[pick_move(st, "covariate", "sleep")], "sleep")
  expect_equal(st$moves$value[pick_move(st, "outliers", 2.5)], "2.5")
  expect_true(is.na(pick_move(st, "reset")))
})

test_that("resolve_action accepts ids, types and keywords", {
  st <- fresh_state()
  expect_equal(resolve_action(1, st)$kind, "move")
  expect_equal(resolve_action("more_data", st)$move$type, "more_data")
  expect_equal(resolve_action("file_drawer", st)$kind, "file_drawer")
})

test_that("resolve_action explains bad strategy output", {
  st <- fresh_state()
  expect_error(resolve_action(999, st), "only moves 1-")
  expect_error(resolve_action("reset", st), "isn't available")
  expect_error(resolve_action("yolo", st), "must return")
  expect_error(resolve_action(NULL, st), "must return")
  st$can_submit <- FALSE
  expect_error(resolve_action("submit", st), "Check state\\$can_submit")
})

test_that("autopilot is reproducible and its summaries add up", {
  a <- autopilot(strat_greedy, n_careers = 15, seed = 4, progress = FALSE)
  b <- autopilot(strat_greedy, n_careers = 15, seed = 4, progress = FALSE)
  expect_s3_class(a, "forkingpaths_autopilot")
  expect_identical(a$careers, b$careers)
  expect_equal(a$strategy, "strat_greedy")
  expect_equal(nrow(a$careers), 15)
  expect_equal(nrow(a$studies), sum(a$careers$studies))
  expect_true(all(a$careers$reason %in% c("tenure", "denied", "fired")))
  totals <- tapply(a$studies$points, a$studies$career, sum)
  expect_equal(as.vector(totals), a$careers$points)
  expect_output(print(a), "Tenure")
  st <- summary(a)
  expect_true(st$tenure + st$denied + st$fired == 1)
})

test_that("the honest strategy never hacks and is never fired", {
  res <- autopilot(strat_honest, n_careers = 30, seed = 5, progress = FALSE)
  expect_true(all(res$studies$suspicion == 0))
  expect_true(all(res$careers$reason != "fired"))
})

test_that("optional stopping only ever recruits", {
  st <- fresh_state(6)
  st$can_submit <- FALSE
  expect_equal(st$moves$type[strat_optional_stopping(st)], "more_data")
  st$moves <- st$moves[st$moves$type != "more_data", ]
  expect_equal(strat_optional_stopping(st), "file_drawer")
  # Without perks: 6 DoF / 2 per recruit = at most 3 recruits = suspicion 3.
  res <- autopilot(strat_optional_stopping, n_careers = 10, seed = 6,
                   progress = FALSE)
  year1 <- res$studies[res$studies$study <= 3, ]
  expect_true(all(year1$suspicion <= 3))
})

test_that("the cautious strategy respects its suspicion budget", {
  res <- autopilot(strat_cautious, n_careers = 10, seed = 7,
                   progress = FALSE)
  expect_true(all(res$studies$suspicion <= 4))
})

test_that("max_steps file-drawers a stalling strategy", {
  res <- autopilot(function(state) "outliers", n_careers = 2, seed = 8,
                   max_steps = 1, progress = FALSE)
  expect_true(all(res$careers$stalled == 9))
  expect_true(all(res$studies$outcome == "file drawer"))
  expect_output(print(res), "max_steps")
})

test_that("a custom perk strategy is used", {
  res <- autopilot(strat_honest, n_careers = 3, seed = 9, progress = FALSE,
    perk_strategy = function(offer, perks) {
      if ("thick_skin" %in% offer) "thick_skin" else offer[length(offer)]
    })
  expect_true(all(nchar(res$careers$perks) > 0))
  expect_error(
    autopilot(strat_honest, n_careers = 1, seed = 9, progress = FALSE,
              perk_strategy = function(offer, perks) "not_a_perk"),
    "not on offer")
})

test_that("compare_strategies returns one row per strategy", {
  cmp <- compare_strategies(honest = strat_honest, strat_optional_stopping,
                            n_careers = 5, seed = 1, progress = FALSE)
  expect_s3_class(cmp, "forkingpaths_comparison")
  expect_equal(cmp$strategy, c("honest", "strat_optional_stopping"))
  expect_length(attr(cmp, "results"), 2)
  expect_output(print(cmp), "careers per strategy")
})

test_that("plot methods draw without error", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  res <- autopilot(strat_greedy, n_careers = 5, seed = 10, progress = FALSE)
  expect_silent(plot(res))
  none <- autopilot(function(state) "file_drawer", n_careers = 2, seed = 1,
                    progress = FALSE)
  expect_silent(plot(none))
  cmp <- compare_strategies(a = strat_honest, b = strat_greedy,
                            n_careers = 3, seed = 1, progress = FALSE)
  expect_silent(plot(cmp))
})

test_that("perk_list documents the perk ids", {
  expect_true(all(c("id", "name", "description") %in% names(perk_list())))
})
