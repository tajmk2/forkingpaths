make_study <- function(seed = 1, perks = character()) {
  set.seed(seed)
  new_study(1L, perks)
}

move_of <- function(study, type, value = NULL) {
  opts <- Filter(function(m) m$type == type &&
                   (is.null(value) || identical(m$value, value)),
                 move_options(study))
  opts[[1]]
}

test_that("a fresh study offers no reset and no current values", {
  s <- make_study()
  types <- vapply(move_options(s), `[[`, "", "type")
  expect_false("reset" %in% types)
  outcome_values <- unlist(lapply(Filter(function(m) m$type == "outcome",
                                         move_options(s)), `[[`, "value"))
  expect_setequal(outcome_values, c("y2", "y3"))
  subgroup_values <- unlist(lapply(Filter(function(m) m$type == "subgroup",
                                          move_options(s)), `[[`, "value"))
  expect_false("all" %in% subgroup_values)
})

test_that("apply_move spends DoF, adds suspicion and extends the trail", {
  s <- make_study()
  s2 <- apply_move(s, move_of(s, "outcome", "y2"))
  expect_equal(s2$dof, s$dof - 2)
  expect_equal(s2$suspicion, 2)
  expect_equal(s2$spec$outcome, "y2")
  expect_equal(nrow(s2$trail), 2)
  expect_equal(s2$trail$p[2], analyse(s2$data, s2$spec)$p)
})

test_that("covariates toggle on and off", {
  s <- make_study()
  s <- apply_move(s, move_of(s, "covariate", "age"))
  expect_equal(s$spec$covariates, "age")
  m <- move_of(s, "covariate", "age")
  expect_match(m$label, "Stop adjusting")
  s <- apply_move(s, m)
  expect_length(s$spec$covariates, 0)
})

test_that("reset restores the preregistered spec but keeps suspicion", {
  s <- make_study()
  s <- apply_move(s, move_of(s, "subgroup", "men"))
  s <- apply_move(s, move_of(s, "reset"))
  expect_true(is_default_spec(s$spec))
  expect_equal(s$suspicion, 2)
  expect_equal(s$dof, s$dof_start - 2)
})

test_that("recruiting adds 10 participants with unique ids", {
  s <- make_study()
  s2 <- apply_move(s, move_of(s, "more_data"))
  expect_equal(nrow(s2$data), nrow(s$data) + 10)
  expect_equal(s2$data$id, seq_len(nrow(s2$data)))
})

test_that("you cannot spend DoF you don't have", {
  s <- make_study()
  s$dof <- 1
  expect_error(apply_move(s, move_of(s, "outcome")), "degrees of freedom")
  types <- vapply(affordable_moves(s), `[[`, "", "type")
  expect_false(any(c("outcome", "subgroup", "more_data") %in% types))
  expect_true("outliers" %in% types)
})

test_that("perks change move costs and starting conditions", {
  expect_equal(move_cost("more_data"), 2)
  expect_equal(move_cost("more_data", "whisperer"), 1)
  s <- make_study(perks = c("ra_army", "big_grant"))
  expect_equal(s$dof, BASE_DOF + 2)
  expect_equal(nrow(s$data), 80)
})

test_that("one-tailed can only be switched on once", {
  s <- make_study()
  s <- apply_move(s, move_of(s, "one_tailed"))
  types <- vapply(move_options(s), `[[`, "", "type")
  expect_false("one_tailed" %in% types)
})
