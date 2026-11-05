# Researcher degrees of freedom ---------------------------------------------

# Cost in degrees of freedom (DoF), and how suspicious each move looks to
# Reviewer 2. `reset` is free but cannot un-explore the paths you took.
MOVE_TYPES <- data.frame(
  type = c("outliers", "covariate", "outcome", "subgroup", "log",
           "more_data", "one_tailed", "reset"),
  category = c("Exclude outliers", "Add or drop a covariate",
               "Switch the outcome", "Analyse a subgroup",
               "Transform the outcome", "Recruit 10 more participants",
               "Go one-tailed (we predicted it!)",
               "Return to the preregistered analysis"),
  cost = c(1, 1, 2, 2, 1, 2, 2, 0),
  suspicion = c(1, 1, 2, 2, 1, 1, 3, 0),
  stringsAsFactors = FALSE
)

move_cost <- function(type, perks = character()) {
  cost <- MOVE_TYPES$cost[MOVE_TYPES$type == type]
  if (type == "more_data" && "whisperer" %in% perks) cost <- 1
  cost
}

move_suspicion <- function(type) {
  MOVE_TYPES$suspicion[MOVE_TYPES$type == type]
}

new_move <- function(type, value = NULL, label) {
  list(type = type, value = value, label = label)
}

#' Every move available from the current study state
#'
#' Moves that would leave the analysis unchanged are not offered.
#' Affordability is not checked here; see `affordable_moves()`.
#' @keywords internal
#' @noRd
move_options <- function(study) {
  spec <- study$spec
  labels <- study$labels
  out <- list()

  for (k in setdiff(c(3, 2.5, 2), spec$outlier_sd)) {
    out[[length(out) + 1]] <- new_move("outliers", k,
      sprintf("Drop responses more than %s SD from the mean", format(k)))
  }
  if (is.finite(spec$outlier_sd)) {
    out[[length(out) + 1]] <- new_move("outliers", Inf,
      "Put the outliers back")
  }

  for (v in COVARIATES) {
    lab <- if (v %in% spec$covariates) {
      sprintf("Stop adjusting for %s", v)
    } else {
      sprintf("Adjust for %s", v)
    }
    out[[length(out) + 1]] <- new_move("covariate", v, lab)
  }

  for (y in setdiff(names(labels), spec$outcome)) {
    out[[length(out) + 1]] <- new_move("outcome", y,
      sprintf("Make '%s' the primary outcome", labels[[y]]))
  }

  for (g in setdiff(names(SUBGROUPS), spec$subgroup)) {
    lab <- if (g == "all") "Go back to analysing everyone" else
      sprintf("Analyse %s", SUBGROUPS[[g]])
    out[[length(out) + 1]] <- new_move("subgroup", g, lab)
  }

  out[[length(out) + 1]] <- new_move("log", !isTRUE(spec$log),
    if (isTRUE(spec$log)) "Undo the log transform" else
      "Log-transform the outcome")

  out[[length(out) + 1]] <- new_move("more_data", 10L,
    "Recruit 10 more participants (just to be sure)")

  if (!isTRUE(spec$one_tailed)) {
    out[[length(out) + 1]] <- new_move("one_tailed", TRUE,
      "Switch to a one-tailed test")
  }

  if (!is_default_spec(spec)) {
    out[[length(out) + 1]] <- new_move("reset", NULL,
      "Return to the preregistered analysis")
  }
  out
}

affordable_moves <- function(study, perks = character()) {
  Filter(function(m) move_cost(m$type, perks) <= study$dof,
         move_options(study))
}

#' Apply one move to a study
#'
#' Deducts DoF, adds suspicion, updates the spec or the data, and appends
#' the new p-value to the study's trail.
#' @keywords internal
#' @noRd
apply_move <- function(study, move, perks = character()) {
  cost <- move_cost(move$type, perks)
  if (cost > study$dof) {
    stop("Not enough degrees of freedom left for that.", call. = FALSE)
  }
  if (move$type == "more_data") {
    extra <- simulate_participants(move$value, study$effect)
    d <- rbind(study$data, extra)
    d$id <- seq_len(nrow(d))
    study$data <- d
  } else {
    study$spec <- spec_after(study$spec, move)
  }
  study$dof <- study$dof - cost
  study$suspicion <- study$suspicion + move_suspicion(move$type)
  res <- analyse(study$data, study$spec)
  study$trail <- rbind(study$trail, data.frame(
    step = nrow(study$trail), move = move$label, p = res$p,
    stringsAsFactors = FALSE
  ))
  study
}

#' The analysis spec after a (non-data) move
#' @keywords internal
#' @noRd
spec_after <- function(spec, move) {
  switch(move$type,
    outliers = spec$outlier_sd <- move$value,
    covariate = {
      spec$covariates <- if (move$value %in% spec$covariates) {
        setdiff(spec$covariates, move$value)
      } else {
        c(spec$covariates, move$value)
      }
    },
    outcome = spec$outcome <- move$value,
    subgroup = spec$subgroup <- move$value,
    log = spec$log <- isTRUE(move$value),
    one_tailed = spec$one_tailed <- TRUE,
    reset = spec <- default_spec(),
    stop("Unknown move: ", move$type, call. = FALSE)
  )
  spec
}
