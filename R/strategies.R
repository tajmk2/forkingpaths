# Built-in strategies -------------------------------------------------------

#' Built-in autopilot strategies
#'
#' Ready-made strategies to run with [autopilot()], or to copy and improve.
#'
#' * `strat_honest`: runs the preregistered analysis and submits only if it
#'   is significant.
#' * `strat_greedy`: keeps taking whichever move gives the lowest p (via
#'   [peek()]) until it can submit or runs out of DoF.
#' * `strat_cautious`: greedy, but never lets suspicion go past 4.
#' * `strat_optional_stopping`: only ever recruits more participants.
#'
#' @param state The state passed by [autopilot()].
#' @return An action: `"submit"`, `"file_drawer"` or a move id.
#' @name strategies
NULL

#' @rdname strategies
#' @export
strat_honest <- function(state) {
  if (state$can_submit) "submit" else "file_drawer"
}

greedy_with_budget <- function(state, budget) {
  if (state$can_submit) return("submit")
  options <- peek(state)
  options <- options[options$type != "reset" & !is.na(options$p_after) &
                       state$suspicion + options$suspicion <= budget, ]
  if (nrow(options) == 0) return("file_drawer")
  options$id[which.min(options$p_after)]
}

#' @rdname strategies
#' @export
strat_greedy <- function(state) greedy_with_budget(state, Inf)

#' @rdname strategies
#' @export
strat_cautious <- function(state) greedy_with_budget(state, 4)

#' @rdname strategies
#' @export
strat_optional_stopping <- function(state) {
  if (state$can_submit) return("submit")
  id <- pick_move(state, "more_data")
  if (is.na(id)) "file_drawer" else id
}
