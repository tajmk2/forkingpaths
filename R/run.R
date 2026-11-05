# A career: three years, three studies a year -----------------------------

JOURNALS <- data.frame(
  name = c("The Journal of Significant Results",
           "Annals of Surprising Effects",
           "Prestige: A Journal of Big Claims"),
  alpha = c(0.05, 0.01, 0.005),
  points = c(100, 250, 500),
  replication = c(0.35, 0.55, 0.8),
  stringsAsFactors = FALSE
)

PERKS <- data.frame(
  id = c("ra_army", "friendly_editor", "big_grant", "niche_field",
         "prereg_halo", "thick_skin", "whisperer"),
  name = c("Army of RAs", "Friendly editor", "Big grant", "Niche field",
           "Preregistration halo", "Thick skin", "Sample-size whisperer"),
  description = c(
    "+2 degrees of freedom in every study.",
    "Reviewer 2 is half as likely to catch you.",
    "Studies start with 80 participants instead of 40.",
    "Nobody bothers replicating you: replication chance -25 points.",
    "Honest (zero-suspicion) papers are worth triple points.",
    "+1 maximum credibility, and heal 1.",
    "Recruiting more participants costs only 1 DoF."
  ),
  stringsAsFactors = FALSE
)

STUDIES_PER_YEAR <- 3L
N_YEARS <- 3L
TENURE_BAR <- 600
BASE_DOF <- 6
P_TRUE_EFFECT <- 0.3
REVIEWER_RATE <- 0.07

journal_for <- function(index) {
  JOURNALS[min(ceiling(index / STUDIES_PER_YEAR), nrow(JOURNALS)), ]
}

#' Probability that Reviewer 2 spots the forking paths
#' @keywords internal
#' @noRd
reviewer_catch_prob <- function(suspicion, perks = character()) {
  p <- 1 - exp(-REVIEWER_RATE * suspicion)
  if ("friendly_editor" %in% perks) p <- p / 2
  p
}

replication_prob <- function(journal, perks = character()) {
  p <- journal$replication
  if ("niche_field" %in% perks) p <- p - 0.25
  max(0, min(1, p))
}

empty_history <- function() {
  data.frame(
    study = integer(), title = character(), journal = character(),
    p = numeric(), suspicion = numeric(), true_effect = numeric(),
    outcome = character(), replication = character(), points = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Start a new career
#' @keywords internal
#' @noRd
new_run <- function() {
  list(
    index = 0L, n_studies = STUDIES_PER_YEAR * N_YEARS,
    credibility = 3, max_credibility = 3, points = 0,
    perks = character(), perk_offer = NULL,
    history = empty_history(), study = NULL,
    over = FALSE, reason = NA_character_
  )
}

#' Generate a study
#' @keywords internal
#' @noRd
new_study <- function(index, perks = character()) {
  labels <- OUTCOME_SETS[[sample.int(length(OUTCOME_SETS), 1)]]
  names(labels) <- c("y1", "y2", "y3")
  labels <- as.list(labels)
  intervention <- sample(INTERVENTIONS, 1)
  effect <- if (stats::runif(1) < P_TRUE_EFFECT) {
    sample(c(0.3, 0.5, 0.8), 1)
  } else {
    0
  }
  n <- if ("big_grant" %in% perks) 80L else 40L
  data <- simulate_participants(n, effect)
  spec <- default_spec()
  list(
    index = index,
    title = sprintf("Does %s improve %s?", intervention, labels$y1),
    labels = labels,
    effect = effect,
    data = data,
    spec = spec,
    dof = BASE_DOF + if ("ra_army" %in% perks) 2 else 0,
    dof_start = BASE_DOF + if ("ra_army" %in% perks) 2 else 0,
    suspicion = 0,
    journal = journal_for(index),
    trail = data.frame(step = 0L, move = "Preregistered analysis",
                       p = analyse(data, spec)$p, stringsAsFactors = FALSE)
  )
}

start_next_study <- function(run) {
  if (run$over) stop("This career is over.", call. = FALSE)
  run$index <- run$index + 1L
  run$study <- new_study(run$index, run$perks)
  run
}

current_result <- function(run) analyse(run$study$data, run$study$spec)

can_submit <- function(run) {
  res <- current_result(run)
  res$ok && res$p < run$study$journal$alpha
}

#' Submit the current study
#'
#' Reviewer 2 checks first. If the paper survives it is published, and then
#' it may be picked for replication: fresh data at 2.5 times the sample
#' size, analysed exactly as reported.
#'
#' @return The run, with `last_report` describing what happened.
#' @keywords internal
#' @noRd
submit_study <- function(run) {
  s <- run$study
  j <- s$journal
  res <- analyse(s$data, s$spec)
  if (!(res$ok && res$p < j$alpha)) {
    stop("Not significant: no journal will take this.", call. = FALSE)
  }

  report <- list(caught = FALSE, published = FALSE, replicated = NA,
                 replication_p = NA_real_, points = 0)
  p_catch <- reviewer_catch_prob(s$suspicion, run$perks)

  if (stats::runif(1) < p_catch) {
    report$caught <- TRUE
    run$credibility <- run$credibility - 1
    outcome <- "caught by Reviewer 2"
    replication <- "-"
  } else {
    report$published <- TRUE
    mult <- if (s$suspicion == 0 && "prereg_halo" %in% run$perks) 3 else 1
    report$points <- j$points * mult
    outcome <- "published"
    replication <- "not attempted"

    if (stats::runif(1) < replication_prob(j, run$perks)) {
      n_rep <- 2L * ceiling(2.5 * nrow(s$data) / 2)
      rep <- analyse(simulate_participants(n_rep, s$effect), s$spec)
      report$replication_p <- rep$p
      report$replicated <- rep$ok && rep$p < 0.05 && rep$estimate > 0
      if (report$replicated) {
        report$points <- report$points + j$points / 2
        run$credibility <- min(run$max_credibility, run$credibility + 1)
        replication <- "replicated"
      } else {
        run$credibility <- run$credibility - 1
        replication <- "failed"
      }
    }
    run$points <- run$points + report$points
  }

  run$last_report <- report
  finish_study(run, res$p, outcome, replication, report$points)
}

#' Abandon the current study
#' @keywords internal
#' @noRd
file_drawer <- function(run) {
  res <- current_result(run)
  run$last_report <- NULL
  finish_study(run, res$p, "file drawer", "-", 0)
}

finish_study <- function(run, p, outcome, replication, points) {
  s <- run$study
  run$history <- rbind(run$history, data.frame(
    study = s$index, title = s$title, journal = s$journal$name, p = p,
    suspicion = s$suspicion, true_effect = s$effect, outcome = outcome,
    replication = replication, points = points, stringsAsFactors = FALSE
  ))
  run$study <- NULL

  if (run$credibility <= 0) {
    run$over <- TRUE
    run$reason <- "fired"
  } else if (run$index >= run$n_studies) {
    run$over <- TRUE
    run$reason <- if (run$points >= TENURE_BAR) "tenure" else "denied"
  } else if (run$index %% STUDIES_PER_YEAR == 0L) {
    pool <- setdiff(PERKS$id, run$perks)
    run$perk_offer <- if (length(pool) <= 3) pool else sample(pool, 3)
  }
  run
}

#' Take a perk from the current offer
#' @keywords internal
#' @noRd
choose_perk <- function(run, perk) {
  if (!perk %in% run$perk_offer) {
    stop("That perk is not on offer.", call. = FALSE)
  }
  run$perks <- c(run$perks, perk)
  if (perk == "thick_skin") {
    run$max_credibility <- run$max_credibility + 1
    run$credibility <- run$credibility + 1
  }
  run$perk_offer <- NULL
  run
}

perk_name <- function(id) PERKS$name[match(id, PERKS$id)]
