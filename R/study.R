# Data generation and the analysis engine -----------------------------------

INTERVENTIONS <- c(
  "power posing", "mindful breathing", "red-tinted paper", "dark chocolate",
  "standing desks", "cold showers", "gratitude journaling",
  "listening to Mozart", "holding a warm cup", "smelling cinnamon",
  "fidget spinners", "office houseplants", "blue light glasses",
  "chewing gum", "a five-minute nap"
)

# Each row: primary outcome, then two "closely related" alternatives.
OUTCOME_SETS <- list(
  c("wellbeing", "mood", "life satisfaction"),
  c("creativity", "divergent thinking", "idea count"),
  c("memory recall", "word recall", "recognition accuracy"),
  c("self-control", "persistence", "impulse resistance"),
  c("productivity", "task speed", "focus rating"),
  c("empathy", "perspective taking", "helping behaviour")
)

COVARIATES <- c("age", "gender", "sleep")

SUBGROUPS <- c(
  all = "everyone", women = "women only", men = "men only",
  under_40 = "under-40s only", over_40 = "40+ only"
)

#' Simulate study participants
#'
#' Two arms, alternating assignment, three outcomes on a 0-100-ish scale.
#' `y1` is the primary outcome and carries the full true effect (in SD
#' units); `y2` and `y3` correlate about 0.6 with `y1` and carry 60% of it.
#' Around 6% of responses are wild (heavy tails), so outlier rules matter.
#'
#' @param n Number of participants.
#' @param effect True standardised treatment effect.
#' @return A data frame.
#' @keywords internal
#' @noRd
simulate_participants <- function(n, effect = 0) {
  treat <- rep_len(c(0L, 1L), n)
  age <- round(stats::runif(n, 18, 70))
  gender <- factor(sample(c("female", "male"), n, replace = TRUE),
                   levels = c("female", "male"))
  sleep <- round(stats::rnorm(n, 7, 1.2), 1)
  latent <- stats::rnorm(n)
  nuisance <- 0.15 * (age - 44) / 15 + 0.15 * (sleep - 7) / 1.2

  make_y <- function(shared, eff) {
    z <- shared * latent + sqrt(1 - shared^2) * stats::rnorm(n)
    wild <- stats::runif(n) < 0.06
    z[wild] <- z[wild] + stats::rnorm(sum(wild), 0, 3)
    pmax(1, round(50 + 10 * (eff * treat + nuisance + z), 1))
  }

  data.frame(
    id = seq_len(n),
    group = factor(ifelse(treat == 1L, "treatment", "control"),
                   levels = c("control", "treatment")),
    treat = treat,
    age = age,
    gender = gender,
    sleep = sleep,
    y1 = make_y(1, effect),
    y2 = make_y(0.6, 0.6 * effect),
    y3 = make_y(0.6, 0.6 * effect)
  )
}

#' The preregistered analysis
#' @keywords internal
#' @noRd
default_spec <- function() {
  list(outcome = "y1", covariates = character(), outlier_sd = Inf,
       subgroup = "all", log = FALSE, one_tailed = FALSE)
}

is_default_spec <- function(spec) identical(spec, default_spec())

subgroup_rows <- function(data, subgroup) {
  keep <- switch(subgroup,
    all = return(seq_len(nrow(data))),
    women = data$gender == "female",
    men = data$gender == "male",
    under_40 = data$age < 40,
    over_40 = data$age >= 40,
    stop("Unknown subgroup: ", subgroup, call. = FALSE)
  )
  which(keep)
}

#' Run the current analysis
#'
#' Subgroup, then optional log transform, then outlier exclusion (|z| above
#' `outlier_sd`, computed within the subgroup), then
#' `lm(outcome ~ treat + covariates)`. Covariates that are constant in the
#' analysed data are dropped silently, as any desperate analyst would.
#'
#' Works on index vectors rather than data frames, because autopilot calls
#' it hundreds of thousands of times.
#'
#' @param keep_data Also return `kept`, the analysed rows as a data frame
#'   with the analysed outcome in `.y` (for plotting).
#' @return A list with `p`, `estimate`, `t`, `df`, `n`, `ok`, `rows`
#'   (analysed row indices) and, if requested, `kept`.
#' @keywords internal
#' @noRd
analyse <- function(data, spec, keep_data = FALSE) {
  rows <- subgroup_rows(data, spec$subgroup)
  y <- data[[spec$outcome]][rows]
  if (isTRUE(spec$log)) y <- log(y)

  if (is.finite(spec$outlier_sd) && length(y) > 2) {
    s <- stats::sd(y)
    if (is.finite(s) && s > 0) {
      keep <- abs(y - mean(y)) / s <= spec$outlier_sd
      rows <- rows[keep]
      y <- y[keep]
    }
  }

  result <- function(p, est, t, df, ok) {
    out <- list(p = p, estimate = est, t = t, df = df, n = length(rows),
                ok = ok, rows = rows)
    if (keep_data) {
      d <- data[rows, , drop = FALSE]
      d$.y <- y
      out$kept <- d
    }
    out
  }
  fail <- function() result(1, 0, 0, NA_real_, FALSE)

  treat <- data$treat[rows]
  covs <- lapply(spec$covariates, function(v) as.numeric(data[[v]][rows]))
  covs <- Filter(function(x) length(unique(x)) > 1, covs)
  n <- length(rows)
  if (n <= length(covs) + 3 || length(unique(treat)) < 2) return(fail())

  # OLS via QR: identical to lm(y ~ treat + covs), but fast enough for
  # autopilot to run thousands of careers.
  X <- matrix(c(rep(1, n), treat, unlist(covs)), nrow = n)
  fit <- qr(X)
  df <- n - fit$rank
  if (fit$rank < ncol(X) || df < 1) return(fail())
  beta <- qr.coef(fit, y)
  sigma2 <- sum(qr.resid(fit, y)^2) / df
  se <- sqrt(sigma2 * chol2inv(qr.R(fit))[2, 2])
  if (!is.finite(se) || se <= 0) return(fail())

  est <- beta[[2]]
  t <- est / se
  p <- if (isTRUE(spec$one_tailed)) {
    stats::pt(t, df, lower.tail = FALSE)
  } else {
    2 * stats::pt(-abs(t), df)
  }
  result(p, est, t, df, TRUE)
}

#' Describe an analysis spec in one line
#' @keywords internal
#' @noRd
describe_spec <- function(spec, labels) {
  y <- labels[[spec$outcome]]
  if (isTRUE(spec$log)) y <- sprintf("log(%s)", y)
  rhs <- paste(c("group", spec$covariates), collapse = " + ")
  bits <- sprintf("%s ~ %s", y, rhs)
  if (is.finite(spec$outlier_sd)) {
    bits <- c(bits, sprintf("drop |z| > %s", format(spec$outlier_sd)))
  }
  if (spec$subgroup != "all") bits <- c(bits, SUBGROUPS[[spec$subgroup]])
  if (isTRUE(spec$one_tailed)) bits <- c(bits, "one-tailed")
  paste(bits, collapse = " | ")
}
