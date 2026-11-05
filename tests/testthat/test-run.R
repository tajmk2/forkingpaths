# Force a run whose current study is significant, with a controllable truth.
significant_run <- function(effect, replication = 1, suspicion = 0,
                            perks = character()) {
  set.seed(10)
  run <- new_run()
  run$perks <- perks
  run <- start_next_study(run)
  run$study$effect <- effect
  d <- simulate_participants(40, effect = 0)
  d$y1 <- d$y1 + 15 * d$treat
  run$study$data <- d
  run$study$suspicion <- suspicion
  run$study$journal$replication <- replication
  run
}

test_that("reviewer catch probability rises with suspicion", {
  expect_equal(reviewer_catch_prob(0), 0)
  p <- reviewer_catch_prob(0:20)
  expect_true(all(diff(p) > 0))
  expect_equal(reviewer_catch_prob(10, "friendly_editor"),
               reviewer_catch_prob(10) / 2)
})

test_that("journals get stricter each year", {
  expect_equal(journal_for(1)$alpha, 0.05)
  expect_equal(journal_for(4)$alpha, 0.01)
  expect_equal(journal_for(9)$alpha, 0.005)
})

test_that("an honest paper with a real effect publishes and replicates", {
  run <- significant_run(effect = 1.5)
  expect_true(can_submit(run))
  run$credibility <- 2
  run <- submit_study(run)
  expect_true(run$last_report$published)
  expect_true(run$last_report$replicated)
  expect_equal(run$credibility, 3)
  expect_equal(run$points, 150)
  expect_equal(run$history$replication, "replicated")
  expect_null(run$study)
})

test_that("a lucky false positive fails replication and costs credibility", {
  run <- significant_run(effect = 0)
  run <- submit_study(run)
  expect_true(run$last_report$published)
  expect_false(run$last_report$replicated)
  expect_equal(run$credibility, 2)
  expect_equal(run$points, 100)
})

test_that("no replication attempt leaves credibility alone", {
  run <- significant_run(effect = 0, replication = 0)
  run <- submit_study(run)
  expect_true(is.na(run$last_report$replicated))
  expect_equal(run$credibility, 3)
  expect_equal(run$history$replication, "not attempted")
})

test_that("the preregistration halo triples honest points only", {
  run <- submit_study(significant_run(0, replication = 0,
                                      perks = "prereg_halo"))
  expect_equal(run$points, 300)
})

test_that("Reviewer 2 eventually catches a heavily hacked paper", {
  caught <- vapply(1:40, function(i) {
    run <- significant_run(effect = 0, replication = 0, suspicion = 60)
    set.seed(i)
    submit_study(run)$last_report$caught
  }, logical(1))
  expect_gt(mean(caught), 0.9)
})

test_that("non-significant studies cannot be submitted", {
  set.seed(10)
  run <- start_next_study(new_run())
  run$study$data$y1 <- 50 + rnorm(40, 0, 0.01)
  expect_false(can_submit(run))
  expect_error(submit_study(run), "Not significant")
})

test_that("losing all credibility ends the career", {
  run <- significant_run(effect = 0)
  run$credibility <- 1
  run <- submit_study(run)
  expect_true(run$over)
  expect_equal(run$reason, "fired")
})

test_that("perks are offered after every third study", {
  set.seed(11)
  run <- new_run()
  for (i in 1:3) run <- file_drawer(start_next_study(run))
  expect_length(run$perk_offer, 3)
  expect_true(all(run$perk_offer %in% PERKS$id))
  run$perk_offer <- c("thick_skin", "ra_army", "whisperer")
  run <- choose_perk(run, "thick_skin")
  expect_equal(run$max_credibility, 4)
  expect_equal(run$credibility, 4)
  expect_null(run$perk_offer)
  expect_error(choose_perk(run, "ra_army"), "not on offer")
})

test_that("nine file-drawered studies means tenure denied", {
  set.seed(12)
  run <- new_run()
  while (!run$over) {
    run <- file_drawer(start_next_study(run))
    if (!is.null(run$perk_offer)) run <- choose_perk(run, run$perk_offer[1])
  }
  expect_equal(nrow(run$history), 9)
  expect_equal(run$reason, "denied")
  expect_error(start_next_study(run), "over")
})
