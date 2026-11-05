test_that("simulate_participants returns a balanced, positive dataset", {
  set.seed(1)
  d <- simulate_participants(40)
  expect_equal(nrow(d), 40)
  expect_named(d, c("id", "group", "treat", "age", "gender", "sleep",
                    "y1", "y2", "y3"))
  expect_equal(as.vector(table(d$treat)), c(20, 20))
  expect_true(all(d$y1 >= 1 & d$y2 >= 1 & d$y3 >= 1))
  expect_equal(levels(d$group), c("control", "treatment"))
})

test_that("a true effect shows up in the primary outcome", {
  set.seed(2)
  d <- simulate_participants(4000, effect = 0.5)
  diff <- mean(d$y1[d$treat == 1]) - mean(d$y1[d$treat == 0])
  expect_gt(diff, 3)
  expect_lt(diff, 7)
})

test_that("the default analysis matches a pooled-variance t-test", {
  set.seed(3)
  d <- simulate_participants(40)
  res <- analyse(d, default_spec())
  tt <- t.test(y1 ~ treat, data = d, var.equal = TRUE)
  expect_equal(res$p, tt$p.value, tolerance = 1e-10)
  expect_equal(res$n, 40)
  expect_true(res$ok)
})

test_that("one-tailed p halves the two-tailed p for a positive estimate", {
  set.seed(4)
  d <- simulate_participants(40)
  d$y1 <- d$y1 + 20 * d$treat
  two <- analyse(d, default_spec())
  spec <- default_spec()
  spec$one_tailed <- TRUE
  one <- analyse(d, spec)
  expect_gt(two$estimate, 0)
  expect_equal(one$p, two$p / 2, tolerance = 1e-10)
})

test_that("one-tailed p is large when the effect goes the wrong way", {
  d <- data.frame(treat = rep(0:1, 10), gender = "female", age = 30,
                  y1 = rep(c(60, 40), 10) + rnorm(20, 0, 0.1))
  spec <- default_spec()
  spec$one_tailed <- TRUE
  expect_gt(analyse(d, spec)$p, 0.99)
})

test_that("subgroups, outliers and log transforms change what is analysed", {
  set.seed(5)
  d <- simulate_participants(60)
  spec <- default_spec()

  spec$subgroup <- "women"
  res <- analyse(d, spec)
  expect_equal(res$n, sum(d$gender == "female"))
  expect_true(all(res$kept$gender == "female"))

  spec <- default_spec()
  d$y1[1] <- 500
  spec$outlier_sd <- 2
  res <- analyse(d, spec)
  expect_false(1 %in% res$kept$id)

  spec <- default_spec()
  spec$log <- TRUE
  expect_equal(analyse(d, spec)$kept$.y, log(d$y1))
})

test_that("constant covariates are dropped instead of erroring", {
  set.seed(6)
  d <- simulate_participants(60)
  spec <- default_spec()
  spec$subgroup <- "women"
  spec$covariates <- "gender"
  plain <- default_spec()
  plain$subgroup <- "women"
  expect_equal(analyse(d, spec)$p, analyse(d, plain)$p)
})

test_that("degenerate data gives p = 1 and ok = FALSE", {
  d <- simulate_participants(4)
  d$treat <- 0L
  res <- analyse(d, default_spec())
  expect_false(res$ok)
  expect_equal(res$p, 1)
})

test_that("describe_spec summarises every hack", {
  labels <- list(y1 = "mood", y2 = "joy", y3 = "zest")
  expect_equal(describe_spec(default_spec(), labels), "mood ~ group")
  spec <- list(outcome = "y2", covariates = c("age", "sleep"),
               outlier_sd = 2.5, subgroup = "men", log = TRUE,
               one_tailed = TRUE)
  expect_equal(describe_spec(spec, labels),
    "log(joy) ~ group + age + sleep | drop |z| > 2.5 | men only | one-tailed")
})
