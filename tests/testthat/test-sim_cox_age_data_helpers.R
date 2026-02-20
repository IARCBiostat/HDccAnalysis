# source("R/sim_cox_age_data_helpers.R")
# library(HDccAnalysis)

test_that(".as_len_n recycles scalars and errors on wrong length", {
  expect_equal(.as_len_n(5, 3, "x"), rep(5, 3))
  expect_error(.as_len_n(1:2, 3, "x"), "`x` must have length 1 or n.")
})

test_that(".check_ages enforces increasing ages", {
  expect_true(.check_ages(c(1, 2), c(2, 3)))
  expect_error(.check_ages(c(1, 2), c(1, 1.5)), "All `censor_age` must be > `entry_age`.")
})

test_that(".as_x_matrix validates dimensions", {
  x <- data.frame(a = 1:2, b = 3:4)
  expect_equal(dim(.as_x_matrix(x, 2)), c(2, 2))
  expect_error(.as_x_matrix(x, 3), "`x` must have n rows.")
})

test_that(".check_beta checks length", {
  expect_equal(.check_beta(c(1, 2), 2), c(1, 2))
  expect_error(.check_beta(1, 2), "Length of `beta` must match ncol\\(x\\).")
})

test_that(".make_age_rate_tbl builds from scalar lambda and validates bands", {
  tbl <- .make_age_rate_tbl(NULL, 0.1)
  expect_equal(tbl$rate, 0.1)
  expect_equal(tbl$age_lo, -Inf)
  expect_equal(tbl$age_hi, Inf)

  good <- data.frame(age_lo = c(0, 60), age_hi = c(60, 80), rate = c(0.1, 0.2))
  expect_equal(.make_age_rate_tbl(good, NULL)$rate, c(0.1, 0.2))

  bad <- data.frame(age_lo = 0, age_hi = 0, rate = 0.1)
  expect_error(.make_age_rate_tbl(bad, NULL), "age_hi > age_lo")
})

test_that(".overlap_weight computes overlap length", {
  expect_equal(.overlap_weight(band_lower_age = 0, band_upper_age = 10, entry_age = 2, exit_age = 5), 3)
  expect_equal(.overlap_weight(band_lower_age = 5, band_upper_age = 10, entry_age = 0, exit_age = 4), 0)
})

test_that(".scale_to_avg_inc rescales single-band rate to target", {
  tbl <- data.frame(age_lo = -Inf, age_hi = Inf, rate = 0.1)
  age_in <- c(50, 55)
  age_out <- c(60, 65)
  exp_eta <- rep(1, 2)
  scaled <- .scale_to_avg_inc(tbl, age_in, age_out, avg_inc_rate = 0.2, exp_eta = exp_eta)
  expect_equal(scaled$rate, 0.2, tolerance = 1e-12)
})

test_that(".simulate_one respects band changes and censoring", {
  lk <- list(
    lo = c(0, 60),
    hi = c(60, 100),
    rate = c(0.1, 0.2)
  )

  set.seed(2)
  res <- .simulate_one(entry_age = 50, censor_age = 70, exp_eta = 1, lookup_table = lk)

  set.seed(2)
  wait1 <- stats::rexp(1, rate = 0.1)
  wait2 <- stats::rexp(1, rate = 0.2)
  expected_exit <- if (wait1 < 10) 50 + wait1 else 60 + wait2
  expected_exit <- min(expected_exit, 70)

  expect_equal(res$age_exit, expected_exit, tolerance = 1e-12)
  expect_equal(res$event, 1L)
})
