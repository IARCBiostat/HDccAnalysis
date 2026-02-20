
test_that("baseline_hazard_by_age handles band transitions", {
  tbl <- data.frame(
    age_lo = c(0, 60),
    age_hi = c(60, 100),
    rate = c(0.1, 0.2)
  )

  set.seed(2)
  out <- sim_cox_age_data(
    n = 1,
    entry_age = 50,
    censor_age = 70,
    beta = 0,
    covariates = matrix(0, nrow = 1, ncol = 1),
    baseline_hazard_by_age = tbl
  )

  # First waiting time in band1; if it exceeds the 10-year band width, we jump to band2 and draw again.
  set.seed(2)
  wait1 <- rexp(1, rate = 0.1)
  wait2 <- rexp(1, rate = 0.2)
  expected_exit <- if (wait1 < 10) 50 + wait1 else 60 + wait2
  expected_exit <- min(expected_exit, 70) # administrative censoring at 70

  expect_equal(out$censor_age, expected_exit, tolerance = 1e-12)
  expect_equal(out$event, 1L)
})

test_that("baseline_hazard_by_age errors when follow-up outside support", {
  tbl <- data.frame(
    age_lo = 20,
    age_hi = 30,
    rate = 0.1
  )

  expect_error(
    sim_cox_age_data(
      n = 1,
      entry_age = 10,   # below support
      censor_age = 25,
      beta = 0,
      covariates = matrix(0, nrow = 1, ncol = 1),
      baseline_hazard_by_age = tbl
    ),
    "Cohort ages fall outside `baseline_hazard_by_age` support."
  )
})