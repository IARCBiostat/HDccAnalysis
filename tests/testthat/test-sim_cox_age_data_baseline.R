test_that("sim_cox_age_data simulates with a constant baseline hazard", {
  set.seed(123)
  entry_age = c(50, 55)
  censor_age = c(60, 65)

  out <- sim_cox_age_data(
    n = 2,
    entry_age = entry_age,
    censor_age = censor_age,
    beta = 0,
    covariates = matrix(0, nrow = 2, ncol = 1),
    baseline_hazard = 0.1
  )

  expect_equal(colnames(out), c("id", "entry_age", "censor_age", "event", "Exp1"))
  expect_equal(out$event, c(1L, 1L))

  # Wait time in each attained-age interval is exponential with rate = baseline_hazard * exp(xβ), and here xβ = 0.
  set.seed(123)
  expected_wait <- rexp(2, rate = 0.1)
  expected_age_out <- pmin(entry_age + expected_wait, censor_age)

  expect_equal(
    out$censor_age,
    expected_age_out,
    tolerance = 1e-12
  )

  # Achieved incidence rate = total events / total person-time.
  expected_incidence_rate <- sum(out$event) / sum(expected_age_out - entry_age)
  expect_equal(
    attr(out, "achieved_incidence_rate"),
    expected_incidence_rate,
    tolerance = 1e-12
  )
})

test_that("scalar ages recycle to length n", {
  set.seed(99)
  out <- sim_cox_age_data(
    n = 3,
    entry_age = 40,
    censor_age = 41,
    beta = 0,
    covariates = matrix(0, nrow = 3, ncol = 1),
    baseline_hazard = 0.001
  )

  expect_equal(nrow(out), 3L)
  expect_equal(out$entry_age, rep(40, 3))
  expect_true(all(out$censor_age <= 41))
  expect_true(all(out$censor_age > out$entry_age))
})

test_that("censor_age must exceed entry_age", {
  expect_error(
    sim_cox_age_data(
      n = 1,
      entry_age = 10,
      censor_age = 9,
      beta = 0,
      covariates = matrix(0, nrow = 1, ncol = 1),
      baseline_hazard = 0.1
    ),
    "All `censor_age` must be > `entry_age`."
  )
})
