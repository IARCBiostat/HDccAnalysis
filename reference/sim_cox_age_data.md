# Simulate attained-age survival data under a Cox PH model

Simulate attained-age survival data under a Cox PH model

## Usage

``` r
sim_cox_age_data(
  n,
  entry_age,
  censor_age,
  beta,
  covariates,
  baseline_hazard = 0.004,
  baseline_hazard_by_age = NULL,
  target_avg_baseline_hazard = NULL
)
```

## Arguments

- n:

  Integer, number of subjects.

- entry_age:

  Numeric scalar or length-n vector of entry ages.

- censor_age:

  Numeric scalar or length-n vector of administrative censoring ages.

- beta:

  Numeric vector of regression coefficients with length p.

- covariates:

  Numeric matrix or data.frame with n rows and p columns.

- baseline_hazard:

  Optional positive scalar specifying a constant baseline hazard over
  attained age. Must be NULL if `baseline_hazard_by_age` is provided.

- baseline_hazard_by_age:

  Optional data.frame with columns age_lo, age_hi, hazard defining
  piecewise-constant baseline hazard over attained age intervals
  \[age_lo, age_hi). Must be NULL if `baseline_hazard` is provided.

- target_avg_baseline_hazard:

  Optional positive scalar. If provided, rescales the baseline hazard so
  the cohort-weighted average equals this value.

## Value

data.frame with id, entry_age, censor_age, time, event and covariates.

## Details

Exactly one of `baseline_hazard` or `baseline_hazard_by_age` must be
specified (i.e. one must be non-NULL). Supplying both or neither results
in an error.
