# Validate or construct an attained-age baseline hazard table

Ensures that a valid attained-age baseline hazard table is available.
Either a piecewise-constant hazard table (`baseline_hazard_by_age`) must
be supplied, or a single constant baseline hazard
(`constant_baseline_hazard`) must be provided.

## Usage

``` r
.make_age_rate_tbl(baseline_hazard_by_age, constant_baseline_hazard)
```

## Arguments

- baseline_hazard_by_age:

  Optional data.frame with columns `age_lo`, `age_hi`, and `rate`,
  defining piecewise-constant baseline hazard bands `[age_lo, age_hi)`.

- constant_baseline_hazard:

  Optional positive scalar giving a constant baseline hazard over all
  attained ages.

## Value

A data.frame with columns `age_lo`, `age_hi`, and `rate`, sorted by
`age_lo`.

## Details

If a constant baseline hazard is given, a one-row table covering
`(-Inf, Inf)` is constructed.

If a table is supplied, it is validated, coerced to numeric, ordered by
`age_lo`, and returned.

Exactly one of `baseline_hazard_by_age` or `constant_baseline_hazard`
must be non-`NULL`. Supplying both or neither results in an error.

All age bands must satisfy `age_hi > age_lo` and all baseline hazard
`rate` values must be strictly positive.

## Examples

``` r
.make_age_rate_table(
  baseline_hazard_by_age = data.frame(
    age_lo = c(50, 60),
    age_hi = c(60, 70),
    rate   = c(0.01, 0.02)
  ),
  constant_baseline_hazard = NULL
)
#> Error in .make_age_rate_table(baseline_hazard_by_age = data.frame(age_lo = c(50,     60), age_hi = c(60, 70), rate = c(0.01, 0.02)), constant_baseline_hazard = NULL): could not find function ".make_age_rate_table"

.make_age_rate_table(
  baseline_hazard_by_age = NULL,
  constant_baseline_hazard = 0.01
)
#> Error in .make_age_rate_table(baseline_hazard_by_age = NULL, constant_baseline_hazard = 0.01): could not find function ".make_age_rate_table"
```
