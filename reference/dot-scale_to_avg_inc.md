# Scale baseline hazard to a target average incidence rate

Adjusts the rates in an attained-age baseline hazard table so that the
cohort's **expected** average incidence rate equals a user-specified
target. The expectation is taken under a piecewise-constant proportional
hazards model with subject-specific multiplier `exp_eta`.

## Usage

``` r
.scale_to_avg_inc(tbl, entry_age, censor_age, avg_inc_rate, exp_eta)
```

## Arguments

- tbl:

  data.frame with columns `age_lo`, `age_hi`, `rate`. Defines the
  baseline hazard over attained age bands `[age_lo, age_hi)`.

- entry_age:

  Numeric vector of entry ages.

- censor_age:

  Numeric vector of exit/censoring ages.

- avg_inc_rate:

  Optional positive scalar giving the desired average incidence rate
  over the cohort's follow-up. If `NULL`, `tbl` is returned unchanged.

- exp_eta:

  Numeric vector of length `length(entry_age)` giving the
  subject-specific hazard multipliers \\\exp(x\beta)\\ used when
  computing expected events and expected time at risk.

## Value

A data.frame identical to `tbl` but with `rate` scaled so that the
cohort's expected average incidence rate equals `avg_inc_rate`.

## Details

The function finds a single scaling factor \\s\\ applied to all baseline
rates \\\lambda_k\\ such that \$\$
\frac{\mathbb{E}\[N(s)\]}{\mathbb{E}\[T(s)\]} = \texttt{avg\\inc\\rate},
\$\$ where \\\mathbb{E}\[N(s)\] = \sum_i \\1 - \exp(-H_i(s))\\\\ is the
expected number of events and \\\mathbb{E}\[T(s)\]\\ is the expected
time at risk, both implied by the scaled hazards over each subject's
follow-up window `[entry_age, censor_age)` and the attained-age bands
`[age_lo_k, age_hi_k)`.

All baseline rates are multiplied by the same factor, so relative
hazards are unchanged; only the marginal event rate is altered.

The target average incidence rate is defined for the cohort under the
model implied by `tbl` and `exp_eta`. Scaling is performed by solving
for the multiplicative factor using
[`uniroot`](https://rdrr.io/r/stats/uniroot.html).

If `avg_inc_rate` is non-`NULL` and not a positive finite number, the
function errors. If a bracketing interval for the root cannot be found,
the function errors.

## See also

[`sim_cox_age_data`](https://IARCBiostat.github.io/HDccAnalysis/reference/sim_cox_age_data.md)

## Examples

``` r
tbl <- data.frame(
  age_lo = c(50, 60),
  age_hi = c(60, 70),
  rate   = c(0.01, 0.02)
)

entry_age  <- c(52, 55, 61)
censor_age <- c(65, 63, 69)
exp_eta    <- rep(1, length(entry_age))

.scale_to_avg_inc(tbl, entry_age, censor_age, avg_inc_rate = 0.015, exp_eta)
#> Error in .scale_to_avg_inc(tbl, entry_age, censor_age, avg_inc_rate = 0.015,     exp_eta): could not find function ".scale_to_avg_inc"
```
