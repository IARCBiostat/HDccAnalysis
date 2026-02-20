# Simulate a single subject's event time under piecewise-constant hazard

Simulates an event time for one subject followed from entry age
`entry_age` to administrative censoring age `censor_age` under an
attained-age baseline hazard that is piecewise constant over age bands.
The subject-specific hazard is \$\$ h(a) = h_0(a) \exp(\eta), \$\$ where
`exp_eta = exp(eta)` is the Cox proportional hazards multiplier.

## Usage

``` r
.simulate_one(entry_age, censor_age, exp_eta, lookup_table)
```

## Arguments

- entry_age:

  Numeric scalar giving the subject's age at study entry.

- censor_age:

  Numeric scalar giving the administrative censoring age. Must satisfy
  `censor_age > entry_age`.

- exp_eta:

  Positive scalar equal to \\\exp(\eta)\\, the subject-specific hazard
  multiplier.

- lookup_table:

  List produced by `.prep_lookup()`, containing numeric vectors `lo`,
  `hi`, and `rate` defining the attained-age baseline hazard bands.

## Value

A list with components:

- age_exit:

  Numeric scalar giving the exit age (event or censoring).

- event:

  Integer scalar equal to 1 if an event occurred and 0 if
  administratively censored.

## Details

The simulation proceeds band-by-band. At the current attained age
`current_age`, the baseline hazard \\h_0(a)\\ is obtained from the age
band containing that age. A waiting time is drawn from an exponential
distribution with rate `h_0(a) * exp_eta`. If the waiting time occurs
before the end of the current age band (or before `censor_age`), an
event occurs. Otherwise the subject advances to the next band boundary
and the procedure repeats until `censor_age`.

Age bands are treated as left-closed, right-open intervals:
`[age_lo, age_hi)`. An attained age equal to `age_hi` is assigned to the
next band.

The baseline hazard must be defined for all attained ages reachable in
`[entry_age, censor_age)`. If no age band contains the current attained
age, the function errors.

The method is exact for piecewise-constant hazards because within each
age band the hazard is constant and the waiting time distribution is
exponential.

## Examples

``` r
lk <- list(
  lo = c(50, 60),
  hi = c(60, 70),
  rate = c(0.01, 0.02)
)

set.seed(1)
.simulate_one(entry_age = 55,
              censor_age = 65,
              exp_eta = 1.5,
              lookup_table = lk)
#> Error in .simulate_one(entry_age = 55, censor_age = 65, exp_eta = 1.5,     lookup_table = lk): could not find function ".simulate_one"
```
