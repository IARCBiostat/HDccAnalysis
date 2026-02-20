# Retrieve baseline hazard rate and band index at a given attained age

Determines which attained-age band contains a given `attained_age` and
returns both the band index and the corresponding baseline hazard rate.

## Usage

``` r
.rate_and_band_at_age(attained_age, lookup_table, tol = 1e-12)
```

## Arguments

- attained_age:

  Numeric scalar giving the current attained age.

- lookup_table:

  List containing numeric vectors `lo`, `hi`, and `rate`, defining the
  lower bounds, upper bounds, and baseline hazard rates for each age
  band.

- tolerance:

  Small non-negative numeric scalar used when comparing `attained_age`
  to band boundaries.

## Value

A list with components:

- band_index:

  Integer index of the age band containing `attained_age`.

- baseline_rate:

  Numeric scalar baseline hazard rate for the identified band.

If `attained_age` lies outside the support of the age bands, both
elements are returned as `NA`.

## Details

Age bands are defined by `lookup_table$lo` and `lookup_table$hi` and are
treated as left-closed, right-open intervals: `[lo, hi)`. The final band
is treated as closed on the right to ensure coverage of its upper
boundary.

A numerical `tolerance` is used to avoid instability at band boundaries
due to floating-point rounding.

## Examples

``` r
lookup_table <- list(
  lo   = c(50, 60),
  hi   = c(60, 70),
  rate = c(0.01, 0.02)
)

.rate_and_band_at_age(attained_age = 55,
                      lookup_table = lookup_table)
#> Error in .rate_and_band_at_age(attained_age = 55, lookup_table = lookup_table): could not find function ".rate_and_band_at_age"
```
