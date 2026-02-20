# Compute follow-up overlap with an attained-age band

Computes the amount of follow-up time contributed within a single
attained-age band.

## Usage

``` r
.overlap_weight(band_lower_age, band_upper_age, entry_age, exit_age)
```

## Arguments

- band_lower_age:

  Numeric scalar or vector giving the lower boundary of the attained-age
  band.

- band_upper_age:

  Numeric scalar or vector giving the upper boundary of the attained-age
  band.

- entry_age:

  Numeric scalar or vector giving age at entry.

- exit_age:

  Numeric scalar or vector giving age at exit (event or censoring).

## Value

Numeric scalar or vector giving the overlap duration. Returns 0 where
the intervals do not intersect.

## Details

Specifically, this function returns the length of the intersection
between the subject's follow-up interval `[entry_age, exit_age)` and the
attained-age band `[band_lower_age, band_upper_age)`.

All intervals are treated as left-closed, right-open.

## Examples

``` r
.overlap_weight(
  band_lower_age = 50,
  band_upper_age = 60,
  entry_age = 55,
  exit_age = 65
)
#> Error in .overlap_weight(band_lower_age = 50, band_upper_age = 60, entry_age = 55,     exit_age = 65): could not find function ".overlap_weight"
```
