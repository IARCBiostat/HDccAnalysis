# Reformat age-group incidence data into an age-rate lookup table

Reformat age-group incidence data into an age-rate lookup table

## Usage

``` r
generate_age_rate_tbl(
  dat,
  trage_col = "trage_invs",
  rate_col = "tx_inc_2008",
  open_hi = 120,
  interval = c("closed", "half-open")
)
```

## Arguments

- dat:

  A data.frame with columns `trage_invs` and `tx_inc_2008`.

- trage_col:

  Name of the age-band column.

- rate_col:

  Name of the incidence-rate column.

- open_hi:

  The upper bound to use for open-ended bands like `"[85;++]"`. Must be
  finite; default 120.

- interval:

  One of "closed" (default) or "half-open".

  - "closed": interprets `"[15;19]"` as inclusive years 15–19, so age_hi
    = 20

  - "half-open": interprets as `[15, 19)`, so age_hi = 19

## Value

A data.frame with columns: age_lo, age_hi, rate.

## Examples

``` r
ex <- data.frame(
    trage_invs = c("[15;19]", "[20;24]", "[85;++]"),
    tx_inc_2008 = c(2.5e-06, 6.0e-06, 4.4e-03)
)
make_age_rate_tbl(ex)
#> Error in make_age_rate_tbl(ex): could not find function "make_age_rate_tbl"
```
