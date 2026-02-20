Statistical methods for analysing **case–cohort study designs** in
**high-dimensional** settings.

HDccAnalysis supports: - **Typical designs**: all cases outside the
subcohort are included - **Untypical designs**: only a subset of cases
outside the subcohort are included - Multiple weighting schemes:
**Prentice**, **Barlow**, and **Binder** - Tools for **evaluation** and
**performance assessment** in reproducible workflows

------------------------------------------------------------------------

## Installation

``` r
remotes::install_github("IARCBiostat/HDccAnalysis")
```

------------------------------------------------------------------------

## Getting started

### Simulate data

Use
[`HDccAnalysis::sim_cox_age_data()`](https://IARCBiostat.github.io/HDccAnalysis/reference/sim_cox_age_data.md)
to simulate attained-age survival data under varying incidence rates.

- Function reference:
  [`sim_cox_age_data()`](https://IARCBiostat.github.io/HDccAnalysis/reference/sim_cox_age_data.md)
  (pkgdown) `reference/sim_cox_age_data.html`
- Vignette: **Simulating data** — walkthrough with a scalar baseline
  hazard `articles/simulating-data.html`

------------------------------------------------------------------------

## Documentation

- **Reference index**: `reference/`
- **Articles / vignettes**: `articles/`

(If you have more key functions, add a small “Core functions” bullet
list here.)

------------------------------------------------------------------------

## Contributing

Bug reports and feature requests: open an **issue**. Code contributions:
**pull requests** are welcome via GitHub.
