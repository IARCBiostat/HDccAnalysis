High-Dimensional Case-Cohort Analysis tools in R.

## Features

- Simulate attained-age survival data for Cox PH models with constant or
  piecewise baseline hazards (`sim_cox_age_data`).
- Scale baseline hazards to target incidence rates.
- Utilities for case-cohort designs and weighting schemes (Prentice,
  Barlow, Binder).

## Installation

``` r
# From GitHub
remotes::install_github("IARCBiostat/HDccAnalysis")

# Or from a local checkout
remotes::install_local(".", upgrade = "never")
```

## Documentation

### Data simulations

The
[HDccAnalysis::sim_cox_age_data()](https://IARCBiostat.github.io/HDccAnalysis/reference/sim_cox_age_data.md)
function can be used to simulate survival data with different incidence
rates. You can follow these vignettes to simulate survival data:

- Vignette: [Simulating
  data](https://IARCBiostat.github.io/HDccAnalysis/articles/simulating-data.md)
  — walkthrough of data generation with a scalar baseline hazard.
- Reference: function help pages generated from the `R/` source.

## Contributing

Issues and pull requests are welcome at the GitHub repository.
