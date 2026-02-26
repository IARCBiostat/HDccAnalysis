# Age-specific cancer incidence rates for males and females in France in 2008, extracted and processed from publicly available Santé publique France (SPF) cancer surveillance data.

Age-specific cancer incidence rates for males and females in France in
2008, extracted and processed from publicly available Santé publique
France (SPF) cancer surveillance data.

## Format

A data frame with the following columns:

- age_lo:

  Lower bound of the age band (years).

- age_hi:

  Upper bound of the age band (years).

- rate:

  Incidence rate for the corresponding age band (per person-year).

- site:

  Cancer site identifier (e.g. "colon_rectum").

- sex:

  Sex ("male", "female").

## Source

Santé publique France (SPF). Cancer incidence, mortality, and survival
estimates.
<http://invs.santepubliquefrance.fr/Dossiers-thematiques/Maladies-chroniques-et-traumatismes/Cancers/Surveillance-epidemiologique-des-cancers/Estimations-de-l-incidence-de-la-mortalite-et-de-la-survie-stade-au-diagnostic>

## Details

The table was derived from published cancer incidence estimates produced
by Santé publique France. The original data were processed and
reformatted to provide age-band–specific incidence rates suitable for
simulation and methodological examples.
