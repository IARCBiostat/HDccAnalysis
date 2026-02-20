.stop1 <- function(...) stop(..., call. = FALSE)

.as_len_n <- function(v, n, name) {
    if (length(v) == 1L) v <- rep(v, n)
    if (length(v) != n) .stop1("`", name, "` must have length 1 or n.")
    v
}

.check_ages <- function(entry_age, censor_age) {
    if (any(!is.finite(entry_age)) || any(!is.finite(censor_age))) {
        .stop1("Non-finite values in `entry_age`/`censor_age`.")
    }
    if (any(censor_age <= entry_age)) .stop1("All `censor_age` must be > `entry_age`.")
    invisible(TRUE)
}

.as_x_matrix <- function(x, n) {
    if (is.null(x)) .stop1("Provide `x`.")
    x <- as.matrix(x)
    if (nrow(x) != n) .stop1("`x` must have n rows.")
    storage.mode(x) <- "double"
    x
}

.check_beta <- function(beta, p) {
    beta <- as.numeric(beta)
    if (length(beta) != p) .stop1("Length of `beta` must match ncol(x).")
    beta
}

#' Validate or construct an attained-age baseline hazard table
#'
#' Ensures that a valid attained-age baseline hazard table is available.
#' Either a piecewise-constant hazard table
#' (\code{baseline_hazard_by_age}) must be supplied, or a single constant
#' baseline hazard (\code{constant_baseline_hazard}) must be provided.
#'
#' If a constant baseline hazard is given, a one-row table covering
#' \code{(-Inf, Inf)} is constructed.
#'
#' If a table is supplied, it is validated, coerced to numeric, ordered by
#' \code{age_lo}, and returned.
#'
#' @param baseline_hazard_by_age Optional data.frame with columns
#'   \code{age_lo}, \code{age_hi}, and \code{rate}, defining
#'   piecewise-constant baseline hazard bands
#'   \code{[age_lo, age_hi)}.
#' @param constant_baseline_hazard Optional positive scalar giving a
#'   constant baseline hazard over all attained ages.
#'
#' @return A data.frame with columns \code{age_lo}, \code{age_hi}, and
#'   \code{rate}, sorted by \code{age_lo}.
#'
#' @details
#' Exactly one of \code{baseline_hazard_by_age} or
#' \code{constant_baseline_hazard} must be non-\code{NULL}. Supplying both
#' or neither results in an error.
#'
#' All age bands must satisfy \code{age_hi > age_lo} and all baseline
#' hazard \code{rate} values must be strictly positive.
#'
#' @examples
#' .make_age_rate_table(
#'   baseline_hazard_by_age = data.frame(
#'     age_lo = c(50, 60),
#'     age_hi = c(60, 70),
#'     rate   = c(0.01, 0.02)
#'   ),
#'   constant_baseline_hazard = NULL
#' )
#'
#' .make_age_rate_table(
#'   baseline_hazard_by_age = NULL,
#'   constant_baseline_hazard = 0.01
#' )
.make_age_rate_tbl <- function(baseline_hazard_by_age,
                                 constant_baseline_hazard) {

    required_columns <- c("age_lo", "age_hi", "rate")

    if (is.null(baseline_hazard_by_age)) {

        if (is.null(constant_baseline_hazard)) {
            .stop1("Provide `baseline_hazard_by_age` or ",
                   "`constant_baseline_hazard`.")
        }

        if (length(constant_baseline_hazard) != 1L ||
            !is.finite(constant_baseline_hazard) ||
            constant_baseline_hazard <= 0) {
            .stop1("`constant_baseline_hazard` must be a single ",
                   "positive number.")
        }

        return(data.frame(
            age_lo = -Inf,
            age_hi =  Inf,
            rate   = as.numeric(constant_baseline_hazard),
            stringsAsFactors = FALSE
        ))
    }

    if (!all(required_columns %in% names(baseline_hazard_by_age))) {
        .stop1("`baseline_hazard_by_age` must contain columns: ",
               "age_lo, age_hi, rate.")
    }

    age_rate_table <- baseline_hazard_by_age[, required_columns]

    age_rate_table$age_lo <- as.numeric(age_rate_table$age_lo)
    age_rate_table$age_hi <- as.numeric(age_rate_table$age_hi)
    age_rate_table$rate   <- as.numeric(age_rate_table$rate)

    age_rate_table <- age_rate_table[
        order(age_rate_table$age_lo),
        ,
        drop = FALSE
    ]

    if (any(!is.finite(age_rate_table$age_lo)) ||
        any(!is.finite(age_rate_table$age_hi)) ||
        any(!is.finite(age_rate_table$rate))) {
        .stop1("Non-finite values in `baseline_hazard_by_age`.")
    }

    if (any(age_rate_table$age_hi <= age_rate_table$age_lo)) {
        .stop1("All age bands must satisfy age_hi > age_lo.")
    }

    if (any(age_rate_table$rate <= 0)) {
        .stop1("All baseline hazard `rate` values must be > 0.")
    }

    age_rate_table
}

#' Compute follow-up overlap with an attained-age band
#'
#' Computes the amount of follow-up time contributed within a single
#' attained-age band.
#'
#' Specifically, this function returns the length of the intersection
#' between the subject's follow-up interval
#' \code{[entry_age, exit_age)} and the attained-age band
#' \code{[band_lower_age, band_upper_age)}.
#'
#' All intervals are treated as left-closed, right-open.
#'
#' @param band_lower_age Numeric scalar or vector giving the lower
#'   boundary of the attained-age band.
#' @param band_upper_age Numeric scalar or vector giving the upper
#'   boundary of the attained-age band.
#' @param entry_age Numeric scalar or vector giving age at entry.
#' @param exit_age Numeric scalar or vector giving age at exit
#'   (event or censoring).
#'
#' @return Numeric scalar or vector giving the overlap duration.
#'   Returns 0 where the intervals do not intersect.
#'
#' @examples
#' .overlap_weight(
#'   band_lower_age = 50,
#'   band_upper_age = 60,
#'   entry_age = 55,
#'   exit_age = 65
#' )
.overlap_weight <- function(band_lower_age,
                            band_upper_age,
                            entry_age,
                            exit_age) {

    pmax(
        0,
        pmin(exit_age, band_upper_age) -
            pmax(entry_age, band_lower_age)
    )
}

#' Scale baseline hazard to a target average incidence rate
#'
#' Adjusts the rates in an attained-age baseline hazard table so that the
#' cohort's **expected** average incidence rate equals a user-specified target.
#' The expectation is taken under a piecewise-constant proportional hazards
#' model with subject-specific multiplier \code{exp_eta}.
#'
#' The function finds a single scaling factor \eqn{s} applied to all baseline
#' rates \eqn{\lambda_k} such that
#' \deqn{
#'   \frac{\mathbb{E}[N(s)]}{\mathbb{E}[T(s)]} = \texttt{avg\_inc\_rate},
#' }
#' where \eqn{\mathbb{E}[N(s)] = \sum_i \{1 - \exp(-H_i(s))\}} is the expected
#' number of events and \eqn{\mathbb{E}[T(s)]} is the expected time at risk,
#' both implied by the scaled hazards over each subject's follow-up window
#' \code{[entry_age, censor_age)} and the attained-age bands
#' \code{[age_lo_k, age_hi_k)}.
#'
#' All baseline rates are multiplied by the same factor, so relative hazards
#' are unchanged; only the marginal event rate is altered.
#'
#' @param tbl data.frame with columns \code{age_lo}, \code{age_hi}, \code{rate}.
#'   Defines the baseline hazard over attained age bands
#'   \code{[age_lo, age_hi)}.
#' @param entry_age Numeric vector of entry ages.
#' @param censor_age Numeric vector of exit/censoring ages.
#' @param avg_inc_rate Optional positive scalar giving the desired average
#'   incidence rate over the cohort's follow-up. If \code{NULL}, \code{tbl} is
#'   returned unchanged.
#' @param exp_eta Numeric vector of length \code{length(entry_age)} giving the
#'   subject-specific hazard multipliers \eqn{\exp(x\beta)} used when computing
#'   expected events and expected time at risk.
#'
#' @return A data.frame identical to \code{tbl} but with \code{rate} scaled so
#'   that the cohort's expected average incidence rate equals
#'   \code{avg_inc_rate}.
#'
#' @details
#' The target average incidence rate is defined for the cohort under the model
#' implied by \code{tbl} and \code{exp_eta}. Scaling is performed by solving for
#' the multiplicative factor using \code{\link[stats]{uniroot}}.
#'
#' If \code{avg_inc_rate} is non-\code{NULL} and not a positive finite number,
#' the function errors. If a bracketing interval for the root cannot be found,
#' the function errors.
#'
#' @seealso
#' \code{\link{sim_cox_age_data}}
#'
#' @examples
#' tbl <- data.frame(
#'   age_lo = c(50, 60),
#'   age_hi = c(60, 70),
#'   rate   = c(0.01, 0.02)
#' )
#'
#' entry_age  <- c(52, 55, 61)
#' censor_age <- c(65, 63, 69)
#' exp_eta    <- rep(1, length(entry_age))
#'
#' .scale_to_avg_inc(tbl, entry_age, censor_age, avg_inc_rate = 0.015, exp_eta)
.scale_to_avg_inc <- function(tbl, entry_age, censor_age, avg_inc_rate, exp_eta) {
  if (is.null(avg_inc_rate)) return(tbl)
  if (!is.finite(avg_inc_rate) || avg_inc_rate <= 0) {
    .stop1("`avg_inc_rate` must be a positive number.")
  }

  age_band_lower <- tbl$age_lo
  age_band_upper <- tbl$age_hi
  baseline_hazard_rate <- tbl$rate

  n_age_bands <- length(baseline_hazard_rate)
  n_individuals <- length(entry_age)

  # Precompute overlap durations: followup_overlap[i, k]
  followup_overlap <- matrix(0, nrow = n_individuals, ncol = n_age_bands)
  for (band_index in seq_len(n_age_bands)) {
    followup_overlap[, band_index] <- pmax(
      0,
      pmin(censor_age, age_band_upper[band_index]) -
        pmax(entry_age, age_band_lower[band_index])
    )
  }

  cohort_expected_incidence_rate <- function(scaling_factor) {
    # Cumulative hazard increments per individual and age band.
    cumulative_hazard_increment <- matrix(0, nrow = n_individuals, ncol = n_age_bands)
    for (band_index in seq_len(n_age_bands)) {
      cumulative_hazard_increment[, band_index] <-
        (scaling_factor * baseline_hazard_rate[band_index] * exp_eta) *
        followup_overlap[, band_index]
    }

    total_cumulative_hazard <- rowSums(cumulative_hazard_increment)
    expected_events <- sum(1 - exp(-total_cumulative_hazard))

    # Survival at the start of each age band.
    cumulative_hazard_before_band <- matrix(0, nrow = n_individuals, ncol = n_age_bands)
    if (n_age_bands > 1L) {
      cumulative_hazard_before_band[, 2:n_age_bands] <- t(apply(
        cumulative_hazard_increment[, 1:(n_age_bands - 1L), drop = FALSE],
        1,
        cumsum
      ))
    }
    survival_at_band_start <- exp(-cumulative_hazard_before_band)

    # Expected time at risk: sum_k S_start * (1 - exp(-h*dt)) / h
    expected_time_at_risk <- 0
    for (band_index in seq_len(n_age_bands)) {
      band_hazard <- scaling_factor * baseline_hazard_rate[band_index] * exp_eta
      band_overlap <- followup_overlap[, band_index]

      time_contribution <- ifelse(
        band_hazard > 0,
        survival_at_band_start[, band_index] *
          (1 - exp(-band_hazard * band_overlap)) / band_hazard,
        survival_at_band_start[, band_index] * band_overlap
      )
      expected_time_at_risk <- expected_time_at_risk + sum(time_contribution)
    }

    expected_events / expected_time_at_risk
  }

  root_function <- function(scaling_factor) {
    cohort_expected_incidence_rate(scaling_factor) - avg_inc_rate
  }

  # Bracket scaling_factor: small -> ~0 rate, large -> high rate
  scaling_lower <- 0
  scaling_upper <- 1
  while (root_function(scaling_upper) < 0 && scaling_upper < 1e8) {
    scaling_upper <- scaling_upper * 2
  }

  if (!is.finite(root_function(scaling_upper)) || root_function(scaling_upper) < 0) {
    .stop1("Failed to bracket a scaling factor for `avg_inc_rate`.")
  }

  scaling_solution <- stats::uniroot(
    root_function,
    lower = scaling_lower,
    upper = scaling_upper,
    tol = 1e-10
  )$root

  tbl$rate <- tbl$rate * scaling_solution
  tbl
}

#' Retrieve baseline hazard rate and band index at a given attained age
#'
#' Determines which attained-age band contains a given
#' \code{attained_age} and returns both the band index and the
#' corresponding baseline hazard rate.
#'
#' Age bands are defined by \code{lookup_table$lo} and
#' \code{lookup_table$hi} and are treated as left-closed,
#' right-open intervals: \code{[lo, hi)}. The final band is treated
#' as closed on the right to ensure coverage of its upper boundary.
#'
#' A numerical \code{tolerance} is used to avoid instability at band
#' boundaries due to floating-point rounding.
#'
#' @param attained_age Numeric scalar giving the current attained age.
#' @param lookup_table List containing numeric vectors \code{lo},
#'   \code{hi}, and \code{rate}, defining the lower bounds,
#'   upper bounds, and baseline hazard rates for each age band.
#' @param tolerance Small non-negative numeric scalar used when
#'   comparing \code{attained_age} to band boundaries.
#'
#' @return A list with components:
#' \describe{
#'   \item{band_index}{Integer index of the age band containing
#'   \code{attained_age}.}
#'   \item{baseline_rate}{Numeric scalar baseline hazard rate for the
#'   identified band.}
#' }
#'
#' If \code{attained_age} lies outside the support of the age bands,
#' both elements are returned as \code{NA}.
#'
#' @examples
#' lookup_table <- list(
#'   lo   = c(50, 60),
#'   hi   = c(60, 70),
#'   rate = c(0.01, 0.02)
#' )
#'
#' .rate_and_band_at_age(attained_age = 55,
#'                       lookup_table = lookup_table)
.rate_and_band_at_age <- function(attained_age, lookup_table, tol = 1e-12) {
  band_lower_bounds <- lookup_table$lo
  band_upper_bounds <- lookup_table$hi
  band_rates <- lookup_table$rate
  n_bands <- length(band_lower_bounds)

  # Outside support (with tolerance)
  if (attained_age < band_lower_bounds[1] - tol || attained_age > band_upper_bounds[n_bands] + tol) {
    return(list(band_index = NA_integer_, baseline_rate = NA_real_))
  }

  # Clamp right boundary into last band (closed on right)
  if (attained_age >= band_upper_bounds[n_bands] - tol) {
    return(list(band_index = n_bands, baseline_rate = band_rates[n_bands]))
  }

  # Prefer half-open membership: [band_lower_bounds, band_upper_bounds)
  band_index <- findInterval(attained_age, band_lower_bounds, rightmost.closed = FALSE)

  if (band_index == 0L) {
    return(list(band_index = NA_integer_, baseline_rate = NA_real_))
  }

  # If we're at/over the upper boundary for k, advance to next band
  if (attained_age >= band_upper_bounds[band_index] - tol) {
    band_index <- band_index + 1L
  }

  if (band_index < 1L || band_index > n_bands) {
    return(list(band_index = NA_integer_, baseline_rate = NA_real_))
  }

  list(band_index = band_index, baseline_rate = band_rates[band_index])
}

#' Simulate a single subject's event time under piecewise-constant hazard
#'
#' Simulates an event time for one subject followed from entry age
#' \code{entry_age} to administrative censoring age \code{censor_age}
#' under an attained-age baseline hazard that is piecewise constant over
#' age bands. The subject-specific hazard is
#' \deqn{
#'   \lambda(a) = \lambda_0(a) \exp(\eta),
#' }
#' where \code{exp_eta = exp(eta)} is the Cox proportional hazards multiplier.
#'
#' The simulation proceeds band-by-band. At the current attained age
#' \code{current_age}, the baseline hazard \eqn{\lambda_0(a)} is obtained
#' from the age band containing that age. A waiting time is drawn from an
#' exponential distribution with rate
#' \code{\lambda_0(a) * exp_eta}. If the waiting time occurs before the end
#' of the current age band (or before \code{censor_age}), an event occurs.
#' Otherwise the subject advances to the next band boundary and the procedure
#' repeats until \code{censor_age}.
#'
#' Age bands are treated as left-closed, right-open intervals:
#' \code{[age_lo, age_hi)}. An attained age equal to \code{age_hi} is assigned
#' to the next band.
#'
#' @param entry_age Numeric scalar giving the subject's age at study entry.
#' @param censor_age Numeric scalar giving the administrative censoring age.
#'   Must satisfy \code{censor_age > entry_age}.
#' @param exp_eta Positive scalar equal to \eqn{\exp(\eta)}, the
#'   subject-specific hazard multiplier.
#' @param lookup_table List produced by \code{.prep_lookup()}, containing
#'   numeric vectors \code{lo}, \code{hi}, and \code{rate} defining the
#'   attained-age baseline hazard bands.
#'
#' @return A list with components:
#' \describe{
#'   \item{age_exit}{Numeric scalar giving the exit age (event or censoring).}
#'   \item{event}{Integer scalar equal to 1 if an event occurred and 0 if
#'   administratively censored.}
#' }
#'
#' @details
#' The baseline hazard must be defined for all attained ages reachable in
#' \code{[entry_age, censor_age)}. If no age band contains the current
#' attained age, the function errors.
#'
#' The method is exact for piecewise-constant hazards because within each
#' age band the hazard is constant and the waiting time distribution is
#' exponential.
#'
#' @examples
#' lk <- list(
#'   lo = c(50, 60),
#'   hi = c(60, 70),
#'   rate = c(0.01, 0.02)
#' )
#'
#' set.seed(1)
#' .simulate_one(entry_age = 55,
#'               censor_age = 65,
#'               exp_eta = 1.5,
#'               lookup_table = lk)
.simulate_one <- function(entry_age,
                          censor_age,
                          exp_eta,
                          lookup_table) {

    current_age <- entry_age

    while (current_age < censor_age) {

        rate_info <- .rate_and_band_at_age(current_age, lookup_table)

        if (!is.finite(rate_info$baseline_rate)) {
            .stop1("No baseline rate at attained age ", current_age, ".")
        }

        baseline_rate_current_band <- rate_info$baseline_rate
        band_index <- rate_info$band_index

        band_upper_age <- lookup_table$hi[band_index]
        band_exit_age <- min(censor_age, band_upper_age)

        time_available_in_band <- band_exit_age - current_age

        waiting_time <- stats::rexp(
            1L,
            rate = baseline_rate_current_band * exp_eta
        )

        if (waiting_time < time_available_in_band) {
            return(list(
                age_exit = current_age + waiting_time,
                event = 1L
            ))
        }

        current_age <- band_exit_age
    }

    list(age_exit = censor_age, event = 0L)
}
