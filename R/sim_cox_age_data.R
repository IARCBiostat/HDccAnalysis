#' Simulate attained-age survival data under a Cox PH model
#'
#' @param n Integer, number of subjects.
#' @param entry_age Numeric scalar or length-n vector of entry ages.
#' @param censor_age Numeric scalar or length-n vector of administrative
#'   censoring ages.
#' @param beta Numeric vector of regression coefficients with length p.
#' @param covariates Numeric matrix or data.frame with n rows and p columns.
#' @param baseline_hazard Optional positive scalar specifying a constant
#'   baseline hazard over attained age. Must be NULL if
#'   `baseline_hazard_by_age` is provided.
#' @param baseline_hazard_by_age Optional data.frame with columns
#'   age_lo, age_hi, hazard defining piecewise-constant baseline hazard
#'   over attained age intervals [age_lo, age_hi). Must be NULL if
#'   `baseline_hazard` is provided.
#' @param target_avg_baseline_hazard Optional positive scalar. If provided,
#'   rescales the baseline hazard so the cohort-weighted average equals this
#'   value.
#'
#' @details
#' Exactly one of `baseline_hazard` or `baseline_hazard_by_age`
#' must be specified (i.e. one must be non-NULL). Supplying both or
#' neither results in an error.
#'
#' @return data.frame with id, entry_age, censor_age, time, event and covariates.
#' @export
sim_cox_age_data <- function(n,
                             entry_age,
                             censor_age,
                             beta,
                             covariates,
                             baseline_hazard = 0.004,
                             baseline_hazard_by_age = NULL,
                             target_avg_baseline_hazard = NULL){
    if (length(n) != 1L || !is.finite(n) || n <= 0) {
        .stop1("`n` must be a positive integer.")
    }
    n <- as.integer(n)

    entry_age <- .as_len_n(entry_age, n, "entry_age")
    censor_age <- .as_len_n(censor_age, n, "censor_age")
    .check_ages(entry_age, censor_age)

    x <- .as_x_matrix(covariates, n)
    p <- ncol(x)
    beta <- .check_beta(beta, p)
    exp_eta <- exp(as.numeric(x %*% beta))

    tbl <- .make_age_rate_tbl(baseline_hazard_by_age, baseline_hazard)
    # tbl <- .scale_to_avg_inc(tbl, age_in, age_out, avg_inc_rate)
    tbl <- .scale_to_avg_inc(tbl, entry_age, censor_age, target_avg_baseline_hazard, exp_eta)
    lookup_table <- list(lo = tbl$age_lo, hi = tbl$age_hi, rate = tbl$rate)

    if (min(entry_age) < min(lookup_table$lo) || max(censor_age) > max(lookup_table$hi)) {
        .stop1("Cohort ages fall outside `baseline_hazard_by_age` support.")
    }


    event <- integer(n)
    age_exit <- numeric(n)

    for (i in seq_len(n)) {
        si <- .simulate_one(entry_age[i], censor_age[i], exp_eta[i], lookup_table)
        age_exit[i] <- si$age_exit
        event[i] <- si$event
    }

    time <- age_exit - entry_age
    colnames(x) <- if (is.null(colnames(x))) paste0("Exp", seq_len(p)) else colnames(x)

    out <- data.frame(
        id = seq_len(n),
        entry_age = entry_age,
        censor_age = age_exit,
        event = event,
        stringsAsFactors = FALSE
    )
    out <- cbind(out, as.data.frame(x, stringsAsFactors = FALSE))
    attr(out, "achieved_incidence_rate") <- sum(event) / sum(time)
    return(tibble::as_tibble(out))
}
