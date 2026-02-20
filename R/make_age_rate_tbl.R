#' Reformat age-group incidence data into an age-rate lookup table
#'
#' @param dat A data.frame with columns `trage_invs` and `tx_inc_2008`.
#' @param trage_col Name of the age-band column.
#' @param rate_col Name of the incidence-rate column.
#' @param open_hi The upper bound to use for open-ended bands like "[85;++]".
#'   Must be finite; default 120.
#' @param interval One of "closed" (default) or "half-open".
#'   - "closed": interprets "[15;19]" as inclusive years 15–19, so age_hi = 20
#'   - "half-open": interprets as [15, 19), so age_hi = 19
#'
#' @return A data.frame with columns: age_lo, age_hi, rate.
#'
#' @examples
#' ex <- data.frame(
#'     trage_invs = c("[15;19]", "[20;24]", "[85;++]"),
#'     tx_inc_2008 = c(2.5e-06, 6.0e-06, 4.4e-03)
#' )
#' make_age_rate_tbl(ex)
#' @export
.make_age_rate_tbl <- function(dat,
                              trage_col = "trage_invs",
                              rate_col = "tx_inc_2008",
                              open_hi = 120,
                              interval = c("closed", "half-open")) {
    interval <- match.arg(interval)

    if (!is.data.frame(dat)) stop("`dat` must be a data.frame.")
    if (!trage_col %in% names(dat)) stop("Missing column: ", trage_col)
    if (!rate_col %in% names(dat)) stop("Missing column: ", rate_col)
    if (!is.finite(open_hi) || open_hi <= 0) stop("`open_hi` must be finite > 0.")

    tr <- as.character(dat[[trage_col]])
    rate <- as.numeric(dat[[rate_col]])

    # Strip spaces and outer brackets: "[15;19]" -> "15;19"
    tr0 <- gsub("\\s+", "", tr)
    tr0 <- gsub("^\\[|\\]$", "", tr0)

    parts <- strsplit(tr0, ";", fixed = TRUE)

    lo <- vapply(parts, function(x) suppressWarnings(as.integer(x[1])), integer(1))

    hi_raw <- vapply(
        parts,
        function(x) {
            if (length(x) < 2) {
                return(NA_character_)
            }
            x[2]
        },
        character(1)
    )

    hi <- suppressWarnings(as.integer(hi_raw))
    is_open <- is.na(hi) & hi_raw %in% c("++", "+", "INF", "Inf", "inf")

    if (any(is.na(lo))) stop("Failed to parse lower bounds from `", trage_col, "`.")
    if (any(is.na(hi) & !is_open)) {
        bad <- unique(tr[is.na(hi) & !is_open])
        stop("Failed to parse upper bounds for: ", paste(bad, collapse = ", "))
    }

    hi[is_open] <- as.integer(open_hi)

    # Convert to a consistent half-open band for lookup by attained age:
    # If bracket means inclusive integer ages, "[15;19]" -> [15,20)
    if (interval == "closed") {
        hi <- hi + 1L
    }

    out <- data.frame(
        age_lo = lo,
        age_hi = hi,
        rate = rate,
        stringsAsFactors = FALSE
    )

    out <- out[order(out$age_lo), , drop = FALSE]

    # Basic checks
    if (any(out$age_hi <= out$age_lo)) stop("Found band(s) with age_hi <= age_lo.")
    if (any(!is.finite(out$rate))) stop("Non-finite values in `", rate_col, "`.")
    if (any(out$rate < 0)) stop("Negative rates found in `", rate_col, "`.")

    out
}
