#' Columns that may be carried forward, and columns that must not be
#'
#' `narwc_fill_columns()` returns the survey-state columns [fill_narwc()] fills
#' by default: variables recorded once and left blank until they change.
#'
#' @section Two kinds of column:
#' A NARWC record carries two kinds of variable, and the distinction decides
#' whether a blank may be filled.
#'
#' **State** persists until something changes it. `LEGTYPE` stays 2 until the
#' aircraft leaves the census line; `BEAUFORT` stays 3 until the sea state is
#' re-assessed. A blank means "as above", and filling it recovers information
#' that was deliberately not repeated.
#'
#' **Measurements** belong to their own record. A position, a time, a declination
#' angle, a species, a group size — each describes one moment. A blank means the
#' measurement was not taken, and filling it fabricates one.
#'
#' @section Filling a sighting column is the dangerous case:
#' `SPECCODE`, `NUMBER`, `SIGHTNO`, `STRIP`, `ANGLEL`, `ANGLER`, `S_LAT` and
#' `S_LONG` are refused outright. Carrying `SPECCODE` and `NUMBER` forward would
#' replicate one sighting onto every subsequent record until the next sighting —
#' turning a single group of three right whales into hundreds — and every count
#' downstream would be wrong by orders of magnitude. `EVENTNO`, `TIME`,
#' `LATITUDE`, and `LONGITUDE` are refused for the same reason: they are what
#' makes a record distinct from the one before it.
#'
#' @return `narwc_fill_columns()` returns a character vector of fillable column
#'   names; `narwc_never_fill()` returns those that are refused.
#'
#' @seealso [fill_narwc()]
#'
#' @examples
#' narwc_fill_columns()
#' narwc_never_fill()
#'
#' @export
narwc_fill_columns <- function() {
  c(
    # Leg state
    "LEGTYPE", "LEGSTAGE", "LEGNO",
    # Sighting conditions
    "VISIBLTY", "BEAUFORT", "CLOUD", "GLAREL", "GLARER", "WX", "SURFTEMP",
    # Platform state
    "ALT", "HEADING", "PLATFORM",
    # Survey design
    "STRATUM", "BLOCK"
  )
}

#' @rdname narwc_fill_columns
#' @export
narwc_never_fill <- function() {
  c(
    # Sightings: filling these replicates detections.
    "SPECCODE", "TAXCODE", "IDREL", "NUMBER", "NUMCALF", "SIGHTNO",
    "STRIP", "ANGLEL", "ANGLER", "S_LAT", "S_LONG", "S_TIME", "PHOTOS",
    # Per-record measurements and identifiers.
    "FILEID", "EVENTNO", "YEAR", "MONTH", "DAY", "TIME", "DATE",
    "LATITUDE", "LONGITUDE"
  )
}


#' Carry survey state forward into blank rows
#'
#' NARWC data often records a value once and leaves it blank until it changes —
#' `LEGTYPE` is entered as `2` at the start of a census line and the rows beneath
#' it are empty until the leg type changes. This fills those blanks, **within a
#' survey day and file**, so that downstream code sees the state each record was
#' actually flown under.
#'
#' @section Grouping is the whole point:
#' An ungrouped fill runs the length of the file. The sea state from the last
#' record of one survey day carries into the first records of the next; a leg
#' number carries across a `FILEID` boundary into a different survey entirely.
#' Neither is recoverable afterwards, because the filled value is
#' indistinguishable from a recorded one.
#'
#' The scripts this package was rewritten from filled these columns with no
#' grouping at all (`DataExploration.R:52`, `:71`). `by` defaults to `FILEID` and
#' `DATE`, and if neither is present this function warns rather than quietly
#' filling across everything.
#'
#' @section Direction, and what "up" actually does:
#' `tidyr::fill()` semantics. `"down"` is the direction the recording convention
#' justifies: a blank means "as above".
#'
#' `"downup"` fills down first and then fills up, so the only values it fills
#' backwards are those *before the first recorded value in a group*. That is a
#' smaller claim than it sounds, but it is still a guess: the state before
#' anything was logged is genuinely unknown, and back-filling asserts it matched
#' whatever was recorded first. Where a day's records begin on transit before the
#' first `LEGTYPE` is entered, back-filling a `2` would mark that transit as
#' census effort.
#'
#' Because those are the inferred values rather than the recovered ones, the
#' report counts them separately. A large backward count is worth looking at.
#'
#' @section A caution on `LEGSTAGE`:
#' Filling `LEGSTAGE` down is the least safe of the defaults. If a file records
#' `1` (begin line) and leaves the continuation rows blank, filling down marks
#' every record of that line as "begin line" rather than `2` (continue) — and
#' since on-effort eligibility is `LEGSTAGE == 2`, the whole line would drop out
#' of every distance calculation. Whether that happens depends on the recording
#' convention of the file in hand. Check the `LEGSTAGE` counts in the report
#' against what you expect before trusting a filled column.
#'
#' @param dat A data frame of NARWC survey data, in survey order.
#' @param columns Columns to fill. Defaults to [narwc_fill_columns()]. Anything
#'   in [narwc_never_fill()] is an error rather than a silent omission.
#' @param by Grouping columns. `NULL` (default) uses `FILEID` and `DATE` where
#'   present.
#' @param direction `"downup"` (default), `"down"`, or `"up"`, as
#'   [tidyr::fill()].
#' @param quiet Suppress the report of what was filled. Default `FALSE`.
#'
#' @return `dat` with the requested columns filled.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*. NARWC Reference Document
#' 2023-01.
#'
#' @seealso [narwc_fill_columns()], [read_narwc()], [validate_narwc()]
#'
#' @examples
#' dat <- data.frame(
#'   FILEID = "A", DATE = as.Date("2024-04-01"), EVENTNO = 1:5,
#'   LEGTYPE = c(2, NA, NA, 1, NA), BEAUFORT = c(3, NA, NA, NA, NA)
#' )
#' fill_narwc(dat)
#'
#' # Refuses to replicate a sighting
#' try(fill_narwc(dat, columns = "NUMBER"))
#'
#' @export
fill_narwc <- function(dat, columns = narwc_fill_columns(), by = NULL,
                       direction = c("downup", "down", "up"), quiet = FALSE) {
  direction <- match.arg(direction)
  stopifnot(is.data.frame(dat))

  refused <- intersect(columns, narwc_never_fill())
  if (length(refused)) {
    rlang::abort(paste0(
      "These columns must not be carried forward: ",
      paste0("`", refused, "`", collapse = ", "), ".\n",
      "They are per-record measurements, not survey state. Filling a sighting ",
      "column replicates one detection onto every row beneath it; filling a ",
      "position or time fabricates a measurement. See `?narwc_fill_columns`."
    ))
  }

  cols <- intersect(columns, names(dat))
  if (is_empty_df(dat) || !length(cols)) {
    return(dat)
  }

  if (is.null(by)) by <- default_fill_grouping(dat)
  if (!length(by)) {
    rlang::warn(paste0(
      "No `FILEID` or `DATE` column, so values are being carried across the ",
      "whole data frame - including across survey days and files, where they ",
      "cannot be undone. Pass `by` to group the fill."
    ))
  } else {
    require_columns(dat, by)
  }

  before <- vapply(dat[cols], function(x) sum(is.na(x)), integer(1))

  grouped <- if (length(by)) {
    dplyr::group_by(dat, dplyr::across(dplyr::all_of(by)))
  } else {
    dat
  }

  out <- grouped
  filled_down <- stats::setNames(integer(length(cols)), cols)
  if (direction %in% c("down", "downup")) {
    out <- tidyr::fill(out, dplyr::all_of(cols), .direction = "down")
    mid <- vapply(out[cols], function(x) sum(is.na(x)), integer(1))
    filled_down <- before - mid
  }

  filled_up <- stats::setNames(integer(length(cols)), cols)
  if (direction %in% c("up", "downup")) {
    mid <- vapply(out[cols], function(x) sum(is.na(x)), integer(1))
    out <- tidyr::fill(out, dplyr::all_of(cols), .direction = "up")
    after <- vapply(out[cols], function(x) sum(is.na(x)), integer(1))
    filled_up <- mid - after
  }

  out <- dplyr::ungroup(out)
  class(out) <- unique(c(setdiff(class(dat), class(out)), class(out)))

  if (!quiet) {
    report_filled(filled_down, filled_up, by)
  }
  out
}

# FILEID and DATE where present: one occupation of one survey day.
default_fill_grouping <- function(dat) {
  intersect(c("FILEID", "DATE"), names(dat))
}

# Say what was recovered and what was inferred. The backward count is the one
# worth reading: those values were guessed from the first record of a group.
report_filled <- function(filled_down, filled_up, by) {
  total <- sum(filled_down) + sum(filled_up)
  if (total == 0L) {
    return(invisible(NULL))
  }

  per <- filled_down + filled_up
  per <- sort(per[per > 0], decreasing = TRUE)

  lines <- paste0(
    "`fill_narwc()` filled ", total, " value", if (total > 1) "s" else "",
    if (length(by)) paste0(", grouped by ", paste(by, collapse = ", ")) else
      ", ungrouped",
    "."
  )
  lines <- c(lines, paste0("  carried forward:  ", sum(filled_down)))
  if (sum(filled_up) > 0) {
    lines <- c(lines, paste0(
      "  carried backward: ", sum(filled_up),
      "  <- inferred from the first recorded value in each group"
    ))
  }
  lines <- c(lines, paste0(
    "  ", paste(paste0(names(per), " ", per), collapse = ", ")
  ))

  rlang::inform(paste(lines, collapse = "\n"))
}
