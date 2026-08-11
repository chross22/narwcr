#' Record one validation finding
#'
#' Builds a single row of the report that [validate_narwc()] returns. Packages
#' that add their own checks use this so that their findings have the same
#' shape as the handbook-general ones and can be bound together with them.
#'
#' @param check Name of the check, used as the `check` column.
#' @param severity `"error"`, `"warning"`, or `"note"`.
#' @param column The column involved, or `NA`.
#' @param rows Integer vector of affected row indices. Stored capped at 100.
#' @param message Human-readable description of the problem.
#' @param n Number of records affected. Defaults to `length(rows)`; give it
#'   explicitly for a finding that is about the table rather than particular
#'   rows, such as a column being absent altogether.
#'
#' @return A one-row tibble.
#'
#' @examples
#' narwc_finding(
#'   "my_check", "warning", "ALT", rows = c(3L, 7L),
#'   message = "ALT looks implausible."
#' )
#'
#' @export
narwc_finding <- function(check, severity, column = NA_character_,
                          rows = integer(0), message, n = length(rows)) {
  tibble::tibble(
    check = check,
    severity = severity,
    column = column %|NA|% NA_character_,
    n = n,
    rows = list(utils::head(rows, 100L)),
    message = message
  )
}

# A finding only when there is something to report. Checks are written as a
# series of these, so an empty result falls out rather than being tested for.
flag <- function(check, severity, column, rows, message) {
  if (!length(rows)) {
    return(NULL)
  }
  narwc_finding(check, severity, column, rows, message)
}

#' The handbook-general check set
#'
#' Returns the checks that [validate_narwc()] runs by default: those that
#' follow from the NARWC handbook itself, and so are true of any NARWC extract
#' regardless of what it will be used for.
#'
#' @section Adding your own:
#' A check is a function of one argument, the data frame, returning findings
#' built with [narwc_finding()] — `NULL`, one finding, or a list of them.
#' Because the set is an ordinary named list, a package adds to it by
#' concatenating:
#'
#' ```r
#' validate_narwc(dat, checks = c(narwc_checks(), my_checks()))
#' ```
#'
#' and drops one it does not want by name:
#'
#' ```r
#' keep <- setdiff(names(narwc_checks()), "time_format")
#' validate_narwc(dat, checks = narwc_checks()[keep])
#' ```
#'
#' Checks that depend on what the data will be *used for* belong in the package
#' that uses it, not here. `distsamp` keeps the ones about declination angles
#' and exact sighting positions for that reason: they are only problems if you
#' are computing a right-angle distance.
#'
#' @return A named list of functions.
#'
#' @examples
#' names(narwc_checks())
#'
#' @export
narwc_checks <- function() {
  list(
    required_columns   = check_required_columns,
    code_books         = check_code_books,
    legstage_placement = check_legstage_placement,
    legstage_sequence  = check_legstage_sequence,
    event_order        = check_event_order,
    time_format        = check_time_format,
    coordinates        = check_coordinates,
    sighting_counts    = check_sighting_counts,
    sightno_without_species = check_sightno_without_species,
    sightno_duplicated = check_sightno_duplicates,
    sightno_non_target = check_sightno_non_target,
    exact_position     = check_exact_position,
    extra_columns      = check_extra_columns
  )
}

#' Check survey data against the NARWC handbook
#'
#' Runs a set of structural and code-book checks against a standardised NARWC
#' data frame and reports every problem found. Validation never stops on error
#' and never modifies the data: the result is a report you read, so that you can
#' decide which problems matter for your analysis.
#'
#' @section Checks performed:
#' By default, the handbook-general set from [narwc_checks()]:
#' \describe{
#'   \item{`missing_required`}{A column in [narwc_schema()]`$required` is absent.
#'     Severity `error`.}
#'   \item{`missing_values`}{`NA` in a required column.}
#'   \item{`unknown_code`}{A value of `LEGTYPE`, `LEGSTAGE`, `IDREL`, `TAXCODE`,
#'     or `STRATUM` that is not in the handbook's code book.}
#'   \item{`legstage_off_census`}{`LEGSTAGE` recorded on a record that is not a
#'     census line. Handbook 8.A.20: for dedicated aerial surveys `LEGSTAGE` is
#'     recorded only during census tracks (`LEGTYPE == 2`), except for code 7.}
#'   \item{`sighting_at_boundary`}{A sighting recorded at a `LEGSTAGE` of 1, 3,
#'     4, or 5. Handbook 8.A.20 and 4.2: sightings should not occur at
#'     begin-line, break-off, resume, or end-line events.}
#'   \item{`legstage_sequence`}{`LEGSTAGE` does not follow a logical order
#'     within a line occupation. Handbook 8.A.20: a line begins (1), continues
#'     (2), may break off to circle (3) and resume (4), and ends (5). A line
#'     must begin with 1, nothing may follow an end-line, and a resume cannot
#'     appear without a break-off before it.}
#'   \item{`legstage_break_off_unresumed`}{A line ends at a break-off (3) with
#'     no resume and no end-line: the aircraft left the census line and the
#'     record never brings it back.}
#'   \item{`legstage_line_not_closed`}{A line has no end-line (5). A note
#'     rather than a warning, because a line abandoned for weather or re-flown
#'     later legitimately has none.}
#'   \item{`eventno_not_increasing`}{`EVENTNO` does not increase through a
#'     `FILEID`. Repeated values are allowed — the handbook (4.2) assigns one
#'     event several sightings — but decreases indicate mis-sorted records.}
#'   \item{`sightno_without_species`}{`SIGHTNO` is set on records with no
#'     `SPECCODE`. Handbook 8.A.27: data-logging programs number every forced
#'     record — line starts, weather and altitude changes — not only sightings,
#'     and those numbers are meant to be cleared during processing. A file that
#'     still carries them will overcount detections.}
#'   \item{`sightno_duplicated`}{`SIGHTNO` is repeated within a `FILEID`, which
#'     handbook 8.A.27 does not allow and calls a recurring problem in
#'     submitted datasets. `999` is excluded, being deliberate. Anything keyed
#'     on `FILEID` and `SIGHTNO` will match the wrong record.}
#'   \item{`sightno_non_target`}{`SIGHTNO` is `999`, the CETAP marker for
#'     non-target species — seals, sharks, sunfish — recorded so they could be
#'     removed before analysis (handbook 8.A.27). A note, not a warning: the
#'     file is correct, and duplicates of it are expected. The analysis has to
#'     exclude them.}
#'   \item{`bad_time_format`}{`TIME` is not a 6-digit `hhmmss` in 24-hour form
#'     (handbook 8.A.37). Four-digit `hhmm` times are reported separately as a
#'     warning since they are still found in older data.}
#'   \item{`coordinates_out_of_range`}{Latitude outside \[-90, 90\] or longitude
#'     outside \[-180, 180\].}
#'   \item{`positive_west_longitude`}{Every longitude is positive. Handbook
#'     8.A.22 requires west longitudes to be negative; all-positive longitudes
#'     in a western North Atlantic dataset mean the sign convention was lost.}
#'   \item{`sighting_without_number`}{`SPECCODE` present but `NUMBER` missing.
#'     Handbook 8.A.24 requires `NUMBER` for all sightings.}
#'   \item{`exact_position_out_of_range`}{`S_LAT` or `S_LONG` outside the range
#'     a coordinate can take. Handbook 8.A.33 and 8.A.34 give the exact sighting
#'     position in decimal degrees.}
#'   \item{`columns_outside_handbook`}{Columns present that are not NARWC
#'     handbook variables. Survey programmes add their own; they are carried
#'     through uninterpreted, and one that encodes position, effort, or distance
#'     must be mapped explicitly. See [narwc_profiles()].}
#' }
#'
#' Checks specific to a particular analysis live with that analysis; see
#' [narwc_checks()] for how to add them.
#'
#' @param dat A data frame of NARWC survey data, ideally from [read_narwc()].
#' @param checks A named list of check functions. Defaults to [narwc_checks()].
#'
#' @return A tibble with one row per problem found, and columns:
#'   \describe{
#'     \item{`check`}{Name of the check, as listed above.}
#'     \item{`severity`}{`"error"`, `"warning"`, or `"note"`.}
#'     \item{`column`}{The column involved, or `NA`.}
#'     \item{`n`}{Number of records affected.}
#'     \item{`rows`}{List column of affected row indices (capped at 100).}
#'     \item{`message`}{Human-readable description.}
#'   }
#'   A zero-row tibble means every check passed.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*. NARWC Reference Document
#' 2023-01. University of Rhode Island, Graduate School of Oceanography. Every
#' check above cites the section it derives from.
#'
#' Kenney, R.D. (2002) *Quality-control Issues for Data Submissions to the North
#' Atlantic Right Whale Consortium Database.* NARWC Reference Document 2002-02.
#'
#' @seealso [narwc_checks()] for the default set, [narwc_finding()] for writing
#'   your own.
#'
#' @examples
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' issues <- validate_narwc(read_narwc(path))
#' issues[, c("check", "severity", "n")]
#'
#' # Run only the code-book checks
#' validate_narwc(read_narwc(path), checks = narwc_checks()["code_books"])
#'
#' @export
validate_narwc <- function(dat, checks = narwc_checks()) {
  stopifnot(is.data.frame(dat))
  if (!is.list(checks) || (length(checks) && !all(vapply(checks, is.function, logical(1))))) {
    rlang::abort("`checks` must be a list of functions; see `narwc_checks()`.")
  }

  # A check may return nothing, one finding, or a list of them. A tibble is
  # itself a list, so a bare one has to be wrapped before flattening or
  # `unlist()` would take it apart into its columns.
  out <- lapply(checks, function(f) {
    res <- f(dat)
    if (is.data.frame(res)) list(res) else res
  })
  out <- Filter(Negate(is.null), unlist(out, recursive = FALSE))

  if (!length(out)) {
    return(tibble::tibble(
      check = character(), severity = character(), column = character(),
      n = integer(), rows = list(), message = character()
    ))
  }
  dplyr::bind_rows(out)
}


# --- the handbook-general checks --------------------------------------------
#
# Each takes the data frame and returns a list of findings. They are ordinary
# functions rather than methods so that a caller can pick individual ones out
# of `narwc_checks()` by name.

check_required_columns <- function(dat) {
  schema <- narwc_schema()
  out <- list()

  for (nm in setdiff(schema$required, names(dat))) {
    # A missing column has no affected rows, so `n` must be given explicitly.
    out[[length(out) + 1L]] <- narwc_finding(
      "missing_required", "error", nm, integer(0),
      paste0("Required column `", nm, "` is absent."),
      n = NA_integer_
    )
  }

  for (nm in intersect(schema$required, names(dat))) {
    out[[length(out) + 1L]] <- flag(
      "missing_values", "error", nm, which(is.na(dat[[nm]])),
      paste0("Required column `", nm, "` contains missing values.")
    )
  }

  out
}

check_code_books <- function(dat) {
  lapply(c("LEGTYPE", "LEGSTAGE", "IDREL", "TAXCODE", "STRATUM"), function(nm) {
    if (!nm %in% names(dat)) {
      return(NULL)
    }
    allowed <- names(narwc_codes(nm))
    observed <- as.character(dat[[nm]])
    flag(
      "unknown_code", "warning", nm,
      which(!is.na(observed) & !observed %in% allowed),
      paste0(
        "`", nm, "` contains values outside the handbook code book (permitted: ",
        paste(allowed, collapse = ", "), ")."
      )
    )
  })
}

check_legstage_placement <- function(dat) {
  out <- list()

  if (all(c("LEGTYPE", "LEGSTAGE") %in% names(dat))) {
    out[[length(out) + 1L]] <- flag(
      "legstage_off_census", "note", "LEGSTAGE",
      which(
        !is.na(dat$LEGSTAGE) & dat$LEGSTAGE != 7 &
          !is.na(dat$LEGTYPE) & dat$LEGTYPE != 2
      ),
      paste0(
        "LEGSTAGE recorded on records that are not census lines. Handbook ",
        "8.A.20: for dedicated aerial surveys LEGSTAGE is recorded only ",
        "during LEGTYPE 2, except code 7."
      )
    )
  }

  if (all(c("LEGSTAGE", "SPECCODE") %in% names(dat))) {
    out[[length(out) + 1L]] <- flag(
      "sighting_at_boundary", "warning", "LEGSTAGE",
      which(!is.na(dat$SPECCODE) & dat$LEGSTAGE %in% c(1, 3, 4, 5)),
      paste0(
        "Sightings recorded at LEGSTAGE 1, 3, 4, or 5. Handbook 8.A.20: ",
        "sightings should not occur at begin-line, break-off, resume, or ",
        "end-line events."
      )
    )
  }

  out
}

check_legstage_sequence <- function(dat) {
  seq_check <- legstage_sequence_check(dat)

  list(
    flag(
      "legstage_sequence", "warning", "LEGSTAGE", seq_check$bad,
      paste0(
        "LEGSTAGE does not follow a logical sequence within a line occupation. ",
        "Handbook 8.A.20: a line begins (1), continues (2), may break off to ",
        "circle (3) and resume (4), and ends (5). A line must begin with 1, ",
        "nothing may follow an end-line, and a break-off must be resumed."
      )
    ),
    flag(
      "legstage_break_off_unresumed", "warning", "LEGSTAGE", seq_check$dangling,
      paste0(
        "A line occupation ends at LEGSTAGE 3, a break off to circle, with no ",
        "resume (4) and no end-line (5). The aircraft left the census line and ",
        "the record never brings it back."
      )
    ),
    flag(
      "legstage_line_not_closed", "note", "LEGSTAGE", seq_check$open,
      paste0(
        "A line occupation has no end-line (LEGSTAGE 5). This is normal for a ",
        "line abandoned mid-flight - for weather, or to re-fly it later - and a ",
        "problem only if the line was flown to completion."
      )
    )
  )
}

check_event_order <- function(dat) {
  if (!all(c("FILEID", "EVENTNO") %in% names(dat))) {
    return(NULL)
  }

  bad <- integer(0)
  for (fid in unique(dat$FILEID)) {
    idx <- which(dat$FILEID == fid)
    ev <- dat$EVENTNO[idx]
    drops <- which(diff(ev) < 0)
    if (length(drops)) bad <- c(bad, idx[drops + 1L])
  }

  list(flag(
    "eventno_not_increasing", "warning", "EVENTNO", sort(bad),
    "EVENTNO decreases within a FILEID; records may be mis-sorted."
  ))
}

check_time_format <- function(dat) {
  if (!"TIME" %in% names(dat)) {
    return(NULL)
  }

  tm <- dat$TIME
  ok <- is.na(tm) | (tm >= 0 & tm <= 235959)

  list(
    flag(
      "bad_time_format", "warning", "TIME", which(!ok),
      "TIME outside the range of a valid hhmmss clock time (handbook 8.A.37)."
    ),
    flag(
      "bad_time_format", "note", "TIME",
      which(!is.na(tm) & tm > 0 & tm <= 2359 & tm %% 1 == 0),
      paste0(
        "TIME values look like four-digit hhmm rather than six-digit hhmmss ",
        "(handbook 8.A.37). Ambiguous with early-morning hhmmss times."
      )
    )
  )
}

check_coordinates <- function(dat) {
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(dat))) {
    return(NULL)
  }

  out <- list(flag(
    "coordinates_out_of_range", "error", "LATITUDE/LONGITUDE",
    which(
      (!is.na(dat$LATITUDE) & abs(dat$LATITUDE) > 90) |
        (!is.na(dat$LONGITUDE) & abs(dat$LONGITUDE) > 180)
    ),
    "Latitude outside [-90, 90] or longitude outside [-180, 180]."
  ))

  # A whole-column finding: it is the absence of any negative longitude that is
  # wrong, so no individual row is at fault.
  lon <- dat$LONGITUDE[!is.na(dat$LONGITUDE)]
  if (length(lon) && all(lon > 0)) {
    out[[length(out) + 1L]] <- narwc_finding(
      "positive_west_longitude", "warning", "LONGITUDE", integer(0),
      paste0(
        "All longitudes are positive. Handbook 8.A.22 requires west ",
        "longitudes to be negative; the sign convention may have been lost."
      ),
      n = length(lon)
    )
  }

  out
}

check_sighting_counts <- function(dat) {
  if (!all(c("SPECCODE", "NUMBER") %in% names(dat))) {
    return(NULL)
  }

  list(flag(
    "sighting_without_number", "warning", "NUMBER",
    which(!is.na(dat$SPECCODE) & is.na(dat$NUMBER)),
    "SPECCODE present but NUMBER missing (handbook 8.A.24)."
  ))
}

# Whether an exact sighting position is a possible coordinate at all. Whether it
# is a *plausible* one — close enough to the event that logged it — depends on
# what the position is for, so that check belongs to the analysis package.
check_exact_position <- function(dat) {
  if (!all(c("S_LAT", "S_LONG") %in% names(dat))) {
    return(NULL)
  }

  s_lat <- suppressWarnings(as.numeric(dat$S_LAT))
  s_lon <- suppressWarnings(as.numeric(dat$S_LONG))

  list(flag(
    "exact_position_out_of_range", "error", "S_LAT/S_LONG",
    which((!is.na(s_lat) & abs(s_lat) > 90) |
            (!is.na(s_lon) & abs(s_lon) > 180)),
    paste0(
      "Exact sighting latitude outside [-90, 90] or longitude outside ",
      "[-180, 180]. Handbook 8.A.33 and 8.A.34 give these in decimal ",
      "degrees; degrees and decimal minutes will fail this check."
    )
  ))
}

check_extra_columns <- function(dat) {
  unknown <- unrecognised_columns(names(dat))
  if (!length(unknown)) {
    return(NULL)
  }

  hits <- matching_profiles(unknown)
  msg <- paste0(
    "Columns present that are not NARWC handbook variables: ",
    paste0("`", sort(unknown), "`", collapse = ", "), ". "
  )
  msg <- paste0(msg, if (length(hits)) {
    paste0(
      "Some are declared by the \"", hits[1], "\" profile; see ",
      "`narwc_profiles()`. They are carried through uninterpreted."
    )
  } else {
    paste0(
      "They are carried through uninterpreted. If any encodes position, ",
      "effort, or distance, it must be mapped explicitly - `narwcr` will ",
      "not infer meaning from a column name."
    )
  })

  list(narwc_finding(
    "columns_outside_handbook", "note",
    paste(sort(unknown), collapse = ", "), integer(0), msg,
    n = length(unknown)
  ))
}


# --- LEGSTAGE sequence ------------------------------------------------------
#
# Handbook 8.A.20 gives LEGSTAGE as the stage of a survey line, and the stages
# describe a progression rather than a set of independent labels: a line begins
# (1), continues (2), may break off to circle (3) and resume (4), and ends (5).
# Codes 6 and 7 mark a kind of sighting rather than a stage of the line, so they
# take no part in the sequence.
#
# The permitted transitions, [from, to]:
#
#        to:  1   2   3   4   5
#   from 1        x   x       x
#   from 2        x   x       x
#   from 3                x
#   from 4        x   x       x
#   from 5
#
# Nothing may follow an end-line, a break-off must be resumed, and a resume
# cannot appear without a break-off before it - each falls out of the table
# rather than being special-cased.
legstage_allowed <- local({
  m <- matrix(FALSE, nrow = 5, ncol = 5)
  m[1, c(2, 3, 5)] <- TRUE
  m[2, c(2, 3, 5)] <- TRUE
  m[3, 4] <- TRUE
  m[4, c(2, 3, 5)] <- TRUE
  m
})

# Rows whose LEGSTAGE cannot follow the one before it within the same line
# occupation, plus lines that never close.
#
# Occupations come from LEGNO3 where it exists. Without it they are derived the
# way make_leg_id() does, because grouping on LEGNO alone would merge a line
# flown, abandoned, and re-flown the same day into one sequence - and its second
# "begin line" would then look like a violation when it is the correct record of
# a second occupation.
#
# Assumes survey order. Records out of order are reported separately by
# `eventno_not_increasing`.
legstage_sequence_check <- function(dat) {
  empty <- list(bad = integer(0), open = integer(0), dangling = integer(0))
  if (!all(c("LEGSTAGE", "LEGNO") %in% names(dat)) || is_empty_df(dat)) {
    return(empty)
  }

  stage <- suppressWarnings(as.integer(dat$LEGSTAGE))
  on_census <- if ("LEGTYPE" %in% names(dat)) {
    is.na(dat$LEGTYPE) | dat$LEGTYPE == 2
  } else {
    rep(TRUE, nrow(dat))
  }

  # Structural stages only, on the census line. A circling record carries no
  # LEGSTAGE and so drops out here, which is what lets 3 -> 4 stay adjacent
  # across the excursion between them.
  keep <- which(!is.na(stage) & stage >= 1L & stage <= 5L & on_census)
  if (length(keep) < 1L) {
    return(empty)
  }

  occupation <- if ("LEGNO3" %in% names(dat)) {
    as.character(dat$LEGNO3)
  } else {
    legno <- as.character(dat$LEGNO)
    paste(legno, rle_id(legno), sep = "_")
  }
  day <- if ("DATE" %in% names(dat)) as.character(dat$DATE) else ""
  key <- paste(day, occupation)[keep]
  stage <- stage[keep]

  n <- length(stage)
  first <- c(TRUE, key[-1] != key[-n])
  last <- c(key[-1] != key[-n], TRUE)

  prev <- c(1L, stage[-n])
  prev[first] <- 1L # never used, but must index the matrix
  ok <- legstage_allowed[cbind(prev, stage)]

  list(
    bad = keep[(!first & !ok) | (first & stage != 1L)],
    open = keep[last & stage != 5L & stage != 3L],
    dangling = keep[last & stage == 3L]
  )
}


# Handbook 8.A.27. SIGHTNO is "required for all sighting records ... and is not
# allowed for non-sighting records". Data-logging programs assign one to every
# forced record - line starts, weather changes, altitude changes - and the
# handbook's own processing step deletes those "by searching for records where
# SPECCODE is missing but SIGHTNO > 0". A file that still carries them has not
# had that step run, and anything counting sighting numbers will overcount.
check_sightno_without_species <- function(dat) {
  if (!all(c("SIGHTNO", "SPECCODE") %in% names(dat))) {
    return(NULL)
  }

  bad <- which(!is.na(dat$SIGHTNO) & dat$SIGHTNO > 0 &
                 (is.na(dat$SPECCODE) | !nzchar(trimws(dat$SPECCODE))))

  list(flag(
    "sightno_without_species", "warning", "SIGHTNO", bad,
    paste0(
      "SIGHTNO is set on records with no SPECCODE. Data loggers number every ",
      "forced record, not only sightings (handbook 8.A.27); those numbers are ",
      "meant to be cleared during processing. These are not detections."
    )
  ))
}

# Handbook 8.A.27: "duplicate numbers within a file are not allowed", and
# "duplicate SIGHTNOs are a recurring problem in submitted datasets". 999 is
# excluded because it is a deliberate marker rather than a mistake, and is
# reported separately.
check_sightno_duplicates <- function(dat) {
  if (!all(c("FILEID", "SIGHTNO") %in% names(dat))) {
    return(NULL)
  }

  bad <- integer(0)
  for (fid in unique(dat$FILEID)) {
    idx <- which(dat$FILEID == fid & !is.na(dat$SIGHTNO) & dat$SIGHTNO != 999)
    sn <- dat$SIGHTNO[idx]
    # Records sharing an EVENTNO are one event with several sightings, which
    # is the legitimate case; their SIGHTNOs must still differ from each other.
    bad <- c(bad, idx[duplicated(sn) | duplicated(sn, fromLast = TRUE)])
  }
  bad <- sort(unique(bad))

  list(flag(
    "sightno_duplicated", "warning", "SIGHTNO", bad,
    paste0(
      "SIGHTNO is duplicated within a FILEID, which handbook 8.A.27 does not ",
      "allow. Sightings added after the fact are the usual cause. Anything ",
      "keyed on FILEID and SIGHTNO will match the wrong record."
    )
  ))
}

# Handbook 8.A.27: during CETAP, "sightings of non-target species (seals,
# sharks, sunfish, etc.) were assigned sighting numbers of 999 to facilitate
# removal prior to any analysis", and duplicate 999s are expected. A note, not
# a warning - the file is correct; it is the analysis that must exclude them.
check_sightno_non_target <- function(dat) {
  if (!"SIGHTNO" %in% names(dat)) {
    return(NULL)
  }

  bad <- which(!is.na(dat$SIGHTNO) & dat$SIGHTNO == 999)

  list(flag(
    "sightno_non_target", "note", "SIGHTNO", bad,
    paste0(
      "SIGHTNO 999 is the CETAP marker for non-target species - seals, ",
      "sharks, sunfish - recorded so they could be removed before analysis ",
      "(handbook 8.A.27). Duplicates of it are expected and are not an error. ",
      "Exclude these before estimating density."
    )
  ))
}
