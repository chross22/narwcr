#' Was visibility acceptable?
#'
#' Interprets the NARWC `VISIBLTY` variable, which carries two different
#' encodings in the same column, and reports whether each record met a
#' visibility threshold.
#'
#' @section Why this is not a simple comparison:
#' Handbook 8.A.38 explains that `VISIBLTY` was originally a one-digit code
#' recording only whether visibility reached the 2-nautical-mile CETAP standard
#' and, if not, the weather responsible. In 2004 the field was redefined to hold
#' the actual estimated clear visibility in nautical miles. During the 2021
#' archive update the old codes were folded back into `VISIBLTY` **as negative
#' numbers**:
#'
#' \tabular{rl}{
#'   `-1` \tab clear visibility for at least 2 nautical miles \cr
#'   `-2` \tab less than 2 miles, fog \cr
#'   `-3` \tab less than 2 miles, haze \cr
#'   `-4` \tab less than 2 miles, rain \cr
#'   `-5` \tab less than 2 miles, snow
#' }
#'
#' So a plain `VISIBLTY >= 2` test — as used by the original processing code —
#' marks every legacy record as unacceptable, including `-1`, which actually
#' records *good* visibility. On a multi-year dataset that silently discards all
#' pre-2004 effort.
#'
#' A `-1` record asserts only that visibility reached 2 nmi, so it cannot
#' satisfy a threshold stricter than 2 and returns `NA` in that case rather than
#' a false `TRUE`.
#'
#' @param visibility Numeric vector of `VISIBLTY` values.
#' @param min_nmi Minimum acceptable clear visibility, in nautical miles.
#'   Defaults to `2`, the CETAP standard.
#'
#' @return A logical vector, `NA` where the record cannot answer the question.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, section 8.A.38. NARWC Reference
#' Document 2023-01.
#'
#' @examples
#' visibility_ok(c(5, 1.5, -1, -2, NA))
#'
#' # A legacy "clear" code cannot support a stricter threshold
#' visibility_ok(-1, min_nmi = 3)
#'
#' @export
visibility_ok <- function(visibility, min_nmi = 2) {
  v <- as.numeric(visibility)
  out <- rep(NA, length(v))

  legacy_clear <- !is.na(v) & v == -1
  legacy_poor  <- !is.na(v) & v <= -2 & v >= -5
  modern       <- !is.na(v) & v >= 0

  # -1 asserts "at least 2 nmi" and nothing more.
  out[legacy_clear] <- if (min_nmi <= 2) TRUE else NA
  out[legacy_poor]  <- FALSE
  out[modern]       <- v[modern] >= min_nmi

  out
}


#' Flag on-effort records
#'
#' Determines, for each record, whether the platform was on effort: on a
#' designated census track, in survey conditions good enough for the sightings
#' to be usable in a density estimate.
#'
#' @section Criteria:
#' A record is on effort when all of the following hold:
#'
#' * `LEGTYPE` is in `legtype_on_effort` — by default `2`, a line-transect
#'   survey line (handbook 8.A.21). Transits, cross-legs, and circling do not
#'   contribute effort to a density estimate.
#' * `BEAUFORT` is at most `max_beaufort`.
#' * `ALT` is below `max_alt_m`, in metres (handbook 8.A.1).
#' * Visibility clears `min_visibility_nmi`, via [visibility_ok()].
#'
#' A criterion whose column is absent from the data is skipped, with a message.
#' A criterion whose value is `NA` fails, unless `na_action = "pass"`.
#'
#' The defaults are the CETAP standard. Kenney and Winn (1986, p. 347) state the
#' criteria applied to that programme's data as "observer(s) formally on watch,
#' clear visibility of at least 2 miles, and sea states of Beaufort 3 or lower";
#' surveys were flown at 750 ft (229 m) (p. 346). The handbook (8.A.38) confirms
#' 2 nautical miles as the standard for acceptable survey conditions defined
#' during CETAP. Every threshold here is an argument, because a different
#' programme may reasonably choose differently.
#'
#' Records are *not* removed. Off-effort records are retained because the
#' distance between consecutive on-effort positions still needs them for
#' correct effort accounting, and because sightings made while circling are
#' attached back to the segment they came from.
#'
#' @param dat A data frame of NARWC survey data, ideally from [read_narwc()].
#' @param max_beaufort Highest acceptable Beaufort sea state. Default `3`.
#' @param max_alt_m Highest acceptable aircraft altitude in metres. Default
#'   `366`, which is 1,200 feet.
#' @param min_visibility_nmi Minimum acceptable visibility in nautical miles.
#'   Default `2`, the CETAP standard.
#' @param legtype_on_effort Integer vector of `LEGTYPE` values that count as
#'   effort. Default `2`.
#' @param na_action What to do when a criterion's value is missing: `"fail"`
#'   (default, treat the record as off effort) or `"pass"` (ignore the missing
#'   criterion).
#'
#' @return `dat` with an integer `OnOff.Effort` column added: `1` on effort,
#'   `0` off effort.
#'
#' @references
#' Kenney, R.D. and Winn, H.E. (1986) Cetacean high-use habitats of the
#' northeast United States continental shelf. *Fishery Bulletin* 84(2):345-357.
#'
#' CETAP (1982) *A Characterization of Marine Mammals and Turtles in the Mid- and
#' North-Atlantic Areas of the U.S. Outer Continental Shelf, Final Report.*
#' Cetacean and Turtle Assessment Program, University of Rhode Island. Bureau of
#' Land Management, Washington, DC.
#'
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*. NARWC Reference Document
#' 2023-01.
#'
#' @seealso [visibility_ok()], [on_effort_census_rows()]
#'
#' @examples
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' dat <- flag_effort(read_narwc(path))
#' table(dat$OnOff.Effort)
#'
#' # A stricter sea-state cutoff
#' table(flag_effort(read_narwc(path), max_beaufort = 2)$OnOff.Effort)
#'
#' @export
flag_effort <- function(dat,
                        max_beaufort = 3,
                        max_alt_m = 366,
                        min_visibility_nmi = 2,
                        legtype_on_effort = 2L,
                        na_action = c("fail", "pass")) {
  na_action <- match.arg(na_action)
  require_columns(dat, "LEGTYPE")

  if (is_empty_df(dat)) {
    dat$OnOff.Effort <- integer(0)
    return(dat)
  }

  n <- nrow(dat)
  resolve <- function(x) {
    if (na_action == "pass") {
      x[is.na(x)] <- TRUE
    } else {
      x[is.na(x)] <- FALSE
    }
    x
  }

  ok <- resolve(dat$LEGTYPE %in% legtype_on_effort)

  if ("BEAUFORT" %in% names(dat)) {
    ok <- ok & resolve(dat$BEAUFORT <= max_beaufort)
  } else {
    rlang::inform("No `BEAUFORT` column; sea-state criterion skipped.")
  }

  if ("ALT" %in% names(dat)) {
    ok <- ok & resolve(dat$ALT < max_alt_m)
  } else {
    rlang::inform("No `ALT` column; altitude criterion skipped.")
  }

  if ("VISIBLTY" %in% names(dat)) {
    ok <- ok & resolve(visibility_ok(dat$VISIBLTY, min_nmi = min_visibility_nmi))
  } else {
    rlang::inform("No `VISIBLTY` column; visibility criterion skipped.")
  }

  stopifnot(length(ok) == n)
  dat$OnOff.Effort <- as.integer(ok)
  dat
}


#' Identify separate occupations of a survey line
#'
#' Builds `LEGNO3`, an identifier that distinguishes each continuous occupation
#' of a survey line from any later re-occupation of the same line on the same
#' day.
#'
#' A `LEGNO` can be started, abandoned — for fog, say — and picked up again
#' hours later. Treating both stretches as one line would accumulate a spurious
#' point-to-point distance across the gap, joining the end of the first stretch
#' to the start of the second. `LEGNO3` pastes `LEGNO` together with a
#' run-length index so the two stretches stay separate.
#'
#' @param dat A data frame with a `LEGNO` column, in survey order.
#' @param sort Sort by `DATE`, `FILEID`, and `EVENTNO` first? Default `TRUE`.
#'   Run-length identification is meaningless on unsorted records.
#' @param quiet Suppress the note naming how many occupations had no `LEGNO`
#'   to be identified by. Default `FALSE`.
#'
#' @section How an occupation is found:
#' Three signals, taken in that order and judged per survey day, because one
#' part of a file may record line numbers where another records only
#' begin-line events.
#'
#' \describe{
#'   \item{A begin-line record}{`LEGSTAGE == 1` always opens an occupation.
#'     Without this a line flown twice under one number is silently a single
#'     occupation, since nothing about `LEGNO` changes between the two.}
#'   \item{A change of `LEGNO`}{Opens an occupation, as it always has.}
#'   \item{A run of census track}{Only where the day records neither of the
#'     above. This is inference rather than a reading of what was recorded, so
#'     those occupations are named `derived_<n>` and reported.}
#' }
#'
#' `LEGNO3` records which of the three an occupation came from, because they
#' are not equally trustworthy: `4_12` was named by its line number, `line_12`
#' has a begin-line record but no number to name it with, and `derived_12` was
#' inferred from census track alone. Counting the records under each is the
#' way to see how much of a dataset's line structure was read and how much was
#' guessed:
#'
#' ```r
#' table(sub("_[0-9]+$", "", dat$LEGNO3))
#' ```
#'
#' An occupation never spans two days: `DATE` and `FILEID` bound it, and it
#' closes at its end-line record (`LEGSTAGE == 5`). Records that are not part
#' of any line keep `LEGNO3` of `NA`: transit out to the survey area before the
#' first line, and the ferry between one line ending and the next beginning.
#'
#' Closing at the end-line matters more than it sounds. Those records are
#' off effort either way, so effort totals do not change — but they are still
#' *positions*, and a segment midpoint computed from them lands out on the
#' ferry rather than on the track. On one real extract 24% of all records sat
#' after an end-line inside an occupation, almost all of it transit and
#' cross-leg.
#'
#' @return `dat` with `LEGNO2` (a character copy of `LEGNO`) and `LEGNO3` added.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, section 8.A.19. NARWC Reference
#' Document 2023-01.
#'
#' @examples
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' dat <- make_leg_id(read_narwc(path))
#' unique(dat$LEGNO3)
#'
#' @export
make_leg_id <- function(dat, sort = TRUE, quiet = FALSE) {
  require_columns(dat, "LEGNO")

  if (is_empty_df(dat)) {
    dat$LEGNO2 <- character(0)
    dat$LEGNO3 <- character(0)
    return(dat)
  }

  if (sort) {
    order_cols <- intersect(c("DATE", "FILEID", "EVENTNO"), names(dat))
    if (length(order_cols)) {
      dat <- dplyr::arrange(dat, dplyr::across(dplyr::all_of(order_cols)))
    }
  }

  dat$LEGNO2 <- as.character(dat$LEGNO)
  n <- nrow(dat)

  day_cols <- intersect(c("DATE", "FILEID"), names(dat))
  day <- if (length(day_cols)) {
    do.call(paste, c(lapply(day_cols, function(nm) as.character(dat[[nm]])),
                     sep = "\r"))
  } else {
    rep("", n)
  }

  stage <- if ("LEGSTAGE" %in% names(dat)) {
    suppressWarnings(as.integer(dat$LEGSTAGE))
  } else {
    rep(NA_integer_, n)
  }
  census <- if ("LEGTYPE" %in% names(dat)) {
    !is.na(dat$LEGTYPE) & dat$LEGTYPE == 2
  } else {
    rep(FALSE, n)
  }

  # Which signal a day carries decides how its occupations are found. A day is
  # judged on its own: one part of a file may record line numbers while another
  # records only begin-line events.
  begin <- !is.na(stage) & stage == 1L
  has_begin <- day %in% unique(day[begin])
  has_legno <- day %in% unique(day[!is.na(dat$LEGNO2)])

  changed <- function(x) c(TRUE, x[-1] != x[-n] | xor(is.na(x[-1]), is.na(x[-n])))
  new_day <- c(TRUE, day[-1] != day[-n])

  start <- new_day |
    # A begin-line record always opens an occupation. Without this a line
    # flown twice under one number is silently a single occupation, because
    # nothing about LEGNO changes between them.
    (has_begin & begin) |
    (has_legno & changed(dat$LEGNO2) & !is.na(dat$LEGNO2)) |
    # Neither recorded: a run of census records is the only remaining
    # evidence of where a line starts and stops.
    (!has_begin & !has_legno & census & c(TRUE, !census[-n]))
  start[is.na(start)] <- FALSE

  occ <- cumsum(start)

  # One label per occupation, from the first line number recorded anywhere in
  # it, so LEGNO3 is constant across an occupation even where LEGNO is not.
  lab <- rep(NA_character_, max(occ))
  seen <- which(!is.na(dat$LEGNO2))
  if (length(seen)) {
    first <- seen[!duplicated(occ[seen])]
    lab[occ[first]] <- dat$LEGNO2[first]
  }

  # Not every stretch between two starts is a line. Records before the day's
  # first line — transit out to the survey area, with no LEGNO, no begin-line
  # record and no census record — are not an occupation of anything, and stay
  # NA as they always have. Only a stretch with some evidence of a line gets
  # an identifier.
  any_in_occ <- function(flag) {
    hit <- rep(FALSE, max(occ))
    hit[unique(occ[flag])] <- TRUE
    hit
  }
  occ_begin <- any_in_occ(begin)
  occ_census <- any_in_occ(census)
  is_line <- !is.na(lab) | occ_begin | occ_census

  # Three kinds, and they are not equally trustworthy. An occupation with a
  # LEGNO is named. One without a LEGNO but with a begin-line record is still
  # something the observers recorded — only its number is missing, so it is
  # `line_<n>`. One with neither was inferred from a run of census track, and
  # `derived_<n>` says so. Calling both of the last two "derived" would hide
  # how much of a dataset's line structure is read and how much is guessed.
  dat$LEGNO3 <- ifelse(
    !is.na(lab[occ]), paste(lab[occ], occ, sep = "_"),
    ifelse(occ_begin[occ], paste0("line_", occ),
           ifelse(is_line[occ], paste0("derived_", occ), NA_character_))
  )

  # A line ends where the record says it ends. Everything between an end-line
  # record and the next occupation is ferry — transit, cross-leg, off-watch —
  # and leaving it inside the occupation puts those positions into the track,
  # where they pull segment midpoints out along the transit and take the
  # covariates sampled at those midpoints with them.
  end <- !is.na(stage) & stage == 5L
  last_end <- rep(NA_integer_, max(occ))
  ended <- which(end)
  if (length(ended)) {
    last_end[occ[ended]] <- ended
  }
  after_end <- !is.na(last_end[occ]) & seq_len(n) > last_end[occ]
  stranded <- sum(after_end & census)
  dat$LEGNO3[after_end] <- NA_character_

  if (stranded && !quiet) {
    rlang::warn(paste0(
      stranded, " census record", if (stranded > 1) "s" else "",
      " fall after the end-line record of their occupation and are now",
      " outside any line. LEGTYPE 2 after LEGSTAGE 5 is a coding problem -",
      " the line was closed and then continued - and those records no longer",
      " contribute effort."
    ))
  }

  unnamed <- sum(is.na(lab) & occ_begin)
  derived <- sum(is.na(lab) & !occ_begin & occ_census)
  if ((unnamed || derived) && !quiet) {
    rlang::inform(paste0(
      # `is_line`, not `max(occ)`: the stretches between two starts include
      # the transit out to the survey area and the ferry between lines, and
      # those are not occupations of anything.
      "`make_leg_id()` found ", sum(is_line), " line occupations. ",
      if (unnamed) {
        paste0(unnamed, " have a begin-line record but no LEGNO to name them",
               " (`line_<n>`). ")
      } else "",
      if (derived) {
        paste0(derived, " fall on days recording neither a LEGNO nor a ",
               "begin-line record, and were inferred from runs of census ",
               "track (`derived_<n>`) - that is a guess about where lines ",
               "start and stop, not a reading of one. ")
      } else "",
      "The rest keep their line number."
    ))
  }
  dat
}

#' Which records are eligible for a right-angle distance?
#'
#' Reports the records that are on effort, on a census line, and continuing it.
#'
#' @section Why this is shared:
#' Handbook 8.A.31 restricts a right-angle distance measurement to records in
#' this state. Every distance source in a downstream package has to agree about
#' which sightings are fittable, and the only way to guarantee that is for them
#' all to ask the same function. It lives here, rather than in the package that
#' computes distances, because the rule is the handbook's rather than any one
#' analysis's.
#'
#' Columns that are absent cannot disqualify a record, so a table carrying only
#' some of `OnOff.Effort`, `LEGTYPE` and `LEGSTAGE` is judged on what it has.
#' That is deliberate: it lets a partial extract be used, but it means a `TRUE`
#' is only as strong as the columns present.
#'
#' @param dat A data frame of NARWC records.
#'
#' @return A logical vector with one element per row of `dat`.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, section 8.A.31. NARWC Reference
#' Document 2023-01.
#'
#' @examples
#' dat <- data.frame(
#'   OnOff.Effort = c(1, 1, 0, 1),
#'   LEGTYPE      = c(2, 2, 2, 1),
#'   LEGSTAGE     = c(2, 5, 2, 2)
#' )
#' on_effort_census_rows(dat)
#'
#' @export
on_effort_census_rows <- function(dat) {
  ok <- rep(TRUE, nrow(dat))
  if ("OnOff.Effort" %in% names(dat)) {
    ok <- ok & !is.na(dat$OnOff.Effort) & dat$OnOff.Effort == 1
  }
  if ("LEGTYPE" %in% names(dat)) {
    ok <- ok & !is.na(dat$LEGTYPE) & dat$LEGTYPE == 2
  }
  if ("LEGSTAGE" %in% names(dat)) {
    ok <- ok & !is.na(dat$LEGSTAGE) & dat$LEGSTAGE == 2
  }
  ok
}


#' Speed implied by consecutive position fixes
#'
#' The distance from each record to the next, divided by the time between them.
#' Computed from what the receiver logged rather than from any speed column, so
#' it is available on any file carrying positions and a clock.
#'
#' @section Why this exists:
#' Nothing in a NARWC file reliably says what the platform was. `PLATFORM` is
#' optional, has no code book, and is empty on real extracts; `LEGTYPE` codes
#' 5 and 6 mark shipboard records but a mixed file may use the aerial codes
#' throughout. Speed cannot be mistaken for something else: a survey aircraft
#' flies at 90-120 knots and a vessel surveys at about 10, and no arrangement
#' of the other columns makes one look like the other.
#'
#' @param dat A data frame with `LATITUDE`, `LONGITUDE` and `TIME`, in survey
#'   order.
#' @param by Columns identifying a stretch to compute within, so speed is never
#'   taken across a break. `NULL` (default) uses `LEGNO3` when present, else
#'   `DATE` and `FILEID`.
#' @param max_gap Ignore intervals longer than this many seconds. Default
#'   `300`. A long gap is a break in the record rather than slow travel, and
#'   including it drags the speed down.
#'
#' @return A numeric vector of knots, one per row: the speed from that record
#'   to the next. `NA` at the end of each stretch and wherever the interval is
#'   unusable.
#'
#' @seealso [classify_platform()], which turns this into a platform label.
#'
#' @examples
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' dat <- make_leg_id(read_narwc(path, quiet = TRUE), quiet = TRUE)
#' summary(track_speed(dat))
#'
#' @export
track_speed <- function(dat, by = NULL, max_gap = 300) {
  require_columns(dat, c("LATITUDE", "LONGITUDE", "TIME"))
  n <- nrow(dat)
  if (n < 2L) {
    return(rep(NA_real_, n))
  }

  key <- speed_key(dat, by)

  tt <- sprintf("%06d", ifelse(is.na(dat$TIME), 0L, as.integer(round(dat$TIME))))
  tsec <- as.numeric(substr(tt, 1, 2)) * 3600 +
    as.numeric(substr(tt, 3, 4)) * 60 + as.numeric(substr(tt, 5, 6))

  gap <- c(diff(tsec), NA_real_)
  step <- c(haversine_km(
    utils::head(dat$LATITUDE, -1), utils::head(dat$LONGITUDE, -1),
    utils::tail(dat$LATITUDE, -1), utils::tail(dat$LONGITUDE, -1)
  ) * 1000, NA_real_)

  same <- c(!is.na(key[-n]) & key[-n] == key[-1], FALSE)
  ok <- same & !is.na(gap) & gap > 0 & gap <= max_gap & !is.na(step) &
    !is.na(dat$TIME)

  out <- rep(NA_real_, n)
  out[ok] <- step[ok] / gap[ok] * 1.94384
  out
}


#' Label each record's platform from how fast it was moving
#'
#' Classifies every record as `"aerial"`, `"vessel"` or `"stationary"` using
#' the median [track_speed()] of the stretch it belongs to. A whole stretch
#' takes one label, because a platform does not change mid-line.
#'
#' @section What this is for:
#' A single NARWC extract can hold both an aerial and a shipboard survey with
#' nothing to tell them apart. On the file this was built against, `PLATFORM`
#' was `NA` on all 1,394,556 records and the aerial `LEGTYPE` codes were used
#' throughout — but 738 line occupations flew at 40-131 knots and 299 moved at
#' about 10. That matters because the distance machinery is not
#' platform-agnostic: a declination angle and a strip width are aircraft
#' measurements, and a shipboard survey measures by reticle and bearing.
#'
#' Prefer a recorded signal where the file has one. On that extract every
#' aerial occupation carried a `LEGNO` and no vessel occupation did, agreeing
#' with speed on 1037 of 1038 — a line number is a reading, where a speed
#' threshold is an inference.
#'
#' @param dat A data frame with `LATITUDE`, `LONGITUDE` and `TIME`.
#' @param by Passed to [track_speed()].
#' @param aerial_min Knots at or above which a stretch is aerial. Default `40`,
#'   comfortably between a surveying vessel and the slowest survey aircraft.
#' @param stationary_max Knots at or below which a stretch is stationary.
#'   Default `2`.
#' @param max_gap Passed to [track_speed()].
#'
#' @return A factor with levels `"stationary"`, `"vessel"`, `"aerial"`, one per
#'   row, `NA` where the stretch has too little usable time to judge.
#'
#' @seealso [track_speed()]
#'
#' @examples
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' dat <- make_leg_id(read_narwc(path, quiet = TRUE), quiet = TRUE)
#' table(classify_platform(dat), useNA = "ifany")
#'
#' @export
classify_platform <- function(dat, by = NULL, aerial_min = 40,
                              stationary_max = 2, max_gap = 300) {
  kn <- track_speed(dat, by = by, max_gap = max_gap)
  n <- nrow(dat)
  key <- speed_key(dat, by)

  # One label per stretch, from its median. A single interval can be anything -
  # a repeated fix, a dropped second - and a platform does not change mid-line.
  med <- tapply(kn, key, function(v) stats::median(v, na.rm = TRUE))
  per_row <- unname(med[key])

  factor(
    ifelse(is.na(per_row), NA_character_,
           ifelse(per_row <= stationary_max, "stationary",
                  ifelse(per_row >= aerial_min, "aerial", "vessel"))),
    levels = c("stationary", "vessel", "aerial")
  )
}


# The stretches speed is computed and judged over. `LEGNO3` where a record is
# on a line — but a record on no line has `LEGNO3` of NA, and NA pasted into a
# key is the single string "NA", which would put every transit, ferry and
# circling record in an entire archive into one group with one median speed
# and one verdict. Those fall back to their own day and file, which is the
# smallest stretch a platform can be judged over when there is no line.
speed_key <- function(dat, by = NULL) {
  n <- nrow(dat)
  paste_cols <- function(cols) {
    do.call(paste, c(lapply(cols, function(nm) as.character(dat[[nm]])),
                     sep = "\r"))
  }

  if (!is.null(by)) {
    return(if (length(by)) paste_cols(by) else rep("", n))
  }

  day <- intersect(c("DATE", "FILEID"), names(dat))
  fallback <- if (length(day)) paste0("day\r", paste_cols(day)) else rep("", n)

  if (!"LEGNO3" %in% names(dat)) {
    return(fallback)
  }
  ifelse(is.na(dat$LEGNO3), fallback, paste0("leg\r", as.character(dat$LEGNO3)))
}
