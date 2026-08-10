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
make_leg_id <- function(dat, sort = TRUE) {
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
  dat$LEGNO3 <- paste(dat$LEGNO2, rle_id(dat$LEGNO2), sep = "_")
  dat$LEGNO3[is.na(dat$LEGNO2)] <- NA_character_
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
