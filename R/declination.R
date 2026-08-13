#' Build ANGLEL and ANGLER from one declination column and a side
#'
#' The handbook records a declination angle in `ANGLEL` or `ANGLER` according
#' to which side of the aircraft the sighting was on (8.A.2). Some survey
#' programmes instead keep a single angle column and a separate left/right
#' flag. This splits the one into the two.
#'
#' @section Why this is not automatic:
#' `narwcr` will not infer meaning from a column name, and this mapping cannot
#' be inferred safely. A column called `Decl_Angle` might hold a declination
#' below the horizon, an inclination above it, or a bearing; `Left_or_Right`
#' might use `L`/`R`, `1`/`2`, or `port`/`starboard`. Naming the columns and
#' the codes is the caller asserting what they mean, which is a different thing
#' from the package guessing.
#'
#' @section What it checks:
#' A declination angle is measured down from the horizontal, so `perp_distance()`
#' computes `ALT / tan(angle)`: 90 degrees is directly below the aircraft and
#' gives a perpendicular distance of zero, and an angle at or below 0 is
#' undefined. Values outside `(0, 90]` are reported and left alone rather than
#' converted into a distance that cannot be right.
#'
#' Records whose side is missing or unrecognised are reported and skipped: an
#' angle with no side cannot be placed, and guessing a side would put sightings
#' on the wrong half of the track line.
#'
#' @param dat A data frame, ideally from [read_narwc()].
#' @param angle Name of the column holding the declination angle, in degrees.
#' @param side Name of the column saying which side the sighting was on.
#' @param left,right The values of `side` meaning left and right. Compared
#'   after trimming whitespace and ignoring case.
#' @param overwrite Replace values already in `ANGLEL`/`ANGLER`? Default
#'   `FALSE`, which fills only the blanks — a recorded angle is not displaced by
#'   a derived one.
#' @param quiet Suppress the report of what was set and what was skipped.
#'   Default `FALSE`.
#'
#' @return `dat` with `ANGLEL` and `ANGLER` added or filled.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, section 8.A.2. NARWC Reference
#' Document 2023-01.
#'
#' @seealso `distsamp::perp_distance()`, which consumes these columns.
#'
#' @examples
#' dat <- data.frame(
#'   SPECCODE = c("RIWH", "RIWH", "FIWH"),
#'   Decl_Angle = c(40, 20, 90),
#'   Left_or_Right = c("L", "R", "R")
#' )
#' angles_from_declination(dat, "Decl_Angle", "Left_or_Right")
#'
#' @export
angles_from_declination <- function(dat, angle, side, left = "L", right = "R",
                                    overwrite = FALSE, quiet = FALSE) {
  stopifnot(is.data.frame(dat))
  require_columns(dat, c(angle, side))

  a <- suppressWarnings(as.numeric(dat[[angle]]))
  s <- toupper(trimws(as.character(dat[[side]])))
  is_left <- !is.na(s) & s %in% toupper(trimws(left))
  is_right <- !is.na(s) & s %in% toupper(trimws(right))

  # A declination is measured down from the horizontal: 90 is straight below
  # and gives a perpendicular distance of zero. Outside (0, 90] there is no
  # distance to compute, so those are left for validation to report rather
  # than turned into a number.
  usable <- !is.na(a) & a > 0 & a <= 90
  bad_angle <- sum(!is.na(a) & !usable)
  no_side <- sum(usable & !is_left & !is_right)

  for (nm in c("ANGLEL", "ANGLER")) {
    if (!nm %in% names(dat)) dat[[nm]] <- NA_real_
    if (!is.numeric(dat[[nm]])) {
      dat[[nm]] <- suppressWarnings(as.numeric(dat[[nm]]))
    }
  }

  set <- function(nm, which_side) {
    room <- if (overwrite) rep(TRUE, nrow(dat)) else is.na(dat[[nm]])
    hit <- usable & which_side & room
    dat[[nm]][hit] <<- a[hit]
    sum(hit)
  }
  n_left <- set("ANGLEL", is_left)
  n_right <- set("ANGLER", is_right)

  if (!quiet) {
    rlang::inform(paste0(
      "`angles_from_declination()` set ", n_left, " ANGLEL and ", n_right,
      " ANGLER from `", angle, "` and `", side, "`."
    ))
  }
  if (bad_angle && !quiet) {
    rlang::warn(paste0(
      bad_angle, " value", if (bad_angle > 1) "s" else "", " of `", angle,
      "` fall outside (0, 90] degrees and were left alone. A declination is ",
      "measured down from the horizontal, so 90 is directly below the ",
      "aircraft and anything at or below 0 has no perpendicular distance."
    ))
  }
  if (no_side && !quiet) {
    rlang::warn(paste0(
      no_side, " usable angle", if (no_side > 1) "s" else "", " had no ",
      "recognised side in `", side, "` and were skipped. An angle without a ",
      "side cannot be placed, and guessing one puts the sighting on the wrong ",
      "half of the track line."
    ))
  }
  dat
}
