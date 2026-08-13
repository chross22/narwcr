#' Reconstruct the line state on records that record no LEGSTAGE
#'
#' `LEGSTAGE` marks what is happening to a survey line — it begins (1),
#' continues (2), breaks off to circle (3), resumes (4), ends (5). Programmes
#' record it when it *changes*, so a record taken mid-line often carries none
#' at all. This fills in the state those records were in.
#'
#' @section Why this is not [fill_narwc()]:
#' Carrying `LEGSTAGE` forward as a value is wrong. `1` means "begin line", an
#' event; copying it onto the next thousand records claims the line began a
#' thousand times, and since `1 -> 1` is not a legal transition it manufactures
#' the sequence errors it looks like it should fix.
#'
#' What carries forward is the *state*, not the code. Handbook 8.A.20:
#'
#' \tabular{ll}{
#'   after 1 begin \tab the line is continuing, so 2 \cr
#'   after 2 continue \tab still continuing, 2 \cr
#'   after 4 resume \tab continuing again, 2 \cr
#'   after 3 break off \tab off the line, circling — left `NA` \cr
#'   after 5 end \tab the line is over — left `NA` \cr
#'   before any event \tab unknown — left `NA`
#' }
#'
#' So the only code ever written is `2`, and only where the line is genuinely
#' continuing. A record before the first event, during a circle, or after the
#' line closed keeps its `NA` and stays ineligible — which is correct, because
#' a detection made while not searching the line breaks the distance-sampling
#' assumptions rather than merely the bookkeeping.
#'
#' @section What it is for:
#' `on_effort_census_rows()` requires `LEGSTAGE == 2`, and handbook 8.A.31
#' restricts a right-angle distance measurement to records in that state. On a
#' real archive 1,928 of 2,280 on-effort census sightings carried no
#' `LEGSTAGE`, so 85% of the detections that could have informed a detection
#' function were excluded for a code nobody wrote down.
#'
#' @param dat A data frame with `LEGSTAGE`, in survey order — from
#'   [make_leg_id()], which sorts.
#' @param by Columns identifying one line occupation, so a state never carries
#'   across a break. `NULL` (default) uses `LEGNO3` when present, else `DATE`
#'   and `FILEID`.
#' @param quiet Suppress the report of how many were filled. Default `FALSE`.
#'
#' @return `dat` with `LEGSTAGE` filled where the state is known, and a logical
#'   `LEGSTAGE_FILLED` marking every record this function wrote to.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, sections 8.A.20 and 8.A.31.
#' NARWC Reference Document 2023-01.
#'
#' @seealso [on_effort_census_rows()], which is what this makes answerable;
#'   [fill_narwc()] for columns whose *value* carries forward.
#'
#' @examples
#' dat <- data.frame(
#'   FILEID = "F", EVENTNO = 1:6, LEGNO3 = "1_1", LEGTYPE = 2,
#'   LEGSTAGE = c(1, NA, NA, 5, NA, NA)
#' )
#' fill_legstage(dat, quiet = TRUE)$LEGSTAGE
#'
#' @export
fill_legstage <- function(dat, by = NULL, quiet = FALSE) {
  require_columns(dat, "LEGSTAGE")
  n <- nrow(dat)
  if (!n) {
    dat$LEGSTAGE_FILLED <- logical(0)
    return(dat)
  }

  stage <- suppressWarnings(as.integer(dat$LEGSTAGE))

  if (is.null(by)) {
    by <- if ("LEGNO3" %in% names(dat)) "LEGNO3" else
      intersect(c("DATE", "FILEID"), names(dat))
  }
  key <- if (length(by)) {
    do.call(paste, c(lapply(by, function(nm) as.character(dat[[nm]])),
                     sep = "\r"))
  } else {
    rep("", n)
  }
  starts <- c(TRUE, key[-1] != key[-n])

  # Last recorded LEGSTAGE at or before each record, never carried across an
  # occupation boundary. A sentinel at the start of a group that has no code
  # of its own stops the previous group's state leaking in.
  val <- stage
  val[starts & is.na(val)] <- -1L
  known <- which(!is.na(val))
  at <- findInterval(seq_len(n), known)
  carried <- rep(NA_integer_, n)
  carried[at > 0] <- val[known[at[at > 0]]]
  carried[!is.na(carried) & carried == -1L] <- NA_integer_

  # Only where the line is genuinely continuing, and only on a census record:
  # handbook 8.A.20 records LEGSTAGE during census tracks, so a circling or
  # transit record does not acquire one.
  fillable <- is.na(stage) & !is.na(carried) & carried %in% c(1L, 2L, 4L)
  if ("LEGTYPE" %in% names(dat)) {
    fillable <- fillable & !is.na(dat$LEGTYPE) & dat$LEGTYPE == 2
  }

  dat$LEGSTAGE[fillable] <- 2
  dat$LEGSTAGE_FILLED <- fillable

  if (any(fillable) && !quiet) {
    rlang::inform(paste0(
      "`fill_legstage()` set LEGSTAGE to 2 on ", sum(fillable), " record",
      if (sum(fillable) > 1) "s" else "",
      " continuing a line that recorded no code. Records before the first ",
      "event, during a circle, or after an end-line keep their `NA` and stay ",
      "ineligible. `LEGSTAGE_FILLED` marks every one."
    ))
  }
  dat
}
