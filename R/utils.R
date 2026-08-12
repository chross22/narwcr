#' @keywords internal
#'
#' @references
#' The data format and survey protocol:
#'
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*. NARWC Reference Document
#' 2023-01. University of Rhode Island, Graduate School of Oceanography,
#' Narragansett, RI.
#'
#' CETAP (1982) *A Characterization of Marine Mammals and Turtles in the Mid- and
#' North-Atlantic Areas of the U.S. Outer Continental Shelf, Final Report.*
#' Cetacean and Turtle Assessment Program, University of Rhode Island. Bureau of
#' Land Management, Washington, DC.
#'
#' Kenney, R.D. and Winn, H.E. (1986) Cetacean high-use habitats of the northeast
#' United States continental shelf. *Fishery Bulletin* 84(2):345-357.
#'
#' @importFrom rlang .data
"_PACKAGE"

# Abort on a bad enum-style argument, listing the permitted values.
cli_abort_bad_arg <- function(arg, value, allowed, call = rlang::caller_env()) {
  rlang::abort(
    paste0(
      "`", arg, "` must be one of ",
      paste0("\"", allowed, "\"", collapse = ", "),
      ", not \"", value, "\"."
    ),
    call = call
  )
}

# Abort when required columns are missing.
abort_missing_columns <- function(missing, what = "input", call = rlang::caller_env()) {
  rlang::abort(
    paste0(
      "`", what, "` is missing required column",
      if (length(missing) > 1) "s" else "", ": ",
      paste0("`", missing, "`", collapse = ", "), "."
    ),
    call = call
  )
}

# Stop unless every name in `cols` is present in `dat`.
require_columns <- function(dat, cols, what = "dat", call = rlang::caller_env()) {
  missing <- setdiff(cols, names(dat))
  if (length(missing)) {
    abort_missing_columns(missing, what = what, call = call)
  }
  invisible(dat)
}

# Does a column carry anything at all? Uses the same definition of missing as
# `blank_to_na()`, because a column of "." is empty whatever `nzchar()` thinks,
# and "." is what NARWC writes for missing.
has_values <- function(x) {
  any(!is.na(blank_to_na(x)))
}

# Treat the NARWC missing-value placeholder "." (and blanks) as NA.
blank_to_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  x[trimws(x) %in% c("", ".", "NA", "na")] <- NA_character_
  x
}

# Run-length id: 1, 1, 2, 2, 2, 3, ... incrementing whenever `x` changes.
# Used to separate a survey line that was flown, abandoned, and later re-flown
# on the same day into distinct occupations.
rle_id <- function(x) {
  if (!length(x)) {
    return(integer(0))
  }
  key <- ifelse(is.na(x), "NA", as.character(x))
  cumsum(c(TRUE, key[-1] != key[-length(key)]))
}

# `%||%` without depending on a particular rlang export version.
`%|NA|%` <- function(x, y) ifelse(is.na(x), y, x)

is_empty_df <- function(x) is.null(x) || nrow(x) == 0L
