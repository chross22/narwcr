#' Survey-programme column profiles
#'
#' Columns that particular survey programmes add beyond the NARWC handbook, what
#' each is understood to mean, and how confident that understanding is.
#'
#' @section Why this exists:
#' A NARWC extract is not the only shape this data arrives in. Individual survey
#' programmes carry their own derived columns, and a processed "ready for model"
#' file may have a dozen of them. They are not in handbook Table 1, so
#' [narwc_schema()] does not know about them and [read_narwc()] would otherwise
#' drop them without comment.
#'
#' This registry records what is known about them, per programme. It is expected
#' to grow: CCS is the first entry because it is the one whose files this package
#' was originally written against, and it is an exception rather than a
#' representative case.
#'
#' @section Detection suggests, declaration acts:
#' [read_narwc()] will tell you when a file's extra columns match a known
#' profile, but it will never apply one because a name matched. That restraint is
#' deliberate, and `Tr_SIGHTING` is the reason: it means "sighting made from the
#' track-line" in CCS files, and there is nothing to stop another programme using
#' the same name for something else. A column name is not a contract. Acting on a
#' profile requires naming it — `read_narwc(profile = "ccs")`.
#'
#' The same caution applies in the other direction. A profile's presence in this
#' registry means the columns have been identified, not that this package
#' interprets them. `role` records which are actually used for anything.
#'
#' @section Adding a programme:
#' A new profile needs the programme's name, the columns it adds, and — the part
#' that is usually missing — what each column actually means, from a data
#' dictionary rather than from the column name. Entries whose meaning has been
#' guessed are marked `confidence = "unconfirmed"` and should stay that way until
#' someone who ran the survey confirms them.
#'
#' @param profile Optional profile name to filter to. `NULL` (default) returns
#'   every entry.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{`profile`}{Short profile name, for `read_narwc(profile = )`.}
#'     \item{`programme`}{The survey programme.}
#'     \item{`column`}{Column name as it appears in that programme's files.}
#'     \item{`meaning`}{What the column holds.}
#'     \item{`role`}{What `narwcr` does with it: `"passthrough"` if nothing.}
#'     \item{`confidence`}{`"confirmed"` or `"unconfirmed"`.}
#'   }
#'
#' @seealso [read_narwc()], [narwc_schema()], [validate_narwc()]
#'
#' @examples
#' narwc_profiles()
#' narwc_profiles("ccs")
#'
#' # The names a profile would keep
#' narwc_profiles("ccs")$column
#'
#' @export
narwc_profiles <- function(profile = NULL) {
  out <- tibble::tribble(
    ~profile, ~programme, ~column, ~meaning, ~role, ~confidence,

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "IS_LAT", paste(
      "Aircraft latitude on the track-line at the moment of initial sighting,",
      "before any break-off to circle."
    ), "passthrough", "confirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "IS_LONG", "Aircraft longitude at initial sighting; see `IS_LAT`.",
    "passthrough", "confirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "IS_SPECCODE", "Species recorded at initial sighting.",
    "passthrough", "unconfirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "Tr_SIGHTING", paste(
      "Whether the sighting was made from the track-line. Encodes the same",
      "intent as this package's on-effort census eligibility, and is a useful",
      "cross-check on it, but is not used as the mechanism."
    ), "passthrough", "confirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "OBSSIGHT", paste(
      "Unknown. Never assigned or filtered on in the scripts this package was",
      "rewritten from, so its meaning cannot be inferred from its use. The name",
      "suggests an observer flag; that is a guess. Needs a data dictionary."
    ), "passthrough", "unconfirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "Effort_Type", "An effort classification of unknown definition.",
    "passthrough", "unconfirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "Date_UTC", "Date, already converted to UTC.",
    "passthrough", "unconfirmed",

    "ccs", "Center for Coastal Studies, Cape Cod Bay",
    "Time_UTC", "Time, already converted to UTC.",
    "passthrough", "unconfirmed"
  )

  if (!is.null(profile)) {
    known <- unique(out$profile)
    if (!profile %in% known) {
      cli_abort_bad_arg("profile", profile, known)
    }
    out <- out[out$profile == profile, , drop = FALSE]
  }
  out
}

# Every column any known profile declares.
all_profile_columns <- function() {
  unique(narwc_profiles()$column)
}

# Columns this package derives rather than reads, and so must not report as
# foreign to the handbook.
narwcr_derived_columns <- c(
  "DATE", "LEGNO2", "LEGNO3", "CIRCLE", "OnOff.Effort",
  "pt2pt.effort", "Effort", "new_trackno",
  "seg_id", "seg_no", "seg_eff", "mid_lat", "mid_lon",
  "mean_beaufort", "wt_beaufort", "n_records", "start_time", "events", "case",
  "distance", "side", "along", "bearing", "distbegin", "distend"
)

# Columns of `nms` that are not NARWC handbook variables. Columns a known
# profile declares are still reported - they are genuinely outside the handbook,
# and the caller should know they are carried uninterpreted - but the message
# names the profile rather than giving generic advice. Columns this package
# derives itself are not foreign and are excluded.
unrecognised_columns <- function(nms) {
  schema <- narwc_schema()
  known <- c(
    schema$required, schema$optional, names(schema$aliases),
    narwcr_derived_columns
  )
  setdiff(nms, known)
}

# Which profiles a set of column names looks like, most-matched first. Used to
# say "this looks like CCS" - never to act.
matching_profiles <- function(nms) {
  reg <- narwc_profiles()
  hits <- reg[reg$column %in% nms, , drop = FALSE]
  if (!nrow(hits)) {
    return(character(0))
  }
  counts <- sort(table(hits$profile), decreasing = TRUE)
  names(counts)
}
