#' Read NARWC-format survey data
#'
#' Reads a NARWC sightings-database extract from a CSV file, or standardises an
#' already-loaded data frame, into the column names and types the rest of the
#' package expects.
#'
#' It resolves column names onto the handbook's, coerces the numeric NARWC
#' variables, turns the database's missing-value placeholders (`"."`, `""`) into
#' `NA`, and drops records with no position. Beyond that it does not filter,
#' repair, or reject anything — use [validate_narwc()] to find problems and
#' [flag_effort()] to decide what counts as effort.
#'
#' A `DATE` column of class `Date` is derived from `YEAR`, `MONTH`, and `DAY`
#' when all three are present.
#'
#' @section Column names do not have to match exactly:
#' Real extracts spell things their own way. Matching ignores case and
#' separators, so `Event`, `event_no`, and `EventNo` all reach `EVENTNO` without
#' anyone editing a spreadsheet first, and the alias table in
#' [narwc_schema()]`$aliases` covers the rest.
#'
#' **This is not fuzzy matching.** Nothing is guessed by edit distance —
#' `EVENTN0` with a zero stays `EVENTN0` — and nothing is ever renamed onto a
#' canonical column that is already present, so a correctly named column always
#' wins. Matches that took an inference are reported; exact entries in the alias
#' table are not, since announcing `LAT_DD` on every read would bury the ones
#' worth a second look.
#'
#' @section Where `TIME` comes from:
#' Programmes record the clock they record. `TIME` is taken from the first of
#' `TIME`, then a UTC column (`TIME_UTC`, `GMT`, `TIME_GMT`), then a local one
#' (`TIME_LOC`, `TIME_LOCAL`) — GMT and UTC being the same clock. A file
#' carrying both zones lands on UTC. If yours is consistent it does not much
#' matter which; if it is not, decide before segmenting, because effort is
#' accumulated in record order.
#'
#' @section Records with no position:
#' Dropped by default, and reported. A record with no `LATITUDE` or `LONGITUDE`
#' contributes no effort and cannot place a sighting — but left in, it does not
#' announce itself: a great-circle distance from a missing position is `NA`, and
#' the usual way of accumulating effort turns that `NA` into a zero. Losing the
#' record visibly is better than counting it as zero distance flown.
#' `drop_missing_position = FALSE` keeps them.
#'
#' @section GPS track columns:
#' A `Trk*` column is the platform's own GPS track log. Where a file carries
#' both `TrkLatitude` and a plain `LATITUDE`, they are not two spellings of one
#' thing: the track log is what the receiver recorded, and the plain column is
#' the position entered for the platform, which on a file covering both a vessel
#' and an aircraft is a different place. The track log wins, and the displaced
#' column is kept as `LATITUDE_ORIGINAL` rather than dropped, with a warning
#' naming both. This is the only case where a column already carrying a
#' canonical name does not win; `prefer_source = FALSE` turns it off.
#'
#' The same applies to the clock: `TrkTime_UTC` is taken ahead of any other UTC
#' spelling, and displaces a plain `TIME`. `TrkTime_Local` does not — it is
#' preferred only among the local spellings, because moving the whole dataset
#' onto another zone to gain the receiver's seconds is not a trade this makes
#' unasked. UTC still comes before local either way.
#'
#' `LEGTYPE_BK` displaces a plain `LEGTYPE` by the same rule. That one is a
#' MEMDR-era data quirk rather than anything to do with a GPS, but it is the
#' same shape of problem: where a file carries both, the `_BK` column is the
#' leg type to believe.
#'
#' With none of these columns present nothing changes — `LATITUDE`,
#' `LONGITUDE` and `LEGTYPE` are used exactly as they are.
#'
#' @section A missing EVENTNO:
#' Supplied rather than left as `NA`, because `make_leg_id()` sorts by `DATE`,
#' `FILEID` and `EVENTNO`, and `order()` puts `NA` last — so a missing event
#' number moves records out of survey order before the run-length logic that
#' builds `LEGNO3` sees them. That does not error; it produces the wrong lines.
#'
#' The fill follows handbook 8.A.10. `EVENTNO` is a sequentially assigned
#' record number that must increase within a file; skipped numbers are allowed
#' and duplicates are not, with one exception — several sightings may share an
#' event number, and then *all of the non-sighting variables must be identical
#' across all records*. So an event here is a run of consecutive records
#' agreeing on every non-sighting variable, a number already recorded anywhere
#' in that run covers the whole run, and only an event with no number at all is
#' given one.
#'
#' Where the recorded numbers leave no room for an event being inserted, the
#' numbers from that point forward are increased. That is the handbook's own
#' remedy — "it is usually necessary to correct the event numbers from that
#' point forward in the file" — and it warns, because records elsewhere
#' referring to the old numbers past that point will no longer match.
#'
#' Only values are supplied, never the column itself. A file with no `EVENTNO`
#' column is missing a required variable, and inventing one would hide that.
#'
#' @section Units:
#' `ALT` is metres throughout (handbook 8.A.1), and it feeds the right-angle
#' distances in `distsamp`. A column whose *name* declares feet — `TrkAltitude_ft`,
#' `ALTFT`, `ALTITUDEFT` — is multiplied by `0.3048` on the way in, and the
#' multiplier is recorded in the `factor` column of [narwc_column_mapping()].
#' A file carrying both `TrkAltitude_m` and `TrkAltitude_ft` uses the metres one
#' and converts nothing — unless the metres column is empty, in which case the
#' feet one is used and converted. Precedence is written over spellings and
#' says nothing about which column a file actually filled in, so a column with
#' no values in it never outranks one that has them. The same applies to the
#' GPS track columns: an empty `TrkLatitude` displaces nothing.
#'
#' @section Columns that are not in the handbook:
#' Survey programmes add their own derived columns, and a processed "ready for
#' model" file may carry a dozen. They are not handbook Table 1 variables, so by
#' default they are **dropped** — and this function says so rather than dropping
#' them silently, naming what went and pointing at [narwc_profiles()] when they
#' match a known survey programme.
#'
#' Three ways to keep them:
#'
#' \describe{
#'   \item{`profile = "ccs"`}{Keeps the columns that programme is known to add.
#'     See [narwc_profiles()] for what is registered.}
#'   \item{`extra_columns = c(...)`}{Keeps exactly what you name. Glob patterns
#'     work, so `"Trk*"` keeps a family whose exact names differ between
#'     extracts.}
#'   \item{`extra_columns = NULL`}{Keeps every column in the input.}
#' }
#'
#' Naming a profile keeps its columns; it does not interpret them. A column name
#' is not a contract between programmes — `Tr_SIGHTING` means one thing in a CCS
#' file and nothing in particular anywhere else — so this function will tell you
#' what a file looks like and leave the decision to you.
#'
#' @param x A path to a CSV file, or a data frame.
#' @param extra_columns Character vector of additional column names to keep
#'   beyond those in [narwc_schema()]. Use `NULL` to keep every column in the
#'   input.
#' @param profile Survey-programme profile whose extra columns should be kept,
#'   for example `"ccs"`. `NULL` (default) keeps only the handbook columns. See
#'   [narwc_profiles()].
#' @param drop_missing_position Drop records with no `LATITUDE` or `LONGITUDE`.
#'   Default `TRUE`; see below.
#' @param prefer_source Let a better-known source take precedence over a
#'   canonical column of the same name — `TrkLatitude` over a plain `LATITUDE`,
#'   and `LEGTYPE_BK` over a plain `LEGTYPE`. Default `TRUE`; see below.
#'   `FALSE` restores "the column already named `LATITUDE` always wins".
#' @param assume_alt_m The survey altitude in **metres**, used where a record
#'   has none. `NULL` (default) leaves a missing `ALT` missing. Records given
#'   this value are marked in an `ALT_ASSUMED` column and the fill is reported.
#'
#'   Metres, not feet: `ALT` is metres throughout (handbook 8.A.1), so a
#'   750-foot survey altitude is `assume_alt_m = 228.6`. Passing `750` would
#'   put every record above `flag_effort()`'s 366 m ceiling and take them all
#'   off effort — the opposite of what filling an altitude is usually for. Use
#'   `assume_alt_ft` and avoid the question.
#'
#'   This is a stated altitude, not a measured one. `ALT` feeds
#'   `perp_distance()`, so every right-angle distance computed from a filled
#'   record inherits whatever you state here — which is why it is off by
#'   default, and why `ALT_ASSUMED` exists to find those records afterwards.
#' @param assume_alt_ft The same, stated in feet — `assume_alt_ft = 750` for a
#'   750-foot survey — and converted to metres for you. Survey teams state
#'   altitude in feet and the package stores metres, so this is the safer of
#'   the two. Giving both is an error.
#' @param make_eventno Supply the missing values of an `EVENTNO` column that
#'   has some. Default `TRUE`; see below. `FALSE` leaves them `NA`. A file with
#'   no `EVENTNO` column at all is left alone either way — that is a missing
#'   required variable for [validate_narwc()] to report.
#' @param quiet Suppress the messages naming matched columns, dropped columns,
#'   dropped records, and unit conversions. Default `FALSE`.
#' @param ... Passed to [utils::read.csv()] when `x` is a path.
#'
#' @return A tibble with the recognised NARWC columns, standardised names and
#'   types, and a derived `DATE` column. Carries the class
#'   `"narwc_data"` so downstream functions can tell standardised input
#'   from a raw data frame.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*. NARWC Reference Document
#' 2023-01.
#'
#' @seealso [validate_narwc()] to check the result against the handbook,
#'   [narwc_schema()] for the recognised columns, [narwc_profiles()] for the
#'   columns individual survey programmes add.
#'
#' @examples
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' dat <- read_narwc(path)
#' head(dat[, c("FILEID", "EVENTNO", "LEGTYPE", "LEGSTAGE", "SPECCODE")])
#'
#' # Keep a survey programme's own columns
#' narwc_profiles("ccs")$column
#'
#' @export
read_narwc <- function(x, extra_columns = character(), profile = NULL,
                       drop_missing_position = TRUE, prefer_source = TRUE,
                       make_eventno = TRUE, assume_alt_m = NULL,
                       assume_alt_ft = NULL,
                       quiet = FALSE, ...) {
  dat <- if (is.data.frame(x)) {
    x
  } else if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      rlang::abort(paste0("File not found: ", x))
    }
    utils::read.csv(x, stringsAsFactors = FALSE, colClasses = "character", ...)
  } else {
    rlang::abort("`x` must be a data frame or a path to a single CSV file.")
  }

  dat <- tibble::as_tibble(dat)
  schema <- narwc_schema()

  # 1. Resolve input columns onto the canonical names.
  aliases <- schema$aliases
  resolved <- resolve_columns(names(dat), schema, prefer_source, dat)
  if (length(resolved$renames)) {
    names(dat)[match(names(resolved$renames), names(dat))] <-
      unname(resolved$renames)
    if (!quiet) report_renamed_columns(resolved$renames, resolved$inferred)
  }
  attr(dat, "column_mapping") <-
    column_mapping_table(resolved$renames, resolved$inferred,
                         resolved$conversions)

  # 2. Select the columns we recognise, plus anything explicitly requested.
  #    Dropping a column the caller may need is a real loss, so say what went.
  if (!is.null(profile)) {
    extra_columns <- unique(c(extra_columns, narwc_profiles(profile)$column))
  }
  if (!is.null(extra_columns)) {
    # A column displaced by a GPS track column is kept without being asked for.
    # It was in the file under a name this package recognises, and moving it
    # aside is our doing, not the caller's — dropping it here would make
    # `prefer_source` quietly destructive.
    # `DATE` is not a handbook Table 1 variable — the handbook carries YEAR,
    # MONTH and DAY — but a file that supplies `Date_UTC` has told us the date
    # on a known clock, and dropping it here meant falling back to rebuilding
    # the date from three columns that may be on a different one.
    keep <- c(schema$required, schema$optional, "DATE",
              unname(resolved$displaced),
              expand_column_globs(extra_columns, names(dat)))
    dropped <- setdiff(names(dat), keep)
    dat <- dat[, intersect(keep, names(dat)), drop = FALSE]

    # An alias left behind because its canonical column was already present is
    # a duplicate, not a loss. Reporting it would send the caller looking for
    # information that is still there under the other name.
    redundant <- names(aliases)[aliases %in% names(dat)]
    dropped <- setdiff(dropped, redundant)

    if (length(dropped) && !quiet) {
      report_dropped_columns(dropped)
    }
  }

  # 3. NARWC writes "." for missing. Blank those before coercion so that a
  #    single "." does not turn a whole column into NA-with-warning.
  dat[] <- lapply(dat, blank_to_na)

  # 4. Coerce the numeric NARWC variables. A column that had values going in
  #    and is entirely NA coming out has been emptied by the coercion, not by
  #    the data - the failure that hid 1.4 million missing times.
  for (nm in intersect(narwc_numeric_columns, names(dat))) {
    if (is.numeric(dat[[nm]])) next
    # Blanks became NA at step 3, so a plain NA count is enough — and calling
    # trimws() on every value of every numeric column is not free at scale.
    had <- sum(!is.na(dat[[nm]]))
    dat[[nm]] <- if (nm %in% c("TIME", "S_TIME")) {
      parse_narwc_time(dat[[nm]])
    } else {
      suppressWarnings(as.numeric(dat[[nm]]))
    }
    if (had && all(is.na(dat[[nm]]))) {
      rlang::warn(paste0(
        "`", nm, "` had ", had, " value", if (had > 1) "s" else "",
        " but none of them could be read as a number, so the column is now ",
        "entirely NA. Check the format it arrived in."
      ))
    }
  }

  # 4b. Rescale anything whose input name declared a different unit. After
  #     step 4, because until then these columns are still character.
  dat <- apply_unit_conversions(dat, resolved$conversions, quiet)

  # 4c. A preferred source replaces the column it displaced only where it
  #     actually has a value. Preference is about which column to believe when
  #     both say something, not about which column exists: a GPS track log
  #     covers the years the receiver was fitted, and an archive spanning
  #     decades has the plain column for everything before that. Replacing
  #     wholesale emptied LATITUDE on every older record, and
  #     `drop_missing_position` then removed them - 3,754,148 of 5,148,704 on
  #     a real archive, leaving exactly the GPS-logger era behind.
  #
  #     After the unit conversion, so the displaced column is not rescaled by
  #     a factor that belongs to the column that displaced it.
  for (target in names(resolved$displaced)) {
    keep <- unname(resolved$displaced[[target]])
    if (!all(c(target, keep) %in% names(dat))) next

    orig <- dat[[keep]]
    if (target %in% narwc_numeric_columns && !is.numeric(orig)) {
      orig <- if (target %in% c("TIME", "S_TIME")) {
        parse_narwc_time(orig)
      } else {
        suppressWarnings(as.numeric(orig))
      }
    }

    gap <- is.na(dat[[target]]) & !is.na(orig)
    if (any(gap)) {
      dat[[target]][gap] <- orig[gap]
      if (!quiet) {
        rlang::inform(paste0(
          "`", keep, "` supplied ", sum(gap), " value",
          if (sum(gap) > 1) "s" else "", " of `", target,
          "` that the preferred source did not record. A preferred source is ",
          "the one to believe where both speak, not a replacement for the ",
          "years it does not cover."
        ))
      }
    }
  }

  # 5. Reconcile DATE with the date parts, in whichever direction the file
  #    leaves open. YEAR, MONTH and DAY are required columns, so a file that
  #    carries a date but not its parts fails validation for three variables
  #    it demonstrably has.
  parts <- all(c("YEAR", "MONTH", "DAY") %in% names(dat))
  from_parts <- function() {
    as.Date(sprintf("%04d-%02d-%02d", dat$YEAR, dat$MONTH, dat$DAY))
  }

  if (!"DATE" %in% names(dat)) {
    if (parts) dat$DATE <- from_parts()
  } else if (!inherits(dat$DATE, "Date")) {
    # A supplied date column can be in a format `as.Date()` will not take. That
    # is a reason to fall back to the date parts, not to fail the whole read —
    # but it has to be said out loud, because the parts may be on another clock.
    parsed <- suppressWarnings(as.Date(dat$DATE))

    # `as.Date()` does not reliably fail on a format it cannot read: it tries
    # "%Y/%m/%d" too, so "4/2/2024" is taken as the year 4 rather than
    # rejected. Checking the parsed year against the file's own `YEAR` catches
    # that, and tolerates a one-year gap so a genuine UTC-to-local difference
    # across New Year is not mistaken for garbage.
    unreadable <- all(is.na(parsed)) && any(!is.na(dat$DATE))
    if (!unreadable && parts) {
      off <- abs(as.integer(format(parsed, "%Y")) - dat$YEAR)
      unreadable <- any(!is.na(off) & off > 1)
    }

    if (unreadable && parts) {
      rlang::warn(paste0(
        "The `DATE` column could not be read as a date, so it has been ",
        "rebuilt from `YEAR`, `MONTH` and `DAY`. If those are on a different ",
        "clock from `TIME`, dates near midnight will be wrong."
      ))
      dat$DATE <- from_parts()
    } else {
      dat$DATE <- parsed
    }
  }

  # 5a. And the other direction. Splitting a date into its parts is exact —
  #     nothing is inferred and no clock is assumed, unlike rebuilding a date
  #     from parts that may be on a different one. Only blanks are filled, so
  #     a file that records both keeps what it recorded.
  if ("DATE" %in% names(dat) && inherits(dat$DATE, "Date")) {
    filled <- character(0)
    for (nm in c("YEAR", "MONTH", "DAY")) {
      from <- as.numeric(format(dat$DATE, c(YEAR = "%Y", MONTH = "%m",
                                            DAY = "%d")[[nm]]))
      gap <- if (nm %in% names(dat)) is.na(dat[[nm]]) else rep(TRUE, nrow(dat))
      if (!any(gap & !is.na(from))) next
      if (!nm %in% names(dat)) dat[[nm]] <- NA_real_
      dat[[nm]][gap] <- from[gap]
      filled <- c(filled, nm)
    }
    if (length(filled) && !quiet) {
      rlang::inform(paste0(
        "`read_narwc()` filled ", paste(filled, collapse = ", "),
        " from `DATE`. They are required columns and splitting a date into ",
        "them is exact, so a file carrying the date but not its parts does ",
        "not need to fail validation for them."
      ))
    }
  }

  # 5a2. A stated survey altitude, where the file records none. Survey teams
  #      state altitude in feet; the package stores metres. Converting here
  #      rather than asking the caller to is the whole point of the argument.
  if (!is.null(assume_alt_ft)) {
    if (!is.null(assume_alt_m)) {
      rlang::abort("Give `assume_alt_m` or `assume_alt_ft`, not both.")
    }
    assume_alt_m <- assume_alt_ft * 0.3048
  }
  if (!is.null(assume_alt_m)) {
    if (!"ALT" %in% names(dat)) dat$ALT <- NA_real_
    gap <- is.na(dat$ALT)
    dat$ALT_ASSUMED <- gap
    if (any(gap)) {
      dat$ALT[gap] <- assume_alt_m
      if (!quiet) {
        rlang::inform(paste0(
          "`read_narwc()` set ALT to ", assume_alt_m, " m on ", sum(gap),
          " record", if (sum(gap) > 1) "s" else "", " that recorded none. ",
          "`ALT_ASSUMED` marks them. ALT feeds `perp_distance()`, so any ",
          "right-angle distance computed from those records rests on the ",
          "altitude you stated rather than one that was measured."
        ))
      }
    }
  }

  # 5b. Supply an EVENTNO where the file has none. This is not cosmetic:
  #     `make_leg_id()` sorts by DATE, FILEID and EVENTNO, and `order()` sends
  #     NA to the end, so a missing event number silently moves records out of
  #     survey order before the run-length logic that builds LEGNO3 ever sees
  #     them. The result is not an error, just the wrong lines.
  if (make_eventno) {
    dat <- supply_event_number(dat, quiet)
  }

  # 6. A record with no position cannot contribute effort or place a sighting.
  #    Left in, it silently contributes zero distance, which is worse than
  #    losing it visibly.
  if (drop_missing_position && all(c("LATITUDE", "LONGITUDE") %in% names(dat))) {
    gone <- is.na(dat$LATITUDE) | is.na(dat$LONGITUDE)
    if (any(gone)) {
      dat <- dat[!gone, , drop = FALSE]
      if (!quiet) {
        rlang::inform(paste0(
          "Dropped ", sum(gone), " record", if (sum(gone) > 1) "s" else "",
          " with no LATITUDE or LONGITUDE. Such a record contributes no ",
          "effort and cannot place a sighting; left in, it would count as ",
          "zero distance rather than as missing. Keep them with ",
          "`drop_missing_position = FALSE`."
        ))
      }
    }
  }

  class(dat) <- unique(c("narwc_data", class(dat)))
  dat
}

# Map the input's column names onto the canonical ones.
#
# Three passes, most confident first: an exact canonical name is left alone; a
# name that matches one after normalising case and separators is renamed; and
# an alias is applied, normalised the same way. `Event`, `event_no` and
# `EventNo` all reach `EVENTNO` without anyone editing a spreadsheet.
#
# Normalising is not fuzzy matching. Nothing is guessed by edit distance, and
# nothing is renamed onto a canonical column that is already present - the
# real one always wins. Every rename is reported, because a column name is the
# one piece of provenance a reader has.
resolve_columns <- function(nms, schema, prefer_source = TRUE, values = NULL) {
  canonical <- c(schema$required, schema$optional)
  norm <- function(x) toupper(gsub("[^A-Za-z0-9]", "", x))

  input_norm <- norm(nms)
  taken <- nms[nms %in% canonical]
  renames <- character(0)

  # The one documented exception to "the real one always wins": a GPS track
  # column displaces a canonical column of the same name. The displaced column
  # is renamed rather than dropped, so nothing is lost and both are readable.
  displaced <- character(0)
  for (target in if (prefer_source) names(narwc_preferred_source) else character(0)) {
    if (!target %in% taken) next
    src <- which(input_norm %in% norm(narwc_preferred_source[[target]]))
    if (!length(src)) next
    # And an empty preferred source displaces nothing — taking `TrkLatitude`
    # over a populated `LATITUDE` because the file happens to carry the column
    # would replace real positions with NA.
    if (!is.null(values) &&
        !any(vapply(src, function(i) has_values(values[[nms[i]]]),
                    logical(1)))) next
    keep <- paste0(target, "_ORIGINAL")
    if (keep %in% nms) next
    displaced[target] <- keep
    taken <- setdiff(taken, target)
  }

  # Target, and whether reaching it took an inference. An exact alias is a
  # documented mapping; anything found only after normalising case or
  # separators is a judgement about what the author meant.
  target_of <- function(i) {
    if (nms[i] %in% canonical) return(c(NA_character_, "FALSE"))

    exact_alias <- unname(schema$aliases[match(nms[i], names(schema$aliases))])
    if (!is.na(exact_alias)) return(c(exact_alias, "FALSE"))

    hit <- canonical[match(input_norm[i], norm(canonical))]
    if (!is.na(hit)) return(c(hit, "TRUE"))

    alias_hit <- unname(schema$aliases[match(input_norm[i],
                                             norm(names(schema$aliases)))])
    if (!is.na(alias_hit)) return(c(alias_hit, "TRUE"))
    c(NA_character_, "FALSE")
  }

  # Preferred order among several inputs claiming the same canonical name.
  priority <- function(nm, target) {
    order_for <- narwc_alias_priority[[target]]
    if (is.null(order_for)) return(1)
    m <- match(norm(nm), norm(order_for))
    if (is.na(m)) length(order_for) + 1L else m
  }

  found <- vapply(seq_along(nms), target_of, character(2))
  wants <- found[1, ]
  guessed <- found[2, ] == "TRUE"
  inferred <- logical(0)
  conversions <- numeric(0)

  for (target in unique(stats::na.omit(wants))) {
    if (target %in% taken) next
    claimants <- which(wants == target)

    # A column with nothing in it cannot outrank one that has data. Priority
    # is written over spellings — metres before feet, UTC before local — and
    # says nothing about which column the file actually filled in. A file
    # carrying both `TrkAltitude_m` and `TrkAltitude_ft` with only the feet one
    # populated would otherwise take the empty column and hand back all NA.
    if (length(claimants) > 1L && !is.null(values)) {
      filled <- vapply(claimants, function(i) has_values(values[[nms[i]]]),
                       logical(1))
      if (any(filled)) claimants <- claimants[filled]
    }

    if (length(claimants) > 1L) {
      # A column whose name *is* the canonical one bar case or punctuation
      # outranks one that only got there through an alias: `Latitude` beats
      # `Lat` for LATITUDE. Documented priority breaks any remaining tie.
      is_canonical <- norm(nms[claimants]) != norm(target)
      claimants <- claimants[order(
        is_canonical,
        vapply(claimants, function(i) priority(nms[i], target), numeric(1))
      )]
    }
    pick <- claimants[1]

    # Several columns claiming one canonical name is only unremarkable where
    # the order is documented - TIME from UTC before local. Anywhere else it
    # means the file has two columns that could be the same thing, and which
    # one was taken is a decision worth seeing. Borrowed from msomgom's
    # standardize_survey_columns(), which warns rather than choosing quietly.
    if (length(claimants) > 1L && is.null(narwc_alias_priority[[target]])) {
      rlang::warn(paste0(
        "More than one column could be `", target, "`: ",
        paste0("`", nms[claimants], "`", collapse = ", "),
        ". Using `", nms[pick], "`. Rename the others if that is wrong."
      ))
    }

    if (!is.null(displaced[target]) && !is.na(displaced[target])) {
      rlang::warn(paste0(
        "`", nms[pick], "` is being used as `", target, "`, and the file's own ",
        "`", target, "` column is kept as `", displaced[[target]], "`. Where a ",
        "file carries both, `", nms[pick], "` is the one to believe. Pass ",
        "`prefer_source = FALSE` to keep `", target, "` as it is."
      ))
    }

    factor_for <- narwc_unit_factors()[[target]]
    if (!is.null(factor_for)) {
      m <- match(norm(nms[pick]), norm(names(factor_for)))
      if (!is.na(m)) conversions[target] <- unname(factor_for[m])
    }

    renames[nms[pick]] <- target
    inferred <- c(inferred, guessed[pick])
    taken <- c(taken, target)
  }

  # Appended last so that `inferred` stays aligned with `renames`. A displaced
  # column is moved aside by an explicit rule, never by an inference.
  for (target in names(displaced)) {
    renames[target] <- displaced[[target]]
    inferred <- c(inferred, FALSE)
  }

  list(renames = renames, inferred = inferred, conversions = conversions,
       displaced = displaced)
}

# The whole dictionary, so a rename can be checked rather than trusted. Each
# line is marked with how it was reached: an exact entry in the alias table, or
# a match found only after normalising case and separators. The second kind is
# the one to read.
report_renamed_columns <- function(renames, inferred) {
  if (!length(renames)) {
    return(invisible(NULL))
  }
  how <- ifelse(inferred, "  <- inferred", "")
  width <- max(nchar(names(renames)))
  lines <- sprintf("  %-*s -> %s%s", width, names(renames), unname(renames), how)

  rlang::inform(paste0(
    "`read_narwc()` renamed ", length(renames), " column",
    if (length(renames) > 1) "s" else "", ":\n",
    paste(lines, collapse = "\n"),
    "\n",
    if (any(inferred)) {
      paste0(
        "Lines marked `inferred` matched only after ignoring case and ",
        "separators. Check they are what you meant"
      )
    } else {
      "All matched an exact entry in the alias table"
    },
    "; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it."
  ))
}


#' What the column names were changed to
#'
#' The record of how [read_narwc()] or [standardize_narwc_columns()] renamed a
#' data frame's columns, so a rename can be checked rather than trusted.
#'
#' @section Why keep it:
#' Matching ignores case and separators, which is what makes a real export
#' readable without hand-editing — and also what makes it possible for a column
#' to be renamed onto something you did not intend. The mapping travels with the
#' data as an attribute so the question "where did this column come from" has an
#' answer after the fact, not only in a message that has scrolled away.
#'
#' @param dat A data frame from [read_narwc()] or
#'   [standardize_narwc_columns()].
#'
#' @return A tibble with `original`, `standardized`, `match`, and `factor`.
#'   `match` is either `"alias"` for an exact entry in the alias table or
#'   `"inferred"` for one found after normalising case and separators. `factor`
#'   is the multiplier applied to reach the canonical unit — `0.3048` for an
#'   altitude that arrived in feet — and `NA` where the values were taken as
#'   they were. Zero rows when nothing was renamed.
#'
#' @seealso [read_narwc()], [standardize_narwc_columns()]
#'
#' @examples
#' raw <- data.frame(Event = 1, Lat_DD = 43, Long_DD = -69, Sea_State = 3)
#' dat <- standardize_narwc_columns(raw, quiet = TRUE)
#' narwc_column_mapping(dat)
#'
#' @export
narwc_column_mapping <- function(dat) {
  m <- attr(dat, "column_mapping")
  if (is.null(m)) {
    return(tibble::tibble(
      original = character(0), standardized = character(0),
      match = character(0)
    ))
  }
  m
}

# Build the record that travels with the data.
column_mapping_table <- function(renames, inferred, conversions = numeric(0)) {
  standardized <- unname(renames)
  tibble::tibble(
    original = names(renames),
    standardized = standardized,
    match = ifelse(inferred, "inferred", "alias"),
    factor = unname(conversions[match(standardized, names(conversions))])
  )
}

# Handbook 8.A.37: TIME is hhmmss in 24-hour form, and that is what this
# package stores. Real files also carry "12:34:56", "12:34", and whole
# timestamps like "2024-04-01T12:34:56Z" - and `as.numeric()` turns every one
# of those into NA without a word, which is how a file can arrive with a
# perfectly good clock and come out with no times at all.
#
# The last clock-looking piece of the string is taken, so a bare time and a
# full timestamp both work. Seconds are optional and default to zero.
parse_narwc_time <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  s <- trimws(as.character(x))
  s[!nzchar(s)] <- NA_character_
  out <- suppressWarnings(as.numeric(s))

  clock <- is.na(out) & !is.na(s) & grepl("[0-9]{1,2}:[0-9]{2}", s)
  if (any(clock)) {
    hit <- regmatches(
      s[clock],
      regexpr("[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?", s[clock])
    )
    parts <- strsplit(hit, ":", fixed = TRUE)
    out[clock] <- vapply(parts, function(p) {
      p <- suppressWarnings(as.numeric(p))
      p[1] * 10000 + p[2] * 100 + if (length(p) > 2 && !is.na(p[3])) p[3] else 0
    }, numeric(1))
  }
  out
}

# Give every record an EVENTNO, following what EVENTNO means in the handbook
# rather than merely filling the holes.
#
# Two rules govern it. It numbers an *event* — the aircraft's position and
# status at a moment — so several records of the same event share one number;
# a sighting is tied to the event it was made from, it does not get its own.
# And it increases through a FILEID, which is the invariant
# `eventno_not_increasing` already checks.
#
# So the fill works on events, not rows: records are grouped into events by
# date, time and position in the order the file already has them, an event
# that carries a number anywhere among its records lends it to the rest, and
# only an event with no number at all needs one invented. Those are given
# whole numbers that fit between their neighbours. Where no whole number fits,
# nothing conformant can be inserted, and the FILEID is renumbered from 1 with
# a warning — that is a real change to existing identifiers and is not done
# quietly.
supply_event_number <- function(dat, quiet = FALSE) {
  if (!nrow(dat)) {
    if (!"EVENTNO" %in% names(dat)) dat$EVENTNO <- numeric(0)
    return(dat)
  }

  # Only values are supplied, never the column. A file with no EVENTNO column
  # at all is missing a required variable, which `validate_narwc()` reports —
  # inventing one here would hide that, and would also quietly undo the rule
  # that a near-miss spelling like `EVENTN0` is left alone rather than guessed.
  if (!"EVENTNO" %in% names(dat) || !anyNA(dat$EVENTNO)) {
    return(dat)
  }

  n_before <- sum(is.na(dat$EVENTNO))
  file_key <- if ("FILEID" %in% names(dat)) as.character(dat$FILEID) else
    rep("", nrow(dat))

  # Handbook 8.A.10: records may share an event number only when they are
  # multiple sightings at one event, and then "all of the non-sighting
  # variables must be identical across all records". So an event is a run of
  # consecutive records agreeing on every non-sighting variable.
  # "Every non-sighting variable" means every variable the *survey* recorded.
  # Columns this package added are not survey variables: a displaced
  # `LATITUDE_ORIGINAL` carries the same information as the `LATITUDE` that
  # replaced it, and `ALT_ASSUMED` records a decision rather than an
  # observation. Letting them in makes event numbering depend on how the file
  # was read — the same extract read with and without the displacement gave
  # different EVENTNO on all 227,779 records.
  event_cols <- setdiff(
    names(dat),
    c(narwc_sighting_columns(), "EVENTNO", "FILEID",
      grep("_ORIGINAL$", names(dat), value = TRUE), "ALT_ASSUMED")
  )
  shifted <- character(0)

  for (f in unique(file_key)) {
    i <- which(file_key == f)
    ev <- if (length(event_cols)) {
      rle_id(do.call(paste, c(
        lapply(event_cols, function(nm) as.character(dat[[nm]][i])), sep = "\r"
      )))
    } else {
      seq_along(i)
    }

    # `rle_id()` numbers the events 1..k in order, so the first number
    # recorded for each can be gathered in one pass. Scanning the file once
    # per event instead is quadratic, and this runs on millions of records.
    have <- dat$EVENTNO[i]
    known <- rep(NA_real_, max(ev))
    seen <- which(!is.na(have))
    if (length(seen)) {
      first <- seen[!duplicated(ev[seen])]
      known[ev[first]] <- have[first]
    }

    out <- assign_event_numbers(known)
    if (out$moved > 0L) shifted <- c(shifted, f)
    dat$EVENTNO[i] <- out$v[ev]
  }

  if (!quiet) {
    rlang::inform(paste0(
      "`read_narwc()` supplied ", n_before, " missing EVENTNO value",
      if (n_before > 1) "s" else "",
      ". Records agreeing on every non-sighting variable are one event and ",
      "share a number (handbook 8.A.10); `make_eventno = FALSE` leaves them ",
      "as `NA`."
    ))
  }
  if (length(shifted)) {
    rlang::warn(paste0(
      "Inserting the missing event numbers in FILEID ",
      paste0("`", unique(shifted), "`", collapse = ", "),
      " left no free number, so the numbers from that point forward were ",
      "increased to keep EVENTNO increasing - the correction handbook 8.A.10 ",
      "prescribes for an inserted event. Any record elsewhere referring to ",
      "the old numbers past that point will no longer match."
    ))
  }
  dat
}

# Handbook 8.A.10: EVENTNO "must increase sequentially within a file", skipped
# numbers are allowed and duplicates are not. So walk the events in order,
# keep every number that is already larger than the last one used, and give
# an event without a number the next one free. Where a recorded number is no
# longer free - the file left no room for the event being inserted - it moves
# up, which is the handbook's own remedy: "it is usually necessary to correct
# the event numbers from that point forward in the file".
assign_event_numbers <- function(known) {
  j <- seq_along(known)

  # The walk written in closed form, because it runs once per event and there
  # can be millions. Each event takes its recorded number, or the next one
  # free after the event before it — so an event ends up at least `j`, and at
  # least one more than any earlier number already claimed:
  #     v[j] = j + max(0, max over i <= j of (known[i] - i))
  a <- ifelse(is.na(known), -Inf, known)
  v <- j + pmax(0, cummax(a - j))

  list(v = v, moved = sum(!is.na(known) & v != known))
}

# Rescale the columns whose input spelling named a unit that is not the
# canonical one. Applied after coercion, because a character column cannot be
# multiplied and silently skipping the conversion is the failure this exists
# to prevent.
apply_unit_conversions <- function(dat, conversions, quiet = FALSE) {
  for (target in names(conversions)) {
    if (!target %in% names(dat)) next
    if (!is.numeric(dat[[target]])) {
      rlang::warn(paste0(
        "`", target, "` needs converting to the canonical unit but is ",
        class(dat[[target]])[1], ", not numeric. It has been left as it is."
      ))
      next
    }
    dat[[target]] <- dat[[target]] * conversions[[target]]
    if (!quiet) {
      rlang::inform(paste0(
        "`", target, "` was multiplied by ", conversions[[target]],
        " to reach the unit this package uses."
      ))
    }
  }
  dat
}

# Glob patterns in `extra_columns`, so `Trk*` keeps a family of columns whose
# exact names differ between extracts.
expand_column_globs <- function(cols, nms) {
  if (!length(cols)) return(cols)
  out <- unlist(lapply(cols, function(p) {
    if (!grepl("[*?]", p)) return(p)
    nms[grepl(utils::glob2rx(p), nms)]
  }))
  unique(out)
}

# Tell the caller which columns were discarded, and whether they look like a
# known survey programme's. Information, not action: naming the profile is the
# caller's decision, because a column name means whatever the programme that
# wrote it says it means.
report_dropped_columns <- function(dropped) {
  lines <- paste0(
    "`read_narwc()` dropped ", length(dropped),
    " column", if (length(dropped) > 1) "s" else "",
    " not in the NARWC handbook schema:\n  ",
    paste(sort(dropped), collapse = ", ")
  )

  hits <- matching_profiles(dropped)
  if (length(hits)) {
    reg <- narwc_profiles(hits[1])
    n <- length(intersect(dropped, reg$column))
    lines <- c(lines, paste0(
      n, " of these are declared by the \"", hits[1], "\" profile (",
      reg$programme[1], ").\n",
      "Keep them with `profile = \"", hits[1], "\"`; see `narwc_profiles()`."
    ))
  } else {
    lines <- c(lines, paste0(
      "Keep them with `extra_columns = `, or all columns with ",
      "`extra_columns = NULL`.\nIf any of them carries position, effort, or ",
      "distance information, it must be mapped explicitly - `narwcr` will ",
      "not guess from a column name."
    ))
  }

  rlang::inform(paste(lines, collapse = "\n"))
}


#' Standardise column names onto the NARWC handbook's
#'
#' Renames a data frame's columns to their canonical NARWC names, matching
#' case-insensitively and ignoring separators. This is the step [read_narwc()]
#' does first, exported on its own so that other packages working with NARWC
#' survey data can use the same vocabulary rather than maintain a second one.
#'
#' @section Why this is shared:
#' Every pipeline that reads real survey exports hits the same wall: the files
#' say `Event`, `event_no`, or `Event No.` and the code wants `EVENTNO`. Solving
#' it once per package means the vocabularies drift, and a column recognised in
#' one place is silently dropped in another. The alias table here is the merge
#' of two such attempts — `distsamp`'s and `msomgom`'s — and this package exists
#' so that it is the only one that grows.
#'
#' @section What it will not do:
#' Guess by edit distance. `EVENTN0` with a zero stays `EVENTN0`, because a
#' column that is nearly a name is not that name. It also never renames onto a
#' canonical column that already exists, so a correctly named column always
#' wins, and it warns when two columns could both be one canonical name.
#'
#' Policy stays with the caller. This renames columns and nothing else: it does
#' not fill defaults, drop records, or coerce types. `read_narwc()` does those,
#' and packages with different needs keep their own on top. The standing example
#' is `ALT`: `msomgom` defaults a missing altitude to a nominal survey height,
#' which is reasonable for occupancy but would be wrong in `distsamp`, where
#' `ALT` feeds a right-angle distance and a fabricated altitude gives a
#' fabricated distance. Neither default belongs here.
#'
#' @param dat A data frame.
#' @param quiet Suppress the report of inferred matches. Default `FALSE`.
#' @param prefer_source Let a `Trk*` GPS track column take precedence over a
#'   canonical column of the same name. Default `TRUE`. See [read_narwc()].
#'
#' @return `dat` with columns renamed where a match was found.
#'
#' @seealso [read_narwc()], [narwc_schema()]
#'
#' @examples
#' raw <- data.frame(Event = 1, Lat = 43, Long = -69, Sea_State = 3,
#'                   check.names = FALSE)
#' names(standardize_narwc_columns(raw, quiet = TRUE))
#'
#' @export
standardize_narwc_columns <- function(dat, quiet = FALSE, prefer_source = TRUE) {
  stopifnot(is.data.frame(dat))
  if (!ncol(dat)) {
    return(dat)
  }
  resolved <- resolve_columns(names(dat), narwc_schema(), prefer_source, dat)
  if (length(resolved$renames)) {
    names(dat)[match(names(resolved$renames), names(dat))] <-
      unname(resolved$renames)
    if (!quiet) report_renamed_columns(resolved$renames, resolved$inferred)
  }
  dat <- apply_unit_conversions(dat, resolved$conversions, quiet)
  attr(dat, "column_mapping") <-
    column_mapping_table(resolved$renames, resolved$inferred,
                         resolved$conversions)
  dat
}
