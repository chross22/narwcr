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
#' canonical name does not win; `prefer_track = FALSE` turns it off.
#'
#' With no `Trk*` column present nothing changes — `LATITUDE` and `LONGITUDE`
#' are used exactly as they are.
#'
#' @section Units:
#' `ALT` is metres throughout (handbook 8.A.1), and it feeds the right-angle
#' distances in `distsamp`. A column whose *name* declares feet — `TrkAltitude_ft`,
#' `ALTFT`, `ALTITUDEFT` — is multiplied by `0.3048` on the way in, and the
#' multiplier is recorded in the `factor` column of [narwc_column_mapping()].
#' A file carrying both `TrkAltitude_m` and `TrkAltitude_ft` uses the metres one
#' and converts nothing.
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
#' @param prefer_track Let a `Trk*` GPS track column take precedence over a
#'   canonical column of the same name — `TrkLatitude` over a plain `LATITUDE`.
#'   Default `TRUE`; see below. `FALSE` restores "the column already named
#'   `LATITUDE` always wins".
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
                       drop_missing_position = TRUE, prefer_track = TRUE,
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
  resolved <- resolve_columns(names(dat), schema, prefer_track)
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
    # `prefer_track` quietly destructive.
    keep <- c(schema$required, schema$optional, resolved$displaced,
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

  # 4. Coerce the numeric NARWC variables.
  for (nm in intersect(narwc_numeric_columns, names(dat))) {
    if (!is.numeric(dat[[nm]])) {
      dat[[nm]] <- suppressWarnings(as.numeric(dat[[nm]]))
    }
  }

  # 4b. Rescale anything whose input name declared a different unit. After
  #     step 4, because until then these columns are still character.
  dat <- apply_unit_conversions(dat, resolved$conversions, quiet)

  # 5. Derive DATE when the date parts are all present.
  if (all(c("YEAR", "MONTH", "DAY") %in% names(dat)) && !"DATE" %in% names(dat)) {
    dat$DATE <- as.Date(sprintf("%04d-%02d-%02d", dat$YEAR, dat$MONTH, dat$DAY))
  } else if ("DATE" %in% names(dat) && !inherits(dat$DATE, "Date")) {
    dat$DATE <- as.Date(dat$DATE)
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
resolve_columns <- function(nms, schema, prefer_track = TRUE) {
  canonical <- c(schema$required, schema$optional)
  norm <- function(x) toupper(gsub("[^A-Za-z0-9]", "", x))

  input_norm <- norm(nms)
  taken <- nms[nms %in% canonical]
  renames <- character(0)

  # The one documented exception to "the real one always wins": a GPS track
  # column displaces a canonical column of the same name. The displaced column
  # is renamed rather than dropped, so nothing is lost and both are readable.
  displaced <- character(0)
  for (target in if (prefer_track) names(narwc_preferred_source) else character(0)) {
    if (!target %in% taken) next
    if (!any(input_norm %in% norm(narwc_preferred_source[[target]]))) next
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
        "`", target, "` column is kept as `", displaced[[target]], "`. A ",
        "`Trk*` column is the GPS track log; a plain `", target, "` beside it ",
        "is the position recorded for the platform. Pass ",
        "`prefer_track = FALSE` to keep `", target, "` as it is."
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
       displaced = unname(displaced))
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
#' @param prefer_track Let a `Trk*` GPS track column take precedence over a
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
standardize_narwc_columns <- function(dat, quiet = FALSE, prefer_track = TRUE) {
  stopifnot(is.data.frame(dat))
  if (!ncol(dat)) {
    return(dat)
  }
  resolved <- resolve_columns(names(dat), narwc_schema(), prefer_track)
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
