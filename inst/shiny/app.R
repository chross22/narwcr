## The interactive viewer for a NARWC extract.
##
## narwcr's job is to get an extract into one predictable shape. This is the
## first thing worth doing with that shape: looking at it. A map answers the
## questions a validation report cannot -- whether the tracks are where the
## survey says it flew, whether a decade is missing, whether the sightings sit
## on the lines or somewhere else entirely.
##
## Five decisions shape the rest of this file.
##
## PREPARATION IS WHOLE-TABLE; FILTERING IS FOR DISPLAY ONLY. `make_leg_id()`
## finds line occupations from runs of consecutive records and `fill_legstage()`
## carries a state forward through them. Both read a record's neighbours. Run
## either on a table already cut down to one year and every line crossing the
## boundary is split in two, while the first record of the year loses the state
## it inherited from December. So the pipeline runs once, over everything, and
## the time controls filter what it produced.
##
## THE DATA TYPE IS READ, NOT ASSUMED. Aerial, vessel and opportunistic records
## arrive in one extract and are told apart by LEGTYPE (handbook 8.A.21). Where
## LEGTYPE says nothing the platform is inferred from how fast it was moving,
## via `classify_platform()` -- and the map says which records were read and
## which were inferred, because those are not the same claim. PAM is not in the
## sightings archive at all and comes from its own file.
##
## EFFORT CRITERIA ARE A CONTROL, NOT A CONSTANT. `flag_effort()` takes a sea
## state, an altitude and a visibility, and every one of them is an argument
## because a programme may reasonably choose differently. They are on the
## sidebar: moving the Beaufort cutoff and watching how much track survives is
## the fastest way to see what a threshold costs.
##
## THE MAP DOES NOT RESET WHEN A FILTER MOVES. Zooming into the Bay of Fundy and
## then stepping through the years should step through the years, not throw the
## view back to the whole northwest Atlantic each time. The basemap is rendered
## once and every data layer is redrawn through `leafletProxy()`, which leaves
## the viewport alone. "Zoom to selection" is a button, because it is a request.
##
## THE SEAFLOOR IS OPTIONAL AND OFF UNTIL ASKED FOR. Depth contours are cut
## from a real bathymetry grid rather than read off a basemap tile, which means
## the first draw fetches one from NOAA. An app that reaches out to a server
## the moment it opens is not what the rest of this package does, so the
## control starts unticked, the grid is cached where the rest of this stack
## caches it, and which depths are drawn is the reader's choice -- the 100 m
## and 200 m isobaths are what a right whale habitat map is read against, and
## no default should decide that for them.
##
## NOTHING IS DROPPED SILENTLY. A browser cannot draw four million positions, so
## tracks are thinned before drawing -- and the number thinned away is on
## screen, next to the map, every time. The failure this package exists to
## prevent is a dataset that quietly looks smaller than it is.

library(shiny)
library(narwcr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# A relative path typed into a file box means relative to where the app was
# launched, which is what someone typing "data/extract.csv" means. It is not
# where the app is running: `shiny::runApp()` moves into the app's own
# directory, so an unresolved relative path is looked for inside the installed
# package. Rewritten only when the file is actually found there, so that a
# Google Drive id or URL - which is not a path and must not be joined to one -
# passes through untouched for `narwc_fetch()` to deal with.
resolve_path <- function(p) {
  p <- trimws(p)
  wd <- getOption("narwcr.app_wd")
  if (!nzchar(p) || is.null(wd) || grepl("^(/|~|[A-Za-z]:[\\\\/])", p)) {
    return(p)
  }
  candidate <- file.path(wd, p)
  if (file.exists(candidate)) candidate else p
}

# How many track positions may be drawn before they are thinned, and how many
# line occupations may be drawn one at a time (which is what buys a hover label
# naming the line) before they are merged into one fast layer. Both are about
# what a browser will do rather than about what is interesting, so both are
# reported in the interface rather than buried here.
max_track_points <- 30000L
max_labelled_lines <- 150L

# Okabe-Ito, as everywhere else in this stack: eight qualitative colours that
# stay distinguishable under the common forms of colour blindness. Species are
# lumped to eight so that the legend stays a legend.
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")

# One colour per data type, used for tracks, for markers when the user colours
# by type, and for the PAM stations. Drawn from the same set, so a figure made
# from this app and one made by the rest of the stack agree.
type_colours <- c(
  aerial = "#0072B2", vessel = "#009E73", opportunistic = "#E69F00",
  PAM = "#CC79A7", unknown = "#999999"
)
ferry_colour <- "#9aa5ad"

# Shallow to deep. Mid-blue upward rather than pale, because a pale isobath
# over the pale blue of an ocean basemap is an isobath nobody can see.
depth_ramp <- grDevices::colorRampPalette(c("#5d8aa8", "#0b2545"))

# The depths worth offering. 100 and 200 are the two a North Atlantic right
# whale distribution is usually read against -- the shelf and the shelf break --
# and they are what this opens on.
# The basemaps, and the overlays the control offers. "PAM stations" is only
# offered when there are stations: a tickbox for a layer that does not exist
# reads as a layer that is empty, which is a different and much worse claim.
base_groups <- c("Ocean", "Quiet", "Satellite", "Streets")
overlay_groups <- function(has_pam) {
  c("Depth contours", "On-effort track", "Other track", "Sightings",
    if (isTRUE(has_pam)) "PAM stations")
}

depth_choices <- c(20, 50, 100, 200, 500, 1000, 2000, 3000, 4000)
default_depths <- c(100, 200, 1000)

# ------------------------------------------------------------ code books ----

# A display convenience, not a code book. The handbook's species list (8.A.29)
# is the authority and this covers only the codes common in North Atlantic
# survey data; anything unmatched is shown exactly as it was recorded, and the
# four-letter code is always shown beside the name, so nothing here can rename
# a sighting into something it is not. Extend or correct it without touching
# this file:
#
#   options(narwcr.species_labels = c(RIDO = "Risso's dolphin"))
species_label_table <- function() {
  base <- c(
    RIWH = "North Atlantic right whale", HUWH = "humpback whale",
    FIWH = "fin whale", MIWH = "minke whale", SEWH = "sei whale",
    BLWH = "blue whale", SPWH = "sperm whale", KIWH = "killer whale",
    PIWH = "pilot whale", HAPO = "harbour porpoise",
    ATWS = "Atlantic white-sided dolphin", BODO = "bottlenose dolphin",
    HASE = "harbour seal", GRSE = "grey seal",
    LOTU = "loggerhead turtle", LETU = "leatherback turtle",
    BASH = "basking shark", OCSU = "ocean sunfish"
  )
  extra <- getOption("narwcr.species_labels")
  if (is.character(extra) && length(extra) && !is.null(names(extra))) {
    base[names(extra)] <- unname(extra)
  }
  base
}

# "RIWH - North Atlantic right whale" where the code is known, "RIWH" where it
# is not. The code never disappears.
species_label <- function(code) {
  tab <- species_label_table()
  code <- as.character(code)
  hit <- unname(tab[code])
  ifelse(is.na(hit), code, paste0(code, " - ", hit))
}

# Which data type each LEGTYPE is (handbook 8.A.21).
#
# The handbook has no code for a *dedicated* shipboard survey: every shipboard
# record in the archive is a platform of opportunity, codes 5 and 6. So the four
# types this app draws are not four LEGTYPE families. "Vessel" is shipboard,
# "opportunistic" is the aerial platforms of opportunity (7 and 9), and both
# facts are on the Source tab so the split is never a silent one. A programme
# that reads its own extract differently says so in one line:
#
#   options(narwcr.legtype_types = c("5" = "opportunistic", "6" = "opportunistic"))
legtype_type_table <- function() {
  base <- c(
    "0" = "aerial", "1" = "aerial", "2" = "aerial", "3" = "aerial",
    "4" = "aerial", "5" = "vessel", "6" = "vessel",
    "7" = "opportunistic", "9" = "opportunistic"
  )
  extra <- getOption("narwcr.legtype_types")
  if (is.character(extra) && length(extra) && !is.null(names(extra))) {
    base[names(extra)] <- unname(extra)
  }
  base
}

survey_types <- c("aerial", "vessel", "opportunistic", "unknown")

# A code's meaning from the handbook's own book, with the code kept. This is
# what the popups are for: a reader should not have to hold LEGSTAGE 4 in their
# head to know that the aircraft had resumed the line.
code_meaning <- function(variable, value) {
  book <- narwc_codes(variable)
  key <- as.character(value)
  meaning <- unname(book[key])
  ifelse(is.na(key), NA_character_,
         ifelse(is.na(meaning), key, paste0(key, " - ", meaning)))
}

# TIME is HHMMSS held as a number, so 12500 is five past one in the morning and
# not the twelve thousand five hundredth of anything.
format_time <- function(x) {
  ifelse(is.na(x), NA_character_,
         sub("^(..)(..)(..)$", "\\1:\\2:\\3", sprintf("%06d", as.integer(x))))
}

fmt_int <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")

# Sighting rows: the ones naming a species. Every record in a NARWC extract is
# a position; only some of them are also a detection, and conflating the two is
# how effort gets counted as animals.
sightings_of <- function(dat) {
  if (!"SPECCODE" %in% names(dat)) return(dat[0, , drop = FALSE])
  code <- trimws(as.character(dat$SPECCODE))
  dat[!is.na(code) & nzchar(code), , drop = FALSE]
}

# ----------------------------------------------------------- preparation ----

# Everything that reads a record's neighbours, run once over the whole table.
prepare <- function(dat) {
  # `make_leg_id()` wants a LEGNO column and derives occupations from runs of
  # census track where its values are missing. An extract carrying no such
  # column gets an empty one rather than being refused: derived line identity
  # is exactly what that path is for.
  if (!"LEGNO" %in% names(dat)) dat$LEGNO <- NA_character_
  dat <- make_leg_id(dat, quiet = TRUE)
  if ("LEGSTAGE" %in% names(dat)) dat <- fill_legstage(dat, quiet = TRUE)

  # What a track is drawn along. A line occupation where one was found; failing
  # that the day's file, so transit and ferry still draw as a path rather than
  # as a heap of unconnected points.
  day <- paste(as.character(dat$FILEID), as.character(dat$DATE), sep = "\r")
  dat$TRACK <- ifelse(is.na(dat$LEGNO3), paste0("~", day), dat$LEGNO3)
  dat$TRACK <- paste(day, dat$TRACK, sep = "\r")

  typed <- survey_type(dat)
  dat$DATATYPE <- typed$type
  dat$TYPESOURCE <- typed$source
  dat
}

# Aerial, vessel or opportunistic, and where the answer came from.
#
# LEGTYPE first, because it is what the observers recorded. Where it is missing
# or is a code the table above does not name, speed decides: a survey aircraft
# flies at 90-120 knots and a survey vessel makes about ten, and nothing else
# is going to be mistaken for either. That second answer is a good inference and
# still an inference, which is why `TYPESOURCE` keeps the two apart -- the same
# reason `make_leg_id()` writes `line_3` and `derived_3` rather than calling
# both derived.
survey_type <- function(dat) {
  n <- nrow(dat)
  if (!n) {
    return(list(type = factor(character(0), levels = survey_types),
                source = character(0)))
  }
  tab <- legtype_type_table()
  type <- rep(NA_character_, n)
  source <- rep(NA_character_, n)

  if ("LEGTYPE" %in% names(dat)) {
    hit <- unname(tab[as.character(dat$LEGTYPE)])
    type <- hit
    source[!is.na(hit)] <- "LEGTYPE"
  }

  if (anyNA(type) && all(c("LATITUDE", "LONGITUDE", "TIME") %in% names(dat))) {
    kind <- tryCatch(
      suppressWarnings(classify_platform(dat, by = "TRACK")),
      error = function(e) NULL
    )
    if (!is.null(kind)) {
      # "stationary" is not one of the four types. A drifting vessel and a
      # recorder on the bottom look the same from a track log, and guessing
      # between them from speed alone is the guess this package does not make.
      guess <- as.character(kind)
      guess[!guess %in% c("aerial", "vessel")] <- NA_character_
      fill <- is.na(type) & !is.na(guess)
      type[fill] <- guess[fill]
      source[fill] <- "speed"
    }
  }

  type[is.na(type)] <- "unknown"
  source[is.na(source)] <- "unrecorded"
  list(type = factor(type, levels = survey_types), source = source)
}

# --------------------------------------------------------------- drawing ----

# Distance along the track, in kilometres, summed between consecutive positions
# of one occupation. The handbook (8.A.10) notes that the further apart two
# fixes are the less a straight line between them reconstructs what was flown,
# so where the receiver recorded its own TRKDIST that is used instead.
track_km <- function(dat) {
  if (!nrow(dat)) return(0)
  if ("TRKDIST" %in% names(dat) && any(!is.na(dat$TRKDIST))) {
    return(sum(as.numeric(dat$TRKDIST), na.rm = TRUE) / 1000)
  }
  n <- nrow(dat)
  if (n < 2L) return(0)
  same <- dat$TRACK[-n] == dat$TRACK[-1]
  # narwcr's own haversine, called from narwcr's own app, so a distance shown
  # here and a distance computed by the package are the same number.
  step <- narwcr:::haversine_km(
    dat$LATITUDE[-n], dat$LONGITUDE[-n], dat$LATITUDE[-1], dat$LONGITUDE[-1]
  )
  sum(step[same], na.rm = TRUE)
}

# Every nth position, with the first and last of each track always kept, so a
# thinned line still starts and ends where the survey did. Returns the kept rows
# and how many were dropped, because the second number has to be shown.
thin_tracks <- function(dat, max_points) {
  n <- nrow(dat)
  if (n <= max_points) return(list(dat = dat, dropped = 0L, every = 1L))
  every <- as.integer(ceiling(n / max_points))
  idx <- seq_len(n)
  first <- !duplicated(dat$TRACK)
  last <- !duplicated(dat$TRACK, fromLast = TRUE)
  keep <- (idx %% every == 0L) | first | last
  list(dat = dat[keep, , drop = FALSE], dropped = sum(!keep), every = every)
}

# Coordinates with an NA row between tracks. leaflet reads the gap as a break
# and draws one polyline per run, which is one call instead of one per line --
# the difference between a map that draws and a map that hangs.
break_at_track <- function(dat) {
  n <- nrow(dat)
  if (!n) return(list(lng = numeric(0), lat = numeric(0)))
  brk <- which(c(FALSE, dat$TRACK[-1] != dat$TRACK[-n]))
  idx <- seq_len(n)
  if (length(brk)) {
    # A half-index in front of each new track's first record, which then reads
    # back as NA: one gap, in the right place, without copying the coordinates.
    pos <- sort(c(idx, brk - 0.5))
    idx <- ifelse(pos %% 1 == 0, as.integer(pos), NA_integer_)
  }
  list(lng = dat$LONGITUDE[idx], lat = dat$LATITUDE[idx])
}

# Species reduced to at most eight named levels plus one labelled "other", so
# the legend names what a reader is looking for and every sighting still gets
# drawn. A meaning is not an identifier: RIWH has to stay findable.
lump_species <- function(code, max_levels = 8L) {
  ids <- as.character(code)
  tab <- sort(table(ids, useNA = "no"), decreasing = TRUE)
  if (length(tab) <= max_levels) return(factor(ids, levels = names(tab)))
  top <- names(tab)[seq_len(max_levels - 1L)]
  lab <- paste0("other (", length(tab) - length(top), " more)")
  ids[!ids %in% top] <- lab
  factor(ids, levels = c(top, lab))
}

# All the popups at once. Built row by row this was a data-frame subset per
# sighting, which is a visible pause on an archive carrying tens of thousands
# of them; there is nothing here that has to look at one row.
sighting_popups <- function(dat, bathy = NULL) {
  n <- nrow(dat)
  if (!n) return(character(0))

  col <- function(nm) if (nm %in% names(dat)) dat[[nm]] else rep(NA, n)
  line <- function(label, value) {
    v <- as.character(value)
    ifelse(!is.na(v) & nzchar(v), paste0("<b>", label, ":</b> ", v, "<br/>"), "")
  }

  vis <- suppressWarnings(as.numeric(col("VISIBLTY")))
  # Before 2004 this column held a code rather than a distance, and the 2021
  # update folded those codes back in as negative numbers. Showing -1 as
  # "-1 nmi" would report the handbook's word for good visibility as the worst
  # reading in the file.
  vis_text <- ifelse(is.na(vis), NA_character_,
                     ifelse(vis < 0, code_meaning("VISIBLTY", vis),
                            paste0(vis, " nmi")))
  ok <- suppressWarnings(visibility_ok(vis))
  vis_text <- ifelse(is.na(vis_text) | is.na(ok), vis_text,
                     paste0(vis_text,
                            ifelse(ok, " (acceptable)", " (below threshold)")))

  calves <- suppressWarnings(as.numeric(col("NUMCALF")))
  effort <- suppressWarnings(as.integer(col("OnOff.Effort")))
  type <- as.character(col("DATATYPE"))
  src <- as.character(col("TYPESOURCE"))
  type_text <- ifelse(is.na(type), NA_character_,
                      ifelse(src == "speed", paste0(type, " (inferred from speed)"),
                             type))

  paste0(
    "<div style='min-width:230px'>",
    "<b style='font-size:1.05em'>", species_label(col("SPECCODE")), "</b><br/>",
    line("Group size", col("NUMBER")),
    line("Calves", ifelse(is.na(calves) | calves == 0, NA, calves)),
    line("ID reliability", code_meaning("IDREL", col("IDREL"))),
    line("Date", format(col("DATE"), "%Y-%m-%d")),
    line("Time", format_time(col("TIME"))),
    line("Position", sprintf("%.4f, %.4f", col("LATITUDE"), col("LONGITUDE"))),
    # Only where a grid is already in hand. Nothing is fetched for a popup's
    # sake: the depth appears once the contours have been asked for, and is
    # absent rather than guessed at until then.
    line("Seafloor depth", if (is.null(bathy)) NA else {
      d <- depth_at(bathy, col("LONGITUDE"), col("LATITUDE"))
      ifelse(is.na(d), NA, paste0(round(d), " m"))
    }),
    line("Data type", type_text),
    line("On effort", c("no", "yes")[effort + 1L]),
    line("Leg type", code_meaning("LEGTYPE", col("LEGTYPE"))),
    line("Leg stage", code_meaning("LEGSTAGE", col("LEGSTAGE"))),
    line("Line occupation", col("LEGNO3")),
    line("Sea state", col("BEAUFORT")),
    line("Visibility", vis_text),
    line("Stratum", code_meaning("STRATUM", col("STRATUM"))),
    line("File / event", paste(col("FILEID"), col("EVENTNO"), sep = " / ")),
    "</div>"
  )
}

# Which of `flag_effort()`'s criteria is doing the eliminating.
#
# "0 km of effort in 2024" and "1,254 sightings in 2024" are both true at once
# whenever a criterion's column is empty for that era: a missing value fails a
# criterion by default, so a year that stopped recording altitude has no effort
# at all while still having every sighting it ever made. That is the failure
# this package was written about, and the map should be able to say which
# column caused it rather than leaving a zero on the screen.
effort_criteria <- function(dat, legtype_on_effort = 2L, max_beaufort = 3,
                            max_alt_m = 366, min_visibility_nmi = 2,
                            na_action = "fail") {
  n <- nrow(dat)
  if (!n) return(NULL)
  resolve <- function(x) {
    x[is.na(x)] <- (na_action == "pass")
    x
  }
  out <- list()
  push <- function(label, raw, present) {
    out[[length(out) + 1L]] <<- data.frame(
      Criterion = label,
      `Not recorded` = sum(!present),
      Passing = sum(resolve(raw)),
      `Passing %` = sprintf("%.1f%%", 100 * sum(resolve(raw)) / n),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  if ("LEGTYPE" %in% names(dat)) {
    push(paste0("LEGTYPE is ", paste(legtype_on_effort, collapse = " or ")),
         dat$LEGTYPE %in% legtype_on_effort, !is.na(dat$LEGTYPE))
  }
  # A dropped criterion keeps its row. "Not recorded: 1,394,338" against
  # "not applied" is the whole diagnosis in one line, and deleting the row
  # would take the number that explains the decision away with it.
  if ("BEAUFORT" %in% names(dat)) {
    if (is.null(max_beaufort)) {
      push("BEAUFORT - not applied", rep(TRUE, n), !is.na(dat$BEAUFORT))
    } else {
      push(paste0("BEAUFORT at most ", max_beaufort),
           dat$BEAUFORT <= max_beaufort, !is.na(dat$BEAUFORT))
    }
  }
  if ("ALT" %in% names(dat)) {
    if (is.null(max_alt_m)) {
      push("ALT - not applied", rep(TRUE, n), !is.na(dat$ALT))
    } else {
      push(paste0("ALT below ", max_alt_m, " m"),
           dat$ALT < max_alt_m, !is.na(dat$ALT))
    }
  }
  if ("VISIBLTY" %in% names(dat)) {
    if (is.null(min_visibility_nmi)) {
      push("VISIBLTY - not applied", rep(TRUE, n), !is.na(dat$VISIBLTY))
    } else {
      push(paste0("visibility at least ", min_visibility_nmi, " nmi"),
           visibility_ok(dat$VISIBLTY, min_nmi = min_visibility_nmi),
           !is.na(dat$VISIBLTY))
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# ---------------------------------------------------------------- legend ----
#
# One legend rather than four. leaflet will happily stack a legend per layer,
# and that is how the map ended up saying nothing at all in the one case where
# a reader most needs telling: a selection that draws nothing draws no legend
# either, so an empty map and a broken map look identical.
#
# This one is always on screen. It names exactly what is drawn, and when that
# is nothing it says so.

leg_dot <- function(colour, label) {
  paste0("<div class='nw-leg-row'><span class='nw-leg-dot' style='background:",
         colour, "'></span><span>", label, "</span></div>")
}

leg_line <- function(colour, label, weight = 3) {
  paste0("<div class='nw-leg-row'><span class='nw-leg-line' style='border-top-width:",
         weight, "px; border-top-color:", colour, "'></span><span>", label,
         "</span></div>")
}

leg_ring <- function(label) {
  paste0("<div class='nw-leg-row'><span class='nw-leg-ring'></span><span>",
         label, "</span></div>")
}

leg_ramp <- function(colours, from, to) {
  paste0("<div class='nw-leg-ramp' style='background:linear-gradient(to right,",
         paste(colours, collapse = ","), ")'></div>",
         "<div class='nw-leg-ends'><span>", from, "</span><span>", to,
         "</span></div>")
}

build_legend <- function(sections) {
  keep <- Filter(function(sec) length(sec$rows) > 0L, sections)
  if (!length(keep)) {
    return(paste0(
      "<div class='nw-leg'><div class='nw-leg-none'>Nothing is drawn for ",
      "this selection.</div></div>"
    ))
  }
  body <- vapply(keep, function(sec) {
    paste0("<div class='nw-leg-title'>", sec$title, "</div>",
           paste(sec$rows, collapse = ""))
  }, character(1))
  paste0("<div class='nw-leg'>", paste(body, collapse = ""), "</div>")
}

# ------------------------------------------------------------ bathymetry ----
#
# Depth contours cut from a grid, rather than a basemap that happens to be
# tinted by depth. The difference is that an isobath drawn from a grid is a
# number a reader can name -- this is the 100 m contour, that is the 200 m --
# where a shaded tile is a picture of one.
#
# The grid is ETOPO, fetched by `marmap::getNOAA.bathy()` and cached on disk.
# The cache follows marmap's own filename convention, so a grid already pulled
# down by anything else in this stack is read rather than fetched again, and
# one pulled down here serves them back.

# Where grids are kept. The default is R's own per-package cache directory.
# Point it at the cache the rest of the stack uses and a grid is downloaded
# once for all of them:
#
#   options(narwcr.cache = tools::R_user_dir("datamatch", which = "cache"))
bathy_cache <- function() {
  root <- getOption("narwcr.cache", Sys.getenv("NARWCR_CACHE"))
  if (!length(root) || !nzchar(root)) {
    root <- tools::R_user_dir("narwcr", which = "cache")
  }
  file.path(root, "bathymetry")
}

# The box a grid is fetched for, rounded so that the same data always asks for
# the same file. Padded, because a contour that stops exactly at the outermost
# sighting reads as a coastline that is not there.
bathy_extent <- function(lng, lat, pad = 0.1) {
  if (!length(lng) || !length(lat)) return(NULL)
  rx <- range(lng, na.rm = TRUE)
  ry <- range(lat, na.rm = TRUE)
  # A single-point extent has no width to pad by. Half a degree is about
  # 55 km, enough to show which way the seafloor falls away.
  dx <- max(diff(rx) * pad, 0.5)
  dy <- max(diff(ry) * pad, 0.5)
  list(
    x1 = round(max(-180, rx[1] - dx), 2), x2 = round(min(180, rx[2] + dx), 2),
    y1 = round(max(-90, ry[1] - dy), 2), y2 = round(min(90, ry[2] + dy), 2)
  )
}

# A finer grid over a wider box is a bigger download, and past a point it is a
# download nobody wanted. One arc-minute is ETOPO's own resolution and the
# right answer for a survey area; a whole-ocean extent gets stepped back.
suggest_resolution <- function(ext) {
  if (is.null(ext)) return(1)
  span <- max(ext$x2 - ext$x1, ext$y2 - ext$y1)
  if (span <= 12) 1 else if (span <= 30) 2 else if (span <= 60) 4 else 10
}

fetch_bathy <- function(ext, resolution, path = bathy_cache()) {
  if (!requireNamespace("marmap", quietly = TRUE)) {
    stop("Depth contours need the 'marmap' package. Install it with ",
         "install.packages(\"marmap\").", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # marmap's convention, not this app's: `marmap_coord_x1;y1;x2;y2_res_N.csv`.
  # Reading it directly is the same object `getNOAA.bathy()` would hand back,
  # without the round trip or the running commentary. Should marmap ever change
  # the convention this stops matching and every fetch goes back through
  # `getNOAA.bathy()`, which is slower and chattier, not wrong.
  num <- function(x) format(x, trim = TRUE, scientific = FALSE)
  cached <- file.path(path, sprintf(
    "marmap_coord_%s;%s;%s;%s_res_%s.csv",
    num(ext$x1), num(ext$y1), num(ext$x2), num(ext$y2), num(resolution)
  ))
  if (file.exists(cached)) {
    return(marmap::read.bathy(cached, header = TRUE))
  }
  marmap::getNOAA.bathy(
    lon1 = ext$x1, lon2 = ext$x2, lat1 = ext$y1, lat2 = ext$y2,
    resolution = resolution, keep = TRUE, path = path
  )
}

# A `bathy` object is a matrix with the longitudes on its rownames and the
# latitudes on its colnames. `contourLines()` wants both increasing, and will
# not say so if they are not.
bathy_axes <- function(b) {
  lon <- as.numeric(rownames(b))
  lat <- as.numeric(colnames(b))
  z <- unclass(b)
  io <- order(lon)
  jo <- order(lat)
  list(lon = lon[io], lat = lat[jo], z = z[io, jo, drop = FALSE])
}

# Contours at the depths asked for, as one entry per depth holding coordinates
# with an NA between the pieces -- the same break leaflet reads between two
# tracks. A depth the grid never reaches contributes nothing rather than an
# empty layer, which is how a 4,000 m contour behaves on the Scotian Shelf.
depth_contours <- function(b, depths) {
  ax <- bathy_axes(b)
  depths <- sort(unique(as.numeric(depths)))
  out <- list()
  for (d in depths) {
    pieces <- grDevices::contourLines(x = ax$lon, y = ax$lat, z = ax$z,
                                      levels = -d)
    if (!length(pieces)) next
    lng <- unlist(lapply(pieces, function(pc) c(pc$x, NA_real_)), use.names = FALSE)
    lat <- unlist(lapply(pieces, function(pc) c(pc$y, NA_real_)), use.names = FALSE)
    out[[as.character(d)]] <- list(
      depth = d, lng = utils::head(lng, -1), lat = utils::head(lat, -1),
      pieces = length(pieces)
    )
  }
  out
}

# Depth under a position, from the nearest grid cell. Nearest rather than
# interpolated on purpose: an interpolated value between one cell at 60 m and
# another at 300 m is a depth the seafloor does not have anywhere, and the
# popup is reporting what the grid says rather than modelling the bottom.
# Positive metres below the surface; NA over land, because a sighting there is
# a position problem and reporting it as "-12 m" hides one.
depth_at <- function(b, lon, lat) {
  if (is.null(b) || !length(lon)) return(rep(NA_real_, length(lon)))
  ax <- bathy_axes(b)
  i <- pmax(1L, pmin(length(ax$lon),
                     vapply(lon, function(v) which.min(abs(ax$lon - v)), integer(1))))
  j <- pmax(1L, pmin(length(ax$lat),
                     vapply(lat, function(v) which.min(abs(ax$lat - v)), integer(1))))
  z <- ax$z[cbind(i, j)]
  ifelse(is.na(z) | z >= 0, NA_real_, -z)
}

# ------------------------------------------------------------------- PAM ----
#
# Passive acoustic monitoring is not in the NARWC sightings archive and does not
# have its shape. A recorder sits on the bottom for months and reports, per
# station and per day, whether the species was heard and for how long it was
# listening. So there is no track, no sea state, and no group size -- the effort
# is recorded hours and the detection is a rate, not a count of animals.
#
# The reader below matches column names loosely, in the spirit of
# `read_narwc()`, and reports what it matched. It is deliberately app-local:
# narwcr reads the NARWC sightings database format, and this is a different
# format entirely.

pam_aliases <- list(
  STATION = c("STATION", "STATIONID", "SITE", "SITEID", "RECORDER",
              "DEPLOYMENT", "MOORING", "UNIT"),
  LATITUDE = c("LATITUDE", "LAT", "LATDD", "LATITUDEDD", "YCOORD"),
  LONGITUDE = c("LONGITUDE", "LON", "LONG", "LONDD", "LONGDD",
                "LONGITUDEDD", "XCOORD"),
  DATE = c("DATE", "DAY", "DATETIME", "STARTDATE", "RECORDINGDATE"),
  SPECIES = c("SPECIES", "SPECCODE", "SPP", "SPECIESCODE", "CALLTYPE"),
  DETECTED = c("DETECTED", "DETECTION", "PRESENCE", "PRESENT", "OCCURRENCE",
               "ACOUSTICPRESENCE"),
  HOURS_DETECTED = c("HOURSDETECTED", "DETECTIONHOURS", "DPH",
                     "DETECTIONPOSITIVEHOURS", "NDETECTIONHOURS",
                     "POSITIVEHOURS"),
  HOURS_RECORDED = c("HOURSRECORDED", "RECORDINGHOURS", "EFFORTHOURS",
                     "HOURSMONITORED", "RECORDEDHOURS", "MONITOREDHOURS",
                     "HOURS")
)

# "Y", "yes", "present", "detected", 1, TRUE. Anything else recorded is absence.
# A blank is neither: a day nobody scored is not a day the whale was absent, and
# putting the empty string on the FALSE list turns every gap in the analyst's
# coverage into evidence of silence -- which is the acoustic version of the
# mistake this package exists to stop.
is_detected <- function(x) {
  if (is.logical(x)) return(x)
  v <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(v))
  out[v %in% c("1", "y", "yes", "true", "t", "present", "detected", "d")] <- TRUE
  out[v %in% c("0", "n", "no", "false", "f", "absent", "notdetected",
               "not detected")] <- FALSE
  out
}

read_pam <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  key <- toupper(gsub("[^A-Za-z0-9]", "", names(raw)))

  matched <- character(0)
  take <- function(target) {
    hit <- which(key %in% pam_aliases[[target]])
    if (!length(hit)) return(NULL)
    matched[[target]] <<- names(raw)[hit[1]]
    raw[[hit[1]]]
  }

  out <- data.frame(
    STATION = as.character(take("STATION") %||% NA_character_),
    LATITUDE = suppressWarnings(as.numeric(take("LATITUDE") %||% NA)),
    LONGITUDE = suppressWarnings(as.numeric(take("LONGITUDE") %||% NA)),
    stringsAsFactors = FALSE
  )
  if (all(is.na(out$LATITUDE)) || all(is.na(out$LONGITUDE))) {
    stop("No latitude and longitude column found in the acoustic file. ",
         "One of ", paste(pam_aliases$LATITUDE, collapse = ", "),
         " is needed to place a station on a map.", call. = FALSE)
  }
  if (all(is.na(out$STATION))) out$STATION <- "unnamed station"

  d <- take("DATE")
  out$DATE <- if (is.null(d)) {
    parts <- c("YEAR", "MONTH", "DAY") %in% key
    if (all(parts)) {
      as.Date(sprintf("%04d-%02d-%02d",
                      as.integer(raw[[which(key == "YEAR")[1]]]),
                      as.integer(raw[[which(key == "MONTH")[1]]]),
                      as.integer(raw[[which(key == "DAY")[1]]])))
    } else {
      as.Date(rep(NA, nrow(raw)))
    }
  } else {
    suppressWarnings(as.Date(as.character(d)))
  }
  out$YEAR <- as.integer(format(out$DATE, "%Y"))
  out$MONTH <- as.integer(format(out$DATE, "%m"))

  sp <- take("SPECIES")
  out$SPECIES <- if (is.null(sp)) "unspecified" else trimws(as.character(sp))

  hours_det <- suppressWarnings(as.numeric(take("HOURS_DETECTED") %||% NA))
  hours_rec <- suppressWarnings(as.numeric(take("HOURS_RECORDED") %||% NA))
  det <- is_detected(take("DETECTED") %||% rep(NA, nrow(raw)))

  # Detection-positive hours settle it where they were recorded; a yes/no
  # column settles it where they were not. A row with neither is a row nobody
  # scored, and it counts as neither detected nor absent.
  out$DETECTED <- ifelse(!is.na(hours_det), hours_det > 0, det)
  out$HOURS_DETECTED <- hours_det
  out$HOURS_RECORDED <- hours_rec
  out$SCORED <- !is.na(out$DETECTED)

  attr(out, "pam_mapping") <- data.frame(
    standardized = names(matched), original = unlist(matched),
    row.names = NULL, stringsAsFactors = FALSE
  )
  out[!is.na(out$LATITUDE) & !is.na(out$LONGITUDE), , drop = FALSE]
}

# One row per station over the selected period. Recording effort is the
# denominator: a station that heard whales on four of five days it listened is
# not the same station as one that heard them on four of two hundred, and a map
# drawn on the count alone says they are.
pam_summary <- function(dat) {
  if (!nrow(dat)) {
    return(data.frame(STATION = character(0), LATITUDE = numeric(0),
                      LONGITUDE = numeric(0), days = integer(0),
                      days_detected = integer(0), rate = numeric(0),
                      hours_recorded = numeric(0), hours_detected = numeric(0),
                      first = as.Date(character(0)), last = as.Date(character(0)),
                      species = character(0), stringsAsFactors = FALSE))
  }
  split(dat, dat$STATION) |>
    lapply(function(s) {
      scored <- s[s$SCORED, , drop = FALSE]
      data.frame(
        STATION = s$STATION[1],
        LATITUDE = stats::median(s$LATITUDE, na.rm = TRUE),
        LONGITUDE = stats::median(s$LONGITUDE, na.rm = TRUE),
        days = nrow(scored),
        days_detected = sum(scored$DETECTED, na.rm = TRUE),
        rate = if (nrow(scored)) mean(scored$DETECTED, na.rm = TRUE) else NA_real_,
        hours_recorded = sum(s$HOURS_RECORDED, na.rm = TRUE),
        hours_detected = sum(s$HOURS_DETECTED, na.rm = TRUE),
        first = min(s$DATE, na.rm = TRUE),
        last = max(s$DATE, na.rm = TRUE),
        species = paste(sort(unique(s$SPECIES)), collapse = ", "),
        stringsAsFactors = FALSE
      )
    }) |>
    do.call(what = rbind)
}

pam_popups <- function(st) {
  line <- function(label, value) {
    v <- as.character(value)
    ifelse(!is.na(v) & nzchar(v), paste0("<b>", label, ":</b> ", v, "<br/>"), "")
  }
  paste0(
    "<div style='min-width:230px'>",
    "<b style='font-size:1.05em'>", st$STATION, "</b><br/>",
    "<i>passive acoustic monitoring</i><br/>",
    line("Species", species_label(st$species)),
    line("Days scored", fmt_int(st$days)),
    line("Days detected", fmt_int(st$days_detected)),
    line("Detection rate", ifelse(is.na(st$rate), NA,
                                  sprintf("%.0f%%", 100 * st$rate))),
    line("Hours recorded", ifelse(st$hours_recorded > 0,
                                  fmt_int(st$hours_recorded), NA)),
    line("Hours detected", ifelse(st$hours_detected > 0,
                                  fmt_int(st$hours_detected), NA)),
    line("Recording period", paste(format(st$first, "%Y-%m-%d"), "to",
                                   format(st$last, "%Y-%m-%d"))),
    line("Position", sprintf("%.4f, %.4f", st$LATITUDE, st$LONGITUDE)),
    "</div>"
  )
}

# ---------------------------------------------------------------- data in ----

# Set by run_narwc_app(). Falls back to the shipped example so that pointing
# shiny straight at this directory -- `shiny::runApp("inst/shiny")` after
# load_all(), the usual way to work on the app -- opens on something.
start_data <- getOption("narwcr.app_data")
start_label <- getOption("narwcr.app_source") %||% "shipped example"
if (is.null(start_data)) {
  start_data <- read_narwc(
    system.file("extdata", "narwc-example.csv", package = "narwcr"),
    quiet = TRUE
  )
  start_label <- "shipped example (inst/extdata/narwc-example.csv)"
}
# A path from the launcher is read here rather than there, by the same reader
# the app's own file box uses, so a file that fails reports the failure once
# and in one voice.
start_pam <- getOption("narwcr.app_pam")
start_pam_label <- NULL
if (is.character(start_pam)) {
  start_pam_label <- start_pam
  start_pam <- read_pam(start_pam)
} else if (!is.null(start_pam)) {
  start_pam_label <- "supplied to run_narwc_app()"
}

# --------------------------------------------------------------------- ui ----

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f7f9fa; }
    .nw-title { font-size: 1.5em; font-weight: 600; margin: 0; }
    .nw-sub { color: #5b6b76; margin: 2px 0 14px 0; }
    .nw-stats { display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 10px; }
    .nw-stat { flex: 1 1 110px; background: #fff; border: 1px solid #dce6ec;
               border-radius: 6px; padding: 8px 12px; }
    .nw-stat-value { font-size: 1.25em; font-weight: 600; color: #0b3d5c; }
    .nw-stat-label { font-size: 0.78em; color: #5b6b76; text-transform: uppercase;
                     letter-spacing: 0.04em; }
    .nw-note { color: #7a5c00; background: #fff8e1; border: 1px solid #f0e0a8;
               border-radius: 5px; padding: 6px 10px; font-size: 0.86em;
               margin-bottom: 8px; }
    .nw-section > summary { cursor: pointer; font-weight: 600; padding: 6px 0;
                            color: #0b3d5c; }
    .nw-section-body { padding: 4px 2px 10px 2px; }
    .well { background: #fff; }
    .nw-swatch { display: inline-block; width: 10px; height: 10px;
                 border-radius: 2px; margin-right: 6px; }
    .nw-leg { background: rgba(255,255,255,0.92); padding: 8px 11px;
              border-radius: 5px; box-shadow: 0 1px 5px rgba(0,0,0,0.3);
              font-size: 12px; line-height: 1.5; max-height: 340px;
              overflow-y: auto; max-width: 250px; }
    .nw-leg-title { font-weight: 600; font-size: 11px; text-transform: uppercase;
                    letter-spacing: 0.04em; color: #5b6b76; margin: 6px 0 2px 0; }
    .nw-leg-title:first-child { margin-top: 0; }
    .nw-leg-row { display: flex; align-items: center; gap: 7px; }
    .nw-leg-dot { flex: 0 0 auto; width: 11px; height: 11px; border-radius: 50%;
                  border: 1px solid #2b2b2b; }
    .nw-leg-ring { flex: 0 0 auto; width: 11px; height: 11px; border-radius: 50%;
                   border: 3px solid #CC79A7; }
    .nw-leg-line { flex: 0 0 auto; width: 16px; height: 0;
                   border-top-style: solid; }
    .nw-leg-ramp { height: 9px; border-radius: 2px; margin: 2px 0 1px 0; }
    .nw-leg-ends { display: flex; justify-content: space-between;
                   font-size: 10px; color: #5b6b76; }
    .nw-leg-none { color: #7a5c00; }
  "))),

  div(class = "nw-header",
      p(class = "nw-title", "NARWC survey map"),
      p(class = "nw-sub",
        "Aerial, vessel, opportunistic and acoustic survey data, read and ",
        "standardised by narwcr.")),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      tags$details(
        class = "nw-section", open = NA,
        tags$summary("Data type"),
        div(class = "nw-section-body",
            uiOutput("type_control"),
            helpText(
              "Aerial, vessel and opportunistic are read from LEGTYPE ",
              "(handbook 8.A.21), or inferred from platform speed where ",
              "LEGTYPE says nothing. PAM comes from its own file."))
      ),

      tags$details(
        class = "nw-section", open = NA,
        tags$summary("Time period"),
        div(class = "nw-section-body",
            uiOutput("date_control"),
            checkboxGroupInput(
              "months", "Months", inline = TRUE,
              choices = stats::setNames(1:12, month.abb), selected = 1:12
            ),
            actionLink("months_all", "all"), " / ",
            actionLink("months_none", "none"))
      ),

      tags$details(
        class = "nw-section", open = NA,
        tags$summary("Sightings"),
        div(class = "nw-section-body",
            uiOutput("species_control"),
            radioButtons("colour_by", "Colour by",
                         c("species" = "species", "data type" = "type"),
                         inline = TRUE),
            checkboxInput("effort_only", "On-effort sightings only", FALSE),
            checkboxInput("size_by_number", "Size markers by group size", TRUE),
            checkboxInput("cluster", "Cluster markers when crowded", TRUE),
            uiOutput("idrel_control"))
      ),

      tags$details(
        class = "nw-section", open = NA,
        tags$summary("Effort"),
        div(class = "nw-section-body",
            checkboxInput("show_effort", "Draw on-effort track", TRUE),
            checkboxInput("show_ferry", "Draw transit and off-effort track", FALSE),
            helpText(
              "The criteria below are flag_effort()'s arguments. They decide ",
              "which track counts as effort, and nothing else on this page ",
              "changes."),
            # A dropped altitude ceiling on its own rescues nothing: LEGTYPE
            # is the criterion before all the others, and the handbook has no
            # census code for a shipboard survey, so every vessel record is
            # off effort while this says 2 alone. It has to be a control for
            # the rest of this section to be able to do anything.
            selectizeInput(
              "legtypes", "Leg types that count as effort", multiple = TRUE,
              choices = stats::setNames(
                names(narwc_codes("LEGTYPE")),
                paste0(names(narwc_codes("LEGTYPE")), " - ",
                       unname(narwc_codes("LEGTYPE")))
              ),
              selected = "2",
              options = list(plugins = list("remove_button"))
            ),
            checkboxGroupInput(
              "criteria", "Criteria that apply",
              choiceNames = list("sea state (BEAUFORT)", "altitude (ALT)",
                                 "visibility (VISIBLTY)"),
              choiceValues = c("beaufort", "alt", "vis"),
              selected = c("beaufort", "alt", "vis")
            ),
            helpText(
              "Untick one and it is dropped, not widened. A vessel record ",
              "carries no altitude because it has no altitude to carry, so it ",
              "fails an altitude ceiling however high it is set - and a whole ",
              "shipboard survey goes off effort. Dropping the criterion says ",
              "it does not apply to this platform, which is narrower than ",
              "ignoring every missing value below."),
            conditionalPanel(
              "input.criteria && input.criteria.indexOf('beaufort') > -1",
              numericInput("max_beaufort", "Highest Beaufort sea state", 3,
                           min = 0, max = 12, step = 1)),
            conditionalPanel(
              "input.criteria && input.criteria.indexOf('alt') > -1",
              numericInput("max_alt", "Highest altitude (m)", 366,
                           min = 0, step = 10)),
            conditionalPanel(
              "input.criteria && input.criteria.indexOf('vis') > -1",
              numericInput("min_vis", "Least visibility (nmi)", 2,
                           min = 0, step = 0.5)),
            selectInput("na_action", "A criterion recorded as missing",
                        c("counts as off effort" = "fail",
                          "is ignored" = "pass")))
      ),

      tags$details(
        class = "nw-section",
        tags$summary("Seafloor"),
        div(class = "nw-section-body",
            checkboxInput("show_bathy", "Draw depth contours", FALSE),
            helpText(
              "Cut from an ETOPO grid, not read off the basemap. The first ",
              "draw fetches one from NOAA and caches it; after that it is ",
              "local."),
            selectizeInput(
              "contour_depths", "Contours (m)", multiple = TRUE,
              choices = depth_choices, selected = default_depths,
              options = list(plugins = list("remove_button"))
            ),
            uiOutput("bathy_res_control"),
            uiOutput("bathy_note"))
      ),

      tags$details(
        class = "nw-section",
        tags$summary("Files"),
        div(class = "nw-section-body",
            textInput("path", "Survey extract",
                      placeholder = "path, or a Google Drive id or URL"),
            actionButton("load", "Load extract", class = "btn-primary btn-sm"),
            br(), br(),
            textInput("pam_path", "Acoustic detections",
                      placeholder = "path to a station-day CSV"),
            actionButton("load_pam", "Load PAM", class = "btn-sm"),
            helpText("One row per station and day: station, position, date, ",
                     "species, whether it was detected, and how long the ",
                     "recorder was listening. Column names are matched ",
                     "loosely and what matched is on the Source tab."))
      ),

      hr(),
      actionButton("zoom", "Zoom to selection", class = "btn-sm"),
      downloadButton("download", "Download sightings", class = "btn-sm")
    ),

    mainPanel(
      width = 9,
      uiOutput("stats"),
      uiOutput("notes"),
      tabsetPanel(
        id = "tabs",
        tabPanel("Map", br(),
                 leaflet::leafletOutput("map", height = "640px")),
        tabPanel("Sightings", br(), DT::DTOutput("sightings_table")),
        tabPanel("Summary", br(),
                 h4("By year"), tableOutput("by_year"),
                 h4("By data type"), tableOutput("by_type"),
                 h4("Why records are off effort"),
                 helpText("Each of flag_effort()'s criteria against the ",
                          "current selection. A criterion whose column is not ",
                          "recorded fails every record it covers, which is how ",
                          "a whole era of survey ends up with no effort and ",
                          "all of its sightings."),
                 tableOutput("by_criterion"),
                 h4("By species"), tableOutput("by_species"),
                 h4("Acoustic stations"), tableOutput("by_station")),
        tabPanel("Source", br(),
                 h4("What was read"), verbatimTextOutput("source_text"),
                 h4("Columns renamed"),
                 helpText("read_narwc() matches column names loosely and ",
                          "reports every rename. Nothing was renamed silently."),
                 tableOutput("mapping"),
                 h4("How the data type was decided"),
                 helpText("LEGTYPE where the observers recorded one, platform ",
                          "speed where they did not."),
                 tableOutput("type_source"),
                 h4("Acoustic file"), tableOutput("pam_mapping"),
                 h4("Validation"),
                 helpText("validate_narwc(), over the whole table rather than ",
                          "the current selection."),
                 tableOutput("findings"))
      )
    )
  )
)

# ----------------------------------------------------------------- server ----

server <- function(input, output, session) {

  raw <- reactiveVal(start_data)
  label <- reactiveVal(start_label)
  pam <- reactiveVal(start_pam)
  pam_label <- reactiveVal(start_pam_label)

  observeEvent(input$load, {
    path <- resolve_path(input$path %||% "")
    if (!nzchar(path)) return()
    out <- withProgress(message = "Reading extract", value = 0.4, {
      tryCatch(list(ok = TRUE, dat = read_narwc(narwc_fetch(path), quiet = TRUE)),
               error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    })
    if (!out$ok) {
      showNotification(paste("Could not read that:", out$msg),
                       type = "error", duration = NULL)
      return()
    }
    raw(out$dat)
    label(path)
    showNotification(paste0("Read ", fmt_int(nrow(out$dat)), " records."),
                     type = "message")
  })

  observeEvent(input$load_pam, {
    path <- resolve_path(input$pam_path %||% "")
    if (!nzchar(path)) return()
    out <- withProgress(message = "Reading acoustic detections", value = 0.4, {
      tryCatch(list(ok = TRUE, dat = read_pam(narwc_fetch(path))),
               error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    })
    if (!out$ok) {
      showNotification(paste("Could not read that:", out$msg),
                       type = "error", duration = NULL)
      return()
    }
    pam(out$dat)
    pam_label(path)
    showNotification(paste0("Read ", fmt_int(nrow(out$dat)),
                            " station-days from ",
                            fmt_int(length(unique(out$dat$STATION))),
                            " stations."), type = "message")
  })

  # Whole-table, and only when the table itself changes. Every filter below
  # this line is a display filter.
  prepared <- reactive({
    dat <- raw()
    withProgress(message = "Resolving leg identity, line state and data type",
                 value = 0.5, prepare(dat))
  })

  # Whole-table too, because a criterion is a property of a record and not of a
  # selection: the effort shown for 1999 should not depend on whether 1999 is
  # what happens to be on screen.
  # A dropped criterion is NULL, not a threshold set so wide it cannot bite.
  # `flag_effort()` treats the two differently on purpose: a missing value
  # fails a criterion, so a vessel with no ALT fails an altitude ceiling of
  # any height, and only NULL says the criterion does not apply at all.
  criterion <- function(key, value) {
    if (!controls_ready()) return(value)
    if (key %in% (input$criteria %||% character(0))) value else NULL
  }

  effort_args <- reactive({
    list(
      legtype_on_effort = if (!controls_ready()) 2L else {
        suppressWarnings(as.integer(input$legtypes %||% character(0)))
      },
      max_beaufort = criterion("beaufort", input$max_beaufort %||% 3),
      max_alt_m = criterion("alt", input$max_alt %||% 366),
      min_visibility_nmi = criterion("vis", input$min_vis %||% 2),
      na_action = input$na_action %||% "fail"
    )
  })

  flagged <- reactive({
    args <- effort_args()
    suppressMessages(flag_effort(
      prepared(),
      legtype_on_effort = args$legtype_on_effort,
      max_beaufort = args$max_beaufort,
      max_alt_m = args$max_alt_m,
      min_visibility_nmi = args$min_visibility_nmi,
      na_action = args$na_action
    ))
  })

  # ------------------------------------------- controls built from the data ----

  # A `checkboxGroupInput` with nothing ticked sends NULL, and so does one that
  # has not rendered yet. Read as "no filter" - which is the obvious reading -
  # unticking every data type shows every data type, which is the opposite of
  # what was asked for. These say which NULL is which. Nothing filters until
  # its control exists; after that, empty means empty.
  controls_ready <- reactiveVal(FALSE)
  species_ready <- reactiveVal(FALSE)

  output$type_control <- renderUI({
    controls_ready(TRUE)
    present <- levels(droplevels(prepared()$DATATYPE))
    counts <- table(as.character(prepared()$DATATYPE))
    swatch <- function(k, n) {
      HTML(paste0("<span class='nw-swatch' style='background:",
                  type_colours[[k]], "'></span>", k,
                  " <span style='color:#7a8a95'>(", fmt_int(n), ")</span>"))
    }
    # `choiceNames` and `choiceValues`, not a named `choices` vector: the label
    # carries a colour swatch and a count, and names on a character vector are
    # flattened to text on their way through.
    labels <- lapply(present, function(k) swatch(k, counts[[k]]))
    values <- present

    # PAM only when there is acoustic data to draw. A tickbox for a layer that
    # does not exist reads as a layer that is empty.
    if (!is.null(pam())) {
      labels <- c(labels, list(swatch("PAM", length(unique(pam()$STATION)))))
      values <- c(values, "PAM")
    }
    checkboxGroupInput("types", NULL, choiceNames = labels,
                       choiceValues = values, selected = values)
  })

  output$date_control <- renderUI({
    d <- period_range()
    if (is.null(d)) return(helpText("No readable dates in this data."))
    if (d[1] == d[2]) {
      return(helpText(paste("One survey day:", format(d[1], "%Y-%m-%d"))))
    }
    sliderInput("dates", "Date range", min = d[1], max = d[2], value = d,
                timeFormat = "%Y-%m-%d",
                animate = animationOptions(interval = 1200))
  })

  # The slider has to cover the acoustic record as well as the survey one: a
  # recorder listening through a winter nobody flew is exactly the gap this
  # map is for.
  period_range <- reactive({
    d <- prepared()$DATE
    if (!is.null(pam())) d <- c(d, pam()$DATE)
    d <- d[!is.na(d)]
    if (!length(d)) NULL else range(d)
  })

  # A selection the user made themselves is kept where it still applies, so
  # that switching a data type off does not silently re-tick every species
  # they had narrowed down to.
  keep_selected <- function(previous, available) {
    if (is.null(previous)) return(available)
    still <- intersect(previous, available)
    if (length(still)) still else available
  }

  # The species there are to choose from, so that "none chosen" can be told
  # apart from "nothing to choose".
  species_codes <- reactive({
    codes <- sort(unique(trimws(as.character(sightings_of(in_types())$SPECCODE))))
    if (!is.null(selected_pam_all())) {
      codes <- sort(unique(c(codes, trimws(as.character(selected_pam_all()$SPECIES)))))
    }
    codes[nzchar(codes)]
  })

  idrel_codes <- reactive({
    as.character(sort(unique(stats::na.omit(sightings_of(in_types())$IDREL))))
  })

  output$species_control <- renderUI({
    species_ready(TRUE)
    codes <- species_codes()
    if (!length(codes)) {
      return(helpText("No sightings in the data types now selected."))
    }
    selectizeInput(
      "species", "Species", multiple = TRUE,
      choices = stats::setNames(codes, species_label(codes)),
      selected = keep_selected(isolate(input$species), codes),
      options = list(plugins = list("remove_button"))
    )
  })

  output$idrel_control <- renderUI({
    codes <- idrel_codes()
    if (!length(codes)) return(NULL)
    checkboxGroupInput(
      "idrel", "Identification reliability",
      choices = stats::setNames(
        codes, vapply(codes, function(k) code_meaning("IDREL", k), character(1))
      ),
      selected = keep_selected(isolate(input$idrel), codes)
    )
  })

  observeEvent(input$months_all,
               updateCheckboxGroupInput(session, "months", selected = 1:12))
  observeEvent(input$months_none,
               updateCheckboxGroupInput(session, "months", selected = character(0)))

  # --------------------------------------------------------- the selection ----

  # One time window and one data-type selection, applied to every record --
  # track, sighting and station alike, so the effort on screen is the effort
  # that produced the sightings on screen.
  # The data-type filter, on its own, because the species and reliability
  # controls have to read it too: offering a species that only the aerial
  # survey ever recorded, while the aerial survey is switched off, reads as
  # aerial data still being in the selection.
  in_types <- reactive({
    dat <- flagged()
    if (!controls_ready()) return(dat)
    dat[as.character(dat$DATATYPE) %in% (input$types %||% character(0)), ,
        drop = FALSE]
  })

  in_period <- reactive({
    dat <- in_types()
    keep <- rep(TRUE, nrow(dat))
    if (!is.null(input$dates)) {
      keep <- keep & !is.na(dat$DATE) &
        dat$DATE >= input$dates[1] & dat$DATE <= input$dates[2]
    }
    # `all` and `none` are both offered under the month boxes, and `none` has
    # to mean none.
    if (controls_ready() && "MONTH" %in% names(dat)) {
      months <- as.integer(input$months %||% character(0))
      if (length(months) < 12L) {
        keep <- keep & !is.na(dat$MONTH) & dat$MONTH %in% months
      }
    }
    dat[keep, , drop = FALSE]
  })

  mappable <- reactive({
    dat <- in_period()
    dat[!is.na(dat$LATITUDE) & !is.na(dat$LONGITUDE), , drop = FALSE]
  })

  selected_sightings <- reactive({
    dat <- sightings_of(mappable())
    if (!nrow(dat)) return(dat)
    if (species_ready() && length(species_codes())) {
      dat <- dat[trimws(as.character(dat$SPECCODE)) %in%
                   (input$species %||% character(0)), , drop = FALSE]
    }
    if (species_ready() && length(idrel_codes()) && "IDREL" %in% names(dat)) {
      dat <- dat[is.na(dat$IDREL) | as.character(dat$IDREL) %in%
                   (input$idrel %||% character(0)), , drop = FALSE]
    }
    if (isTRUE(input$effort_only)) {
      dat <- dat[dat$OnOff.Effort == 1L, , drop = FALSE]
    }
    dat
  })

  # How the sightings on screen are grouped and coloured, decided once. The
  # legend and the markers have to agree, and the only way to be sure of that
  # is for both to read the same answer rather than each working it out.
  sighting_groups <- reactive({
    dat <- selected_sightings()
    if (!nrow(dat)) return(NULL)
    by_type <- identical(input$colour_by, "type")
    grp <- if (by_type) {
      droplevels(factor(as.character(dat$DATATYPE), levels = survey_types))
    } else {
      lump_species(trimws(as.character(dat$SPECCODE)))
    }
    cols <- if (by_type) {
      unname(type_colours[levels(grp)])
    } else {
      okabe_ito[seq_len(nlevels(grp))]
    }
    list(
      grp = grp, cols = cols, fill = cols[as.integer(grp)],
      labels = if (by_type) levels(grp) else species_label(levels(grp)),
      title = if (by_type) "Data type" else "Species"
    )
  })

  # Which data types have on-effort track on screen, in the handbook's order.
  effort_types <- reactive({
    if (!isTRUE(input$show_effort)) return(character(0))
    intersect(survey_types, unique(as.character(effort_track()$DATATYPE)))
  })

  effort_track <- reactive({
    dat <- mappable()
    dat[dat$OnOff.Effort == 1L, , drop = FALSE]
  })

  ferry_track <- reactive({
    dat <- mappable()
    dat[dat$OnOff.Effort == 0L, , drop = FALSE]
  })

  selected_pam_all <- reactive({
    dat <- pam()
    if (is.null(dat)) return(NULL)
    if (!is.null(input$types) && !"PAM" %in% input$types) return(NULL)
    dat
  })

  selected_pam <- reactive({
    dat <- selected_pam_all()
    if (is.null(dat)) return(NULL)
    keep <- rep(TRUE, nrow(dat))
    if (!is.null(input$dates)) {
      keep <- keep & !is.na(dat$DATE) &
        dat$DATE >= input$dates[1] & dat$DATE <= input$dates[2]
    }
    if (controls_ready()) {
      months <- as.integer(input$months %||% character(0))
      if (length(months) < 12L) {
        keep <- keep & !is.na(dat$MONTH) & dat$MONTH %in% months
      }
    }
    if (species_ready() && length(species_codes())) {
      keep <- keep & trimws(as.character(dat$SPECIES)) %in%
        (input$species %||% character(0))
    }
    dat[keep, , drop = FALSE]
  })

  pam_stations <- reactive({
    dat <- selected_pam()
    if (is.null(dat) || !nrow(dat)) return(NULL)
    pam_summary(dat)
  })

  # The box a grid is fetched for. Built from the whole table rather than from
  # the current selection: the seafloor does not change with the year on the
  # slider, and refetching a grid every time the date moves would be a download
  # per keystroke.
  data_extent <- reactive({
    dat <- prepared()
    lng <- dat$LONGITUDE
    lat <- dat$LATITUDE
    if (!is.null(pam())) {
      lng <- c(lng, pam()$LONGITUDE)
      lat <- c(lat, pam()$LATITUDE)
    }
    bathy_extent(lng[!is.na(lng)], lat[!is.na(lat)])
  })

  output$bathy_res_control <- renderUI({
    ext <- data_extent()
    fitted <- suggest_resolution(ext)
    selectInput(
      "bathy_res", "Grid resolution",
      choices = stats::setNames(
        c(1, 2, 4, 10),
        paste0(c(1, 2, 4, 10), " arc-minute",
               ifelse(c(1, 2, 4, 10) == 1, "", "s"),
               ifelse(c(1, 2, 4, 10) == fitted, " (fits this extent)", ""))
      ),
      selected = fitted
    )
  })

  # A `reactiveVal` rather than a `reactive()`, because the sighting popups ask
  # for the grid too and must not stall waiting for one. Contours off means the
  # value is NULL and everything that reads it carries on without a depth.
  bathy_grid <- reactiveVal(NULL)

  observe({
    if (!isTRUE(input$show_bathy)) {
      bathy_grid(NULL)
      return()
    }
    ext <- data_extent()
    if (is.null(ext)) {
      bathy_grid(NULL)
      return()
    }
    res <- as.numeric(input$bathy_res %||% suggest_resolution(ext))
    out <- withProgress(
      message = "Fetching bathymetry", value = 0.4,
      tryCatch(fetch_bathy(ext, res), error = function(e) e)
    )
    if (inherits(out, "error")) {
      showNotification(paste("No bathymetry:", conditionMessage(out)),
                       type = "error", duration = NULL)
      bathy_grid(NULL)
      return()
    }
    bathy_grid(out)
  })

  bathy_contours <- reactive({
    b <- bathy_grid()
    if (is.null(b)) return(NULL)
    depths <- as.numeric(input$contour_depths %||% default_depths)
    if (!length(depths)) return(NULL)
    depth_contours(b, depths)
  })

  output$bathy_note <- renderUI({
    if (!requireNamespace("marmap", quietly = TRUE)) {
      return(div(class = "nw-note",
                 "Depth contours need the 'marmap' package: ",
                 tags$code("install.packages(\"marmap\")")))
    }
    if (!isTRUE(input$show_bathy)) return(NULL)
    b <- bathy_grid()
    if (is.null(b)) return(NULL)
    ext <- data_extent()
    cs <- bathy_contours()
    asked <- as.numeric(input$contour_depths %||% default_depths)
    drawn_depths <- as.numeric(names(cs %||% list()))
    missing <- setdiff(asked, drawn_depths)
    helpText(
      sprintf("Grid %d x %d over %.1f to %.1f E, %.1f to %.1f N.",
              nrow(b), ncol(b), ext$x1, ext$x2, ext$y1, ext$y2),
      if (length(missing)) {
        # A depth the grid never reaches is not a depth that failed to draw.
        sprintf(" No %s m contour: the seafloor here never gets that deep.",
                paste(missing, collapse = ", "))
      }
    )
  })

  # What was thinned away, so it can be said out loud.
  drawn <- reactiveValues(dropped = 0L, every = 1L, merged = FALSE, lines = 0L)

  # ------------------------------------------------------------- the map ----

  output$map <- leaflet::renderLeaflet({
    # Rendered once. Everything that changes with a filter goes through
    # leafletProxy() below, which leaves the viewport where the user put it.
    dat <- isolate(mappable())
    # Everything that will be drawn, acoustic stations included: a recorder
    # listening outside the survey box is exactly what this map is for.
    st <- isolate(pam_stations())
    lng <- c(dat$LONGITUDE, st$LONGITUDE)
    lat <- c(dat$LATITUDE, st$LATITUDE)
    # Esri rather than CartoDB.Positron, which is the quiet grey basemap this
    # stack uses elsewhere and now serves tiles reading "API KEY REQUIRED" to
    # anyone without a CARTO account. Ocean first: bathymetry is the context a
    # marine survey is read against, and a road network is not.
    m <- leaflet::leaflet() |>
      leaflet::addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
      leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Quiet") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "Streets") |>
      leaflet::addLayersControl(
        baseGroups = base_groups,
        overlayGroups = overlay_groups(!is.null(st)),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) |>
      leaflet::addScaleBar(position = "bottomleft")
    if (length(lng)) {
      m <- leaflet::fitBounds(m, min(lng), min(lat), max(lng), max(lat))
    } else {
      m <- leaflet::setView(m, -67, 43, zoom = 6)
    }
    m
  })

  # An acoustic file loaded after the page opened brings a layer the control
  # was not built with. Rebuilding it is one call, and leaflet replaces the
  # control rather than stacking a second one beside it.
  observeEvent(!is.null(pam()), {
    leaflet::addLayersControl(
      leaflet::leafletProxy("map"),
      baseGroups = base_groups,
      overlayGroups = overlay_groups(!is.null(pam())),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  }, ignoreInit = TRUE)

  # Tracks, one layer per data type so an aerial line and a vessel line are not
  # the same blue. Off-effort track stays grey whatever drew it: what it says is
  # "this is not effort", and that matters more than which platform it was.
  observe({
    map <- leaflet::leafletProxy("map")
    leaflet::clearGroup(map, "On-effort track")
    leaflet::clearGroup(map, "Other track")

    dropped <- 0L; every <- 1L; merged <- FALSE; lines <- 0L

    draw_track <- function(dat, colour, group, weight) {
      if (!nrow(dat)) return(invisible())
      thin <- thin_tracks(dat, max_track_points)
      dropped <<- dropped + thin$dropped
      every <<- max(every, thin$every)
      d <- thin$dat
      ids <- unique(d$TRACK)
      lines <<- lines + length(ids)
      if (length(ids) <= max_labelled_lines) {
        # Few enough to draw one at a time, which is what buys a hover label
        # naming the line occupation, its date and how far it ran.
        for (id in ids) {
          seg <- d[d$TRACK == id, , drop = FALSE]
          if (nrow(seg) < 2L) next
          leaflet::addPolylines(
            map, lng = seg$LONGITUDE, lat = seg$LATITUDE, group = group,
            color = colour, weight = weight, opacity = 0.75,
            label = sprintf(
              "%s - %s, %s, %s positions, %.0f km",
              if (is.na(seg$LEGNO3[1])) "outside any line" else seg$LEGNO3[1],
              format(seg$DATE[1], "%Y-%m-%d"), as.character(seg$DATATYPE[1]),
              fmt_int(nrow(seg)), track_km(seg)
            )
          )
        }
      } else {
        merged <<- TRUE
        co <- break_at_track(d)
        leaflet::addPolylines(map, lng = co$lng, lat = co$lat, group = group,
                              color = colour, weight = weight, opacity = 0.7)
      }
      invisible()
    }

    if (isTRUE(input$show_ferry)) {
      draw_track(ferry_track(), ferry_colour, "Other track", 1.5)
    }
    eff <- effort_track()
    for (k in effort_types()) {
      draw_track(eff[as.character(eff$DATATYPE) == k, , drop = FALSE],
                 type_colours[[k]], "On-effort track", 2.5)
    }

    drawn$dropped <- dropped
    drawn$every <- every
    drawn$merged <- merged
    drawn$lines <- lines
  })

  observe({
    map <- leaflet::leafletProxy("map")
    leaflet::clearGroup(map, "Sightings")

    dat <- selected_sightings()
    groups <- sighting_groups()
    if (!nrow(dat) || is.null(groups)) return()

    number <- suppressWarnings(as.numeric(dat$NUMBER))
    radius <- if (isTRUE(input$size_by_number) && any(!is.na(number))) {
      # Area with group size, not radius: a circle whose radius is the count
      # shows twenty animals as four hundred. Capped, because one group of 300
      # should not cover the bay it was seen in.
      pmin(22, 3 + 2.2 * sqrt(pmax(1, ifelse(is.na(number), 1, number)) - 1))
    } else {
      5
    }

    cluster <- if (isTRUE(input$cluster) && nrow(dat) > 400L) {
      leaflet::markerClusterOptions(disableClusteringAtZoom = 9)
    } else {
      NULL
    }

    leaflet::addCircleMarkers(
      map, lng = dat$LONGITUDE, lat = dat$LATITUDE, group = "Sightings",
      radius = radius, color = "#2b2b2b", weight = 0.7, opacity = 0.9,
      fillColor = groups$fill, fillOpacity = 0.85,
      popup = sighting_popups(dat, bathy = bathy_grid()),
      label = species_label(dat$SPECCODE),
      clusterOptions = cluster
    )
  })

  # PAM stations: a fixed point with a rate, not a track with detections. Drawn
  # as a ringed marker so it cannot be mistaken for a sighting, sized by how
  # long the recorder listened and filled by how often it heard anything.
  observe({
    map <- leaflet::leafletProxy("map")
    leaflet::clearGroup(map, "PAM stations")

    st <- pam_stations()
    if (is.null(st) || !nrow(st)) return()

    pal <- leaflet::colorNumeric("viridis", domain = c(0, 1), na.color = "#cccccc")
    effort <- if (any(st$hours_recorded > 0)) st$hours_recorded else st$days
    radius <- if (length(unique(effort)) > 1L) {
      6 + 8 * sqrt(effort / max(effort, na.rm = TRUE))
    } else {
      9
    }

    leaflet::addCircleMarkers(
      map, lng = st$LONGITUDE, lat = st$LATITUDE, group = "PAM stations",
      radius = radius, color = type_colours[["PAM"]], weight = 3, opacity = 1,
      fillColor = pal(st$rate), fillOpacity = 0.9,
      popup = pam_popups(st),
      label = sprintf("%s - %s%% of %s days scored", st$STATION,
                      ifelse(is.na(st$rate), "?", round(100 * st$rate)),
                      fmt_int(st$days))
    )
  })

  # Contours under everything else. One call per depth, its pieces separated by
  # the same NA break the tracks use, so a hundred closed rings cost one layer
  # rather than a hundred.
  observe({
    map <- leaflet::leafletProxy("map")
    leaflet::clearGroup(map, "Depth contours")

    cs <- bathy_contours()
    if (is.null(cs) || !length(cs)) return()

    depths <- vapply(cs, function(c) c$depth, numeric(1))
    cols <- depth_ramp(max(2L, length(depths)))[seq_along(depths)]
    for (i in seq_along(cs)) {
      leaflet::addPolylines(
        map, lng = cs[[i]]$lng, lat = cs[[i]]$lat, group = "Depth contours",
        color = cols[i], weight = 1.3, opacity = 0.85,
        label = paste0(depths[i], " m")
      )
    }
  })

  # The legend. Always on screen, naming exactly what is drawn - and saying so
  # when that is nothing, which is the case the four separate legends used to
  # leave blank.
  observe({
    sections <- list()

    groups <- sighting_groups()
    if (!is.null(groups)) {
      sections <- c(sections, list(list(
        title = groups$title,
        rows = mapply(leg_dot, groups$cols, groups$labels, USE.NAMES = FALSE)
      )))
    }

    track_rows <- character(0)
    for (k in effort_types()) {
      track_rows <- c(track_rows,
                      leg_line(type_colours[[k]], paste0(k, ", on effort"), 3))
    }
    if (isTRUE(input$show_ferry) && nrow(ferry_track())) {
      track_rows <- c(track_rows,
                      leg_line(ferry_colour, "transit and off effort", 2))
    }
    sections <- c(sections, list(list(title = "Track", rows = track_rows)))

    st <- pam_stations()
    if (!is.null(st) && nrow(st)) {
      ramp <- leaflet::colorNumeric("viridis", domain = c(0, 1))(seq(0, 1, length.out = 7))
      sections <- c(sections, list(list(
        title = "PAM stations",
        rows = c(leg_ring("size is recording effort"),
                 leg_ramp(ramp, "0%", "100% of days detected"))
      )))
    }

    cs <- bathy_contours()
    if (!is.null(cs) && length(cs)) {
      depths <- vapply(cs, function(co) co$depth, numeric(1))
      cols <- depth_ramp(max(2L, length(depths)))[seq_along(depths)]
      sections <- c(sections, list(list(
        title = "Depth",
        rows = mapply(function(colour, d) leg_line(colour, paste0(d, " m"), 2),
                      cols, depths, USE.NAMES = FALSE)
      )))
    }

    map <- leaflet::leafletProxy("map")
    leaflet::removeControl(map, "legend")
    leaflet::addControl(map, html = HTML(build_legend(sections)),
                        position = "bottomright", layerId = "legend")
  })

  observeEvent(input$zoom, {
    dat <- selected_sightings()
    st <- pam_stations()
    lng <- c(dat$LONGITUDE, st$LONGITUDE)
    lat <- c(dat$LATITUDE, st$LATITUDE)
    if (!length(lng)) {
      dat <- mappable()
      lng <- dat$LONGITUDE
      lat <- dat$LATITUDE
    }
    if (!length(lng)) return()
    leaflet::flyToBounds(leaflet::leafletProxy("map"),
                         min(lng), min(lat), max(lng), max(lat))
  })

  # ------------------------------------------------------ what is on screen ----

  output$stats <- renderUI({
    dat <- in_period()
    sight <- selected_sightings()
    number <- suppressWarnings(as.numeric(sight$NUMBER))
    calves <- suppressWarnings(as.numeric(sight$NUMCALF))
    eff <- effort_track()
    st <- pam_stations()

    stat <- function(value, lab) {
      div(class = "nw-stat",
          div(class = "nw-stat-value", value),
          div(class = "nw-stat-label", lab))
    }
    tiles <- list(
      stat(fmt_int(nrow(dat)), "records"),
      stat(fmt_int(nrow(sight)), "sightings"),
      stat(fmt_int(sum(number, na.rm = TRUE)), "animals"),
      stat(fmt_int(sum(calves, na.rm = TRUE)), "calves"),
      stat(fmt_int(length(unique(stats::na.omit(dat$DATE)))), "survey days"),
      stat(fmt_int(length(unique(stats::na.omit(eff$LEGNO3)))), "line occupations"),
      stat(paste0(fmt_int(round(track_km(eff))), " km"), "on-effort track")
    )
    if (!is.null(st) && nrow(st)) {
      tiles <- c(tiles, list(
        stat(fmt_int(nrow(st)), "PAM stations"),
        stat(fmt_int(sum(st$days)), "station-days"),
        stat(sprintf("%.0f%%", 100 * sum(st$days_detected) / max(1, sum(st$days))),
             "days detected")
      ))
    }
    div(class = "nw-stats", tiles)
  })

  output$notes <- renderUI({
    notes <- character(0)
    if (drawn$dropped > 0L) {
      notes <- c(notes, sprintf(
        paste("Track thinned for drawing: every %sth position kept, %s of them",
              "not drawn. Every distance, count and total on this page is",
              "computed from all of them."),
        drawn$every, fmt_int(drawn$dropped)))
    }
    if (isTRUE(drawn$merged)) {
      notes <- c(notes, sprintf(
        paste("%s line occupations on screen, past the %s that can be drawn",
              "individually, so the track is drawn as one layer and the lines",
              "carry no hover label. Narrow the date range to get them back."),
        fmt_int(drawn$lines), fmt_int(max_labelled_lines)))
    }
    # Sightings are drawn whether or not their record was on effort, but the
    # track is not - so a survey that mostly fails a criterion shows its
    # transects in the markers and nothing underneath them. That reads as
    # missing tracklines rather than as off-effort track left undrawn, and it
    # is the same question as "why does this year have no kilometres".
    off <- nrow(ferry_track())
    total <- nrow(mappable())
    if (total && off && !isTRUE(input$show_ferry)) {
      notes <- c(notes, sprintf(
        paste("%s of the %s positions in this selection are off effort and are",
              "not drawn, so there is no trackline under the sightings made",
              "on them. Tick \"Draw transit and off-effort track\" to see where",
              "the platform went, and \"Why records are off effort\" on the",
              "Summary tab for which criterion put them there."),
        fmt_int(off), fmt_int(total)))
    }
    inferred <- sum(in_period()$TYPESOURCE == "speed")
    if (inferred > 0L) {
      notes <- c(notes, sprintf(
        paste("%s records carry no LEGTYPE this app recognises, and their data",
              "type was inferred from how fast the platform was moving. That is",
              "a good guess and still a guess; the Source tab counts them."),
        fmt_int(inferred)))
    }
    if (!length(notes)) return(NULL)
    lapply(notes, function(n) div(class = "nw-note", n))
  })

  output$sightings_table <- DT::renderDT({
    dat <- selected_sightings()
    cols <- intersect(
      c("DATE", "TIME", "LATITUDE", "LONGITUDE", "SPECCODE", "NUMBER",
        "NUMCALF", "IDREL", "DATATYPE", "OnOff.Effort", "LEGTYPE", "LEGSTAGE",
        "LEGNO3", "BEAUFORT", "VISIBLTY", "FILEID", "EVENTNO"),
      names(dat)
    )
    out <- as.data.frame(dat[, cols, drop = FALSE])
    if ("TIME" %in% names(out)) out$TIME <- format_time(out$TIME)
    DT::datatable(out, rownames = FALSE, filter = "top",
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$by_year <- renderTable({
    dat <- in_period()
    if (!nrow(dat)) return(NULL)
    sight <- selected_sightings()
    eff <- effort_track()
    st <- selected_pam()
    years <- sort(unique(stats::na.omit(dat$YEAR)))
    out <- data.frame(
      Year = as.integer(years),
      Days = vapply(years, function(y)
        length(unique(stats::na.omit(dat$DATE[dat$YEAR == y]))), integer(1)),
      # Beside the kilometres, because "0 km" has two very different causes -
      # no record passed the criteria, or the records that did are a single
      # position each - and the count tells them apart at a glance.
      `On-effort records` = vapply(years, function(y)
        sum(eff$YEAR == y, na.rm = TRUE), integer(1)),
      `On-effort km` = vapply(years, function(y)
        round(track_km(eff[eff$YEAR == y, , drop = FALSE])), numeric(1)),
      Sightings = vapply(years, function(y)
        sum(sight$YEAR == y, na.rm = TRUE), integer(1)),
      Animals = vapply(years, function(y)
        sum(suppressWarnings(as.numeric(sight$NUMBER[sight$YEAR == y])),
            na.rm = TRUE), numeric(1)),
      check.names = FALSE
    )
    if (!is.null(st) && nrow(st)) {
      out$`Station-days` <- vapply(years, function(y)
        sum(st$YEAR == y & st$SCORED, na.rm = TRUE), integer(1))
    }
    out
  }, digits = 0)

  output$by_type <- renderTable({
    dat <- in_period()
    if (!nrow(dat)) return(NULL)
    sight <- selected_sightings()
    eff <- effort_track()
    types <- intersect(survey_types, unique(as.character(dat$DATATYPE)))
    if (!length(types)) return(NULL)
    data.frame(
      `Data type` = types,
      Records = vapply(types, function(k)
        sum(as.character(dat$DATATYPE) == k), integer(1)),
      `From LEGTYPE` = vapply(types, function(k)
        sum(as.character(dat$DATATYPE) == k & dat$TYPESOURCE == "LEGTYPE"),
        integer(1)),
      `On-effort km` = vapply(types, function(k)
        round(track_km(eff[as.character(eff$DATATYPE) == k, , drop = FALSE])),
        numeric(1)),
      Sightings = vapply(types, function(k)
        sum(as.character(sight$DATATYPE) == k), integer(1)),
      check.names = FALSE, row.names = NULL
    )
  }, digits = 0)

  output$by_criterion <- renderTable({
    dat <- in_period()
    if (!nrow(dat)) return(NULL)
    args <- effort_args()
    effort_criteria(
      dat,
      legtype_on_effort = args$legtype_on_effort,
      max_beaufort = args$max_beaufort,
      max_alt_m = args$max_alt_m,
      min_visibility_nmi = args$min_visibility_nmi,
      na_action = args$na_action
    )
  }, digits = 0)

  output$by_species <- renderTable({
    sight <- selected_sightings()
    if (!nrow(sight)) return(NULL)
    code <- trimws(as.character(sight$SPECCODE))
    codes <- names(sort(table(code), decreasing = TRUE))
    data.frame(
      Species = species_label(codes),
      Sightings = as.integer(table(code)[codes]),
      Animals = vapply(codes, function(k)
        sum(suppressWarnings(as.numeric(sight$NUMBER[code == k])),
            na.rm = TRUE), numeric(1)),
      `On effort` = vapply(codes, function(k)
        sum(sight$OnOff.Effort[code == k] == 1L, na.rm = TRUE), integer(1)),
      check.names = FALSE, row.names = NULL
    )
  }, digits = 0)

  output$by_station <- renderTable({
    st <- pam_stations()
    if (is.null(st) || !nrow(st)) {
      return(data.frame(Station = "no acoustic data loaded"))
    }
    data.frame(
      Station = st$STATION,
      Species = st$species,
      From = format(st$first, "%Y-%m-%d"),
      To = format(st$last, "%Y-%m-%d"),
      `Days scored` = st$days,
      `Days detected` = st$days_detected,
      `Detection rate` = ifelse(is.na(st$rate), NA,
                                sprintf("%.0f%%", 100 * st$rate)),
      `Hours recorded` = ifelse(st$hours_recorded > 0, st$hours_recorded, NA),
      check.names = FALSE, row.names = NULL
    )
  }, digits = 0)

  output$source_text <- renderText({
    dat <- prepared()
    lines <- c(
      label(),
      paste0(fmt_int(nrow(dat)), " records, ", ncol(dat), " columns"),
      if (all(is.na(dat$DATE))) "no readable dates" else
        paste0(format(min(dat$DATE, na.rm = TRUE)), " to ",
               format(max(dat$DATE, na.rm = TRUE))),
      paste0(fmt_int(length(unique(stats::na.omit(dat$FILEID)))), " survey files"),
      paste0(fmt_int(length(unique(stats::na.omit(dat$LEGNO3)))),
             " line occupations")
    )
    if (!is.null(pam())) {
      lines <- c(lines, "", paste0("Acoustic: ", pam_label()),
                 paste0(fmt_int(nrow(pam())), " station-days from ",
                        fmt_int(length(unique(pam()$STATION))), " stations"))
    }
    paste(lines, collapse = "\n")
  })

  output$mapping <- renderTable({
    map <- narwc_column_mapping(raw())
    if (is.null(map) || !nrow(map)) return(NULL)
    as.data.frame(map)
  })

  output$type_source <- renderTable({
    dat <- prepared()
    tab <- table(as.character(dat$DATATYPE), dat$TYPESOURCE)
    out <- as.data.frame.matrix(tab)
    data.frame(`Data type` = rownames(out), out, check.names = FALSE,
               row.names = NULL)
  }, digits = 0)

  output$pam_mapping <- renderTable({
    dat <- pam()
    if (is.null(dat)) return(data.frame(note = "No acoustic file loaded."))
    attr(dat, "pam_mapping")
  })

  output$findings <- renderTable({
    out <- validate_narwc(prepared())
    if (!nrow(out)) {
      return(data.frame(check = "none", message = "Nothing to report."))
    }
    as.data.frame(out[, c("check", "severity", "column", "n", "message")])
  }, digits = 0)

  output$download <- downloadHandler(
    filename = function() paste0("narwc-sightings-", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(as.data.frame(selected_sightings()), file,
                       row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
