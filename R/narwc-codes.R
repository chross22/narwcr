#' NARWC database code books
#'
#' Lookup tables for the coded variables in the North Atlantic Right Whale
#' Consortium (NARWC) sightings database, transcribed from Kenney (2023),
#' *The North Atlantic Right Whale Consortium Database: A Guide for Users and
#' Contributors*, Version 8 (NARWC Reference Document 2023-01), Chapter 8.
#'
#' These tables are the single source of truth for the package: validation,
#' effort determination, and sighting filtering all read their permitted values
#' from here rather than hard-coding numeric literals.
#'
#' @param variable Name of a coded NARWC variable. One of `"LEGTYPE"`,
#'   `"LEGSTAGE"`, `"IDREL"`, `"TAXCODE"`, `"STRATUM"`, `"VISIBLTY"`, or
#'   `"WX"`. If
#'   `NULL` (the default), the whole code book is returned.
#'
#' @return A named character vector mapping codes to their meanings, or, when
#'   `variable` is `NULL`, a named list of such vectors.
#'
#' @section Handbook sections:
#' `LEGTYPE` 8.A.21, `LEGSTAGE` 8.A.20, `IDREL` 8.A.16, `TAXCODE` 8.A.36,
#' `STRATUM` 8.A.30, `VISIBLTY` 8.A.38, `WX` 8.A.39.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*. North Atlantic Right Whale
#' Consortium Reference Document 2023-01. University of Rhode Island, Graduate
#' School of Oceanography, Narragansett, Rhode Island.
#'
#' @examples
#' narwc_codes("LEGSTAGE")
#' names(narwc_codes("LEGTYPE"))
#'
#' @export
narwc_codes <- function(variable = NULL) {
  if (is.null(variable)) {
    return(narwc_code_book)
  }
  variable <- toupper(variable)
  if (!variable %in% names(narwc_code_book)) {
    cli_abort_bad_arg(
      "variable", variable, names(narwc_code_book)
    )
  }
  narwc_code_book[[variable]]
}

# Handbook 8.A.21. Note that codes 0-4 describe line-transect (and "relaxed"
# line-transect) aerial surveys, 5-6 shipboard platforms-of-opportunity, and
# 7/9 aerial platforms-of-opportunity. Only LEGTYPE 2 is a census track that
# can contribute effort to a density estimate.
narwc_legtype <- c(
  "0" = "line-transect aerial, off-watch during transit, cross-leg, or circling",
  "1" = "line-transect aerial, transit",
  "2" = "line-transect aerial, survey line",
  "3" = "line-transect aerial, cross-leg",
  "4" = "line-transect aerial, other (circling)",
  "5" = "POP ship, underway",
  "6" = "POP ship, not underway",
  "7" = "POP aerial",
  "9" = "POP aerial, restricted data-recording"
)

# Handbook 8.A.20. For dedicated aerial surveys LEGSTAGE is recorded only
# during census lines (LEGTYPE == 2), with the exception of code 7.
narwc_legstage <- c(
  "1" = "begin line",
  "2" = "continue line",
  "3" = "break off line to circle",
  "4" = "resume line",
  "5" = "end line",
  "6" = "sighting by anyone other than an on-duty observer",
  "7" = "sighting detected in a vertical photograph"
)

# Handbook 8.A.16.
narwc_idrel <- c(
  "1" = "unsure / possible",
  "2" = "probable",
  "3" = "definite / sure",
  "9" = "unknown / not recorded"
)

# Handbook 8.A.36.
narwc_taxcode <- c(
  "0" = "vessel, gear, human activity, debris/pollution",
  "1" = "large cetacean",
  "2" = "medium cetacean",
  "3" = "small cetacean",
  "4" = "other marine mammal",
  "5" = "sea turtle",
  "6" = "shark",
  "7" = "other fish",
  "8" = "bird",
  "9" = "other / unknown"
)

# Handbook 8.A.30.
narwc_stratum <- c(
  "X" = "0-20 fathoms",
  "Y" = "20-50 fathoms",
  "Z" = ">50 fathoms",
  "0" = "non-stratified aerial survey block",
  "A" = "Scotian Shelf block half",
  "B" = "Scotian Shelf block half",
  "I" = "Florida, inshore",
  "O" = "Florida, offshore",
  "M" = "NLPSC year 2+, Martha's Vineyard",
  "R" = "NLPSC year 2+, Rhode Island"
)

# Handbook 8.A.39.
narwc_wx <- c(
  "B" = "both rain (or other precipitation) and fog",
  "C" = "clear",
  "D" = "drizzle",
  "F" = "fog",
  "G" = "gray (heavy overcast and dark, but no precipitation)",
  "H" = "haze",
  "L" = "light rain, intermittent showers",
  "P" = "patchy fog",
  "R" = "rain",
  "S" = "snow",
  "T" = "thunderstorms, squalls",
  "X" = "not recorded"
)

# Handbook 8.A.38. Negative values are the pre-2004 OLDVIZ codes, folded into
# VISIBLTY when the archive was updated. Non-negative values are an actual
# clear-visibility distance in nautical miles.
narwc_visiblty <- c(
  "-1" = "clear visibility for at least 2 nautical miles",
  "-2" = "visibility less than 2 miles, fog",
  "-3" = "visibility less than 2 miles, haze",
  "-4" = "visibility less than 2 miles, rain",
  "-5" = "visibility less than 2 miles, snow"
)

narwc_code_book <- list(
  LEGTYPE  = narwc_legtype,
  LEGSTAGE = narwc_legstage,
  IDREL    = narwc_idrel,
  TAXCODE  = narwc_taxcode,
  STRATUM  = narwc_stratum,
  VISIBLTY = narwc_visiblty,
  WX       = narwc_wx
)

#' NARWC columns recognised by narwcr
#'
#' The subset of the NARWC variable list (handbook Table 1) that this package
#' reads, together with the alternative spellings accepted on input.
#'
#' `required` names the columns without which no segmentation is possible.
#' `optional` names columns that are carried through and used when present.
#' `aliases` maps input column names onto the internal name.
#'
#' @section Coordinate naming:
#' The handbook's canonical event-position columns are `LAT_DD` and `LONG_DD`
#' (8.A.18, 8.A.22). Data extracts distributed by the NARWC database manager,
#' and some upstream processing pipelines, instead use `LATITUDE` and
#' `LONGITUDE`. This package standardises internally on `LATITUDE`/`LONGITUDE`
#' and accepts either spelling on input. `S_LAT`/`S_LONG` (8.A.33, 8.A.34) are
#' the *exact sighting* position and are kept distinct from the event position.
#'
#' @section Version 8 additions:
#' `ANGLEL` and `ANGLER` are declination angles to a sighting, added in Version 8
#' of the handbook. The New England Aquarium survey team stopped recording
#' `STRIP` in 2022 and switched to these, which give a more precise right-angle
#' distance and are less sensitive to changes in survey altitude (8.A.31). They
#' are carried through as optional columns; converting them to perpendicular
#' distance belongs with the detection-function work, which is out of scope for
#' this version.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, Table 1. NARWC Reference Document
#' 2023-01.
#'
#' @return A named list with elements `required`, `optional`, and `aliases`.
#'
#' @examples
#' narwc_schema()$required
#'
#' @export
narwc_schema <- function() {
  list(
    required = c(
      "FILEID", "EVENTNO", "YEAR", "MONTH", "DAY", "TIME",
      "LATITUDE", "LONGITUDE", "LEGTYPE"
    ),
    optional = c(
      "LEGSTAGE", "LEGNO", "ALT", "BEAUFORT", "VISIBLTY", "WX", "CLOUD",
      "GLAREL", "GLARER", "SURFTEMP", "HEADING", "PLATFORM", "STRATUM",
      "BLOCK", "SPECCODE", "TAXCODE", "IDREL", "NUMBER", "NUMCALF",
      "SIGHTNO", "STRIP", "ANGLEL", "ANGLER", "S_LAT", "S_LONG", "S_TIME",
      "PHOTOS", "DDSOURCE", "IDSOURCE", "CONFIDNC", "TRKDIST"
    ),
    aliases = c(
      LAT_DD    = "LATITUDE",
      LONG_DD   = "LONGITUDE",
      LON_DD    = "LONGITUDE",
      LATDD     = "LATITUDE",
      LONGDD    = "LONGITUDE",
      LAT       = "LATITUDE",
      LON       = "LONGITUDE",
      LONG      = "LONGITUDE",
      LATITUDE_DD  = "LATITUDE",
      LONGITUDE_DD = "LONGITUDE",
      LEGTYPE_BK = "LEGTYPE",
      VISIBILITY = "VISIBLTY",
      VISIBLITY  = "VISIBLTY",
      SPECIES    = "SPECCODE",
      SPEC_CODE  = "SPECCODE",
      EVENT      = "EVENTNO",
      EVENT_NO   = "EVENTNO",
      EVENTNUM   = "EVENTNO",
      YR         = "YEAR",
      MO         = "MONTH",
      # Time and date arrive under whichever zone the programme records in.
      # `TIME` wins if present; otherwise UTC before local, so a multi-source
      # dataset lands on one clock. `read_narwc()` reports which was used.
      TIME_UTC   = "TIME",
      GMT        = "TIME",
      TIME_GMT   = "TIME",
      GMT_TIME   = "TIME",
      TIME_LOC   = "TIME",
      TIME_LOCAL = "TIME",
      TIMEUTC    = "TIME",
      TIMELOC    = "TIME",
      DATE_UTC   = "DATE",
      DATE_LOC   = "DATE",
      DATE_LOCAL = "DATE",
      SIGHT_NO   = "SIGHTNO",
      FIELD_SIGHTNO = "SIGHTNO",
      NUM        = "NUMBER",
      GROUPSIZE  = "NUMBER",
      ALTITUDE   = "ALT",
      SEASTATE   = "BEAUFORT",
      SEA_STATE  = "BEAUFORT",
      # Merged in from msomgom's standardize_survey_columns(), which solved
      # this independently against real exports and had seen spellings this
      # table had not.
      FILE       = "FILEID",
      FILENAME   = "FILEID",
      EVENTNUMBER = "EVENTNO",
      EVNO       = "EVENTNO",
      EVENTID    = "EVENTNO",
      MON        = "MONTH",
      DY         = "DAY",
      LNG        = "LONGITUDE",
      LEG        = "LEGTYPE",
      STAGE      = "LEGSTAGE",
      ALTFT      = "ALT",
      ALTITUDEFT = "ALT",
      # Deliberately not HEIGHT. It reads like an altitude alias, but marine
      # survey files carry swell, wave and cloud height far more often than
      # they carry a column called "Height" meaning the aircraft's altitude,
      # and ALT feeds a right-angle distance downstream. Found against real
      # survey data in msomgom, where a substring fallback made it worse:
      # "Swell_Height_m" beat "TrkAltitude_m" to ALT.
      AIRCRAFTALT  = "ALT",
      TRKALTITUDE  = "ALT",
      # The `Trk*` family is the platform's own GPS track log, as distinct from
      # the position the observer wrote down. Where both exist the track log is
      # the recorded truth, which is why these also appear in
      # `narwc_preferred_source`. The `_ft` spelling is converted to metres by
      # `narwc_unit_factors()`; `ALT` is metres throughout (handbook 8.A.1).
      TRKLATITUDE    = "LATITUDE",
      TRKLAT         = "LATITUDE",
      TRKLONGITUDE   = "LONGITUDE",
      TRKLON         = "LONGITUDE",
      TRKLONG        = "LONGITUDE",
      TRKALTITUDE_M  = "ALT",
      TRKALTITUDE_FT = "ALT",
      TRKTIME_UTC    = "TIME",
      TRKTIME_LOCAL  = "TIME",
      # Distance travelled since the previous fix, as the receiver measured it
      # rather than as a straight line between two fixes. Not a handbook
      # variable, but the handbook itself notes (8.A.10) that the farther
      # apart the fixes, the less a straight line between them reconstructs
      # the track — so where this exists it is the better measure of effort.
      # Canonical unit is metres; the nautical-mile spelling is converted.
      TRKDIST_M      = "TRKDIST",
      TRKDIST_NM     = "TRKDIST",
      HDG        = "HEADING",
      COURSE     = "HEADING",
      WEATHER    = "WX",
      CLOUDCOVER = "CLOUD",
      CLOUDS     = "CLOUD",
      VIS        = "VISIBLTY",
      VISNM      = "VISIBLTY",
      BFT        = "BEAUFORT",
      BEAUFORTSCALE = "BEAUFORT",
      SPECODE    = "SPECCODE",
      SPPCODE    = "SPECCODE",
      SPP        = "SPECCODE",
      SPCODE     = "SPECCODE",
      IDRELIABILITY = "IDREL",
      RELIABILITY = "IDREL",
      COUNT      = "NUMBER",
      NUMANIMALS = "NUMBER",
      CONFIDENCE = "CONFIDNC",
      CONF       = "CONFIDNC"
    )
  )
}

# When more than one input column maps to the same canonical name, this is the
# order they are preferred in. Anything not named here comes after these, in
# the order the alias table lists it.
# A list, not c(): `c(TIME = c("a", "b"))` names the elements TIME1 and TIME2,
# so nothing would ever match on the name.
narwc_alias_priority <- list(
  # GMT and UTC are the same clock, so they rank together, ahead of local.
  # The GPS clock wins, but not across zones: a UTC column is still taken
  # before a local one, because landing a dataset on two clocks is a worse
  # failure than taking the observer's UTC time over the receiver's.
  TIME = c("TRKTIME_UTC", "TIME_UTC", "TIMEUTC", "GMT", "TIME_GMT", "GMT_TIME",
           "TRKTIME_LOCAL", "TIME_LOC", "TIMELOC", "TIME_LOCAL"),
  DATE = c("DATE_UTC", "DATE_LOC", "DATE_LOCAL"),
  # A file carrying both `TrkAltitude_m` and `TrkAltitude_ft` is not ambiguous:
  # take the metres one and skip a conversion that can only lose precision.
  ALT = c("TRKALTITUDE_M", "TRKALTITUDE", "TRKALTITUDE_FT", "ALTITUDE",
          "AIRCRAFTALT", "ALTFT", "ALTITUDEFT"),
  TRKDIST = c("TRKDIST_M", "TRKDIST_NM")
)

# Sources that outrank a canonical column *already present under its own name*.
# This is the one exception to "the real one always wins", and it exists
# because the exception is the honest reading of the data: a `Trk*` column is
# the GPS track log, and a plain `LATITUDE` alongside it is the position
# recorded for the platform, which on a vessel-and-aircraft file is not the
# same place. Taking the plain column would silently survey the wrong track.
#
# The displaced column is never discarded — it is kept as `<TARGET>_ORIGINAL`
# and the swap is warned about, because it is a decision worth seeing.
narwc_preferred_source <- list(
  LATITUDE  = c("TRKLATITUDE", "TRKLAT"),
  LONGITUDE = c("TRKLONGITUDE", "TRKLON", "TRKLONG"),
  ALT       = c("TRKALTITUDE_M", "TRKALTITUDE", "TRKALTITUDE_FT"),
  # A MEMDR-era quirk rather than a GPS one, but the same shape of problem:
  # where a file carries both, `LEGTYPE_BK` is the leg type to believe.
  LEGTYPE   = "LEGTYPE_BK",
  # Only the UTC track clock displaces a plain `TIME`. `TrkTime_Local` would
  # move the whole dataset onto another zone to gain the same seconds, which
  # is not a trade this should make without being asked.
  TIME      = "TRKTIME_UTC"
)

# Multipliers onto the canonical unit, keyed by canonical target and then by
# the input column it came from. `ALT` is metres (handbook 8.A.1) and feeds
# `perp_distance()` in distsamp, so a column named in feet that is read as
# metres overstates every right-angle distance by a factor of 3.28.
narwc_unit_factors <- function() {
  list(
    ALT = c(TRKALTITUDE_FT = 0.3048, ALTFT = 0.3048, ALTITUDEFT = 0.3048),
    TRKDIST = c(TRKDIST_NM = 1852)
  )
}

# Columns that are numeric in the NARWC schema. Everything else recognised is
# read as character.
narwc_numeric_columns <- c(
  "EVENTNO", "YEAR", "MONTH", "DAY", "TIME", "LATITUDE", "LONGITUDE",
  "LEGTYPE", "LEGSTAGE", "LEGNO", "ALT", "BEAUFORT", "VISIBLTY", "CLOUD",
  "GLAREL", "GLARER", "SURFTEMP", "HEADING", "PLATFORM", "TAXCODE", "IDREL",
  "NUMBER", "NUMCALF", "SIGHTNO", "STRIP", "ANGLEL", "ANGLER",
  "S_LAT", "S_LONG", "S_TIME", "PHOTOS", "TRKDIST"
)
