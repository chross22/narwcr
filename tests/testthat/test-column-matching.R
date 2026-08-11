# Every message a call emits, not just the first - read_narwc() can report
# renames, dropped columns, and dropped records in one call.
all_messages <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(
    force(expr),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  paste(msgs, collapse = "\n")
}

messy <- function(...) {
  base <- data.frame(
    FileID = "A", Event = 1:3, Year = 2024, Month = 4, Day = 1,
    stringsAsFactors = FALSE
  )
  cbind(base, data.frame(..., stringsAsFactors = FALSE))
}

test_that("case and separators do not have to match", {
  d <- suppressMessages(read_narwc(messy(
    Lat_DD = 43, Long_DD = -69, LegType = 2, leg_no = 1, Sea_State = 3
  )))
  expect_true(all(c("FILEID", "EVENTNO", "YEAR", "MONTH", "DAY",
                    "LATITUDE", "LONGITUDE", "LEGTYPE", "LEGNO",
                    "BEAUFORT") %in% names(d)))
})

test_that("a column already correctly named always wins", {
  d <- suppressMessages(read_narwc(data.frame(
    FILEID = "A", EVENTNO = 1, YEAR = 2024, MONTH = 4, DAY = 1,
    LATITUDE = 43, Lat_DD = 99, LONGITUDE = -69, LEGTYPE = 2,
    stringsAsFactors = FALSE
  )))
  expect_equal(d$LATITUDE, 43)
})

test_that("every rename is reported, marked by how it was matched", {
  # The full dictionary, so a rename can be checked rather than trusted. An
  # exact alias is shown too - it is still a change to the data's column names.
  msg <- all_messages(read_narwc(messy(LAT_DD = 43, LONG_DD = -69, LegType = 2)))
  expect_match(msg, "LAT_DD +-> LATITUDE")
  expect_match(msg, "LegType +-> LEGTYPE")

  # Only the ones that took an inference are flagged as such.
  expect_match(msg, "LegType +-> LEGTYPE +<- inferred")
  expect_no_match(msg, "LAT_DD +-> LATITUDE +<- inferred")
})

test_that("the mapping travels with the data and can be read back", {
  dat <- suppressMessages(read_narwc(messy(LAT_DD = 43, LONG_DD = -69,
                                           LegType = 2)))
  m <- narwc_column_mapping(dat)

  expect_true(all(c("original", "standardized", "match") %in% names(m)))
  expect_equal(m$standardized[m$original == "LAT_DD"], "LATITUDE")
  expect_equal(m$match[m$original == "LAT_DD"], "alias")
  expect_equal(m$match[m$original == "LegType"], "inferred")

  # Nothing renamed, nothing recorded.
  clean <- data.frame(FILEID = "A", EVENTNO = 1, YEAR = 2024, MONTH = 4,
                      DAY = 1, LATITUDE = 43, LONGITUDE = -69, LEGTYPE = 2)
  expect_equal(nrow(narwc_column_mapping(
    suppressMessages(read_narwc(clean))
  )), 0)

  # And a frame that never went through either function has no mapping.
  expect_equal(nrow(narwc_column_mapping(data.frame(a = 1))), 0)
})

test_that("time is taken from whichever zone the file records", {
  for (nm in c("TIME_UTC", "TIME_LOC", "Time_Local", "GMT", "gmt", "Time_GMT")) {
    raw <- messy(LAT_DD = 43, LONG_DD = -69, LegType = 2)
    raw[[nm]] <- "120000"
    d <- suppressMessages(read_narwc(raw))
    expect_true("TIME" %in% names(d), info = nm)
    expect_equal(d$TIME[1], 120000, info = nm)
  }
})

test_that("when two zones are present UTC is preferred, and TIME beats both", {
  raw <- messy(LAT_DD = 43, LONG_DD = -69, LegType = 2,
               TIME_LOC = "080000", TIME_UTC = "120000")
  expect_equal(suppressMessages(read_narwc(raw))$TIME[1], 120000)

  raw$TIME <- "999999"
  expect_equal(suppressMessages(read_narwc(raw))$TIME[1], 999999)
})

test_that("GMT is the same clock as UTC and outranks local", {
  raw <- messy(LAT_DD = 43, LONG_DD = -69, LegType = 2,
               TIME_LOC = "080000", GMT = "120000")
  expect_equal(suppressMessages(read_narwc(raw))$TIME[1], 120000)
})

test_that("records with no position are dropped, and said so", {
  raw <- messy(LAT_DD = c(43, 43.1, NA), LONG_DD = c(-69, NA, -69), LegType = 2)
  expect_match(all_messages(read_narwc(raw)), "Dropped 2 records")

  d <- suppressMessages(read_narwc(raw))
  expect_equal(nrow(d), 1)
  expect_false(anyNA(d$LATITUDE))

  kept <- suppressMessages(read_narwc(raw, drop_missing_position = FALSE))
  expect_equal(nrow(kept), 3)
})

test_that("a file with no missing positions says nothing about them", {
  expect_no_match(
    all_messages(read_narwc(messy(LAT_DD = 43, LONG_DD = -69, LegType = 2))),
    "Dropped"
  )
})

test_that("extra_columns takes glob patterns", {
  raw <- messy(LAT_DD = 43, LONG_DD = -69, LegType = 2,
               Trk_Speed = 1, Trk_Head = 2, Other = 3)
  d <- suppressMessages(read_narwc(raw, extra_columns = "Trk*"))
  expect_true(all(c("Trk_Speed", "Trk_Head") %in% names(d)))
  expect_false("Other" %in% names(d))

  # A plain name still works, and mixing the two is fine.
  d2 <- suppressMessages(read_narwc(raw, extra_columns = c("Trk*", "Other")))
  expect_true("Other" %in% names(d2))
})

test_that("nothing is matched by edit distance", {
  # `EVENTN0` with a zero is not `EVENTNO`, and guessing it would be worse
  # than leaving it alone.
  raw <- messy(LAT_DD = 43, LONG_DD = -69, LegType = 2)
  names(raw)[names(raw) == "Event"] <- "EVENTN0"
  d <- suppressMessages(read_narwc(raw, extra_columns = "EVENTN0"))
  expect_false("EVENTNO" %in% names(d))
  expect_true("EVENTN0" %in% names(d))
})

test_that("two inputs cannot both claim one canonical name", {
  raw <- messy(Lat_DD = 43, latitude = 44, LONG_DD = -69, LegType = 2)
  expect_warning(d <- suppressMessages(read_narwc(raw)), "More than one column")
  expect_equal(sum(names(d) == "LATITUDE"), 1)
  # `latitude` is the canonical name bar case, so it wins over the alias.
  expect_equal(unique(d$LATITUDE), 44)
})

test_that("the shared vocabulary covers what msomgom had learned", {
  # These aliases came from msomgom's standardize_survey_columns(), which met
  # real exports this table had not.
  raw <- data.frame(
    Event = 1, Lat = 43, Long = -69, Bft = 3, Spp = "RIWH",
    Hdg = 90, Vis = 5, Clouds = 2, Weather = 1, Count = 2, Conf = 3,
    check.names = FALSE
  )
  out <- standardize_narwc_columns(raw, quiet = TRUE)
  expect_true(all(c("EVENTNO", "LATITUDE", "LONGITUDE", "BEAUFORT", "SPECCODE",
                    "HEADING", "VISIBLTY", "CLOUD", "WX", "NUMBER",
                    "CONFIDNC") %in% names(out)))
})

test_that("standardize_narwc_columns renames and does nothing else", {
  raw <- data.frame(Event = 1, Lat = 43, Long = -69, Junk = "keep")
  out <- standardize_narwc_columns(raw, quiet = TRUE)

  # No columns dropped, no types coerced, no records removed - policy belongs
  # to the caller.
  expect_equal(nrow(out), nrow(raw))
  expect_true("Junk" %in% names(out))
  expect_equal(ncol(out), ncol(raw))

  # No ALT is invented. msomgom defaults one; here ALT feeds perp_distance(),
  # so a fabricated altitude would produce fabricated distances.
  expect_false("ALT" %in% names(out))
})

test_that("two columns claiming one name warn, and the canonical one wins", {
  expect_warning(
    out <- standardize_narwc_columns(
      data.frame(Lat = 43, Latitude = 44, Long = -69), quiet = TRUE
    ),
    "More than one column could be `LATITUDE`"
  )
  expect_equal(out$LATITUDE, 44)
})

test_that("a documented preference is not warned about", {
  # TIME from UTC before local is a documented order, not an ambiguity.
  expect_silent(
    standardize_narwc_columns(
      data.frame(TIME_UTC = "120000", TIME_LOC = "080000"), quiet = TRUE
    )
  )
})

test_that("an empty data frame is handled", {
  expect_equal(ncol(standardize_narwc_columns(data.frame(), quiet = TRUE)), 0)
})

test_that("a bare Height column is not taken for an altitude", {
  # Regression, from real survey data via msomgom: "height" was an ALT alias,
  # and marine survey files carry swell, wave and cloud height far more often
  # than they carry an aircraft altitude called "Height". ALT feeds a
  # right-angle distance, so a wrong match here fabricates distances.
  raw <- data.frame(Height = 2.1, EVENTNO = 1, check.names = FALSE)
  out <- suppressMessages(standardize_narwc_columns(raw))
  expect_false("ALT" %in% names(out))
  expect_true("Height" %in% names(out))
})

test_that("the altitude columns real survey files use are matched", {
  raw <- data.frame(TrkAltitude = 500, EVENTNO = 1, check.names = FALSE)
  out <- suppressMessages(standardize_narwc_columns(raw))
  expect_equal(out$ALT, 500)
})

# A GPS track column beside a canonical column of the same name --------------

trk_base <- function() {
  data.frame(
    FILEID = "F", EVENTNO = "1", Year = "2024", Month = "4", Day = "1",
    Time_UTC = "120000", LEGTYPE = "2", stringsAsFactors = FALSE
  )
}

test_that("a Trk column outranks a canonical column of the same name", {
  dat <- trk_base()
  dat$TrkLatitude <- "43.5"
  dat$LATITUDE <- "9.9"
  dat$TrkLongitude <- "-69.5"
  dat$LONGITUDE <- "-1.1"

  out <- suppressWarnings(read_narwc(dat, quiet = TRUE))
  expect_equal(out$LATITUDE, 43.5)
  expect_equal(out$LONGITUDE, -69.5)
})

test_that("the displaced column is kept, not dropped", {
  dat <- trk_base()
  dat$TrkLatitude <- "43.5"
  dat$LATITUDE <- "9.9"

  out <- suppressWarnings(read_narwc(dat, quiet = TRUE))
  expect_true("LATITUDE_ORIGINAL" %in% names(out))
  expect_equal(out$LATITUDE_ORIGINAL, "9.9")
})

test_that("displacing a column warns, because it is a decision worth seeing", {
  dat <- trk_base()
  dat$TrkLatitude <- "43.5"
  dat$LATITUDE <- "9.9"
  expect_warning(read_narwc(dat, quiet = TRUE), "LATITUDE_ORIGINAL")
})

test_that("prefer_track = FALSE restores the real-one-wins rule", {
  dat <- trk_base()
  dat$TrkLatitude <- "43.5"
  dat$LATITUDE <- "9.9"

  out <- read_narwc(dat, prefer_track = FALSE, quiet = TRUE)
  expect_equal(out$LATITUDE, 9.9)
  expect_false("LATITUDE_ORIGINAL" %in% names(out))
})

test_that("with no Trk column the plain position is used as it is", {
  dat <- trk_base()
  dat$LATITUDE <- "43.25"
  dat$LONGITUDE <- "-69.25"

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$LATITUDE, 43.25)
  expect_false(any(grepl("_ORIGINAL$", names(out))))
})

# Altitude units -------------------------------------------------------------

test_that("an altitude named in feet is converted to metres", {
  dat <- trk_base()
  dat$TrkLatitude <- "43"
  dat$TrkLongitude <- "-69"
  dat$TrkAltitude_ft <- "751.3"

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$ALT, 751.3 * 0.3048)
  expect_equal(narwc_column_mapping(out)$factor[
    narwc_column_mapping(out)$standardized == "ALT"], 0.3048)
})

test_that("ALTFT is converted too, having been read as metres before", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$ALTFT <- "750"
  expect_equal(read_narwc(dat, quiet = TRUE)$ALT, 750 * 0.3048)
})

test_that("metres beats feet when a file carries both, and is not rescaled", {
  dat <- trk_base()
  dat$TrkLatitude <- "43"
  dat$TrkLongitude <- "-69"
  dat$TrkAltitude_m <- "229"
  dat$TrkAltitude_ft <- "751.3"

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$ALT, 229)
  expect_true(is.na(narwc_column_mapping(out)$factor[
    narwc_column_mapping(out)$standardized == "ALT"]))
})

test_that("an altitude already in metres is left alone", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$ALT <- "229"
  expect_equal(read_narwc(dat, quiet = TRUE)$ALT, 229)
})

# The GPS clock --------------------------------------------------------------

test_that("the track clock is taken ahead of another UTC spelling", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$TrkTime_UTC <- "121500"

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$TIME, 121500)
})

test_that("the track clock displaces a plain TIME column", {
  dat <- trk_base()
  dat$Time_UTC <- NULL
  dat$TIME <- "120000"
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$TrkTime_UTC <- "121500"

  out <- suppressWarnings(read_narwc(dat, quiet = TRUE))
  expect_equal(out$TIME, 121500)
  expect_equal(out$TIME_ORIGINAL, "120000")
})

test_that("a local track clock does not displace a UTC column", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$TrkTime_Local <- "081500"

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$TIME, 120000)
})

# DATE ------------------------------------------------------------------------

test_that("a supplied date column survives instead of being rebuilt", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$Date_UTC <- "2024-04-02"          # deliberately disagrees with Y/M/D

  out <- read_narwc(dat, quiet = TRUE)
  expect_s3_class(out$DATE, "Date")
  expect_equal(out$DATE, as.Date("2024-04-02"))
})

test_that("the date is still derived when no date column is supplied", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$DATE, as.Date("2024-04-01"))
})

test_that("an unreadable date falls back to the parts, and says so", {
  dat <- trk_base()
  dat$LATITUDE <- "43"
  dat$LONGITUDE <- "-69"
  dat$Date_UTC <- "4/2/2024"

  expect_warning(out <- read_narwc(dat, quiet = TRUE), "rebuilt")
  expect_equal(out$DATE, as.Date("2024-04-01"))
})

# A missing EVENTNO -----------------------------------------------------------

ev_base <- function(n = 4) {
  data.frame(
    FILEID = "F", EVENTNO = NA_character_,
    Year = "2024", Month = "4", Day = "1",
    Time_UTC = as.character(120000 + seq_len(n)),
    LATITUDE = as.character(43 + seq_len(n) / 100),
    LONGITUDE = "-69", LEGTYPE = "2", stringsAsFactors = FALSE
  )
}

test_that("an entirely empty EVENTNO column is numbered from 1", {
  out <- read_narwc(ev_base(), quiet = TRUE)
  expect_equal(out$EVENTNO, c(1, 2, 3, 4))
})

test_that("records of one event share a number rather than each taking one", {
  dat <- ev_base(3)
  dat$Time_UTC[3] <- dat$Time_UTC[2]        # same event as row 2
  dat$LATITUDE[3] <- dat$LATITUDE[2]

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$EVENTNO, c(1, 2, 2))
})

test_that("a blank row inherits the number recorded for its own event", {
  dat <- ev_base(3)
  dat$Time_UTC[3] <- dat$Time_UTC[2]
  dat$LATITUDE[3] <- dat$LATITUDE[2]
  dat$EVENTNO <- c("10", "20", NA)

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$EVENTNO, c(10, 20, 20))
})

test_that("a gap is filled with a whole number that fits between neighbours", {
  dat <- ev_base(3)
  dat$EVENTNO <- c("10", NA, "20")

  out <- read_narwc(dat, quiet = TRUE)
  expect_equal(out$EVENTNO, c(10, 11, 20))
  expect_true(all(out$EVENTNO == round(out$EVENTNO)))
})

test_that("no room for a whole number renumbers the FILEID, loudly", {
  dat <- ev_base(3)
  dat$EVENTNO <- c("10", NA, "11")          # nothing fits between 10 and 11

  expect_warning(out <- read_narwc(dat, quiet = TRUE), "renumbered from 1")
  expect_equal(out$EVENTNO, c(1, 2, 3))
})

test_that("EVENTNO increases through the FILEID after filling", {
  dat <- ev_base(5)
  dat$EVENTNO <- c(NA, "5", NA, NA, "20")

  out <- read_narwc(dat, quiet = TRUE)
  expect_false(is.unsorted(out$EVENTNO, strictly = TRUE))
  expect_true(all(out$EVENTNO == round(out$EVENTNO)))
})

test_that("numbering restarts within each FILEID", {
  a <- ev_base(2)
  b <- ev_base(2)
  b$FILEID <- "G"
  out <- read_narwc(rbind(a, b), quiet = TRUE)
  expect_equal(out$EVENTNO, c(1, 2, 1, 2))
})

test_that("a missing EVENTNO column is not invented", {
  dat <- ev_base()
  dat$EVENTNO <- NULL
  expect_false("EVENTNO" %in% names(read_narwc(dat, quiet = TRUE)))
})

test_that("make_eventno = FALSE leaves the column alone", {
  out <- read_narwc(ev_base(), make_eventno = FALSE, quiet = TRUE)
  expect_true(all(is.na(out$EVENTNO)))
})

test_that("an existing complete EVENTNO is untouched", {
  dat <- ev_base(3)
  dat$EVENTNO <- c("7", "8", "9")
  expect_equal(read_narwc(dat, quiet = TRUE)$EVENTNO, c(7, 8, 9))
})
