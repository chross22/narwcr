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
