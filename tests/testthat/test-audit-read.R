# Regressions for defects found in an audit of read_narwc() and its helpers.

audit_base <- function(n = 3) {
  data.frame(
    FILEID = "F1", EVENTNO = seq_len(n), YEAR = 2024, MONTH = 4, DAY = 1,
    LATITUDE = 42 + seq_len(n) / 10, LONGITUDE = -69,
    stringsAsFactors = FALSE
  )
}

read_quiet <- function(dat, ...) {
  suppressWarnings(read_narwc(dat, quiet = TRUE, ...))
}

# 1. A backup spelling fills gaps in its own unit ------------------------------

test_that("a feet backup filling a metres ALT is converted", {
  dat <- audit_base()
  dat$TrkAltitude_m <- c(300, NA, NA)
  dat$TrkAltitude_ft <- c(984, 1000, 1000)
  expect_equal(read_quiet(dat)$ALT, c(300, 304.8, 304.8))
})

test_that("an ALTFT backup filling a canonical ALT is converted", {
  dat <- audit_base()
  dat$ALT <- c(300, NA, NA)
  dat$ALTFT <- c(984, 1000, 1000)
  expect_equal(read_quiet(dat)$ALT, c(300, 304.8, 304.8))
})

test_that("a nautical-mile backup filling TRKDIST is converted", {
  dat <- audit_base()
  dat$TrkDist_m <- c(5, NA, NA)
  dat$TrkDist_nm <- c(1, 1, 2)
  expect_equal(read_quiet(dat)$TRKDIST, c(5, 1852, 3704))
})

# 2. EVENTNO: only missing values are supplied ---------------------------------

test_that("a FILEID with no missing EVENTNO is left exactly as recorded", {
  dat <- rbind(audit_base(), audit_base())
  dat$FILEID <- rep(c("F1", "F2"), each = 3)
  dat$LATITUDE <- 42 + seq_len(6) / 10
  dat$EVENTNO <- c(1, NA, 3, 5, 5, 4)
  expect_equal(read_quiet(dat)$EVENTNO, c(1, 2, 3, 5, 5, 4))
})

test_that("recorded numbers within one event are not overwritten", {
  dat <- audit_base()
  dat$LATITUDE <- 42                        # one event
  dat$EVENTNO <- c(7, NA, 9)
  expect_equal(read_quiet(dat)$EVENTNO, c(7, 7, 9))
})

test_that("no free whole number gives a fractional one, not a renumber", {
  dat <- audit_base()
  dat$EVENTNO <- c(10, NA, 11)
  expect_warning(out <- read_narwc(dat, quiet = TRUE), "fractional")
  expect_equal(out$EVENTNO, c(10, 10.5, 11))
})

# 3. A missing FILEID ----------------------------------------------------------

test_that("records with no FILEID are still numbered", {
  dat <- audit_base()
  dat$FILEID <- NA
  dat$EVENTNO <- c(1, NA, 3)
  expect_equal(read_quiet(dat)$EVENTNO, c(1, 2, 3))
})

# 4. Factors are read through their labels ------------------------------------

test_that("a factor column is read by its labels, not its level codes", {
  dat <- audit_base()
  dat$LATITUDE <- factor(c("42.1", "41.5", "43.9"))
  expect_equal(read_quiet(dat)$LATITUDE, c(42.1, 41.5, 43.9))
})

test_that("blank_to_na() treats a factor's placeholder as missing", {
  expect_equal(blank_to_na(factor(c("1", ".", ""))), c("1", NA, NA))
})

# 5. The track log wins whatever the plain column is called --------------------

test_that("TrkLatitude displaces a plain column spelled `Latitude`", {
  dat <- audit_base()
  names(dat)[names(dat) == "LATITUDE"] <- "Latitude"
  dat$Latitude <- 10
  dat$TrkLatitude <- 42.5
  out <- read_quiet(dat)
  expect_equal(out$LATITUDE, rep(42.5, 3))
  expect_equal(out$LATITUDE_ORIGINAL, rep(10, 3))
})

test_that("a second plain spelling does not win back what the track log took", {
  dat <- audit_base()
  dat$Latitude <- 10
  dat$TrkLatitude <- 42.5
  expect_equal(read_quiet(dat)$LATITUDE, rep(42.5, 3))
})

# 6. Clock times ---------------------------------------------------------------

test_that("AM and PM are honoured", {
  expect_equal(parse_narwc_time(c("1:30:00 PM", "12:15 AM", "12:00 PM")),
               c(133000, 1500, 120000))
})

test_that("an out-of-range or malformed clock is NA", {
  expect_true(all(is.na(parse_narwc_time(c("25:70", "123:45", "13:00 PM")))))
})

test_that("the first clock in a timestamp is taken, not a UTC offset", {
  expect_equal(parse_narwc_time("2024-04-01T12:34:56-05:00"), 123456)
})

# 7. Date parts that do not make a date ---------------------------------------

test_that("a missing or impossible date part gives NA, not an error", {
  dat <- audit_base()
  dat$YEAR <- c(NA, 2024, 2024)
  dat$MONTH <- c(13, 4, 4)
  out <- read_quiet(dat)
  expect_equal(out$DATE, as.Date(c(NA, "2024-04-01", "2024-04-01")))
})

# 8. A DATE as.Date() cannot guess ---------------------------------------------

test_that("a compact 20240401 DATE is read", {
  dat <- audit_base()
  dat$DATE <- "20240401"
  expect_equal(read_quiet(dat)$DATE, rep(as.Date("2024-04-01"), 3))
})

test_that("an unreadable DATE falls back to the parts with a warning", {
  dat <- audit_base()
  dat$DATE <- "not a date"
  expect_warning(out <- read_narwc(dat, quiet = TRUE), "rebuilt")
  expect_equal(out$DATE, rep(as.Date("2024-04-01"), 3))
})

# 9. extra_columns = NULL with a profile --------------------------------------

test_that("extra_columns = NULL keeps everything even with a profile", {
  dat <- audit_base()
  dat$Foo <- 1
  dat$IS_LAT <- 2
  out <- read_quiet(dat, profile = "ccs", extra_columns = NULL)
  expect_true(all(c("Foo", "IS_LAT") %in% names(out)))
})

# 10. The column mapping always has the same columns --------------------------

test_that("the mapping has four columns with no renames or no attribute", {
  cols <- c("original", "standardized", "match", "factor")
  expect_named(narwc_column_mapping(read_quiet(audit_base())), cols)
  expect_named(narwc_column_mapping(data.frame()), cols)
  expect_type(narwc_column_mapping(data.frame())$factor, "double")
})

# 11. Columns this package adds are not foreign -------------------------------

test_that("columns read_narwc() and fill_narwc() add are not reported", {
  expect_equal(
    unrecognised_columns(c("LATITUDE_ORIGINAL", "TIME_ORIGINAL",
                           "ALT_ASSUMED", "LEGSTAGE_FILLED", "Foo")),
    "Foo"
  )
})

# 12. A POSIXct DATE on its own clock -----------------------------------------

test_that("a local POSIXct DATE keeps its local date", {
  dat <- audit_base()
  dat$DATE <- as.POSIXct("2024-04-01 22:30", tz = "America/New_York")
  expect_equal(read_quiet(dat)$DATE, rep(as.Date("2024-04-01"), 3))
})

# 13. %||% is defined for R < 4.4 ----------------------------------------------

test_that("the package defines its own %||%", {
  ns <- asNamespace("narwcr")
  expect_true(exists("%||%", envir = ns, inherits = FALSE))
  f <- get("%||%", envir = ns)
  expect_equal(f(NULL, 2), 2)
  expect_equal(f(1, 2), 1)
})

# 14. Google Drive extension and id recognition -------------------------------

test_that("a Google export keeps its own extension", {
  expect_equal(
    fetch_extension(paste0("https://docs.google.com/spreadsheets/d/",
                           "1AbCdEfGhIjKlMnOpQrStUvWxYz0123/export?format=xlsx")),
    ".xlsx"
  )
  expect_equal(fetch_extension("1AbCdEfGhIjKlMnOpQrStUvWxYz0123456", "xlsx"),
               ".xlsx")
  expect_equal(fetch_extension("1AbCdEfGhIjKlMnOpQrStUvWxYz0123456"), ".csv")
})

test_that("a file name is not taken for a Drive id", {
  expect_false(is_google_drive("survey_extract_2024_final_v2"))
  expect_false(is_google_drive("short_id_123"))
  expect_true(is_google_drive("1AbCdEfGhIjKlMnOpQrStUvWxYz0123456"))
  expect_true(is_google_drive("0B1a2B3c4D5e6F7g8H9i0J1k2L3m"))
  expect_error(narwc_fetch("survey_extract_2024_final_v2"), "Cannot work out")
})

# 15. A directory is not a file -----------------------------------------------

test_that("narwc_fetch() refuses a directory", {
  expect_error(narwc_fetch(tempdir()), "directory")
})

# 16. The EVENTNO count is of records kept ------------------------------------

test_that("EVENTNO is supplied after records with no position are dropped", {
  dat <- audit_base()
  dat$EVENTNO <- c(1, NA, NA)
  dat$LATITUDE <- c(42.1, NA, 42.3)
  msgs <- character(0)
  out <- withCallingHandlers(
    read_narwc(dat),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_equal(out$EVENTNO, c(1, 2))
  expect_true(any(grepl("supplied 1 missing EVENTNO value\\.", msgs)))
})
