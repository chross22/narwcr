test_that("LAT_DD and LONG_DD are accepted", {
  dat <- example_data()
  expect_true(all(c("LATITUDE", "LONGITUDE") %in% names(dat)))
  expect_false(any(c("LAT_DD", "LONG_DD") %in% names(dat)))
  expect_type(dat$LATITUDE, "double")
})

test_that("a canonical name is never clobbered by its alias", {
  raw <- data.frame(
    FILEID = "A", EVENTNO = 1, YEAR = 2024, MONTH = 4, DAY = 1, TIME = 120000,
    LATITUDE = 43, LAT_DD = 99, LONGITUDE = -69, LEGTYPE = 2
  )
  dat <- read_narwc(raw)
  expect_equal(dat$LATITUDE, 43)
})

test_that("NARWC missing-value placeholders become NA", {
  raw <- data.frame(
    FILEID = "A", EVENTNO = "1", YEAR = "2024", MONTH = "4", DAY = "1",
    TIME = "120000", LATITUDE = "43", LONGITUDE = "-69", LEGTYPE = "2",
    LEGNO = ".", SPECCODE = "", stringsAsFactors = FALSE
  )
  dat <- read_narwc(raw)
  expect_true(is.na(dat$LEGNO))
  expect_true(is.na(dat$SPECCODE))
})

test_that("DATE is derived", {
  dat <- example_data()
  expect_s3_class(dat$DATE, "Date")
  expect_equal(min(dat$DATE), as.Date("2024-04-01"))
})

test_that("extra columns can be carried through", {
  raw <- data.frame(
    FILEID = "A", EVENTNO = 1, YEAR = 2024, MONTH = 4, DAY = 1, TIME = 120000,
    LATITUDE = 43, LONGITUDE = -69, LEGTYPE = 2, Effort_Type = "on"
  )
  # Dropping it is now reported; that the message happens is tested in
  # test-profiles.R, so keep it out of the way here.
  expect_false("Effort_Type" %in% names(suppressMessages(read_narwc(raw))))
  expect_true("Effort_Type" %in% names(read_narwc(raw, extra_columns = "Effort_Type")))
  expect_true("Effort_Type" %in% names(read_narwc(raw, extra_columns = NULL)))
})

test_that("a missing file is reported clearly", {
  expect_error(read_narwc("no/such/file.csv"), "not found")
})

test_that("the bundled fixture raises nothing above a note", {
  issues <- validate_narwc(example_data())
  expect_setequal(issues$severity, "note")

  # The one note is the line abandoned when the sea state rose, which has no
  # end-line record. That is a property of the fixture, not a defect in it.
  expect_equal(issues$check, "legstage_line_not_closed")
  expect_equal(issues$n, 1L)
})

test_that("a missing required column is an error-level finding", {
  dat <- example_data()
  dat$LATITUDE <- NULL
  iss <- validate_narwc(dat)
  expect_true("missing_required" %in% iss$check)
  expect_equal(iss$severity[iss$check == "missing_required"], "error")
})

test_that("out-of-book codes are flagged", {
  dat <- example_data()
  dat$LEGTYPE[1] <- 8 # not a NARWC LEGTYPE
  dat$IDREL[!is.na(dat$IDREL)][1] <- 4
  iss <- validate_narwc(dat)
  expect_setequal(iss$column[iss$check == "unknown_code"], c("LEGTYPE", "IDREL"))
})

test_that("sightings at line-boundary events are flagged", {
  dat <- example_data()
  i <- which(dat$LEGSTAGE == 1)[1]
  dat$SPECCODE[i] <- "RIWH"
  iss <- validate_narwc(dat)
  expect_true("sighting_at_boundary" %in% iss$check)
})

test_that("lost longitude sign convention is flagged", {
  dat <- example_data()
  dat$LONGITUDE <- abs(dat$LONGITUDE)
  iss <- validate_narwc(dat)
  expect_true("positive_west_longitude" %in% iss$check)
})

test_that("mis-sorted events are flagged", {
  dat <- example_data()
  dat$EVENTNO[10] <- 1
  iss <- validate_narwc(dat)
  expect_true("eventno_not_increasing" %in% iss$check)
})

# SIGHTNO, handbook 8.A.27 ----------------------------------------------------

sightno_frame <- function() {
  data.frame(
    FILEID = "F", EVENTNO = 1:5, SIGHTNO = c(1, NA, 2, 3, NA),
    SPECCODE = c("RIWH", NA, "HUWH", "FIWH", NA),
    stringsAsFactors = FALSE
  )
}

test_that("a logger SIGHTNO with no species is flagged", {
  dat <- sightno_frame()
  dat$SIGHTNO[2] <- 47                    # forced record, no sighting
  f <- validate_narwc(dat)
  hit <- f[f$check == "sightno_without_species", ]
  expect_equal(hit$n, 1)
  expect_equal(hit$severity, "warning")
})

test_that("a clean file raises no SIGHTNO findings", {
  f <- validate_narwc(sightno_frame())
  expect_false(any(grepl("^sightno_", f$check)))
})

test_that("duplicate SIGHTNO within a FILEID is flagged", {
  dat <- sightno_frame()
  dat$SIGHTNO <- c(1, NA, 2, 2, NA)
  f <- validate_narwc(dat)
  hit <- f[f$check == "sightno_duplicated", ]
  expect_equal(hit$n, 2)
})

test_that("the same SIGHTNO in a different FILEID is not a duplicate", {
  dat <- rbind(sightno_frame(), sightno_frame())
  dat$FILEID <- rep(c("F", "G"), each = 5)
  f <- validate_narwc(dat)
  expect_false("sightno_duplicated" %in% f$check)
})

test_that("999 is reported as non-target, and not as a duplicate", {
  dat <- sightno_frame()
  dat$SIGHTNO <- c(999, NA, 999, 3, NA)
  f <- validate_narwc(dat)
  hit <- f[f$check == "sightno_non_target", ]
  expect_equal(hit$n, 2)
  expect_equal(hit$severity, "note")
  expect_false("sightno_duplicated" %in% f$check)
})

# Altitude units, handbook 8.A.1 ----------------------------------------------

alt_frame_units <- function(alt) {
  data.frame(FILEID = "F", EVENTNO = seq_along(alt), ALT = alt,
             stringsAsFactors = FALSE)
}

test_that("an altitude in feet is flagged", {
  f <- validate_narwc(alt_frame_units(rep(c(1000, 1030, 1050), 10)))
  hit <- f[f$check == "altitude_looks_like_feet", ]
  expect_equal(nrow(hit), 1)
  expect_equal(hit$severity, "warning")
  expect_equal(hit$n, 30)
})

test_that("an altitude in metres is not flagged", {
  f <- validate_narwc(alt_frame_units(rep(c(229, 244, 305), 10)))
  expect_false("altitude_looks_like_feet" %in% f$check)
})

test_that("an altitude implausible in either unit is not claimed to be feet", {
  # 9000 m is absurd; 9000 ft is 2743 m, also absurd for a survey. Saying
  # nothing beats saying the wrong thing.
  f <- validate_narwc(alt_frame_units(rep(9000, 30)))
  expect_false("altitude_looks_like_feet" %in% f$check)
})

test_that("the flagged rows are the ones above the ceiling", {
  dat <- alt_frame_units(c(rep(1030, 20), 229, 244))
  f <- validate_narwc(dat)
  hit <- f[f$check == "altitude_looks_like_feet", ]
  expect_equal(hit$n, 20)
})

test_that("no ALT column and an empty one are both silent", {
  expect_false("altitude_looks_like_feet" %in%
                 validate_narwc(data.frame(FILEID = "F", EVENTNO = 1))$check)
  expect_false("altitude_looks_like_feet" %in%
                 validate_narwc(alt_frame_units(rep(NA_real_, 5)))$check)
})
