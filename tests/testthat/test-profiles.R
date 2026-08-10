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

ccs_frame <- function() {
  dat <- utils::read.csv(example_path(), stringsAsFactors = FALSE)
  dat$Tr_SIGHTING <- 1
  dat$IS_LAT <- dat$LAT_DD
  dat$IS_LONG <- dat$LONG_DD
  dat$OBSSIGHT <- 1
  dat$Effort_Type <- "survey"
  dat
}

test_that("the registry describes what it knows and admits what it does not", {
  reg <- narwc_profiles()
  expect_true(all(c("profile", "programme", "column", "meaning", "role",
                    "confidence") %in% names(reg)))
  expect_true("ccs" %in% reg$profile)

  ccs <- narwc_profiles("ccs")
  expect_true(all(c("IS_LAT", "IS_LONG", "Tr_SIGHTING") %in% ccs$column))

  # The columns Camille confirmed are marked confirmed; OBSSIGHT is not.
  conf <- ccs$confidence[match(c("IS_LAT", "Tr_SIGHTING", "OBSSIGHT"), ccs$column)]
  expect_equal(conf, c("confirmed", "confirmed", "unconfirmed"))

  # Nothing is interpreted yet, and the registry says so rather than implying
  # otherwise.
  expect_true(all(ccs$role == "passthrough"))
})

test_that("an unknown profile is refused with the permitted values", {
  expect_error(narwc_profiles("nefsc"), "ccs")
})

test_that("dropped columns are reported, not silently discarded", {
  dat <- ccs_frame()
  expect_message(read_narwc(dat), "dropped")
  expect_message(read_narwc(dat), "Tr_SIGHTING")

  # And the profile they look like is named.
  expect_message(read_narwc(dat), "ccs")
})

test_that("a profile keeps that programme's columns", {
  dat <- ccs_frame()

  bare <- suppressMessages(read_narwc(dat))
  expect_false("IS_LAT" %in% names(bare))

  kept <- read_narwc(dat, profile = "ccs")
  expect_true(all(c("IS_LAT", "IS_LONG", "Tr_SIGHTING", "OBSSIGHT") %in% names(kept)))

  # Keeping is not interpreting: no new distance columns appear.
  expect_false("distance" %in% names(kept))
})

test_that("keeping a profile's columns emits no dropped-column message", {
  # The rename report still fires - the fixture uses LAT_DD - so check the
  # dropped-column message specifically rather than for silence.
  msgs <- all_messages(read_narwc(ccs_frame(), profile = "ccs"))
  expect_no_match(msgs, "dropped")
})

test_that("quiet suppresses the message and extra_columns still works", {
  dat <- ccs_frame()
  expect_silent(read_narwc(dat, quiet = TRUE))

  named <- suppressMessages(read_narwc(dat, extra_columns = "IS_LAT"))
  expect_true("IS_LAT" %in% names(named))
  expect_false("Tr_SIGHTING" %in% names(named))

  everything <- read_narwc(dat, extra_columns = NULL)
  expect_true("Effort_Type" %in% names(everything))
})

test_that("a plain handbook file drops nothing", {
  expect_no_match(all_messages(read_narwc(example_path())), "dropped")
})

test_that("unfamiliar columns get advice rather than a profile name", {
  dat <- utils::read.csv(example_path(), stringsAsFactors = FALSE)
  dat$SOMETHING_ELSE <- 1
  expect_message(read_narwc(dat), "SOMETHING_ELSE")
  expect_message(read_narwc(dat), "will not guess")
})

test_that("validate_narwc notes columns from outside the handbook", {
  dat <- read_narwc(ccs_frame(), profile = "ccs")
  issues <- validate_narwc(dat)

  expect_true("columns_outside_handbook" %in% issues$check)
  note <- issues[issues$check == "columns_outside_handbook", ]
  expect_equal(note$severity, "note")
  expect_match(note$message, "Tr_SIGHTING")
  expect_match(note$message, "ccs")

  # A handbook-only file raises nothing.
  expect_false("columns_outside_handbook" %in%
                 validate_narwc(example_data())$check)
})

test_that("columns narwcr derives itself are not reported as foreign", {
  dat <- flag_effort(make_leg_id(example_data()))
  expect_false("columns_outside_handbook" %in% validate_narwc(dat)$check)
})

test_that("a redundant alias is not reported as a loss", {
  # LAT_DD alongside an existing LATITUDE is a duplicate, not information the
  # caller needs to go looking for.
  raw <- data.frame(
    FILEID = "A", EVENTNO = 1, YEAR = 2024, MONTH = 4, DAY = 1, TIME = "120000",
    LATITUDE = 43, LAT_DD = 99, LONGITUDE = -69, LEGTYPE = 2
  )
  expect_silent(read_narwc(raw))

  # A genuinely unrecognised column still is reported, and the alias stays out
  # of that message.
  raw$WHAT_IS_THIS <- 1
  msg <- tryCatch(read_narwc(raw), message = conditionMessage)
  expect_match(msg, "WHAT_IS_THIS")
  expect_no_match(msg, "LAT_DD")
})
