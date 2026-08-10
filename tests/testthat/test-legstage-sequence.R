# A line occupation with the LEGSTAGE sequence given, one record per stage.
line <- function(stages, legno = 1, date = "2024-04-01", start = 1) {
  data.frame(
    FILEID = "A",
    EVENTNO = seq_along(stages) + start - 1,
    YEAR = 2024L, MONTH = 4L, DAY = as.integer(substr(date, 9, 10)),
    TIME = 120000 + seq_along(stages),
    LATITUDE = 43 + seq_along(stages) / 100, LONGITUDE = -69,
    LEGTYPE = 2, LEGSTAGE = stages, LEGNO = legno,
    DATE = as.Date(date)
  )
}

checks <- function(dat) validate_narwc(dat)$check

test_that("a well-formed line raises nothing", {
  expect_false("legstage_sequence" %in% checks(line(c(1, 2, 2, 2, 5))))
  # Begin, break off, resume, continue, end.
  expect_false("legstage_sequence" %in% checks(line(c(1, 2, 3, 4, 2, 5))))
  # A one-record line that begins and ends.
  expect_false("legstage_sequence" %in% checks(line(c(1, 5))))
})

test_that("a line must begin with a begin-line", {
  expect_true("legstage_sequence" %in% checks(line(c(2, 2, 5))))
  expect_true("legstage_sequence" %in% checks(line(c(5))))
  expect_true("legstage_sequence" %in% checks(line(c(4, 2, 5))))
})

test_that("nothing may follow an end-line", {
  # The violation the fixture originally contained: an end-line immediately
  # before a break off to circle, which nothing caught.
  iss <- validate_narwc(line(c(1, 2, 5, 3, 4, 2, 5)))
  expect_true("legstage_sequence" %in% iss$check)
  row <- iss$rows[iss$check == "legstage_sequence"][[1]]
  expect_equal(row, 4L) # the 3 that follows the 5

  expect_true("legstage_sequence" %in% checks(line(c(1, 2, 5, 2, 5))))
})

test_that("a second begin-line within one occupation is a violation", {
  expect_true("legstage_sequence" %in% checks(line(c(1, 2, 1, 2, 5))))
})

test_that("a resume without a break-off is a violation", {
  expect_true("legstage_sequence" %in% checks(line(c(1, 2, 4, 2, 5))))
})

test_that("a break-off that is never resumed is reported", {
  iss <- validate_narwc(line(c(1, 2, 3)))
  expect_true("legstage_break_off_unresumed" %in% iss$check)
  # And not double-reported as an unclosed line.
  expect_false("legstage_line_not_closed" %in% iss$check)
})

test_that("an unclosed line is a note, because abandoning one is legitimate", {
  iss <- validate_narwc(line(c(1, 2, 2)))
  expect_true("legstage_line_not_closed" %in% iss$check)
  expect_equal(iss$severity[iss$check == "legstage_line_not_closed"], "note")
  expect_false("legstage_sequence" %in% iss$check)
})

test_that("a line re-flown the same day is two occupations, not one bad line", {
  # The trap: grouping on LEGNO alone merges these into 1,2,2,1,2,2,5 and the
  # second begin-line looks like a violation when it is the correct record of
  # a second occupation of the same line.
  abandoned <- line(c(1, 2, 2), legno = 4, start = 1)
  other <- line(c(1, 2, 5), legno = 5, start = 10)
  reflown <- line(c(1, 2, 2, 5), legno = 4, start = 20)
  dat <- rbind(abandoned, other, reflown)

  expect_false("legstage_sequence" %in% checks(dat))
  # Only the abandoned first occupation is unclosed.
  iss <- validate_narwc(dat)
  expect_equal(iss$n[iss$check == "legstage_line_not_closed"], 1L)
})

test_that("circling records between a break-off and a resume do not break it", {
  dat <- line(c(1, 2, 3, 4, 5))
  # Insert the circling excursion: LEGTYPE 4, no LEGSTAGE, as flag_circling
  # leaves it.
  circ <- dat[rep(3, 3), ]
  circ$LEGTYPE <- 4
  circ$LEGSTAGE <- NA
  circ$EVENTNO <- c(31, 32, 33)
  dat <- rbind(dat[1:3, ], circ, dat[4:5, ])

  expect_false("legstage_sequence" %in% checks(dat))
})

test_that("stages 6 and 7 take no part in the sequence", {
  # A pilot sighting and a photographic detection are kinds of sighting, not
  # stages of the line, and must not interrupt 2 -> 2.
  expect_false("legstage_sequence" %in% checks(line(c(1, 2, 6, 2, 7, 2, 5))))
})

test_that("the check needs LEGNO, and says nothing without it", {
  dat <- line(c(1, 2, 5))
  dat$LEGNO <- NULL
  expect_false(any(grepl("^legstage_(sequence|break_off|line_not)", checks(dat))))
})

test_that("two separate days are two occupations", {
  a <- line(c(1, 2, 5), legno = 1, date = "2024-04-01")
  b <- line(c(1, 2, 5), legno = 1, date = "2024-04-02", start = 10)
  expect_false("legstage_sequence" %in% checks(rbind(a, b)))
})
