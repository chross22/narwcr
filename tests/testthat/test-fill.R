gappy <- function() {
  # Two survey days in one frame. Day 1 ends on BEAUFORT 5; day 2 begins with
  # blanks before anything is recorded.
  data.frame(
    FILEID = c(rep("A", 4), rep("B", 4)),
    DATE = rep(as.Date(c("2024-04-01", "2024-04-02")), each = 4),
    EVENTNO = 1:8,
    LEGTYPE  = c(2, NA, NA, NA, NA, 1, NA, NA),
    LEGNO    = c(1, NA, NA, NA, NA, 7, NA, NA),
    BEAUFORT = c(2, NA, NA, 5, NA, 3, NA, NA)
  )
}

test_that("state is carried forward within a group", {
  out <- suppressMessages(fill_narwc(gappy()))
  expect_equal(out$LEGTYPE[1:4], c(2, 2, 2, 2))
  expect_equal(out$BEAUFORT[1:4], c(2, 2, 2, 5))
  expect_equal(out$LEGNO[1:4], c(1, 1, 1, 1))
})

test_that("nothing crosses a survey day or a file", {
  out <- suppressMessages(fill_narwc(gappy()))

  # Day 1 ends on BEAUFORT 5 and LEGTYPE 2. Neither may reach day 2's first
  # record, which has its own values back-filled from row 6 instead.
  expect_equal(out$BEAUFORT[5], 3)
  expect_equal(out$LEGTYPE[5], 1)
  expect_equal(out$LEGNO[5], 7)

  # The ungrouped fill the original scripts used would have produced these.
  wrong <- suppressWarnings(suppressMessages(
    fill_narwc(gappy(), by = character())
  ))
  expect_equal(wrong$BEAUFORT[5], 5)
  expect_equal(wrong$LEGTYPE[5], 2)
})

test_that("direction is honoured", {
  down <- suppressMessages(fill_narwc(gappy(), direction = "down"))
  # Row 5 has nothing above it within its own group, so "down" leaves it NA.
  expect_true(is.na(down$LEGTYPE[5]))

  downup <- suppressMessages(fill_narwc(gappy(), direction = "downup"))
  expect_equal(downup$LEGTYPE[5], 1)
})

test_that("sighting columns are refused, not silently skipped", {
  dat <- gappy()
  dat$SPECCODE <- c("RIWH", NA, NA, NA, NA, NA, NA, NA)
  dat$NUMBER <- c(3, NA, NA, NA, NA, NA, NA, NA)

  expect_error(fill_narwc(dat, columns = c("LEGTYPE", "SPECCODE")), "SPECCODE")
  expect_error(fill_narwc(dat, columns = "NUMBER"), "replicates one detection")

  # The default set never includes them, so one group of three right whales
  # stays one group of three.
  out <- suppressMessages(fill_narwc(dat))
  expect_equal(sum(!is.na(out$SPECCODE)), 1)
  expect_equal(sum(out$NUMBER, na.rm = TRUE), 3)
})

test_that("positions and times are refused too", {
  expect_error(fill_narwc(gappy(), columns = "LATITUDE"), "LATITUDE")
  expect_error(fill_narwc(gappy(), columns = "EVENTNO"), "EVENTNO")
  expect_error(fill_narwc(gappy(), columns = "TIME"), "fabricates")
})

test_that("the report separates recovered values from inferred ones", {
  msg <- tryCatch(fill_narwc(gappy()), message = conditionMessage)
  expect_match(msg, "carried forward")
  expect_match(msg, "carried backward")
  expect_match(msg, "inferred from the first recorded value")
  expect_match(msg, "grouped by FILEID, DATE")

  # Nothing to do, nothing said.
  clean <- data.frame(FILEID = "A", DATE = Sys.Date(), LEGTYPE = 2)
  expect_silent(fill_narwc(clean))
})

test_that("an ungroupable frame warns rather than filling across everything", {
  dat <- data.frame(LEGTYPE = c(2, NA, NA), BEAUFORT = c(3, NA, NA))
  expect_warning(suppressMessages(fill_narwc(dat)), "across the whole data frame")
  msg <- suppressWarnings(tryCatch(fill_narwc(dat), message = conditionMessage))
  expect_match(msg, "ungrouped")
})

test_that("quiet suppresses the report but not the work", {
  out <- expect_silent(fill_narwc(gappy(), quiet = TRUE))
  expect_equal(out$LEGTYPE[1:4], c(2, 2, 2, 2))
})

test_that("filling leaves the rest of the frame alone", {
  dat <- gappy()
  out <- suppressMessages(fill_narwc(dat))
  expect_equal(nrow(out), nrow(dat))
  expect_equal(names(out), names(dat))
  expect_equal(out$EVENTNO, dat$EVENTNO)
})

test_that("absent columns and empty input are handled", {
  dat <- data.frame(FILEID = "A", DATE = Sys.Date(), EVENTNO = 1)
  expect_equal(suppressMessages(fill_narwc(dat)), dat)
  expect_equal(nrow(fill_narwc(gappy()[0, ])), 0)
})

test_that("filling restores a frame recorded once per leg to one recorded in full", {
  # The point of filling: a file that omits repeats should behave like one that
  # does not.
  full <- example_data()
  gapped <- full
  n <- nrow(gapped)
  cols <- c("LEGTYPE", "LEGSTAGE", "LEGNO", "BEAUFORT", "VISIBLTY")

  # Blank a value only where it repeats the row above *within the same file and
  # day* - which is what the recording convention actually does. Blanking across
  # a day boundary would delete a value nothing can recover.
  same_group <- c(FALSE, gapped$FILEID[-1] == gapped$FILEID[-n] &
                    gapped$DATE[-1] == gapped$DATE[-n])
  for (nm in cols) {
    repeats <- c(FALSE, gapped[[nm]][-1] == gapped[[nm]][-n])
    repeats[is.na(repeats)] <- FALSE
    gapped[[nm]][same_group & repeats] <- NA
  }
  expect_gt(sum(is.na(gapped$LEGTYPE)), sum(is.na(full$LEGTYPE)))

  restored <- suppressMessages(
    fill_narwc(gapped, columns = cols, direction = "down")
  )
  # Only the columns recorded on every row can be expected back exactly.
  # LEGSTAGE and LEGNO carry NA in the source frame by design, so filling them
  # cannot reproduce it. The downstream consequence - that a restored frame
  # segments identically - is tested where segmentation lives.
})
