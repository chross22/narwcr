# Regressions for defects found in the effort, fill, LEGSTAGE, validation and
# STRIP code. One block per defect, in the order they were reported.

# 1. fill_narwc() defaults ----------------------------------------------------

test_that("fill_narwc() does not copy a begin-line LEGSTAGE down the line", {
  d <- data.frame(
    FILEID = "A", DATE = as.Date("2024-04-01"),
    LEGTYPE = c(NA, 2, NA, NA), LEGSTAGE = c(NA, 1, NA, NA)
  )
  expect_false("LEGSTAGE" %in% narwc_fill_columns())

  out <- fill_narwc(d, quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(NA, 1, NA, NA))

  # Asked for explicitly, it is reconstructed as a state, not copied.
  out <- fill_narwc(d, columns = c("LEGTYPE", "LEGSTAGE"), quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(NA, 1, 2, 2))
  expect_equal(out$LEGSTAGE_FILLED, c(FALSE, FALSE, TRUE, TRUE))
})

test_that("fill_narwc() never back-fills LEGTYPE onto leading transit", {
  d <- data.frame(
    FILEID = "A", DATE = as.Date("2024-04-01"),
    LEGTYPE = c(NA, 2, NA, NA), LEGNO = c(NA, 4, NA, NA),
    BEAUFORT = c(NA, 3, NA, NA)
  )
  for (dir in c("downup", "up")) {
    out <- fill_narwc(d, direction = dir, quiet = TRUE)
    expect_true(is.na(out$LEGTYPE[1]), info = dir)
    expect_true(is.na(out$LEGNO[1]), info = dir)
    expect_equal(out$BEAUFORT[1], 3, info = dir)
  }
  expect_equal(fill_narwc(d, quiet = TRUE)$LEGTYPE, c(NA, 2, 2, 2))
})

# 2. fill_legstage() and sighting codes ---------------------------------------

test_that("a mid-line LEGSTAGE 6 or 7 does not become the carried state", {
  for (code in c(6, 7)) {
    d <- data.frame(LEGNO3 = "a", LEGTYPE = 2,
                    LEGSTAGE = c(1, NA, code, NA, NA, 5))
    expect_equal(fill_legstage(d, quiet = TRUE)$LEGSTAGE,
                 c(1, 2, code, 2, 2, 5), info = code)
  }
  # A 6 opening a group does not let the previous group's state leak in.
  d <- data.frame(LEGNO3 = c("a", "a", "b", "b"), LEGTYPE = 2,
                  LEGSTAGE = c(1, NA, 6, NA))
  expect_equal(fill_legstage(d, quiet = TRUE)$LEGSTAGE, c(1, 2, 6, NA))
})

# 3. track_speed() and hhmm TIME ----------------------------------------------

test_that("an hhmm file is read as hhmm", {
  kn <- track_speed(data.frame(LATITUDE = c(42, 42.01), LONGITUDE = -70,
                               TIME = c(1230, 1231)))
  expect_equal(kn[1], 36.0, tolerance = 0.01)

  # Decided per file: an hhmmss file keeps its reading.
  d <- data.frame(FILEID = c("h", "h", "s", "s"),
                  LATITUDE = c(42, 42.01, 42, 42.01), LONGITUDE = -70,
                  TIME = c(1230, 1231, 123000, 123100))
  kn <- track_speed(d, by = "FILEID")
  expect_equal(kn[c(1, 3)], c(36.0, 36.0), tolerance = 0.01)

  # One six-digit value makes the file hhmmss.
  expect_equal(narwc_time_is_hhmm(data.frame(TIME = c(2300, 2359, 1))),
               c(TRUE, TRUE, TRUE))
  expect_equal(narwc_time_is_hhmm(data.frame(TIME = c(2300, 120000))),
               c(FALSE, FALSE))
})

# 4. classify_platform() with no grouping column ------------------------------

test_that("classify_platform() labels a frame with no grouping column", {
  d <- data.frame(LATITUDE = c(42, 42.01, 42.02, 42.03), LONGITUDE = -70,
                  TIME = c(120000, 120010, 120020, 120030))
  expect_equal(as.character(classify_platform(d)), rep("aerial", 4))
  expect_equal(as.character(classify_platform(d, by = character())),
               rep("aerial", 4))
})

# 5. make_leg_id() and NA LEGNO -----------------------------------------------

test_that("an NA LEGNO carries the occupation instead of starting one", {
  out <- make_leg_id(data.frame(LEGNO = c(NA, 1, 1, 1),
                                LEGSTAGE = c(1, 2, 2, 5), LEGTYPE = 2),
                     quiet = TRUE)
  expect_equal(out$LEGNO3, rep("1_1", 4))

  out <- make_leg_id(data.frame(LEGNO = c(1, NA, 1, 1),
                                LEGSTAGE = c(1, 2, 2, 5), LEGTYPE = 2),
                     quiet = TRUE)
  expect_equal(out$LEGNO3, rep("1_1", 4))

  # Transit before the day's first LEGNO, with no begin-line record, stays
  # outside any line; a real change of LEGNO still opens one.
  out <- make_leg_id(data.frame(LEGNO = c(NA, NA, 1, NA, 2, 2),
                                LEGSTAGE = NA, LEGTYPE = c(1, 1, 2, 2, 2, 2)),
                     quiet = TRUE)
  expect_equal(out$LEGNO3, c(NA, NA, "1_2", "1_2", "2_3", "2_3"))
})

# 6. legstage_sequence_check() without LEGNO3 ---------------------------------

test_that("a run of NA LEGNO is not an occupation of its own", {
  res <- legstage_sequence_check(data.frame(
    LEGNO = c(1, NA, NA, NA), LEGSTAGE = c(1, 2, 2, 5), LEGTYPE = 2
  ))
  expect_length(res$bad, 0)
  expect_length(res$open, 0)
  expect_length(res$dangling, 0)

  res <- legstage_sequence_check(data.frame(
    LEGNO = c(NA, 1, 1, 1), LEGSTAGE = c(1, 2, 2, 5), LEGTYPE = 2
  ))
  expect_length(res$bad, 0)

  # A stray begin-line mid-line is still found.
  res <- legstage_sequence_check(data.frame(
    LEGNO = 1, LEGSTAGE = c(1, 2, 1, 2, 5), LEGTYPE = 2
  ))
  expect_equal(res$bad, 3L)
})

# 7. check_time_format() minutes and seconds ----------------------------------

test_that("minutes and seconds of 60 or more are invalid times", {
  res <- check_time_format(data.frame(TIME = c(126000, 129999, 120060, 120000)))
  warn <- Filter(function(f) !is.null(f) && f$severity == "warning", res)
  expect_length(warn, 1)
  expect_equal(warn[[1]]$rows[[1]], 1:3)

  # An hhmm file is checked as hhmm, and noted.
  res <- check_time_format(data.frame(TIME = c(1230, 1275, 2359)))
  res <- Filter(Negate(is.null), res)
  sev <- vapply(res, function(f) f$severity, character(1))
  expect_equal(res[[which(sev == "warning")]]$rows[[1]], 2L)
  expect_equal(res[[which(sev == "note")]]$rows[[1]], 1:3)

  # An early-morning time in an hhmmss file is not called hhmm.
  res <- Filter(Negate(is.null),
                check_time_format(data.frame(TIME = c(2300, 120000))))
  expect_length(res, 0)
})

# 8. STRIP code 0 -------------------------------------------------------------

test_that("STRIP code 0 is listed for the AT-11 only", {
  expect_false(0 %in% narwc_strip_bins("cetap", "skymaster")$code)
  expect_true(0 %in% narwc_strip_bins("cetap", "at-11")$code)
  expect_false(0 %in% narwc_strip_bins("nlpsc")$code)
})

# 9. Fix intervals across midnight --------------------------------------------

test_that("an interval across midnight uses DATE when there is one", {
  d <- data.frame(DATE = as.Date(c("2024-04-01", "2024-04-02")),
                  LATITUDE = c(42, 42.01), LONGITUDE = -70,
                  TIME = c(235950, 10))
  kn <- track_speed(d, by = character())
  expect_equal(kn[1], 1.11 * 1000 / 20 * 1.94384, tolerance = 0.01)

  # Without DATE the gap is negative and unusable.
  d$DATE <- NULL
  expect_true(is.na(track_speed(d)[1]))
})

# 10. check_event_order() and NA ----------------------------------------------

test_that("an NA EVENTNO or FILEID does not hide a decrease", {
  res <- check_event_order(data.frame(FILEID = "A", EVENTNO = c(5, NA, 3)))
  expect_equal(res[[1]]$rows[[1]], 3L)

  res <- check_event_order(data.frame(FILEID = NA, EVENTNO = c(5, 3)))
  expect_equal(res[[1]]$rows[[1]], 2L)

  res <- check_event_order(data.frame(FILEID = c("A", NA, NA),
                                      EVENTNO = c(9, 1, 2)))
  expect_null(res[[1]])
})

# fill_narwc() keeps the class it was given ------------------------------------

test_that("fill_narwc() returns a data.frame for a data.frame", {
  d <- data.frame(FILEID = "A", DATE = as.Date("2024-04-01"),
                  BEAUFORT = c(2, NA, NA))
  out <- fill_narwc(d, quiet = TRUE)
  expect_identical(class(out), "data.frame")
  expect_equal(out$BEAUFORT, c(2, 2, 2))
  expect_s3_class(fill_narwc(tibble::as_tibble(d), quiet = TRUE), "tbl_df")
})
