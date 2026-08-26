test_that("legacy visibility codes are read as codes, not distances", {
  # Handbook 8.A.37: -1 means "clear for at least 2 nmi", not "minus one mile".
  # A plain `VISIBLTY >= 2` test, as the original code used, gets every one of
  # these wrong.
  expect_true(visibility_ok(-1))
  expect_false(visibility_ok(-2)) # fog
  expect_false(visibility_ok(-3)) # haze
  expect_false(visibility_ok(-4)) # rain
  expect_false(visibility_ok(-5)) # snow
})

test_that("modern visibility is compared as nautical miles", {
  expect_true(visibility_ok(5))
  expect_true(visibility_ok(2))
  expect_false(visibility_ok(1.5))
  expect_true(is.na(visibility_ok(NA)))
})

test_that("a legacy clear code cannot satisfy a stricter threshold", {
  # -1 asserts 2 nmi and nothing more, so it cannot answer "was it 3?".
  expect_true(is.na(visibility_ok(-1, min_nmi = 3)))
  expect_true(visibility_ok(-1, min_nmi = 2))
})

test_that("legacy-format effort is not silently discarded", {
  # This is the regression test for the bug that motivated visibility_ok():
  # a whole legacy survey line coded VISIBLTY = -1 must stay on effort.
  dat <- straight_line(n = 11)
  dat$VISIBLTY <- -1
  dat <- flag_effort(dat)
  expect_equal(sum(dat$OnOff.Effort), 11)

  # And a fogged-out line must not.
  dat2 <- straight_line(n = 11)
  dat2$VISIBLTY <- -2
  expect_equal(sum(flag_effort(dat2)$OnOff.Effort), 0)
})

test_that("effort criteria are applied together", {
  dat <- straight_line(n = 10)
  expect_equal(sum(flag_effort(dat)$OnOff.Effort), 10)

  dat$BEAUFORT[1:3] <- 5
  dat$ALT[4] <- 500
  dat$VISIBLTY[5] <- 1
  dat$LEGTYPE[6] <- 1 # transit
  expect_equal(sum(flag_effort(dat)$OnOff.Effort), 4)
})

test_that("only LEGTYPE 2 is on effort by default", {
  dat <- straight_line(n = 5)
  dat$LEGTYPE <- c(0, 1, 2, 3, 4)
  expect_equal(flag_effort(dat)$OnOff.Effort, c(0L, 0L, 1L, 0L, 0L))
})

test_that("missing criterion values fail by default and can be made to pass", {
  dat <- straight_line(n = 5)
  dat$BEAUFORT[2] <- NA
  expect_equal(sum(flag_effort(dat)$OnOff.Effort), 4)
  expect_equal(sum(flag_effort(dat, na_action = "pass")$OnOff.Effort), 5)
})

test_that("absent optional columns are skipped with a message", {
  dat <- straight_line(n = 5)
  dat$BEAUFORT <- NULL
  expect_message(flag_effort(dat), "BEAUFORT")
})

test_that("make_leg_id separates re-occupations of a line", {
  dat <- dplyr::bind_rows(
    straight_line(n = 3, legno = 1),
    straight_line(n = 3, legno = 2),
    straight_line(n = 3, legno = 1)
  )
  dat$EVENTNO <- seq_len(nrow(dat))
  dat <- make_leg_id(dat)
  expect_equal(unique(dat$LEGNO3), c("1_1", "2_2", "1_3"))
})

test_that("flag_effort requires LEGTYPE", {
  dat <- straight_line(n = 3)
  dat$LEGTYPE <- NULL
  expect_error(flag_effort(dat), "LEGTYPE")
})

# What an occupation was found from -------------------------------------------

occ_frame <- function(legno = NA_character_, stage = NA_real_, legtype = 2,
                      date = "2024-04-01", n = 4) {
  data.frame(
    FILEID = "F", EVENTNO = seq_len(n), DATE = as.Date(date),
    LEGNO = legno, LEGSTAGE = stage, LEGTYPE = legtype,
    stringsAsFactors = FALSE
  )
}

test_that("an occupation with a LEGNO keeps its line number", {
  d <- make_leg_id(occ_frame(legno = "7", stage = c(1, 2, 2, 5)), quiet = TRUE)
  expect_true(all(startsWith(d$LEGNO3, "7_")))
  expect_equal(length(unique(d$LEGNO3)), 1)
})

test_that("a begin-line record with no LEGNO is line_, not derived_", {
  d <- make_leg_id(occ_frame(stage = c(1, 2, 2, 5)), quiet = TRUE)
  expect_true(all(startsWith(d$LEGNO3, "line_")))
})

test_that("census track with neither is derived_, and says so", {
  expect_message(
    d <- make_leg_id(occ_frame()),
    "inferred from runs of census track"
  )
  expect_true(all(startsWith(d$LEGNO3, "derived_")))
})

test_that("a begin-line record opens a new occupation under one LEGNO", {
  # A line flown twice under the same number: nothing about LEGNO changes,
  # so only the begin-line record separates the two attempts.
  d <- make_leg_id(
    occ_frame(legno = "4", stage = c(1, 5, 1, 5), n = 4), quiet = TRUE
  )
  expect_equal(length(unique(d$LEGNO3)), 2)
})

test_that("an occupation never spans two days", {
  a <- occ_frame(legno = "4", stage = c(1, 2, 2, 5), date = "2024-04-01")
  b <- occ_frame(legno = "4", stage = c(1, 2, 2, 5), date = "2024-04-02")
  b$EVENTNO <- b$EVENTNO + 100
  d <- make_leg_id(rbind(a, b), quiet = TRUE)
  expect_equal(length(unique(d$LEGNO3)), 2)
  expect_equal(length(unique(paste(d$DATE, d$LEGNO3))), 2)
})

test_that("records that are part of no line stay NA", {
  transit <- occ_frame(legtype = 1, n = 3)
  line <- occ_frame(legno = "4", stage = c(1, 2, 2, 5))
  line$EVENTNO <- line$EVENTNO + 100
  d <- make_leg_id(rbind(transit, line), quiet = TRUE)
  expect_equal(sum(is.na(d$LEGNO3)), 3)
})

test_that("quiet silences the note", {
  expect_silent(make_leg_id(occ_frame(), quiet = TRUE))
})

test_that("an occupation closes at its end-line record", {
  d <- occ_frame(legno = "4", stage = c(1, 2, 5, NA), n = 4)
  d$LEGTYPE <- c(2, 2, 2, 1)          # the last record is the ferry away
  out <- make_leg_id(d, quiet = TRUE)
  expect_equal(sum(!is.na(out$LEGNO3)), 3)
  expect_true(is.na(out$LEGNO3[4]))
})

test_that("a line with no end-line record runs to the next occupation", {
  d <- occ_frame(legno = "4", stage = c(1, 2, 2, 2), n = 4)
  out <- make_leg_id(d, quiet = TRUE)
  expect_false(anyNA(out$LEGNO3))
})

test_that("the ferry between two lines belongs to neither", {
  a <- occ_frame(legno = "4", stage = c(1, 5), n = 2)
  ferry <- occ_frame(legno = NA_character_, stage = NA_real_, legtype = 1, n = 3)
  ferry$EVENTNO <- ferry$EVENTNO + 50
  b <- occ_frame(legno = "5", stage = c(1, 5), n = 2)
  b$EVENTNO <- b$EVENTNO + 100

  out <- make_leg_id(rbind(a, ferry, b), quiet = TRUE)
  expect_equal(sum(is.na(out$LEGNO3)), 3)
  expect_equal(length(unique(na.omit(out$LEGNO3))), 2)
})

test_that("census track after an end-line is reported, not silently dropped", {
  d <- occ_frame(legno = "4", stage = c(1, 5, NA), n = 3)
  d$LEGTYPE <- 2                       # still on census after the line closed
  expect_warning(make_leg_id(d), "coding problem")
})

test_that("the note counts line occupations, not every stretch", {
  # The transit in front of the line is a stretch but not an occupation, and
  # counting it overstates what was found.
  transit <- occ_frame(legno = NA_character_, stage = NA_real_, legtype = 1,
                       n = 3)
  line <- occ_frame(legno = NA_character_, stage = c(1, 2, 5), legtype = 2,
                    n = 3)
  line$EVENTNO <- line$EVENTNO + 100

  msg <- tryCatch(make_leg_id(rbind(transit, line)), message = conditionMessage)
  expect_match(msg, "found 1 line occupation")
  expect_no_match(msg, "found 2 line occupation")
})

# Platform from speed ---------------------------------------------------------

speed_frame <- function(metres_per_fix, n = 30, legno = "1") {
  step <- metres_per_fix / 111120          # degrees of latitude
  data.frame(
    FILEID = "F", EVENTNO = seq_len(n), DATE = as.Date("2024-04-01"),
    LEGNO = legno, LEGSTAGE = c(1, rep(2, n - 2), 5), LEGTYPE = 2,
    LATITUDE = 43 + (seq_len(n) - 1) * step, LONGITUDE = -69,
    TIME = 120000 + seq_len(n) - 1,        # one second apart
    stringsAsFactors = FALSE
  )
}

test_that("track_speed recovers a known speed", {
  # 51.4 m/s is 100 knots.
  kn <- track_speed(make_leg_id(speed_frame(51.4), quiet = TRUE))
  expect_equal(round(median(kn, na.rm = TRUE)), 100)
})

test_that("the last record of a stretch has no speed", {
  d <- make_leg_id(speed_frame(51.4), quiet = TRUE)
  expect_true(is.na(track_speed(d)[nrow(d)]))
})

test_that("speed is never taken across a break", {
  a <- speed_frame(51.4, legno = "1")
  b <- speed_frame(51.4, legno = "2")
  b$EVENTNO <- b$EVENTNO + 100
  b$LATITUDE <- b$LATITUDE + 5           # a long ferry between the two
  b$TIME <- b$TIME + 3600
  d <- make_leg_id(rbind(a, b), quiet = TRUE)

  kn <- track_speed(d)
  expect_lt(max(kn, na.rm = TRUE), 200)  # no absurd cross-line value
})

test_that("classify_platform tells an aircraft from a vessel", {
  air <- make_leg_id(speed_frame(51.4), quiet = TRUE)          # 100 kt
  sea <- make_leg_id(speed_frame(5.3), quiet = TRUE)           # 10 kt
  expect_equal(unique(as.character(classify_platform(air))), "aerial")
  expect_equal(unique(as.character(classify_platform(sea))), "vessel")
})

test_that("a platform that is not moving is its own label", {
  still <- speed_frame(0.1)
  expect_equal(unique(as.character(classify_platform(
    make_leg_id(still, quiet = TRUE)))), "stationary")
})

test_that("a whole stretch takes one label", {
  d <- speed_frame(51.4)
  d$LATITUDE[15] <- d$LATITUDE[14]       # one repeated fix mid-line
  lab <- classify_platform(make_leg_id(d, quiet = TRUE))
  expect_equal(length(unique(as.character(lab))), 1)
})

test_that("the threshold is an argument", {
  sea <- make_leg_id(speed_frame(5.3), quiet = TRUE)
  expect_equal(unique(as.character(
    classify_platform(sea, aerial_min = 5))), "aerial")
})

test_that("off-line records are judged by their own day, not pooled", {
  # A record on no line has LEGNO3 of NA. Pooling every such record in an
  # archive into one group gives them all one median speed and one verdict —
  # and circling records, where many sightings are logged, are all off-line.
  fast <- speed_frame(51.4, n = 30, legno = "1")          # ~100 kt, on a line
  fast$DATE <- as.Date("2024-04-01")

  # An off-line stretch on another day, also fast.
  off <- speed_frame(51.4, n = 30, legno = NA_character_)
  off$DATE <- as.Date("2024-04-02")
  off$LEGSTAGE <- NA_real_
  off$LEGTYPE <- 1
  off$EVENTNO <- off$EVENTNO + 100

  # A slow off-line stretch on a third day.
  slow <- speed_frame(5.3, n = 30, legno = NA_character_)
  slow$DATE <- as.Date("2024-04-03")
  slow$LEGSTAGE <- NA_real_
  slow$LEGTYPE <- 1
  slow$EVENTNO <- slow$EVENTNO + 200

  d <- make_leg_id(rbind(fast, off, slow), quiet = TRUE)
  kind <- classify_platform(d)

  # The two off-line stretches must not share a verdict.
  expect_equal(unique(as.character(kind[d$DATE == as.Date("2024-04-02")])),
               "aerial")
  expect_equal(unique(as.character(kind[d$DATE == as.Date("2024-04-03")])),
               "vessel")
})

test_that("an explicit by is still honoured", {
  d <- make_leg_id(speed_frame(51.4), quiet = TRUE)
  expect_equal(unique(as.character(classify_platform(d, by = "DATE"))),
               "aerial")
})

test_that("a NULL threshold drops its criterion rather than widening it", {
  # The vessel case: no altitude, because there is no altitude to have. A
  # missing value fails a criterion, so raising the ceiling cannot rescue
  # these records and only dropping the criterion can.
  dat <- straight_line(n = 6)
  dat$ALT <- NA_real_
  expect_equal(sum(flag_effort(dat)$OnOff.Effort), 0)
  expect_equal(sum(flag_effort(dat, max_alt_m = Inf)$OnOff.Effort), 0)
  expect_equal(sum(flag_effort(dat, max_alt_m = NULL)$OnOff.Effort), 6)
})

test_that("dropping one criterion leaves the others strict", {
  # `na_action = "pass"` would let these through too, but it would let every
  # other missing criterion through with them.
  dat <- straight_line(n = 6)
  dat$ALT <- NA_real_
  dat$BEAUFORT[1:2] <- NA
  expect_equal(sum(flag_effort(dat, max_alt_m = NULL)$OnOff.Effort), 4)
  expect_equal(sum(flag_effort(dat, na_action = "pass")$OnOff.Effort), 6)
})

test_that("every criterion can be dropped, and LEGTYPE still decides", {
  dat <- straight_line(n = 5)
  dat$LEGTYPE <- c(0, 1, 2, 3, 4)
  out <- flag_effort(dat, max_beaufort = NULL, max_alt_m = NULL,
                     min_visibility_nmi = NULL)
  expect_equal(out$OnOff.Effort, c(0L, 0L, 1L, 0L, 0L))
})
