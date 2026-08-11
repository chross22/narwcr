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
