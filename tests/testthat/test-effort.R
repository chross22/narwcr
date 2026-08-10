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
