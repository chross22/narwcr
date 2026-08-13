ls_frame <- function(stage, legtype = 2, occ = "1_1") {
  data.frame(FILEID = "F", EVENTNO = seq_along(stage), LEGNO3 = occ,
             LEGTYPE = legtype, LEGSTAGE = stage, stringsAsFactors = FALSE)
}

test_that("a blank record after a begin-line is continuing the line", {
  out <- fill_legstage(ls_frame(c(1, NA, NA, 5)), quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(1, 2, 2, 5))
})

test_that("only 2 is ever written, never the event code", {
  out <- fill_legstage(ls_frame(c(1, NA, NA)), quiet = TRUE)
  expect_false(any(out$LEGSTAGE[-1] == 1))
})

test_that("nothing continues past an end-line", {
  out <- fill_legstage(ls_frame(c(1, NA, 5, NA, NA)), quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(1, 2, 5, NA, NA))
})

test_that("a circle is not a census record", {
  # After a break-off the aircraft is off the line until it resumes.
  out <- fill_legstage(ls_frame(c(1, NA, 3, NA, NA, 4, NA)), quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(1, 2, 3, NA, NA, 4, 2))
})

test_that("records before the first event stay unknown", {
  out <- fill_legstage(ls_frame(c(NA, NA, 1, NA)), quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(NA, NA, 1, 2))
})

test_that("a state never carries across an occupation", {
  d <- rbind(ls_frame(c(1, NA), occ = "1_1"),
             ls_frame(c(NA, NA), occ = "2_2"))
  d$EVENTNO <- seq_len(4)
  out <- fill_legstage(d, quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(1, 2, NA, NA))
})

test_that("a non-census record does not acquire a census state", {
  d <- ls_frame(c(1, NA, NA), legtype = c(2, 4, 2))
  out <- fill_legstage(d, quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(1, NA, 2))
})

test_that("what was written is marked and reported", {
  expect_message(out <- fill_legstage(ls_frame(c(1, NA, NA))),
                 "set LEGSTAGE to 2 on 2 records")
  expect_equal(out$LEGSTAGE_FILLED, c(FALSE, TRUE, TRUE))
})

test_that("a recorded code is never overwritten", {
  out <- fill_legstage(ls_frame(c(1, 3, 4, 2)), quiet = TRUE)
  expect_equal(out$LEGSTAGE, c(1, 3, 4, 2))
  expect_false(any(out$LEGSTAGE_FILLED))
})

test_that("it makes the excluded records eligible", {
  d <- ls_frame(c(1, NA, NA, 5))
  d$OnOff.Effort <- 1L
  expect_equal(sum(on_effort_census_rows(d)), 0)
  expect_equal(sum(on_effort_census_rows(fill_legstage(d, quiet = TRUE))), 2)
})

test_that("an empty frame is handled", {
  out <- fill_legstage(ls_frame(1)[0, ], quiet = TRUE)
  expect_equal(nrow(out), 0)
  expect_equal(length(out$LEGSTAGE_FILLED), 0)
})
