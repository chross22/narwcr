decl <- function(angle = c(40, 20, 90), side = c("L", "R", "R"), ...) {
  data.frame(SPECCODE = "RIWH", Decl_Angle = angle, Left_or_Right = side,
             ..., stringsAsFactors = FALSE)
}

test_that("one angle column and a side become ANGLEL and ANGLER", {
  out <- angles_from_declination(decl(), "Decl_Angle", "Left_or_Right",
                                 quiet = TRUE)
  expect_equal(out$ANGLEL, c(40, NA, NA))
  expect_equal(out$ANGLER, c(NA, 20, 90))
})

test_that("the side codes are an argument, and matched loosely", {
  d <- decl(side = c(" port ", "Starboard", "PORT"))
  out <- angles_from_declination(d, "Decl_Angle", "Left_or_Right",
                                 left = "port", right = "starboard",
                                 quiet = TRUE)
  expect_equal(out$ANGLEL, c(40, NA, 90))
  expect_equal(out$ANGLER, c(NA, 20, NA))
})

test_that("90 degrees is kept: directly below is a distance of zero", {
  out <- angles_from_declination(decl(angle = c(90, 90, 90)), "Decl_Angle",
                                 "Left_or_Right", quiet = TRUE)
  expect_equal(out$ANGLEL[1], 90)
})

test_that("an angle outside (0, 90] is reported and left alone", {
  d <- decl(angle = c(0, -5, 120))
  expect_warning(out <- angles_from_declination(d, "Decl_Angle",
                                                "Left_or_Right"),
                 "outside \\(0, 90\\]")
  expect_true(all(is.na(out$ANGLEL)))
  expect_true(all(is.na(out$ANGLER)))
})

test_that("an angle with no recognised side is skipped, not guessed", {
  d <- decl(side = c("L", NA, "?"))
  expect_warning(out <- angles_from_declination(d, "Decl_Angle",
                                                "Left_or_Right"),
                 "no recognised side")
  expect_equal(out$ANGLEL, c(40, NA, NA))
  expect_true(all(is.na(out$ANGLER)))
})

test_that("a recorded angle is not displaced by a derived one", {
  d <- decl()
  d$ANGLEL <- c(11, NA, NA)
  out <- angles_from_declination(d, "Decl_Angle", "Left_or_Right",
                                 quiet = TRUE)
  expect_equal(out$ANGLEL, c(11, NA, NA))
})

test_that("overwrite replaces a recorded angle when asked", {
  d <- decl()
  d$ANGLEL <- c(11, NA, NA)
  out <- angles_from_declination(d, "Decl_Angle", "Left_or_Right",
                                 overwrite = TRUE, quiet = TRUE)
  expect_equal(out$ANGLEL, c(40, NA, NA))
})

test_that("what was set is reported", {
  expect_message(angles_from_declination(decl(), "Decl_Angle",
                                         "Left_or_Right"),
                 "set 1 ANGLEL and 2 ANGLER")
})

test_that("a missing column is an error, not a silent no-op", {
  expect_error(angles_from_declination(decl(), "Nope", "Left_or_Right"),
               "Nope")
})
