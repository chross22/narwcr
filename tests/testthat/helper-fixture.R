# Shared fixtures for the test suite.

example_path <- function() {
  system.file("extdata", "narwc-example.csv", package = "narwcr")
}

example_data <- function() {
  # The fixture uses the handbook's own LAT_DD/LONG_DD, so reading it reports a
  # rename. That is correct behaviour and is tested in test-column-matching.R;
  # here it would only crowd out the message each test is actually about.
  read_narwc(example_path(), quiet = TRUE)
}

# A single straight north-running survey line, `n` positions `step` degrees
# apart, entirely on effort. One step is step * 111.12 km, so expected
# distances are exact and checkable by hand.
straight_line <- function(n = 21, step = 0.01, lat0 = 43, lon = -69,
                          legno = 1, date = "2024-04-01", beaufort = 2) {
  tibble::tibble(
    FILEID = "TEST",
    EVENTNO = seq_len(n),
    YEAR = 2024L, MONTH = 4L, DAY = as.integer(substr(date, 9, 10)),
    TIME = 120000 + seq_len(n) * 25,
    LATITUDE = lat0 + (seq_len(n) - 1) * step,
    LONGITUDE = lon,
    LEGTYPE = 2,
    LEGSTAGE = c(1, rep(2, n - 2), 5),
    LEGNO = legno,
    ALT = 229, BEAUFORT = beaufort, VISIBLTY = 5,
    SPECCODE = NA_character_, IDREL = NA_real_, NUMBER = NA_real_,
    DATE = as.Date(date)
  )
}

KM_PER_DEG <- 60 * 1.852
