# Regressions from an audit of the app's server and helpers. Each test names
# the defect it pins down; the app itself is inst/shiny/app.R.

audit_env <- function() {
  path <- system.file("shiny", "app.R", package = "narwcr")
  skip_if(!nzchar(path), "app not installed")
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  env <- new.env(parent = globalenv())
  suppressMessages(sys.source(path, envir = env, keep.source = FALSE))
  env
}

audit_app_dir <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")
  app_dir
}

audit_pam_path <- function() {
  path <- system.file("extdata", "pam-example.csv", package = "narwcr")
  skip_if(!nzchar(path), "no PAM example")
  path
}

audit_inputs <- list(
  max_beaufort = 3, max_alt = 366, min_vis = 2, na_action = "fail",
  months = 1:12, effort_only = FALSE, track_mode = "effort", cluster = FALSE,
  size_by_number = FALSE, colour_by = "species", show_bathy = FALSE,
  criteria = c("beaufort", "alt", "vis"), legtypes = "2"
)

audit_csv <- function(dat) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(dat, f, row.names = FALSE)
  f
}

# ------------------------------------------------------------- selection ----

test_that("an empty data-type selection hides the acoustic stations too", {
  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = audit_pam_path(),
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    output$type_control
    session$setInputs(types = c("aerial", "PAM"))
    expect_false(is.null(selected_pam_all()))

    session$setInputs(types = character(0))
    expect_null(selected_pam_all())
    expect_null(pam_stations())
  })
})

test_that("clearing every contour depth draws no contours, not the defaults", {
  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  lon <- seq(-70, -68, by = 0.1)
  lat <- seq(42, 44, by = 0.1)
  z <- outer(lon, lat, function(x, y) 400 - 260 * (x + 70) - 100 * (44 - y))
  dimnames(z) <- list(as.character(lon), as.character(lat))

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    session$setInputs(show_bathy = TRUE, contour_depths = c(100, 200))
    bathy_grid(z)
    expect_equal(sort(as.numeric(names(bathy_contours()))), c(100, 200))

    session$setInputs(contour_depths = NULL)
    expect_null(bathy_contours())
  })
})

test_that("an explicitly cleared species selection survives an upstream redraw", {
  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = audit_pam_path(),
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  selected_in <- function(html) {
    m <- regmatches(html, gregexpr("<option value=\"[^\"]*\" selected>", html))[[1]]
    sub("<option value=\"([^\"]*)\".*", "\\1", m)
  }

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    output$type_control
    session$setInputs(types = c("aerial", "PAM"))
    first <- output$species_control
    # Before the control has reported anything, every species is ticked.
    expect_setequal(selected_in(first$html), species_codes())

    session$setInputs(species = species_codes())
    session$setInputs(species = NULL)
    session$setInputs(types = "aerial")
    expect_length(selected_in(output$species_control$html), 0L)
    expect_equal(nrow(selected_sightings()), 0L)
  })
})

test_that("a redraw of the data-type control keeps what was unticked", {
  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)
  pam_data <- audit_env()$read_pam(audit_pam_path())

  ticked <- function(html) {
    m <- regmatches(html, gregexpr("value=\"[^\"]*\" checked", html))[[1]]
    sub("value=\"([^\"]*)\".*", "\\1", m)
  }

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    expect_equal(ticked(output$type_control$html), "aerial")
    session$setInputs(types = character(0))

    # Loading an acoustic file redraws the control. The survey type stays
    # unticked; PAM, offered for the first time, arrives ticked.
    pam(pam_data)
    session$flushReact()
    expect_equal(ticked(output$type_control$html), "PAM")
  })
})

test_that("a stale date range from the previous file does not empty the new one", {
  app_dir <- audit_app_dir()
  one_day <- data.frame(
    FILEID = "F", EVENTNO = 1:4, YEAR = 2024L, MONTH = 6L, DAY = 1L,
    TIME = 120000 + (1:4) * 100, LATITUDE = 43 + (1:4) / 100,
    LONGITUDE = -69, LEGTYPE = 2, LEGSTAGE = 1, LEGNO = 1,
    DATE = as.Date("2024-06-01"), stringsAsFactors = FALSE
  )
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    output$type_control
    session$setInputs(types = "aerial",
                      dates = as.Date(c("2024-04-02", "2024-04-02")))
    expect_lt(nrow(in_period()), nrow(flagged()))

    # A single survey day, outside that range, replaces the slider with text,
    # and the old range stays behind in `input$dates`.
    raw(one_day)
    session$flushReact()
    expect_match(as.character(output$date_control$html), "One survey day")
    expect_equal(nrow(in_period()), nrow(one_day))
  })
})

# ------------------------------------------------------------- the page ----

test_that("the popup's visibility verdict follows the sidebar, and the tickbox", {
  env <- audit_env()
  dat <- data.frame(SPECCODE = "RIWH", VISIBLTY = 3, LATITUDE = 43,
                    LONGITUDE = -69, DATE = as.Date("2024-04-01"))
  expect_match(env$sighting_popups(dat, min_vis = 2), "3 nmi (acceptable)",
               fixed = TRUE)
  expect_match(env$sighting_popups(dat, min_vis = 5),
               "3 nmi (below threshold)", fixed = TRUE)
  off <- env$sighting_popups(dat, min_vis = NULL)
  expect_match(off, "3 nmi", fixed = TRUE)
  expect_false(grepl("acceptable|threshold", off))
})

test_that("the off-effort note names the control that exists", {
  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    output$type_control
    session$setInputs(types = "aerial")
    expect_gt(nrow(ferry_track()), 0)
    html <- paste(unlist(output$notes), collapse = " ")
    expect_false(grepl("Draw transit", html, fixed = TRUE))
    expect_match(html, "Track to draw", fixed = TRUE)
    expect_match(html, "everywhere the platform went", fixed = TRUE)
  })
})

test_that("the By year table carries years with acoustic data and no survey", {
  app_dir <- audit_app_dir()
  pam <- utils::read.csv(audit_pam_path(), stringsAsFactors = FALSE)
  pam$date <- sub("^2024", "2023", pam$date)
  pam_file <- audit_csv(pam)
  on.exit(unlink(pam_file), add = TRUE)
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = pam_file,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    output$type_control
    session$setInputs(types = c("aerial", "PAM"))
    session$setInputs(species = species_codes())
    out <- output$by_year
    expect_match(out, "2023", fixed = TRUE)   # acoustic only
    expect_match(out, "2024", fixed = TRUE)   # the survey

    # No survey record selected at all, and the acoustic years remain.
    session$setInputs(types = "PAM")
    expect_match(output$by_year, "2023", fixed = TRUE)
  })
})

test_that("the download leaves out the app's own drawing key", {
  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    output$type_control
    output$species_control
    output$idrel_control
    session$setInputs(types = "aerial", species = species_codes(),
                      idrel = idrel_codes())
    got <- utils::read.csv(output$download, stringsAsFactors = FALSE)
    expect_equal(nrow(got), nrow(selected_sightings()))
    expect_false("TRACK" %in% names(got))
    expect_true(all(c("LEGNO3", "DATATYPE", "OnOff.Effort") %in% names(got)))
  })
})

# --------------------------------------------------------------- PAM in ----

test_that("YEAR, MONTH and DAY columns are read as one date", {
  env <- audit_env()
  f <- audit_csv(data.frame(site = "A", lat = 43, lon = -69, year = 2024,
                            month = 1:2, day = 5, detected = "Y"))
  on.exit(unlink(f), add = TRUE)
  pam <- env$read_pam(f)
  expect_equal(pam$DATE, as.Date(c("2024-01-05", "2024-02-05")))
  expect_equal(pam$MONTH, 1:2)
})

test_that("a US date is read as one, and a misread year is reported", {
  env <- audit_env()
  f <- audit_csv(data.frame(
    site = "A", lat = 43, lon = -69,
    date = c("01/05/2020", "2020-01-06", "0001-05-20", "not a date"),
    detected = "Y"
  ))
  on.exit(unlink(f), add = TRUE)
  pam <- env$read_pam(f)
  expect_equal(pam$DATE, as.Date(c("2020-01-05", "2020-01-06", NA, NA)))
  map <- attr(pam, "pam_mapping")
  expect_true("DATE not read" %in% map$standardized)
  expect_match(map$original[map$standardized == "DATE not read"], "^2 rows")
})

test_that("an acoustic data frame is matched as loosely as a file", {
  env <- audit_env()
  raw <- data.frame(site = c("A", "A", "B"), lat = 43, lon = -69,
                    date = c("2024-01-01", "2024-01-02", "2024-01-01"),
                    detected = c("Y", "N", "Y"), stringsAsFactors = FALSE)
  pam <- env$normalise_pam(raw)
  expect_equal(pam$STATION, c("A", "A", "B"))
  st <- env$pam_summary(pam)
  expect_equal(nrow(st), 2L)
  # Already-normalised input comes back the same shape.
  again <- env$normalise_pam(pam)
  expect_equal(again$DATE, pam$DATE)
  expect_equal(again$DETECTED, pam$DETECTED)

  app_dir <- audit_app_dir()
  op <- options(narwcr.app_data = NULL, narwcr.app_pam = raw,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)
  shiny::testServer(app_dir, {
    do.call(session$setInputs, audit_inputs)
    session$setInputs(types = "PAM")
    session$setInputs(species = species_codes())
    expect_equal(nrow(pam_stations()), 2L)
  })
})

# -------------------------------------------------------------- distance ----

test_that("track length leaves out the leg into a track and fills missing TRKDIST", {
  env <- audit_env()
  dat <- data.frame(TRACK = c("a", "a", "a", "b", "b"),
                    TRKDIST = c(5000, 1000, NA, 9000, 2000),
                    LATITUDE = c(43, 43.01, 43.02, 44, 44.01),
                    LONGITUDE = -69)
  straight <- narwcr:::haversine_km(43.01, -69, 43.02, -69)
  # 1 km measured, one straight-line step where TRKDIST is missing, and 2 km
  # on the second track. The 5 km and 9 km legs into each track are not
  # part of either.
  expect_equal(env$track_km(dat), 1 + straight + 2)
  expect_equal(env$track_km(dat[1, ]), 0)
})

test_that("track length does not count the step across an off-effort stretch", {
  env <- audit_env()
  # Rows 1-2 and 3-4 are on effort in one track, with an off-effort stretch
  # between them that effort_track() has already removed.
  dat <- data.frame(TRACK = "a", EFFRUN = c(1, 1, 2, 2),
                    TRKDIST = c(NA, 1000, 50000, 2000),
                    LATITUDE = c(43, 43.01, 44, 44.01), LONGITUDE = -69)
  expect_equal(env$track_km(dat), 1 + 2)
})
