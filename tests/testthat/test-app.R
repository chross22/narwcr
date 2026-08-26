# The app's own helpers, tested without starting a server.
#
# `inst/shiny/app.R` ends in `shinyApp()`, which builds an object and does not
# run anything, so the file can be sourced into an environment and its
# functions called directly. That is worth doing: the data-type split, the
# acoustic reader and the drawing thinners all make decisions about what a
# reader is shown, and a decision made only inside a Shiny app is a decision
# nothing checks.

app_env <- function() {
  path <- system.file("shiny", "app.R", package = "narwcr")
  skip_if(!nzchar(path), "app not installed")
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  env <- new.env(parent = globalenv())
  suppressMessages(sys.source(path, envir = env, keep.source = FALSE))
  env
}

test_that("LEGTYPE decides the data type, and says that it did", {
  env <- app_env()
  dat <- data.frame(
    LEGTYPE = c(2, 2, 5, 6, 7, 9, 0),
    LATITUDE = 43, LONGITUDE = -69, TIME = 120000, TRACK = "a"
  )
  out <- env$survey_type(dat)
  expect_equal(as.character(out$type),
               c("aerial", "aerial", "vessel", "vessel",
                 "opportunistic", "opportunistic", "aerial"))
  expect_true(all(out$source == "LEGTYPE"))
})

test_that("a record LEGTYPE cannot place falls back to speed, and is marked", {
  env <- app_env()
  # Two minutes apart, four nautical miles: 120 knots, an aircraft.
  fast <- data.frame(
    LEGTYPE = NA_real_, TRACK = "a", TIME = c(120000, 120200, 120400),
    LATITUDE = c(43, 43.0667, 43.1334), LONGITUDE = -69
  )
  out <- env$survey_type(fast)
  expect_equal(as.character(out$type), rep("aerial", 3))
  expect_equal(out$source, rep("speed", 3))

  # Ten knots over the same interval: a vessel.
  slow <- fast
  slow$LATITUDE <- c(43, 43.00556, 43.01112)
  expect_equal(as.character(env$survey_type(slow)$type), rep("vessel", 3))
})

test_that("a platform that is not moving is not guessed at", {
  env <- app_env()
  still <- data.frame(
    LEGTYPE = NA_real_, TRACK = "a", TIME = c(120000, 120200, 120400),
    LATITUDE = 43, LONGITUDE = -69
  )
  out <- env$survey_type(still)
  expect_equal(as.character(out$type), rep("unknown", 3))
  expect_equal(out$source, rep("unrecorded", 3))
})

test_that("the LEGTYPE mapping can be overridden without editing the app", {
  env <- app_env()
  op <- options(narwcr.legtype_types = c("5" = "opportunistic"))
  on.exit(options(op), add = TRUE)
  dat <- data.frame(LEGTYPE = c(5, 6), LATITUDE = 43, LONGITUDE = -69,
                    TIME = 120000, TRACK = "a")
  expect_equal(as.character(env$survey_type(dat)$type),
               c("opportunistic", "vessel"))
})

test_that("a species code is never replaced by a name it does not have", {
  env <- app_env()
  expect_match(env$species_label("RIWH"), "^RIWH - ")
  expect_equal(env$species_label("ZZZZ"), "ZZZZ")
  op <- options(narwcr.species_labels = c(ZZZZ = "test whale"))
  on.exit(options(op), add = TRUE)
  expect_equal(env$species_label("ZZZZ"), "ZZZZ - test whale")
})

test_that("a blank detection is not an absence", {
  env <- app_env()
  expect_equal(env$is_detected(c("Y", "N", "", NA, "present", "0")),
               c(TRUE, FALSE, NA, NA, TRUE, FALSE))
})

test_that("the acoustic reader matches loosely and keeps effort as the denominator", {
  env <- app_env()
  path <- system.file("extdata", "pam-example.csv", package = "narwcr")
  skip_if(!nzchar(path))
  pam <- env$read_pam(path)
  expect_true(all(c("STATION", "LATITUDE", "LONGITUDE", "DATE", "SPECIES",
                    "DETECTED", "HOURS_RECORDED", "SCORED") %in% names(pam)))
  expect_s3_class(pam$DATE, "Date")
  expect_true(nrow(attr(pam, "pam_mapping")) >= 6)

  st <- env$pam_summary(pam)
  expect_equal(nrow(st), length(unique(pam$STATION)))
  # A day the recorder was off is not a day the whale was absent, so it is in
  # neither the numerator nor the denominator.
  expect_true(all(st$days <= table(pam$STATION)[st$STATION]))
  expect_equal(st$rate, st$days_detected / st$days)
})

test_that("an acoustic file with no position is refused rather than dropped", {
  env <- app_env()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  utils::write.csv(data.frame(site = "A", date = "2024-01-01", detected = "Y"),
                   f, row.names = FALSE)
  expect_error(env$read_pam(f), "latitude and longitude")
})

test_that("thinning keeps the ends of every track and reports what it dropped", {
  env <- app_env()
  dat <- data.frame(
    TRACK = rep(c("a", "b"), each = 50),
    LATITUDE = seq(43, 44, length.out = 100),
    LONGITUDE = seq(-69, -68, length.out = 100)
  )
  out <- env$thin_tracks(dat, 20)
  expect_lt(nrow(out$dat), nrow(dat))
  expect_equal(out$dropped, nrow(dat) - nrow(out$dat))
  expect_gt(out$every, 1L)
  # First and last of each track survive, so a thinned line still starts and
  # ends where the platform did.
  for (id in c("a", "b")) {
    kept <- out$dat[out$dat$TRACK == id, ]
    orig <- dat[dat$TRACK == id, ]
    expect_equal(kept$LATITUDE[1], orig$LATITUDE[1])
    expect_equal(kept$LATITUDE[nrow(kept)], orig$LATITUDE[nrow(orig)])
  }
})

test_that("tracks are separated by an NA rather than joined across the gap", {
  env <- app_env()
  dat <- data.frame(TRACK = c("a", "a", "b", "b"),
                    LATITUDE = c(43, 43.1, 45, 45.1),
                    LONGITUDE = c(-69, -69, -60, -60))
  co <- env$break_at_track(dat)
  expect_equal(length(co$lng), 5L)
  expect_true(is.na(co$lng[3]))
  expect_equal(co$lat[c(1, 2, 4, 5)], dat$LATITUDE)
})

test_that("a long species list is lumped without losing a sighting", {
  env <- app_env()
  code <- c(rep("RIWH", 20), rep("HUWH", 10), paste0("SP", 1:12))
  out <- env$lump_species(code, max_levels = 8L)
  expect_equal(length(out), length(code))
  expect_equal(nlevels(out), 8L)
  expect_match(levels(out)[8], "^other \\(")
  expect_equal(levels(out)[1], "RIWH")
})

test_that("a negative VISIBLTY is read as the code it is, not as a distance", {
  env <- app_env()
  dat <- data.frame(SPECCODE = "RIWH", VISIBLTY = -1, LATITUDE = 43,
                    LONGITUDE = -69, DATE = as.Date("2024-04-01"))
  popup <- env$sighting_popups(dat)
  expect_match(popup, "clear visibility")
  expect_false(grepl("-1 nmi", popup, fixed = TRUE))
})

# ---------------------------------------------------------- bathymetry ----
#
# None of these reach the network. The grid is a matrix with longitudes on its
# rownames and latitudes on its colnames, which is all `marmap` hands back and
# all any of this reads.

fake_bathy <- function() {
  lon <- seq(-70, -68, by = 0.1)
  lat <- seq(42, 44, by = 0.1)
  # A shelf falling away to the southeast: depth increases with distance from
  # the northwest corner, and the northwest corner itself is dry land.
  z <- outer(lon, lat, function(x, y) 400 - 260 * (x + 70) - 100 * (44 - y))
  dimnames(z) <- list(as.character(lon), as.character(lat))
  z
}

test_that("the fetch box is padded and rounded, so the same data asks twice for one file", {
  env <- app_env()
  a <- env$bathy_extent(c(-70, -69), c(42, 43))
  b <- env$bathy_extent(c(-70, -69) + 1e-6, c(42, 43) - 1e-6)
  expect_equal(a, b)
  expect_lt(a$x1, -70)
  expect_gt(a$x2, -69)
})

test_that("a single position still gets a box worth looking at", {
  env <- app_env()
  ext <- env$bathy_extent(-69, 43)
  expect_gt(ext$x2 - ext$x1, 0.5)
  expect_gt(ext$y2 - ext$y1, 0.5)
})

test_that("a wider box asks for a coarser grid", {
  env <- app_env()
  expect_equal(env$suggest_resolution(list(x1 = -70, x2 = -68, y1 = 42, y2 = 44)), 1)
  expect_gt(env$suggest_resolution(list(x1 = -90, x2 = -10, y1 = 0, y2 = 60)), 4)
})

test_that("the cache directory follows the option, so one grid can serve the stack", {
  env <- app_env()
  op <- options(narwcr.cache = "/tmp/somewhere")
  on.exit(options(op), add = TRUE)
  expect_equal(env$bathy_cache(), file.path("/tmp/somewhere", "bathymetry"))
})

test_that("axes are put in increasing order, whatever order they arrived in", {
  env <- app_env()
  z <- fake_bathy()
  flipped <- z[rev(seq_len(nrow(z))), rev(seq_len(ncol(z))), drop = FALSE]
  ax <- env$bathy_axes(flipped)
  expect_false(is.unsorted(ax$lon))
  expect_false(is.unsorted(ax$lat))
  expect_equal(ax$z, env$bathy_axes(z)$z)
})

test_that("a depth the seafloor never reaches contributes nothing, not an empty layer", {
  env <- app_env()
  b <- fake_bathy()
  cs <- env$depth_contours(b, c(100, 200, 9000))
  expect_equal(names(cs), c("100", "200"))
  expect_true(all(vapply(cs, function(c) c$pieces > 0, logical(1))))
})

test_that("contour pieces are separated by an NA rather than joined end to end", {
  env <- app_env()
  cs <- env$depth_contours(fake_bathy(), 200)
  co <- cs[["200"]]
  expect_equal(length(co$lng), length(co$lat))
  # Never a trailing NA: the break belongs between two pieces, not after the
  # last one, where leaflet would read it as a line going nowhere.
  expect_false(is.na(utils::tail(co$lng, 1)))
  if (co$pieces > 1) expect_true(anyNA(co$lng))
})

test_that("depth is reported from the nearest cell, and land is reported as nothing", {
  env <- app_env()
  b <- fake_bathy()
  # Southeast corner: deepest water on this shelf.
  deep <- env$depth_at(b, -68, 44)
  expect_true(is.finite(deep))
  expect_gt(deep, 0)
  # Northwest corner sits above sea level on this grid, and a sighting there is
  # a position problem, not a shallow one.
  expect_true(is.na(env$depth_at(b, -70, 42)))
  expect_equal(env$depth_at(NULL, -69, 43), NA_real_)
})

test_that("a popup carries a depth only when a grid was already in hand", {
  env <- app_env()
  dat <- data.frame(SPECCODE = "RIWH", LATITUDE = 43.5, LONGITUDE = -68.5,
                    DATE = as.Date("2024-04-01"))
  expect_false(grepl("Seafloor depth", env$sighting_popups(dat)))
  expect_match(env$sighting_popups(dat, bathy = fake_bathy()), "Seafloor depth")
})

test_that("a cached grid is read from disk rather than fetched again", {
  env <- app_env()
  skip_if_not_installed("marmap")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  ext <- list(x1 = -70, x2 = -69.5, y1 = 42, y2 = 42.5)
  grid <- expand.grid(x = seq(ext$x1, ext$x2, by = 0.1),
                      y = seq(ext$y1, ext$y2, by = 0.1))
  grid$z <- -100
  utils::write.csv(grid, file.path(dir, sprintf(
    "marmap_coord_%s;%s;%s;%s_res_%s.csv", ext$x1, ext$y1, ext$x2, ext$y2, 1
  )), row.names = FALSE)

  # No network: if the filename convention ever stops matching, this reaches
  # NOAA and the test says so by being slow or by failing offline.
  b <- env$fetch_bathy(ext, 1, path = dir)
  expect_true(all(unclass(b) == -100))
})

test_that("a relative path in a file box means relative to where the app was launched", {
  env <- app_env()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.create(file.path(dir, "extract.csv"))

  op <- options(narwcr.app_wd = dir)
  on.exit(options(op), add = TRUE)

  # shiny::runApp() has moved the working directory into the installed package
  # by the time a box is typed into, so an unresolved relative path is looked
  # for in the wrong place entirely.
  expect_equal(env$resolve_path("extract.csv"), file.path(dir, "extract.csv"))
  expect_equal(env$resolve_path(" extract.csv "), file.path(dir, "extract.csv"))

  # An absolute path is already an answer, and a Google Drive id or URL is not
  # a path at all - joining either to a directory would break it.
  expect_equal(env$resolve_path("/tmp/elsewhere.csv"), "/tmp/elsewhere.csv")
  expect_equal(env$resolve_path("https://drive.google.com/file/d/1AbC/view"),
               "https://drive.google.com/file/d/1AbC/view")
  expect_equal(env$resolve_path("1AbCdEfGhIjKlMnOpQrStUvWxYz"),
               "1AbCdEfGhIjKlMnOpQrStUvWxYz")
  expect_equal(env$resolve_path(""), "")
})

# ----------------------------------------------------- the app with no PAM ----
#
# Acoustic data is optional and most extracts arrive without any. Everything
# that reads it therefore has to cope with nothing being there, and "cope"
# means the map still draws rather than the whole page erroring out on a
# station table nobody asked for.

test_that("every output renders with no acoustic data at all", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(
      max_beaufort = 3, max_alt = 366, min_vis = 2, na_action = "fail",
      months = 1:12, types = c("aerial", "vessel", "opportunistic", "unknown"),
      track_mode = "all", cluster = TRUE,
      size_by_number = TRUE, colour_by = "species", effort_only = FALSE,
      show_bathy = FALSE, contour_depths = c(100, 200)
    )
    expect_gt(nrow(prepared()), 0)
    expect_null(selected_pam())
    expect_null(pam_stations())

    # A browser sends a rendered control's value straight back; `testServer`
    # does not, so the round trip has to be made by hand or every sighting
    # filters out against an input that was never set.
    output$species_control
    output$idrel_control
    session$setInputs(species = species_codes(), idrel = idrel_codes())
    expect_gt(nrow(selected_sightings()), 0)
    # The extent a bathymetry grid would be fetched for still resolves; it is
    # simply the survey's own.
    expect_false(is.null(data_extent()))

    for (out in c("stats", "notes", "by_year", "by_type", "by_species",
                  "by_station", "source_text", "mapping", "type_source",
                  "legtype_table", "pam_mapping", "findings", "type_control",
                  "date_control", "species_control", "idrel_control",
                  "bathy_res_control", "bathy_note")) {
      expect_error(output[[out]], NA, label = out)
    }
  })
})

test_that("the data-type control offers PAM only when there is acoustic data", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  types_offered <- function(pam) {
    op <- options(narwcr.app_data = NULL, narwcr.app_pam = pam,
                  narwcr.app_source = NULL)
    on.exit(options(op), add = TRUE)
    got <- NULL
    shiny::testServer(app_dir, {
      session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                        na_action = "fail", months = 1:12)
      got <<- output$type_control
    })
    got
  }

  without <- types_offered(NULL)
  expect_false(grepl("PAM", without$html, fixed = TRUE))

  with <- types_offered(system.file("extdata", "pam-example.csv",
                                    package = "narwcr"))
  expect_true(grepl("PAM", with$html, fixed = TRUE))
  # The label carries a swatch and a count, not just the bare word.
  expect_true(grepl("nw-swatch", with$html, fixed = TRUE))
})

test_that("the layer control offers a PAM overlay only when there are stations", {
  env <- app_env()
  expect_false("PAM stations" %in% env$overlay_groups(FALSE))
  expect_true("PAM stations" %in% env$overlay_groups(TRUE))
  # NULL is what `!is.null(pam())` gives before anything has loaded, and it
  # must not be read as "yes".
  expect_false("PAM stations" %in% env$overlay_groups(NULL))
  expect_true(all(c("Sightings", "On-effort track", "Depth contours") %in%
                    env$overlay_groups(FALSE)))
})

test_that("an installation with no app in it says so, and says where to look", {
  # The confusing case: `run_narwc_app()` is in memory from a session that
  # loaded a complete narwcr, while the copy on disk was replaced by one built
  # before the app existed. Nothing about "could not locate the app directory"
  # points at that, so the message names the directory and the fix.
  err <- tryCatch(
    with_mocked_bindings(
      run_narwc_app(),
      .package = "base",
      system.file = function(..., package = "base") "",
      find.package = function(...) "/somewhere/narwcr"
    ),
    error = function(e) conditionMessage(e)
  )
  skip_if(is.null(err), "mocking base bindings is unsupported here")
  expect_match(err, "no `shiny/` directory", fixed = TRUE)
  expect_match(err, "/somewhere/narwcr", fixed = TRUE)
  expect_match(err, "restart R", fixed = TRUE)
})

# ------------------------------------------- an empty selection means empty ----
#
# `checkboxGroupInput` sends NULL when nothing is ticked and NULL again before
# it has rendered. Reading both as "no filter" - the obvious reading, and the
# one this app shipped with - makes unticking every data type show every data
# type, and makes the "none" link under the months show all twelve.

test_that("unticking every data type shows no data, and re-ticking brings it back", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                      na_action = "fail", months = 1:12, effort_only = FALSE,
                      track_mode = "effort", cluster = FALSE,
                      size_by_number = FALSE, colour_by = "species",
                      show_bathy = FALSE)
    everything <- nrow(flagged())
    expect_gt(everything, 0)
    output$type_control      # render it, so the app knows the control exists
    output$species_control

    session$setInputs(types = character(0))
    expect_equal(nrow(in_types()), 0L)
    expect_equal(nrow(selected_sightings()), 0L)
    expect_equal(nrow(effort_track()), 0L)

    # The shipped example is aerial throughout, so ticking it back on is the
    # whole table again.
    session$setInputs(types = "aerial")
    expect_equal(nrow(in_types()), everything)
  })
})

test_that("the months `none` link means none", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                      na_action = "fail", months = 1:12, types = "aerial",
                      effort_only = FALSE)
    output$type_control
    expect_gt(nrow(in_period()), 0)

    session$setInputs(months = character(0))
    expect_equal(nrow(in_period()), 0L)

    session$setInputs(months = 4)
    expect_gt(nrow(in_period()), 0)
    session$setInputs(months = 7)
    expect_equal(nrow(in_period()), 0L)   # the example is April
  })
})

test_that("the legend names what is drawn, and says when nothing is", {
  env <- app_env()
  expect_match(env$build_legend(list()), "Nothing is drawn")
  expect_match(env$build_legend(list(list(title = "Track", rows = character(0)))),
               "Nothing is drawn")

  html <- env$build_legend(list(
    list(title = "Species", rows = env$leg_dot("#E69F00", "RIWH - right whale")),
    list(title = "Track", rows = env$leg_line("#0072B2", "aerial, on effort"))
  ))
  expect_match(html, "RIWH - right whale", fixed = TRUE)
  expect_match(html, "aerial, on effort", fixed = TRUE)
  expect_match(html, "#E69F00", fixed = TRUE)
  expect_false(grepl("Nothing is drawn", html, fixed = TRUE))
})


test_that("the effort breakdown names the criterion doing the eliminating", {
  env <- app_env()
  # A year that stopped recording altitude: every record fails the ceiling on a
  # missing value, so the survey has no effort and all of its sightings.
  dat <- data.frame(
    LEGTYPE = 2, BEAUFORT = 1, ALT = NA_real_, VISIBLTY = 5
  )[rep(1, 10), ]
  out <- env$effort_criteria(dat)
  alt <- out[grepl("^ALT", out$Criterion), ]
  expect_equal(alt$`Not recorded`, 10L)
  expect_equal(alt$Passing, 0L)
  # The criteria that were recorded still pass, which is what makes the empty
  # one stand out rather than hide in a single overall zero.
  expect_equal(out$Passing[grepl("^LEGTYPE is", out$Criterion)], 10L)

  # `na_action = "pass"` is the escape hatch, and it shows here too.
  passed <- env$effort_criteria(dat, na_action = "pass")
  expect_equal(passed$Passing[grepl("^ALT", passed$Criterion)], 10L)

  expect_null(env$effort_criteria(dat[0, , drop = FALSE]))
})

test_that("a criterion can be dropped rather than widened", {
  env <- app_env()
  # A shipboard survey: no altitude, because there is no altitude to record.
  dat <- data.frame(LEGTYPE = 2, BEAUFORT = 1, ALT = NA_real_,
                    VISIBLTY = 5)[rep(1, 10), ]

  applied <- env$effort_criteria(dat, max_alt_m = 366)
  expect_equal(applied$Passing[grepl("^ALT below", applied$Criterion)], 0L)

  # A ceiling set impossibly high does not rescue it: a missing value fails a
  # criterion however wide the threshold. Only dropping the criterion does.
  wide <- env$effort_criteria(dat, max_alt_m = 1e6)
  expect_equal(wide$Passing[grepl("^ALT below", wide$Criterion)], 0L)

  dropped <- env$effort_criteria(dat, max_alt_m = NULL)
  row <- dropped[dropped$Criterion == "ALT - not applied", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$Passing, 10L)
  # The count that explains the decision stays on the row.
  expect_equal(row$`Not recorded`, 10L)
  # And the other criteria are untouched, which is the point of dropping one
  # rather than ignoring every missing value.
  expect_true(any(grepl("^BEAUFORT at most", dropped$Criterion)))
})

test_that("unticking a criterion in the app drops it from flag_effort", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                      na_action = "fail", months = 1:12, types = "aerial",
                      criteria = c("beaufort", "alt", "vis"))
    output$type_control
    expect_equal(effort_args()$max_alt_m, 366)

    session$setInputs(criteria = c("beaufort", "vis"))
    expect_null(effort_args()$max_alt_m)
    expect_equal(effort_args()$max_beaufort, 3)

    session$setInputs(criteria = character(0))
    expect_null(effort_args()$max_beaufort)
    expect_null(effort_args()$min_visibility_nmi)
    # LEGTYPE is not one of the three and still governs, so this is not the
    # same as calling everything on effort.
    expect_lt(sum(flagged()$OnOff.Effort), nrow(flagged()))
  })
})

test_that("a shipboard survey can be put on effort, which needs both halves", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  # A vessel: shipboard LEGTYPE, no altitude, because it has none to record.
  ship <- data.frame(
    FILEID = "V01", EVENTNO = 1:20, YEAR = 2024L, MONTH = 6L, DAY = 1L,
    TIME = 100000 + (1:20) * 100, LATITUDE = 43 + (1:20) * 0.01,
    LONGITUDE = -66, LEGTYPE = 5, LEGSTAGE = NA, LEGNO = NA,
    ALT = NA_real_, BEAUFORT = 2, VISIBLTY = 5, SPECCODE = NA, NUMBER = NA,
    DATE = as.Date("2024-06-01"), stringsAsFactors = FALSE
  )
  op <- options(narwcr.app_data = ship, narwcr.app_pam = NULL,
                narwcr.app_source = "test")
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                      na_action = "fail", months = 1:12,
                      criteria = c("beaufort", "alt", "vis"), legtypes = "2")
    output$type_control
    session$setInputs(types = "vessel")

    # LEGTYPE 5 is not a census line, so nothing is on effort however the
    # other criteria are set.
    expect_equal(sum(flagged()$OnOff.Effort), 0L)

    # Counting shipboard legs is not enough on its own: ALT is NA and a
    # missing value fails a criterion, so the altitude ceiling still bites.
    session$setInputs(legtypes = c("2", "5"))
    expect_equal(sum(flagged()$OnOff.Effort), 0L)

    # Both halves together are what puts the survey on effort.
    session$setInputs(criteria = c("beaufort", "vis"))
    expect_equal(sum(flagged()$OnOff.Effort), nrow(ship))
    expect_gt(nrow(effort_track()), 0)
  })
})


test_that("coverage is a mode, and it draws the track effort leaves out", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  op <- options(narwcr.app_data = NULL, narwcr.app_pam = NULL,
                narwcr.app_source = NULL)
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                      na_action = "fail", months = 1:12, types = "aerial",
                      criteria = c("beaufort", "alt", "vis"), legtypes = "2",
                      track_mode = "effort")
    output$type_control
    expect_true(show_effort())
    expect_false(show_ferry())
    # The example carries off-effort positions, and on "effort" they are the
    # ones not drawn.
    expect_gt(nrow(ferry_track()), 0)

    session$setInputs(track_mode = "all")
    expect_true(show_effort())
    expect_true(show_ferry())

    session$setInputs(track_mode = "none")
    expect_false(show_effort())
    expect_equal(effort_types(), character(0))
  })
})

test_that("the LEGTYPE table names the values the file carries", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  app_dir <- system.file("shiny", package = "narwcr")
  skip_if(!nzchar(app_dir), "app not installed")

  # One value the handbook defines and one it does not, which is the case
  # that sends records to the speed fallback.
  dat <- data.frame(
    FILEID = "F", EVENTNO = 1:6, YEAR = 2024L, MONTH = 4L, DAY = 1L,
    TIME = 120000 + (1:6) * 100, LATITUDE = 43, LONGITUDE = -69,
    LEGTYPE = c(2, 2, 2, 8, 8, NA), LEGSTAGE = NA, LEGNO = NA,
    DATE = as.Date("2024-04-01"), stringsAsFactors = FALSE
  )
  op <- options(narwcr.app_data = dat, narwcr.app_pam = NULL,
                narwcr.app_source = "test")
  on.exit(options(op), add = TRUE)

  shiny::testServer(app_dir, {
    session$setInputs(max_beaufort = 3, max_alt = 366, min_vis = 2,
                      na_action = "fail", months = 1:12, legtypes = "2",
                      criteria = c("beaufort", "alt", "vis"))
    # `renderTable` hands back the HTML as a string, where `renderUI` hands
    # back a list carrying one.
    out <- output$legtype_table
    expect_match(out, "not recognised", fixed = TRUE)
    expect_match(out, "not recorded", fixed = TRUE)
    # The handbook's own words for the value it does define.
    expect_match(out, "survey line", fixed = TRUE)
  })
})
