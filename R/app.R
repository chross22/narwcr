#' Look at a NARWC extract on a map
#'
#' Opens an interactive map of a survey extract: where the platform went, what
#' it saw, and when. Sightings, effort and acoustic stations are drawn together
#' over a chosen time period, and the record behind any point is one click away.
#'
#' @section What it draws:
#' * **Sightings** — every record naming a species, coloured by species or by
#'   data type, sized by group size, with a popup giving the record and the
#'   handbook's meaning for each of its codes rather than the bare number.
#' * **Effort** — the track, split into the part that met [flag_effort()]'s
#'   criteria and the part that did not. The criteria are controls: moving the
#'   Beaufort cutoff and watching the on-effort track shrink is the quickest
#'   way to see what a threshold costs.
#' * **Depth contours** — isobaths at chosen depths, cut from a bathymetry
#'   grid. Off until asked for, because the first draw fetches one.
#' * **Acoustic stations** — a fixed recorder with a detection rate, from a
#'   `pam` table. Recording effort is the denominator, because a station that
#'   heard whales on four of five days it listened is not the station that
#'   heard them on four of two hundred.
#'
#' @section Depth contours:
#' Isobaths are cut from an ETOPO grid rather than read off a basemap tile, so
#' a line on the map is a depth that can be named — this is the 100 m contour,
#' that is the 200 m — where a tinted tile is only a picture of one. Which
#' depths are drawn is a control, because the isobath a distribution is read
#' against is the reader's question and not this app's.
#'
#' The grid is fetched by `marmap::getNOAA.bathy()` the first time contours are
#' asked for, which is why they start switched off: an app that reaches a
#' server the moment it opens is not what the rest of this package does. It is
#' cached on disk afterwards, under \code{tools::R_user_dir("narwcr", "cache")}
#' by default and following \pkg{marmap}'s own filename convention — so a grid
#' already downloaded by anything else in this stack is read rather than
#' fetched, and one downloaded here serves them back. Point the cache wherever
#' the rest of the stack keeps its own:
#'
#' ```r
#' options(narwcr.cache = tools::R_user_dir("datamatch", which = "cache"))
#' ```
#'
#' Once a grid is loaded, a sighting's popup also reports the depth under it,
#' from the nearest grid cell. Nothing is fetched for a popup's sake: the depth
#' appears when the contours do, and is absent rather than guessed at until
#' then.
#'
#' @section Aerial, vessel, opportunistic, PAM:
#' The first three arrive in one extract and are told apart by `LEGTYPE`
#' (handbook 8.A.21): codes 0–4 are line-transect aerial, 5–6 shipboard, and
#' 7 and 9 aerial platforms of opportunity. Where `LEGTYPE` says nothing the
#' platform is inferred from how fast it was moving, via [classify_platform()],
#' and the app reports which records were read and which were inferred — those
#' are not the same claim.
#'
#' The handbook has no code for a *dedicated* shipboard survey, so every
#' shipboard record in the archive is a platform of opportunity. Where a
#' programme reads its own extract differently, the mapping is one option:
#'
#' ```r
#' options(narwcr.legtype_types = c("5" = "opportunistic", "6" = "opportunistic"))
#' ```
#'
#' `options(narwcr.species_labels = c(RIDO = "Risso's dolphin"))` extends the
#' species names the popups show, the same way. Neither ever replaces a code:
#' what was recorded is always on screen beside what it means.
#'
#' @section Preparation runs once, over everything:
#' [make_leg_id()] and [fill_legstage()] read a record's neighbours, so running
#' them on a table already filtered to one year would split every line crossing
#' the boundary and strip the state the first record of January inherited from
#' December. The app runs the whole pipeline on the whole table when it loads,
#' and the time controls filter what that produced.
#'
#' @param dat A data frame of NARWC survey data, ideally from [read_narwc()], or
#'   a path — or anything else [narwc_fetch()] resolves — to read one from.
#'   `NULL` (the default) opens the shipped example, and a file can be loaded
#'   from inside the app.
#' @param pam Acoustic detections: one row per station and day, carrying a
#'   station, a position, a date, a species, whether it was detected, and how
#'   long the recorder was listening. A data frame or a path to a CSV; column
#'   names are matched loosely and what matched is shown on the app's Source
#'   tab. `NULL` (the default) draws no acoustic layer.
#' @param ... Passed to `shiny::runApp()`, which is where `launch.browser`,
#'   `port` and `host` go.
#'
#' @return The value of `shiny::runApp()`, invisibly.
#'
#' @seealso [read_narwc()], [flag_effort()], [make_leg_id()]
#'
#' @examples
#' \dontrun{
#' # The shipped example
#' run_narwc_app()
#'
#' # A real extract, read first so the reading report is on the console
#' dat <- read_narwc("extract.csv")
#' run_narwc_app(dat)
#'
#' # With acoustic detections beside it
#' run_narwc_app(dat, pam = "detections.csv")
#' }
#'
#' @export
run_narwc_app <- function(dat = NULL, pam = NULL, ...) {
  # Required rather than optional. As Suggests these produced the worst failure
  # available: a control simply absent from the page, with nothing on screen to
  # say why, on any machine whose library happened not to have them.
  for (pkg in c("shiny", "leaflet", "DT")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The '", pkg, "' package is required to run the app. ",
           "Install it with install.packages('", pkg, "').", call. = FALSE)
    }
  }

  label <- "shipped example"
  if (is.character(dat)) {
    label <- dat
    dat <- read_narwc(narwc_fetch(dat))
  } else if (!is.null(dat)) {
    if (!is.data.frame(dat)) {
      rlang::abort("`dat` must be a data frame or a path to one.")
    }
    label <- paste0("supplied to run_narwc_app(): ", nrow(dat), " records")
  }

  if (is.character(pam)) {
    # Read inside the app, by the same reader the app's own file box uses, so
    # a file that fails reports the failure once and in one voice.
    pam_path <- narwc_fetch(pam)
    if (!file.exists(pam_path)) {
      rlang::abort(paste0("No acoustic file at: ", pam))
    }
    pam <- pam_path
  } else if (!is.null(pam) && !is.data.frame(pam)) {
    rlang::abort("`pam` must be a data frame or a path to a CSV.")
  }

  app_dir <- system.file("shiny", package = "narwcr")
  if (!nzchar(app_dir)) {
    stop("Could not locate the app directory in the installed package.",
         call. = FALSE)
  }

  old <- options(
    narwcr.app_data = dat,
    narwcr.app_source = label,
    narwcr.app_pam = pam
  )
  on.exit(options(old), add = TRUE)

  invisible(shiny::runApp(app_dir, ...))
}
