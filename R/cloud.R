#' Where OneDrive and Google Drive sync to
#'
#' The conventional sync-root locations for this platform, and whether each is
#' present. Most cloud-stored survey data is reachable as an ordinary file path,
#' and this is how you find it.
#'
#' @section Nothing is searched for:
#' Only the documented locations are checked — this does not scan your home
#' directory. If your sync root is somewhere else, pass its path directly; every
#' function here takes an ordinary path.
#'
#' On macOS both clients now sync under `~/Library/CloudStorage`, with a folder
#' per account (`OneDrive-UniversityofMaine`, `GoogleDrive-you@example.com`).
#' The older `~/OneDrive` and `~/Google Drive` locations are still checked
#' because existing installations keep them.
#'
#' @return A tibble with `service`, `path`, and `exists`.
#'
#' @seealso [narwc_fetch()], [read_narwc()]
#'
#' @examples
#' narwc_cloud_roots()
#'
#' # Only the ones actually present
#' subset(narwc_cloud_roots(), exists)
#'
#' @export
narwc_cloud_roots <- function() {
  home <- path.expand("~")
  cloud <- file.path(home, "Library", "CloudStorage")

  candidates <- list(
    c("OneDrive", file.path(home, "OneDrive")),
    c("Google Drive", file.path(home, "Google Drive")),
    c("Google Drive", file.path(home, "Google Drive", "My Drive"))
  )

  # macOS puts one folder per account under CloudStorage. Listing that single
  # directory is not a search: it is where the answer is documented to be.
  if (dir.exists(cloud)) {
    for (nm in list.files(cloud)) {
      service <- if (grepl("^OneDrive", nm)) {
        "OneDrive"
      } else if (grepl("^GoogleDrive", nm)) {
        "Google Drive"
      } else {
        nm
      }
      candidates <- c(candidates, list(c(service, file.path(cloud, nm))))
    }
  }

  # Windows puts the environment variable there for you.
  for (v in c("OneDrive", "OneDriveCommercial", "OneDriveConsumer")) {
    p <- Sys.getenv(v)
    if (nzchar(p)) candidates <- c(candidates, list(c("OneDrive", p)))
  }

  paths <- vapply(candidates, `[`, character(1), 2)
  keep <- !duplicated(paths)

  tibble::tibble(
    service = vapply(candidates, `[`, character(1), 1)[keep],
    path = paths[keep],
    exists = dir.exists(paths[keep])
  )
}


#' Fetch a survey file from cloud storage
#'
#' Resolves a reference to a local file path, downloading it first when it lives
#' in Google Drive or OneDrive. Returns the path, so it composes with
#' [read_narwc()].
#'
#' @section Authentication is yours, not this package's:
#' `narwcr` never handles credentials. You authenticate in your own session —
#' `googledrive::drive_auth()`, or `Microsoft365R::get_business_onedrive()` —
#' and hand the result over. Nothing here stores, caches, or transmits a token,
#' and no argument accepts one.
#'
#' That is also why OneDrive takes a `drive` object rather than a URL: a
#' SharePoint or OneDrive link cannot be resolved without knowing which tenant
#' and account it belongs to, and working that out from a URL would mean
#' guessing at someone's organisation.
#'
#' @section The simplest route is usually a synced folder:
#' Both clients sync to a local directory, and a file inside one is an ordinary
#' path that [read_narwc()] already reads. [narwc_cloud_roots()] finds those
#' directories. Reach for this function when the file is not synced — because
#' it is large, or shared with you but not added to your drive.
#'
#' @param x A local path, a Google Drive file id or URL, or — with `drive`
#'   supplied — a path within that OneDrive or SharePoint drive. A local file
#'   that exists is returned unchanged, so wrapping a path in this is safe; a
#'   directory is an error. A bare Google Drive id is recognised only when it
#'   is 28 to 64 characters of `[A-Za-z0-9_-]` and does not read as a file
#'   name (`survey_extract_2024_final_v2` does), so a mistyped local path is
#'   reported as not found rather than looked up on Google Drive.
#' @param dest Where to write the download. `NULL` (default) uses a temporary
#'   file, which is cleaned up when the session ends.
#' @param drive An authenticated `Microsoft365R` drive object, from
#'   `get_business_onedrive()`, `get_personal_onedrive()`, or a SharePoint
#'   site's `get_drive()`. When given, `x` is a path within it.
#' @param overwrite Overwrite `dest` if it exists. Default `FALSE`.
#' @param ... Passed to `googledrive::drive_download()`, which is where `type`
#'   goes when exporting a Google Sheet as CSV.
#'
#' @return The local path, invisibly for a download and unchanged for a path
#'   that was already local.
#'
#' @seealso [narwc_cloud_roots()], [read_narwc()]
#'
#' @examples
#' # A path that already exists comes straight back, so this is safe to wrap
#' path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
#' identical(narwc_fetch(path), path)
#'
#' \dontrun{
#' # Google Drive: authenticate yourself first
#' googledrive::drive_auth()
#' dat <- read_narwc(narwc_fetch("https://drive.google.com/file/d/1AbC.../view"))
#'
#' # A Google Sheet has to be exported to something tabular
#' narwc_fetch("1AbC...", type = "csv")
#'
#' # OneDrive or SharePoint: hand over an authenticated drive
#' od <- Microsoft365R::get_business_onedrive()
#' dat <- read_narwc(narwc_fetch("Surveys/2024/extract.csv", drive = od))
#'
#' site <- Microsoft365R::get_sharepoint_site("https://x.sharepoint.com/sites/y")
#' dat <- read_narwc(narwc_fetch("extract.csv", drive = site$get_drive()))
#' }
#'
#' @export
narwc_fetch <- function(x, dest = NULL, drive = NULL, overwrite = FALSE, ...) {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    rlang::abort("`x` must be a single non-empty string.")
  }

  # An ordinary path wins over everything, so wrapping a local file is a no-op
  # and a synced cloud folder needs no special handling at all.
  # A regular file only: `file.exists()` is TRUE for a directory too, which
  # was then handed back as if it could be read.
  if (is.null(drive) && file.exists(x)) {
    if (dir.exists(x)) {
      rlang::abort(paste0(
        "`x` is a directory, not a file: ", x, ". Pass the path of the file ",
        "inside it."
      ))
    }
    if (utils::file_test("-f", x)) return(x)
  }

  dest <- dest %||% tempfile(fileext = fetch_extension(x, list(...)$type))
  if (file.exists(dest) && !overwrite) {
    rlang::abort(paste0(
      "`dest` already exists: ", dest,
      ". Pass `overwrite = TRUE` to replace it."
    ))
  }

  if (!is.null(drive)) {
    return(fetch_onedrive(x, dest, drive, overwrite))
  }
  if (is_google_drive(x)) {
    return(fetch_google(x, dest, overwrite, ...))
  }

  rlang::abort(paste0(
    "Cannot work out where to get `", x, "`.\n",
    "It is not a path that exists, and not a Google Drive id or URL. For ",
    "OneDrive or SharePoint, pass an authenticated `drive` object - see ",
    "`?narwc_fetch`. For a synced folder, pass the local path; ",
    "`narwc_cloud_roots()` will help you find it."
  ))
}

# Google Drive ids are opaque strings, so recognise the URL forms and treat a
# bare id conservatively. Real ids are 28 to 44 characters drawn from
# [A-Za-z0-9_-] - "0B..." for older files, "1..." for newer ones - and never
# carry a file extension, which the character set already excludes. A string
# that splits on `_` or `-` into plain words, numbers and version tags
# ("survey_extract_2024_final_v2") is a file name that was not found, not an
# id: a random id essentially never has that shape, and calling it one sent a
# mistyped local path off to Google instead of saying it does not exist.
is_google_drive <- function(x) {
  if (grepl("drive\\.google\\.com|docs\\.google\\.com", x)) {
    return(TRUE)
  }
  if (!grepl("^[A-Za-z0-9_-]{28,64}$", x)) {
    return(FALSE)
  }
  word <- "([A-Za-z]+[0-9]*|[0-9]+)"
  !grepl(paste0("^", word, "([_-]", word, ")+$"), x)
}

fetch_google <- function(x, dest, overwrite, ...) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    rlang::abort(paste0(
      "Reading from Google Drive needs the `googledrive` package. Install it ",
      "with `install.packages(\"googledrive\")`, then authenticate with ",
      "`googledrive::drive_auth()`."
    ))
  }
  googledrive::drive_download(
    googledrive::as_id(x), path = dest, overwrite = overwrite, ...
  )
  invisible(dest)
}

fetch_onedrive <- function(x, dest, drive, overwrite) {
  if (!is.function(drive$download_file)) {
    rlang::abort(paste0(
      "`drive` does not look like a Microsoft365R drive: it has no ",
      "`download_file()` method. Pass the result of ",
      "`Microsoft365R::get_business_onedrive()`, `get_personal_onedrive()`, ",
      "or a SharePoint site's `get_drive()`."
    ))
  }
  drive$download_file(x, dest = dest, overwrite = overwrite)
  invisible(dest)
}

# Keep the extension so read.csv and friends see what they expect. A Google
# export says its format in `type` (passed through to `drive_download()`) or in
# the URL's `format=` / `exportFormat=` query, not in a file name, and a
# workbook saved as ".csv" is not a CSV.
fetch_extension <- function(x, type = NULL) {
  if (is.character(type) && length(type) == 1L &&
      grepl("^\\.?[A-Za-z0-9]{1,5}$", type)) {
    return(paste0(".", tolower(sub("^\\.", "", type))))
  }
  fmt <- regmatches(
    x, regexpr("[?&](export)?[Ff]ormat=[A-Za-z0-9]{1,5}", x)
  )
  if (length(fmt)) {
    return(paste0(".", tolower(sub(".*=", "", fmt[1]))))
  }
  ext <- regmatches(x, regexpr("\\.[A-Za-z0-9]{1,5}($|\\?)", x))
  if (!length(ext)) {
    return(".csv")
  }
  sub("\\?$", "", ext[1])
}
