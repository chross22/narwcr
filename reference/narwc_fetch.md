# Fetch a survey file from cloud storage

Resolves a reference to a local file path, downloading it first when it
lives in Google Drive or OneDrive. Returns the path, so it composes with
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md).

## Usage

``` r
narwc_fetch(x, dest = NULL, drive = NULL, overwrite = FALSE, ...)
```

## Arguments

- x:

  A local path, a Google Drive file id or URL, or — with `drive`
  supplied — a path within that OneDrive or SharePoint drive. A local
  path that exists is returned unchanged, so wrapping a path in this is
  safe.

- dest:

  Where to write the download. `NULL` (default) uses a temporary file,
  which is cleaned up when the session ends.

- drive:

  An authenticated `Microsoft365R` drive object, from
  `get_business_onedrive()`, `get_personal_onedrive()`, or a SharePoint
  site's `get_drive()`. When given, `x` is a path within it.

- overwrite:

  Overwrite `dest` if it exists. Default `FALSE`.

- ...:

  Passed to
  [`googledrive::drive_download()`](https://googledrive.tidyverse.org/reference/drive_download.html),
  which is where `type` goes when exporting a Google Sheet as CSV.

## Value

The local path, invisibly for a download and unchanged for a path that
was already local.

## Authentication is yours, not this package's

`narwcr` never handles credentials. You authenticate in your own session
—
[`googledrive::drive_auth()`](https://googledrive.tidyverse.org/reference/drive_auth.html),
or
[`Microsoft365R::get_business_onedrive()`](https://rdrr.io/pkg/Microsoft365R/man/client.html)
— and hand the result over. Nothing here stores, caches, or transmits a
token, and no argument accepts one.

That is also why OneDrive takes a `drive` object rather than a URL: a
SharePoint or OneDrive link cannot be resolved without knowing which
tenant and account it belongs to, and working that out from a URL would
mean guessing at someone's organisation.

## The simplest route is usually a synced folder

Both clients sync to a local directory, and a file inside one is an
ordinary path that
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
already reads.
[`narwc_cloud_roots()`](https://camilleross.org/narwcr/reference/narwc_cloud_roots.md)
finds those directories. Reach for this function when the file is not
synced — because it is large, or shared with you but not added to your
drive.

## See also

[`narwc_cloud_roots()`](https://camilleross.org/narwcr/reference/narwc_cloud_roots.md),
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)

## Examples

``` r
# A path that already exists comes straight back, so this is safe to wrap
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
identical(narwc_fetch(path), path)
#> [1] TRUE

if (FALSE) { # \dontrun{
# Google Drive: authenticate yourself first
googledrive::drive_auth()
dat <- read_narwc(narwc_fetch("https://drive.google.com/file/d/1AbC.../view"))

# A Google Sheet has to be exported to something tabular
narwc_fetch("1AbC...", type = "csv")

# OneDrive or SharePoint: hand over an authenticated drive
od <- Microsoft365R::get_business_onedrive()
dat <- read_narwc(narwc_fetch("Surveys/2024/extract.csv", drive = od))

site <- Microsoft365R::get_sharepoint_site("https://x.sharepoint.com/sites/y")
dat <- read_narwc(narwc_fetch("extract.csv", drive = site$get_drive()))
} # }
```
