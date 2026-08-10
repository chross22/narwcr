#' NARWC STRIP right-angle distance code books
#'
#' `STRIP` encodes the right-angle distance of a sighting from the track-line as
#' an *interval*, not a point. Two different code books are in use, and which
#' applies depends on the survey programme, the date, and the aircraft.
#'
#' @section The two schemes:
#' \describe{
#'   \item{`"cetap"`}{The original scheme (handbook 8.A.31). Codes `1,2` cover
#'     0-1/4 nmi; that closest interval was subsequently split at 1/8 nmi, giving
#'     `3,4` and `5,6`. Both forms occur in the archive, so `1,2` is kept as the
#'     wider unsplit bin rather than being silently merged. Intervals beyond
#'     1 nmi differ by aircraft: the AT-11 has a single open bin above 1 nmi,
#'     the Skymaster splits at 2 nmi.}
#'   \item{`"nlpsc"`}{Defined for the NLPSC / Massachusetts CEC surveys that
#'     began in October 2011, flown with a Skymaster. Different breakpoints
#'     entirely, running out to 4 nmi.}
#' }
#'
#' Odd codes are the left (port) side of the track, even codes the right
#' (starboard). Code `0` means directly on the track-line and applies only to
#' the AT-11, because of the Skymaster's restricted downward visibility.
#'
#' @section Open-ended bins:
#' The top bin of every scheme is open (`>1`, `>2`, `>4` nmi), so `distend` is
#' `Inf`. This function reports the code book as the handbook defines it and
#' does not truncate. A detection function cannot be fitted to an unbounded bin,
#' so an analysis that fits one has to choose a truncation distance — and that
#' choice belongs to the analysis rather than to the code book.
#'
#' @param scheme `"cetap"` or `"nlpsc"`.
#' @param platform `"skymaster"` or `"at-11"`. Only affects the `"cetap"`
#'   scheme, where the bins above 1 nmi differ by aircraft.
#' @param units `"m"` (default), `"km"`, or `"nmi"`.
#'
#' @return A tibble with `code`, `side`, `distbegin`, `distend`.
#'
#' @references
#' Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database: A
#' Guide for Users and Contributors, Version 8*, section 8.A.31. NARWC Reference
#' Document 2023-01.
#'
#' Kenney, R.D. and Scott, G.P. (1981) Calibration of the Beechcraft AT-11
#' forward observation bubble for population estimation purposes. In CETAP,
#' *A Characterization of Marine Mammals and Turtles in the Mid- and
#' North-Atlantic Areas of the U.S. Outer Continental Shelf, Annual Report for
#' 1979.* Bureau of Land Management, Washington, DC.
#'
#' @examples
#' narwc_strip_bins("nlpsc")
#' narwc_strip_bins("cetap", platform = "at-11", units = "nmi")
#'
#' @export
narwc_strip_bins <- function(scheme = c("cetap", "nlpsc"),
                             platform = c("skymaster", "at-11"),
                             units = c("m", "km", "nmi")) {
  scheme <- match.arg(scheme)
  platform <- match.arg(platform)
  units <- match.arg(units)

  # Handbook 8.A.31, in nautical miles.
  if (scheme == "cetap") {
    pairs <- list(
      c(0, 0, 0),          # on the track-line; AT-11 only
      c(1, 0, 1 / 4),      # the original unsplit closest interval
      c(3, 0, 1 / 8),      # ... subsequently split here
      c(5, 1 / 8, 1 / 4),
      c(7, 1 / 4, 1 / 2),
      c(9, 1 / 2, 3 / 4),
      c(11, 3 / 4, 1)
    )
    pairs <- c(pairs, if (platform == "at-11") {
      list(c(13, 1, Inf))
    } else {
      list(c(13, 1, 2), c(15, 2, Inf))
    })
  } else {
    pairs <- list(
      c(1, 0, 1 / 8),
      c(3, 1 / 8, 1 / 4),
      c(5, 1 / 4, 1 / 2),
      c(7, 1 / 2, 1),
      c(9, 1, 2),
      c(11, 2, 4),
      c(13, 4, Inf)
    )
  }

  out <- do.call(rbind, lapply(pairs, function(p) {
    odd <- p[1]
    if (odd == 0) {
      return(data.frame(code = 0, side = "on-track", distbegin = 0, distend = 0))
    }
    data.frame(
      code = c(odd, odd + 1),
      side = c("left", "right"),
      distbegin = p[2], distend = p[3]
    )
  }))

  scale <- switch(units, nmi = 1, m = 1852, km = 1.852)
  out$distbegin <- out$distbegin * scale
  out$distend <- out$distend * scale

  tibble::as_tibble(out[order(out$code), ])
}
