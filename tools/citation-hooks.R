# Project-specific citation checks for narwcr.
#
# Everything here knows something about *this* project that a shared checker
# cannot. When the generic engine is lifted into a shared repo, this file stays
# behind.
#
# A hook is a function (registry, ctx) returning cc_result(failures, notes).

# The NARWC handbook version this package is written against. Bump only after
# diffing the new version's code books against R/narwc-codes.R.
cited_version <- 8L

#' Has the NARWC published a newer handbook?
#'
#' This is the check that matters most for narwcr and the one no generic
#' checker could provide: the package's correctness depends on the code books in
#' a specific version of a document that gets revised every year or two.
check_narwc_version <- function(registry, ctx) {
  page <- tryCatch(
    paste(readLines("https://www.narwc.org/sightings-database.html", warn = FALSE),
          collapse = " "),
    error = function(e) NULL
  )

  if (is.null(page)) {
    return(cc_result(notes = "Could not reach narwc.org to check the handbook version."))
  }

  # The site exposes the version two ways: in prose ("Version 8"), and in the
  # filename of the linked PDF ("narwc_users_guide__v8_.pdf"). The filename is
  # the reliable one - the prose often sits inside a script-rendered block.
  pats <- c("[Vv]ersion\\s*([0-9]+)", "users_guide[^\"']*?v([0-9]+)")
  nums <- integer(0)
  for (p in pats) {
    hits <- regmatches(page, gregexpr(p, page))[[1]]
    nums <- c(nums, suppressWarnings(as.integer(gsub("\\D", "", hits))))
  }
  nums <- nums[!is.na(nums) & nums > 0 & nums < 100]
  newest <- if (length(nums)) max(nums) else NA_integer_

  if (is.na(newest)) {
    return(cc_result(notes = paste0(
      "No version string found on the NARWC sightings-database page; the page ",
      "layout may have changed. Check by hand."
    )))
  }

  if (newest > cited_version) {
    return(cc_result(failures = paste0(
      "NARWC handbook Version ", newest, " is now available; the package cites ",
      "Version ", cited_version, ".\n",
      "      Review the code books in R/narwc-codes.R against the new version ",
      "before updating the citation - variable definitions and codes do change ",
      "between versions, and section numbers shift whenever a variable is added."
    )))
  }

  cc_say("  ok  handbook still at Version ", cited_version)
  cc_result()
}

#' Hooks for this project, in the order they should run.
citation_hooks <- function() {
  list("6. NARWC handbook version" = check_narwc_version)
}
