# Check survey data against the NARWC handbook

Runs a set of structural and code-book checks against a standardised
NARWC data frame and reports every problem found. Validation never stops
on error and never modifies the data: the result is a report you read,
so that you can decide which problems matter for your analysis.

## Usage

``` r
validate_narwc(dat, checks = narwc_checks())
```

## Arguments

- dat:

  A data frame of NARWC survey data, ideally from
  [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md).

- checks:

  A named list of check functions. Defaults to
  [`narwc_checks()`](https://camilleross.org/narwcr/reference/narwc_checks.md).

## Value

A tibble with one row per problem found, and columns:

- `check`:

  Name of the check, as listed above.

- `severity`:

  `"error"`, `"warning"`, or `"note"`.

- `column`:

  The column involved, or `NA`.

- `n`:

  Number of records affected.

- `rows`:

  List column of affected row indices (capped at 100).

- `message`:

  Human-readable description.

A zero-row tibble means every check passed.

## Checks performed

By default, the handbook-general set from
[`narwc_checks()`](https://camilleross.org/narwcr/reference/narwc_checks.md):

- `missing_required`:

  A column in
  [`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md)`$required`
  is absent. Severity `error`.

- `missing_values`:

  `NA` in a required column.

- `unknown_code`:

  A value of `LEGTYPE`, `LEGSTAGE`, `IDREL`, `TAXCODE`, or `STRATUM`
  that is not in the handbook's code book.

- `legstage_off_census`:

  `LEGSTAGE` recorded on a record that is not a census line. Handbook
  8.A.20: for dedicated aerial surveys `LEGSTAGE` is recorded only
  during census tracks (`LEGTYPE == 2`), except for code 7.

- `sighting_at_boundary`:

  A sighting recorded at a `LEGSTAGE` of 1, 3, 4, or 5. Handbook 8.A.20
  and 4.2: sightings should not occur at begin-line, break-off, resume,
  or end-line events.

- `legstage_sequence`:

  `LEGSTAGE` does not follow a logical order within a line occupation.
  Handbook 8.A.20: a line begins (1), continues (2), may break off to
  circle (3) and resume (4), and ends (5). A line must begin with 1,
  nothing may follow an end-line, and a resume cannot appear without a
  break-off before it.

- `legstage_break_off_unresumed`:

  A line ends at a break-off (3) with no resume and no end-line: the
  aircraft left the census line and the record never brings it back.

- `legstage_line_not_closed`:

  A line has no end-line (5). A note rather than a warning, because a
  line abandoned for weather or re-flown later legitimately has none.

- `eventno_not_increasing`:

  `EVENTNO` does not increase through a `FILEID`. Repeated values are
  allowed — the handbook (4.2) assigns one event several sightings — but
  decreases indicate mis-sorted records.

- `altitude_looks_like_feet`:

  The median `ALT` is implausible read as metres for a survey aircraft
  and ordinary read as feet. Handbook 8.A.1 says `ALT` is metres, but
  also that nearly all submissions arrive in feet and that the
  conversion on import can be switched off — so this is an expected
  state of the data, and it cannot be detected from a column name. Left
  uncorrected, every record above the altitude ceiling drops out of
  effort silently, and `perp_distance()` returns distances 3.28 times
  too large.

- `sightno_without_species`:

  `SIGHTNO` is set on records with no `SPECCODE`. Handbook 8.A.27:
  data-logging programs number every forced record — line starts,
  weather and altitude changes — not only sightings, and those numbers
  are meant to be cleared during processing. A file that still carries
  them will overcount detections.

- `sightno_duplicated`:

  `SIGHTNO` is repeated within a `FILEID`, which handbook 8.A.27 does
  not allow and calls a recurring problem in submitted datasets. `999`
  is excluded, being deliberate. Anything keyed on `FILEID` and
  `SIGHTNO` will match the wrong record.

- `sightno_non_target`:

  `SIGHTNO` is `999`, the CETAP marker for non-target species — seals,
  sharks, sunfish — recorded so they could be removed before analysis
  (handbook 8.A.27). A note, not a warning: the file is correct, and
  duplicates of it are expected. The analysis has to exclude them.

- `bad_time_format`:

  `TIME` is not a 6-digit `hhmmss` in 24-hour form (handbook 8.A.37).
  Four-digit `hhmm` times are reported separately as a warning since
  they are still found in older data.

- `coordinates_out_of_range`:

  Latitude outside \[-90, 90\] or longitude outside \[-180, 180\].

- `positive_west_longitude`:

  Every longitude is positive. Handbook 8.A.22 requires west longitudes
  to be negative; all-positive longitudes in a western North Atlantic
  dataset mean the sign convention was lost.

- `sighting_without_number`:

  `SPECCODE` present but `NUMBER` missing. Handbook 8.A.24 requires
  `NUMBER` for all sightings.

- `exact_position_out_of_range`:

  `S_LAT` or `S_LONG` outside the range a coordinate can take. Handbook
  8.A.33 and 8.A.34 give the exact sighting position in decimal degrees.

- `columns_outside_handbook`:

  Columns present that are not NARWC handbook variables. Survey
  programmes add their own; they are carried through uninterpreted, and
  one that encodes position, effort, or distance must be mapped
  explicitly. See
  [`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md).

Checks specific to a particular analysis live with that analysis; see
[`narwc_checks()`](https://camilleross.org/narwcr/reference/narwc_checks.md)
for how to add them.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*. NARWC Reference Document
2023-01. University of Rhode Island, Graduate School of Oceanography.
Every check above cites the section it derives from.

Kenney, R.D. (2002) *Quality-control Issues for Data Submissions to the
North Atlantic Right Whale Consortium Database.* NARWC Reference
Document 2002-02.

## See also

[`narwc_checks()`](https://camilleross.org/narwcr/reference/narwc_checks.md)
for the default set,
[`narwc_finding()`](https://camilleross.org/narwcr/reference/narwc_finding.md)
for writing your own.

## Examples

``` r
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
issues <- validate_narwc(read_narwc(path))
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
issues[, c("check", "severity", "n")]
#> # A tibble: 1 × 3
#>   check                    severity     n
#>   <chr>                    <chr>    <int>
#> 1 legstage_line_not_closed note         1

# Run only the code-book checks
validate_narwc(read_narwc(path), checks = narwc_checks()["code_books"])
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
#> # A tibble: 0 × 6
#> # ℹ 6 variables: check <chr>, severity <chr>, column <chr>, n <int>,
#> #   rows <list>, message <chr>
```
