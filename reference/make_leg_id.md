# Identify separate occupations of a survey line

Builds `LEGNO3`, an identifier that distinguishes each continuous
occupation of a survey line from any later re-occupation of the same
line on the same day.

## Usage

``` r
make_leg_id(dat, sort = TRUE, quiet = FALSE)
```

## Arguments

- dat:

  A data frame with a `LEGNO` column, in survey order.

- sort:

  Sort by `DATE`, `FILEID`, and `EVENTNO` first? Default `TRUE`.
  Run-length identification is meaningless on unsorted records.

- quiet:

  Suppress the note naming how many occupations had no `LEGNO` to be
  identified by. Default `FALSE`.

## Value

`dat` with `LEGNO2` (a character copy of `LEGNO`) and `LEGNO3` added.

## Details

A `LEGNO` can be started, abandoned — for fog, say — and picked up again
hours later. Treating both stretches as one line would accumulate a
spurious point-to-point distance across the gap, joining the end of the
first stretch to the start of the second. `LEGNO3` pastes `LEGNO`
together with a run-length index so the two stretches stay separate.

## How an occupation is found

Three signals, taken in that order and judged per survey day, because
one part of a file may record line numbers where another records only
begin-line events.

- A begin-line record:

  `LEGSTAGE == 1` always opens an occupation. Without this a line flown
  twice under one number is silently a single occupation, since nothing
  about `LEGNO` changes between the two.

- A change of `LEGNO`:

  Opens an occupation, as it always has.

- A run of census track:

  Only where the day records neither of the above. This is inference
  rather than a reading of what was recorded, so those occupations are
  named `derived_<n>` and reported.

`LEGNO3` records which of the three an occupation came from, because
they are not equally trustworthy: `4_12` was named by its line number,
`line_12` has a begin-line record but no number to name it with, and
`derived_12` was inferred from census track alone. Counting the records
under each is the way to see how much of a dataset's line structure was
read and how much was guessed:

    table(sub("_[0-9]+$", "", dat$LEGNO3))

An occupation never spans two days: `DATE` and `FILEID` bound it, and it
closes at its end-line record (`LEGSTAGE == 5`). Records that are not
part of any line keep `LEGNO3` of `NA`: transit out to the survey area
before the first line, and the ferry between one line ending and the
next beginning.

Closing at the end-line matters more than it sounds. Those records are
off effort either way, so effort totals do not change — but they are
still *positions*, and a segment midpoint computed from them lands out
on the ferry rather than on the track. On one real extract 24% of all
records sat after an end-line inside an occupation, almost all of it
transit and cross-leg.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, section 8.A.19. NARWC
Reference Document 2023-01.

## Examples

``` r
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
dat <- make_leg_id(read_narwc(path))
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
unique(dat$LEGNO3)
#> [1] NA    "1_2" "2_3" "3_4" "4_5" "5_6" "4_7"
```
