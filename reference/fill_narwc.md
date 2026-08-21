# Carry survey state forward into blank rows

NARWC data often records a value once and leaves it blank until it
changes — `LEGTYPE` is entered as `2` at the start of a census line and
the rows beneath it are empty until the leg type changes. This fills
those blanks, **within a survey day and file**, so that downstream code
sees the state each record was actually flown under.

## Usage

``` r
fill_narwc(
  dat,
  columns = narwc_fill_columns(),
  by = NULL,
  direction = c("downup", "down", "up"),
  quiet = FALSE
)
```

## Arguments

- dat:

  A data frame of NARWC survey data, in survey order.

- columns:

  Columns to fill. Defaults to
  [`narwc_fill_columns()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md).
  Anything in
  [`narwc_never_fill()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md)
  is an error rather than a silent omission.

- by:

  Grouping columns. `NULL` (default) uses `FILEID` and `DATE` where
  present.

- direction:

  `"downup"` (default), `"down"`, or `"up"`, as
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html).

- quiet:

  Suppress the report of what was filled. Default `FALSE`.

## Value

`dat` with the requested columns filled.

## Grouping is the whole point

An ungrouped fill runs the length of the file. The sea state from the
last record of one survey day carries into the first records of the
next; a leg number carries across a `FILEID` boundary into a different
survey entirely. Neither is recoverable afterwards, because the filled
value is indistinguishable from a recorded one.

The scripts this package was rewritten from filled these columns with no
grouping at all (`DataExploration.R:52`, `:71`). `by` defaults to
`FILEID` and `DATE`, and if neither is present this function warns
rather than quietly filling across everything.

## Direction, and what "up" actually does

[`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html)
semantics. `"down"` is the direction the recording convention justifies:
a blank means "as above".

`"downup"` fills down first and then fills up, so the only values it
fills backwards are those *before the first recorded value in a group*.
That is a smaller claim than it sounds, but it is still a guess: the
state before anything was logged is genuinely unknown, and back-filling
asserts it matched whatever was recorded first. Where a day's records
begin on transit before the first `LEGTYPE` is entered, back-filling a
`2` would mark that transit as census effort.

Because those are the inferred values rather than the recovered ones,
the report counts them separately. A large backward count is worth
looking at.

## A caution on `LEGSTAGE`

Filling `LEGSTAGE` down is the least safe of the defaults. If a file
records `1` (begin line) and leaves the continuation rows blank, filling
down marks every record of that line as "begin line" rather than `2`
(continue) — and since on-effort eligibility is `LEGSTAGE == 2`, the
whole line would drop out of every distance calculation. Whether that
happens depends on the recording convention of the file in hand. Check
the `LEGSTAGE` counts in the report against what you expect before
trusting a filled column.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*. NARWC Reference Document
2023-01.

## See also

[`narwc_fill_columns()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md),
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md),
[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)

## Examples

``` r
dat <- data.frame(
  FILEID = "A", DATE = as.Date("2024-04-01"), EVENTNO = 1:5,
  LEGTYPE = c(2, NA, NA, 1, NA), BEAUFORT = c(3, NA, NA, NA, NA)
)
fill_narwc(dat)
#> `fill_narwc()` filled 7 values, grouped by FILEID, DATE.
#>   carried forward:  7
#>   BEAUFORT 4, LEGTYPE 3
#> # A tibble: 5 × 5
#>   FILEID DATE       EVENTNO LEGTYPE BEAUFORT
#>   <chr>  <date>       <int>   <dbl>    <dbl>
#> 1 A      2024-04-01       1       2        3
#> 2 A      2024-04-01       2       2        3
#> 3 A      2024-04-01       3       2        3
#> 4 A      2024-04-01       4       1        3
#> 5 A      2024-04-01       5       1        3

# Refuses to replicate a sighting
try(fill_narwc(dat, columns = "NUMBER"))
#> Error in fill_narwc(dat, columns = "NUMBER") : 
#>   These columns must not be carried forward: `NUMBER`.
#> They are per-record measurements, not survey state. Filling a sighting column replicates one detection onto every row beneath it; filling a position or time fabricates a measurement. See `?narwc_fill_columns`.
```
