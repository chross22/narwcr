# Read NARWC-format survey data

Reads a NARWC sightings-database extract from a CSV file, or
standardises an already-loaded data frame, into the column names and
types the rest of the package expects.

## Usage

``` r
read_narwc(
  x,
  extra_columns = character(),
  profile = NULL,
  drop_missing_position = TRUE,
  prefer_source = TRUE,
  make_eventno = TRUE,
  assume_alt_m = NULL,
  assume_alt_ft = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  A path to a CSV file, or a data frame.

- extra_columns:

  Character vector of additional column names to keep beyond those in
  [`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md).
  Use `NULL` to keep every column in the input.

- profile:

  Survey-programme profile whose extra columns should be kept, for
  example `"ccs"`. `NULL` (default) keeps only the handbook columns. See
  [`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md).

- drop_missing_position:

  Drop records with no `LATITUDE` or `LONGITUDE`. Default `TRUE`; see
  below.

- prefer_source:

  Let a better-known source take precedence over a canonical column of
  the same name — `TrkLatitude` over a plain `LATITUDE`, and
  `LEGTYPE_BK` over a plain `LEGTYPE`. Default `TRUE`; see below.
  `FALSE` restores "the column already named `LATITUDE` always wins".

- make_eventno:

  Supply the missing values of an `EVENTNO` column that has some.
  Default `TRUE`; see below. `FALSE` leaves them `NA`. A file with no
  `EVENTNO` column at all is left alone either way — that is a missing
  required variable for
  [`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
  to report.

- assume_alt_m:

  The survey altitude in **metres**, used where a record has none.
  `NULL` (default) leaves a missing `ALT` missing. Records given this
  value are marked in an `ALT_ASSUMED` column and the fill is reported.

  Metres, not feet: `ALT` is metres throughout (handbook 8.A.1), so a
  750-foot survey altitude is `assume_alt_m = 228.6`. Passing `750`
  would put every record above
  [`flag_effort()`](https://camilleross.org/narwcr/reference/flag_effort.md)'s
  366 m ceiling and take them all off effort — the opposite of what
  filling an altitude is usually for. Use `assume_alt_ft` and avoid the
  question.

  This is a stated altitude, not a measured one. `ALT` feeds
  `perp_distance()`, so every right-angle distance computed from a
  filled record inherits whatever you state here — which is why it is
  off by default, and why `ALT_ASSUMED` exists to find those records
  afterwards.

- assume_alt_ft:

  The same, stated in feet — `assume_alt_ft = 750` for a 750-foot survey
  — and converted to metres for you. Survey teams state altitude in feet
  and the package stores metres, so this is the safer of the two. Giving
  both is an error.

- quiet:

  Suppress the messages naming matched columns, dropped columns, dropped
  records, and unit conversions. Default `FALSE`.

- ...:

  Passed to
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) when
  `x` is a path.

## Value

A tibble with the recognised NARWC columns, standardised names and
types, and a derived `DATE` column. Carries the class `"narwc_data"` so
downstream functions can tell standardised input from a raw data frame.

## Details

It resolves column names onto the handbook's, coerces the numeric NARWC
variables, turns the database's missing-value placeholders (`"."`, `""`)
into `NA`, and drops records with no position. Beyond that it does not
filter, repair, or reject anything — use
[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
to find problems and
[`flag_effort()`](https://camilleross.org/narwcr/reference/flag_effort.md)
to decide what counts as effort.

A `DATE` column of class `Date` is derived from `YEAR`, `MONTH`, and
`DAY` when all three are present.

## Column names do not have to match exactly

Real extracts spell things their own way. Matching ignores case and
separators, so `Event`, `event_no`, and `EventNo` all reach `EVENTNO`
without anyone editing a spreadsheet first, and the alias table in
[`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md)`$aliases`
covers the rest.

**This is not fuzzy matching.** Nothing is guessed by edit distance —
`EVENTN0` with a zero stays `EVENTN0` — and nothing is ever renamed onto
a canonical column that is already present, so a correctly named column
always wins. Matches that took an inference are reported; exact entries
in the alias table are not, since announcing `LAT_DD` on every read
would bury the ones worth a second look.

## Where `TIME` comes from

Programmes record the clock they record. `TIME` is taken from the first
of `TIME`, then a UTC column (`TIME_UTC`, `GMT`, `TIME_GMT`), then a
local one (`TIME_LOC`, `TIME_LOCAL`) — GMT and UTC being the same clock.
A file carrying both zones lands on UTC. If yours is consistent it does
not much matter which; if it is not, decide before segmenting, because
effort is accumulated in record order.

## Records with no position

Dropped by default, and reported. A record with no `LATITUDE` or
`LONGITUDE` contributes no effort and cannot place a sighting — but left
in, it does not announce itself: a great-circle distance from a missing
position is `NA`, and the usual way of accumulating effort turns that
`NA` into a zero. Losing the record visibly is better than counting it
as zero distance flown. `drop_missing_position = FALSE` keeps them.

## GPS track columns

A `Trk*` column is the platform's own GPS track log. Where a file
carries both `TrkLatitude` and a plain `LATITUDE`, they are not two
spellings of one thing: the track log is what the receiver recorded, and
the plain column is the position entered for the platform, which on a
file covering both a vessel and an aircraft is a different place. The
track log wins, and the displaced column is kept as `LATITUDE_ORIGINAL`
rather than dropped, with a warning naming both. This is the only case
where a column already carrying a canonical name does not win;
`prefer_source = FALSE` turns it off.

The same applies to the clock: `TrkTime_UTC` is taken ahead of any other
UTC spelling, and displaces a plain `TIME`. `TrkTime_Local` does not —
it is preferred only among the local spellings, because moving the whole
dataset onto another zone to gain the receiver's seconds is not a trade
this makes unasked. UTC still comes before local either way.

`LEGTYPE_BK` displaces a plain `LEGTYPE` by the same rule. That one is a
MEMDR-era data quirk rather than anything to do with a GPS, but it is
the same shape of problem: where a file carries both, the `_BK` column
is the leg type to believe.

With none of these columns present nothing changes — `LATITUDE`,
`LONGITUDE` and `LEGTYPE` are used exactly as they are.

## A missing EVENTNO

Supplied rather than left as `NA`, because
[`make_leg_id()`](https://camilleross.org/narwcr/reference/make_leg_id.md)
sorts by `DATE`, `FILEID` and `EVENTNO`, and
[`order()`](https://rdrr.io/r/base/order.html) puts `NA` last — so a
missing event number moves records out of survey order before the
run-length logic that builds `LEGNO3` sees them. That does not error; it
produces the wrong lines.

The fill follows handbook 8.A.10. `EVENTNO` is a sequentially assigned
record number that must increase within a file; skipped numbers are
allowed and duplicates are not, with one exception — several sightings
may share an event number, and then *all of the non-sighting variables
must be identical across all records*. So an event here is a run of
consecutive records agreeing on every non-sighting variable, a number
already recorded anywhere in that run covers the whole run, and only an
event with no number at all is given one.

Where the recorded numbers leave no room for an event being inserted,
the numbers from that point forward are increased. That is the
handbook's own remedy — "it is usually necessary to correct the event
numbers from that point forward in the file" — and it warns, because
records elsewhere referring to the old numbers past that point will no
longer match.

Only values are supplied, never the column itself. A file with no
`EVENTNO` column is missing a required variable, and inventing one would
hide that.

## Units

`ALT` is metres throughout (handbook 8.A.1), and it feeds the
right-angle distances in `distsamp`. A column whose *name* declares feet
— `TrkAltitude_ft`, `ALTFT`, `ALTITUDEFT` — is multiplied by `0.3048` on
the way in, and the multiplier is recorded in the `factor` column of
[`narwc_column_mapping()`](https://camilleross.org/narwcr/reference/narwc_column_mapping.md).
A file carrying both `TrkAltitude_m` and `TrkAltitude_ft` uses the
metres one and converts nothing — unless the metres column is empty, in
which case the feet one is used and converted. Precedence is written
over spellings and says nothing about which column a file actually
filled in, so a column with no values in it never outranks one that has
them. The same applies to the GPS track columns: an empty `TrkLatitude`
displaces nothing.

## Columns that are not in the handbook

Survey programmes add their own derived columns, and a processed "ready
for model" file may carry a dozen. They are not handbook Table 1
variables, so by default they are **dropped** — and this function says
so rather than dropping them silently, naming what went and pointing at
[`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md)
when they match a known survey programme.

Three ways to keep them:

- `profile = "ccs"`:

  Keeps the columns that programme is known to add. See
  [`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md)
  for what is registered.

- `extra_columns = c(...)`:

  Keeps exactly what you name. Glob patterns work, so `"Trk*"` keeps a
  family whose exact names differ between extracts.

- `extra_columns = NULL`:

  Keeps every column in the input.

Naming a profile keeps its columns; it does not interpret them. A column
name is not a contract between programmes — `Tr_SIGHTING` means one
thing in a CCS file and nothing in particular anywhere else — so this
function will tell you what a file looks like and leave the decision to
you.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*. NARWC Reference Document
2023-01.

## See also

[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
to check the result against the handbook,
[`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md)
for the recognised columns,
[`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md)
for the columns individual survey programmes add.

## Examples

``` r
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
dat <- read_narwc(path)
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
head(dat[, c("FILEID", "EVENTNO", "LEGTYPE", "LEGSTAGE", "SPECCODE")])
#> # A tibble: 6 × 5
#>   FILEID   EVENTNO LEGTYPE LEGSTAGE SPECCODE
#>   <chr>      <dbl>   <dbl>    <dbl> <chr>   
#> 1 AA240401       1       1       NA NA      
#> 2 AA240401       2       1       NA NA      
#> 3 AA240401       3       1       NA HUWH    
#> 4 AA240401       4       2        1 NA      
#> 5 AA240401       5       2        2 NA      
#> 6 AA240401       6       2        2 NA      

# Keep a survey programme's own columns
narwc_profiles("ccs")$column
#> [1] "IS_LAT"      "IS_LONG"     "IS_SPECCODE" "Tr_SIGHTING" "OBSSIGHT"   
#> [6] "Effort_Type" "Date_UTC"    "Time_UTC"   
```
