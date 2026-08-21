# Speed implied by consecutive position fixes

The distance from each record to the next, divided by the time between
them. Computed from what the receiver logged rather than from any speed
column, so it is available on any file carrying positions and a clock.

## Usage

``` r
track_speed(dat, by = NULL, max_gap = 300)
```

## Arguments

- dat:

  A data frame with `LATITUDE`, `LONGITUDE` and `TIME`, in survey order.

- by:

  Columns identifying a stretch to compute within, so speed is never
  taken across a break. `NULL` (default) uses `LEGNO3` when present,
  else `DATE` and `FILEID`.

- max_gap:

  Ignore intervals longer than this many seconds. Default `300`. A long
  gap is a break in the record rather than slow travel, and including it
  drags the speed down.

## Value

A numeric vector of knots, one per row: the speed from that record to
the next. `NA` at the end of each stretch and wherever the interval is
unusable.

## Why this exists

Nothing in a NARWC file reliably says what the platform was. `PLATFORM`
is optional, has no code book, and is empty on real extracts; `LEGTYPE`
codes 5 and 6 mark shipboard records but a mixed file may use the aerial
codes throughout. Speed cannot be mistaken for something else: a survey
aircraft flies at 90-120 knots and a vessel surveys at about 10, and no
arrangement of the other columns makes one look like the other.

## See also

[`classify_platform()`](https://camilleross.org/narwcr/reference/classify_platform.md),
which turns this into a platform label.

## Examples

``` r
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
dat <- make_leg_id(read_narwc(path, quiet = TRUE), quiet = TRUE)
summary(track_speed(dat))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>    0.00   86.46   86.46   81.50   86.46  106.74      19 
```
