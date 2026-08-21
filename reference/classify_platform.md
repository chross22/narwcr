# Label each record's platform from how fast it was moving

Classifies every record as `"aerial"`, `"vessel"` or `"stationary"`
using the median
[`track_speed()`](https://camilleross.org/narwcr/reference/track_speed.md)
of the stretch it belongs to. A whole stretch takes one label, because a
platform does not change mid-line.

## Usage

``` r
classify_platform(
  dat,
  by = NULL,
  aerial_min = 40,
  stationary_max = 2,
  max_gap = 300
)
```

## Arguments

- dat:

  A data frame with `LATITUDE`, `LONGITUDE` and `TIME`.

- by:

  Passed to
  [`track_speed()`](https://camilleross.org/narwcr/reference/track_speed.md).

- aerial_min:

  Knots at or above which a stretch is aerial. Default `40`, comfortably
  between a surveying vessel and the slowest survey aircraft.

- stationary_max:

  Knots at or below which a stretch is stationary. Default `2`.

- max_gap:

  Passed to
  [`track_speed()`](https://camilleross.org/narwcr/reference/track_speed.md).

## Value

A factor with levels `"stationary"`, `"vessel"`, `"aerial"`, one per
row, `NA` where the stretch has too little usable time to judge.

## What this is for

A single NARWC extract can hold both an aerial and a shipboard survey
with nothing to tell them apart. On the file this was built against,
`PLATFORM` was `NA` on all 1,394,556 records and the aerial `LEGTYPE`
codes were used throughout — but 738 line occupations flew at 40-131
knots and 299 moved at about 10. That matters because the distance
machinery is not platform-agnostic: a declination angle and a strip
width are aircraft measurements, and a shipboard survey measures by
reticle and bearing.

Prefer a recorded signal where the file has one. On that extract every
aerial occupation carried a `LEGNO` and no vessel occupation did,
agreeing with speed on 1037 of 1038 — a line number is a reading, where
a speed threshold is an inference.

## See also

[`track_speed()`](https://camilleross.org/narwcr/reference/track_speed.md)

## Examples

``` r
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
dat <- make_leg_id(read_narwc(path, quiet = TRUE), quiet = TRUE)
table(classify_platform(dat), useNA = "ifany")
#> 
#> stationary     vessel     aerial 
#>          0          0        113 
```
