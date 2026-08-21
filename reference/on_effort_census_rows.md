# Which records are eligible for a right-angle distance?

Reports the records that are on effort, on a census line, and continuing
it.

## Usage

``` r
on_effort_census_rows(dat)
```

## Arguments

- dat:

  A data frame of NARWC records.

## Value

A logical vector with one element per row of `dat`.

## Why this is shared

Handbook 8.A.31 restricts a right-angle distance measurement to records
in this state. Every distance source in a downstream package has to
agree about which sightings are fittable, and the only way to guarantee
that is for them all to ask the same function. It lives here, rather
than in the package that computes distances, because the rule is the
handbook's rather than any one analysis's.

Columns that are absent cannot disqualify a record, so a table carrying
only some of `OnOff.Effort`, `LEGTYPE` and `LEGSTAGE` is judged on what
it has. That is deliberate: it lets a partial extract be used, but it
means a `TRUE` is only as strong as the columns present.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, section 8.A.31. NARWC
Reference Document 2023-01.

## Examples

``` r
dat <- data.frame(
  OnOff.Effort = c(1, 1, 0, 1),
  LEGTYPE      = c(2, 2, 2, 1),
  LEGSTAGE     = c(2, 5, 2, 2)
)
on_effort_census_rows(dat)
#> [1]  TRUE FALSE FALSE FALSE
```
