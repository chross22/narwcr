# Reconstruct the line state on records that record no LEGSTAGE

`LEGSTAGE` marks what is happening to a survey line — it begins (1),
continues (2), breaks off to circle (3), resumes (4), ends (5).
Programmes record it when it *changes*, so a record taken mid-line often
carries none at all. This fills in the state those records were in.

## Usage

``` r
fill_legstage(dat, by = NULL, quiet = FALSE)
```

## Arguments

- dat:

  A data frame with `LEGSTAGE`, in survey order — from
  [`make_leg_id()`](https://camilleross.org/narwcr/reference/make_leg_id.md),
  which sorts.

- by:

  Columns identifying one line occupation, so a state never carries
  across a break. `NULL` (default) uses `LEGNO3` when present, else
  `DATE` and `FILEID`.

- quiet:

  Suppress the report of how many were filled. Default `FALSE`.

## Value

`dat` with `LEGSTAGE` filled where the state is known, and a logical
`LEGSTAGE_FILLED` marking every record this function wrote to.

## Why this is not [`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)

Carrying `LEGSTAGE` forward as a value is wrong. `1` means "begin line",
an event; copying it onto the next thousand records claims the line
began a thousand times, and since `1 -> 1` is not a legal transition it
manufactures the sequence errors it looks like it should fix.

What carries forward is the *state*, not the code. Handbook 8.A.20:

|                   |                                    |
|-------------------|------------------------------------|
| after 1 begin     | the line is continuing, so 2       |
| after 2 continue  | still continuing, 2                |
| after 4 resume    | continuing again, 2                |
| after 3 break off | off the line, circling — left `NA` |
| after 5 end       | the line is over — left `NA`       |
| before any event  | unknown — left `NA`                |

So the only code ever written is `2`, and only where the line is
genuinely continuing. A record before the first event, during a circle,
or after the line closed keeps its `NA` and stays ineligible — which is
correct, because a detection made while not searching the line breaks
the distance-sampling assumptions rather than merely the bookkeeping.

## What it is for

[`on_effort_census_rows()`](https://camilleross.org/narwcr/reference/on_effort_census_rows.md)
requires `LEGSTAGE == 2`, and handbook 8.A.31 restricts a right-angle
distance measurement to records in that state. On a real archive 1,928
of 2,280 on-effort census sightings carried no `LEGSTAGE`, so 85% of the
detections that could have informed a detection function were excluded
for a code nobody wrote down.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, sections 8.A.20 and
8.A.31. NARWC Reference Document 2023-01.

## See also

[`on_effort_census_rows()`](https://camilleross.org/narwcr/reference/on_effort_census_rows.md),
which is what this makes answerable;
[`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)
for columns whose *value* carries forward.

## Examples

``` r
dat <- data.frame(
  FILEID = "F", EVENTNO = 1:6, LEGNO3 = "1_1", LEGTYPE = 2,
  LEGSTAGE = c(1, NA, NA, 5, NA, NA)
)
fill_legstage(dat, quiet = TRUE)$LEGSTAGE
#> [1]  1  2  2  5 NA NA
```
