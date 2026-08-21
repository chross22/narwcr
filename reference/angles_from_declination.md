# Build ANGLEL and ANGLER from one declination column and a side

The handbook records a declination angle in `ANGLEL` or `ANGLER`
according to which side of the aircraft the sighting was on (8.A.2).
Some survey programmes instead keep a single angle column and a separate
left/right flag. This splits the one into the two.

## Usage

``` r
angles_from_declination(
  dat,
  angle,
  side,
  left = "L",
  right = "R",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- dat:

  A data frame, ideally from
  [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md).

- angle:

  Name of the column holding the declination angle, in degrees.

- side:

  Name of the column saying which side the sighting was on.

- left, right:

  The values of `side` meaning left and right. Compared after trimming
  whitespace and ignoring case.

- overwrite:

  Replace values already in `ANGLEL`/`ANGLER`? Default `FALSE`, which
  fills only the blanks — a recorded angle is not displaced by a derived
  one.

- quiet:

  Suppress the report of what was set and what was skipped. Default
  `FALSE`.

## Value

`dat` with `ANGLEL` and `ANGLER` added or filled.

## Why this is not automatic

`narwcr` will not infer meaning from a column name, and this mapping
cannot be inferred safely. A column called `Decl_Angle` might hold a
declination below the horizon, an inclination above it, or a bearing;
`Left_or_Right` might use `L`/`R`, `1`/`2`, or `port`/`starboard`.
Naming the columns and the codes is the caller asserting what they mean,
which is a different thing from the package guessing.

## What it checks

A declination angle is measured down from the horizontal, so
`perp_distance()` computes `ALT / tan(angle)`: 90 degrees is directly
below the aircraft and gives a perpendicular distance of zero, and an
angle at or below 0 is undefined. Values outside `(0, 90]` are reported
and left alone rather than converted into a distance that cannot be
right.

Records whose side is missing or unrecognised are reported and skipped:
an angle with no side cannot be placed, and guessing a side would put
sightings on the wrong half of the track line.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, section 8.A.2. NARWC
Reference Document 2023-01.

## See also

`distsamp::perp_distance()`, which consumes these columns.

## Examples

``` r
dat <- data.frame(
  SPECCODE = c("RIWH", "RIWH", "FIWH"),
  Decl_Angle = c(40, 20, 90),
  Left_or_Right = c("L", "R", "R")
)
angles_from_declination(dat, "Decl_Angle", "Left_or_Right")
#> `angles_from_declination()` set 1 ANGLEL and 2 ANGLER from `Decl_Angle` and `Left_or_Right`.
#>   SPECCODE Decl_Angle Left_or_Right ANGLEL ANGLER
#> 1     RIWH         40             L     40     NA
#> 2     RIWH         20             R     NA     20
#> 3     FIWH         90             R     NA     90
```
