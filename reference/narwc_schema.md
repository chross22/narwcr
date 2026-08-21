# NARWC columns recognised by narwcr

The subset of the NARWC variable list (handbook Table 1) that this
package reads, together with the alternative spellings accepted on
input.

## Usage

``` r
narwc_schema()
```

## Value

A named list with elements `required`, `optional`, and `aliases`.

## Details

`required` names the columns without which no segmentation is possible.
`optional` names columns that are carried through and used when present.
`aliases` maps input column names onto the internal name.

## Coordinate naming

The handbook's canonical event-position columns are `LAT_DD` and
`LONG_DD` (8.A.18, 8.A.22). Data extracts distributed by the NARWC
database manager, and some upstream processing pipelines, instead use
`LATITUDE` and `LONGITUDE`. This package standardises internally on
`LATITUDE`/`LONGITUDE` and accepts either spelling on input.
`S_LAT`/`S_LONG` (8.A.33, 8.A.34) are the *exact sighting* position and
are kept distinct from the event position.

## Version 8 additions

`ANGLEL` and `ANGLER` are declination angles to a sighting, added in
Version 8 of the handbook. The New England Aquarium survey team stopped
recording `STRIP` in 2022 and switched to these, which give a more
precise right-angle distance and are less sensitive to changes in survey
altitude (8.A.31). They are carried through as optional columns;
converting them to perpendicular distance belongs with the
detection-function work, which is out of scope for this version.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, Table 1. NARWC Reference
Document 2023-01.

## Examples

``` r
narwc_schema()$required
#> [1] "FILEID"    "EVENTNO"   "YEAR"      "MONTH"     "DAY"       "TIME"     
#> [7] "LATITUDE"  "LONGITUDE" "LEGTYPE"  
```
