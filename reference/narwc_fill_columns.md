# Columns that may be carried forward, and columns that must not be

`narwc_fill_columns()` returns the survey-state columns
[`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)
fills by default: variables recorded once and left blank until they
change.

## Usage

``` r
narwc_fill_columns()

narwc_never_fill()
```

## Value

`narwc_fill_columns()` returns a character vector of fillable column
names; `narwc_never_fill()` returns those that are refused.

## Two kinds of column

A NARWC record carries two kinds of variable, and the distinction
decides whether a blank may be filled.

**State** persists until something changes it. `LEGTYPE` stays 2 until
the aircraft leaves the census line; `BEAUFORT` stays 3 until the sea
state is re-assessed. A blank means "as above", and filling it recovers
information that was deliberately not repeated.

**Measurements** belong to their own record. A position, a time, a
declination angle, a species, a group size — each describes one moment.
A blank means the measurement was not taken, and filling it fabricates
one.

## Filling a sighting column is the dangerous case

`SPECCODE`, `NUMBER`, `SIGHTNO`, `STRIP`, `ANGLEL`, `ANGLER`, `S_LAT`
and `S_LONG` are refused outright. Carrying `SPECCODE` and `NUMBER`
forward would replicate one sighting onto every subsequent record until
the next sighting — turning a single group of three right whales into
hundreds — and every count downstream would be wrong by orders of
magnitude. `EVENTNO`, `TIME`, `LATITUDE`, and `LONGITUDE` are refused
for the same reason: they are what makes a record distinct from the one
before it.

## See also

[`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)

## Examples

``` r
narwc_fill_columns()
#>  [1] "LEGTYPE"  "LEGSTAGE" "LEGNO"    "VISIBLTY" "BEAUFORT" "CLOUD"   
#>  [7] "GLAREL"   "GLARER"   "WX"       "SURFTEMP" "ALT"      "HEADING" 
#> [13] "PLATFORM" "STRATUM"  "BLOCK"   
narwc_never_fill()
#>  [1] "SPECCODE"  "TAXCODE"   "IDREL"     "NUMBER"    "NUMCALF"   "SIGHTNO"  
#>  [7] "STRIP"     "ANGLEL"    "ANGLER"    "S_LAT"     "S_LONG"    "S_TIME"   
#> [13] "PHOTOS"    "FILEID"    "EVENTNO"   "YEAR"      "MONTH"     "DAY"      
#> [19] "TIME"      "DATE"      "LATITUDE"  "LONGITUDE"
```
