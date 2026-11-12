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

## `LEGSTAGE` is not in the default set

`LEGSTAGE` records an event (begin, break off, resume, end), not a
state, so carrying its value forward is wrong: a begin-line `1` copied
down the line marks every record "begin line".
[`fill_legstage()`](https://camilleross.org/narwcr/reference/fill_legstage.md)
reconstructs the state instead, and
[`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)
hands `LEGSTAGE` to it when asked to fill it.

## See also

[`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)

## Examples

``` r
narwc_fill_columns()
#>  [1] "LEGTYPE"  "LEGNO"    "VISIBLTY" "BEAUFORT" "CLOUD"    "GLAREL"  
#>  [7] "GLARER"   "WX"       "SURFTEMP" "ALT"      "HEADING"  "PLATFORM"
#> [13] "STRATUM"  "BLOCK"   
narwc_never_fill()
#>  [1] "SPECCODE"  "TAXCODE"   "IDREL"     "NUMBER"    "NUMCALF"   "SIGHTNO"  
#>  [7] "STRIP"     "ANGLEL"    "ANGLER"    "S_LAT"     "S_LONG"    "S_TIME"   
#> [13] "PHOTOS"    "FILEID"    "EVENTNO"   "YEAR"      "MONTH"     "DAY"      
#> [19] "TIME"      "DATE"      "LATITUDE"  "LONGITUDE"
```
