# NARWC database code books

Lookup tables for the coded variables in the North Atlantic Right Whale
Consortium (NARWC) sightings database, transcribed from Kenney (2023),
*The North Atlantic Right Whale Consortium Database: A Guide for Users
and Contributors*, Version 8 (NARWC Reference Document 2023-01), Chapter
8.

## Usage

``` r
narwc_codes(variable = NULL)
```

## Arguments

- variable:

  Name of a coded NARWC variable. One of `"LEGTYPE"`, `"LEGSTAGE"`,
  `"IDREL"`, `"TAXCODE"`, `"STRATUM"`, `"VISIBLTY"`, or `"WX"`. If
  `NULL` (the default), the whole code book is returned.

## Value

A named character vector mapping codes to their meanings, or, when
`variable` is `NULL`, a named list of such vectors.

## Details

These tables are the single source of truth for the package: validation,
effort determination, and sighting filtering all read their permitted
values from here rather than hard-coding numeric literals.

## Handbook sections

`LEGTYPE` 8.A.21, `LEGSTAGE` 8.A.20, `IDREL` 8.A.16, `TAXCODE` 8.A.36,
`STRATUM` 8.A.30, `VISIBLTY` 8.A.38, `WX` 8.A.39.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*. North Atlantic Right
Whale Consortium Reference Document 2023-01. University of Rhode Island,
Graduate School of Oceanography, Narragansett, Rhode Island.

## Examples

``` r
narwc_codes("LEGSTAGE")
#>                                                   1 
#>                                        "begin line" 
#>                                                   2 
#>                                     "continue line" 
#>                                                   3 
#>                          "break off line to circle" 
#>                                                   4 
#>                                       "resume line" 
#>                                                   5 
#>                                          "end line" 
#>                                                   6 
#> "sighting by anyone other than an on-duty observer" 
#>                                                   7 
#>        "sighting detected in a vertical photograph" 
names(narwc_codes("LEGTYPE"))
#> [1] "0" "1" "2" "3" "4" "5" "6" "7" "9"
```
