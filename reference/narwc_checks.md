# The handbook-general check set

Returns the checks that
[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
runs by default: those that follow from the NARWC handbook itself, and
so are true of any NARWC extract regardless of what it will be used for.

## Usage

``` r
narwc_checks()
```

## Value

A named list of functions.

## Adding your own

A check is a function of one argument, the data frame, returning
findings built with
[`narwc_finding()`](https://camilleross.org/narwcr/reference/narwc_finding.md)
— `NULL`, one finding, or a list of them. Because the set is an ordinary
named list, a package adds to it by concatenating:

    validate_narwc(dat, checks = c(narwc_checks(), my_checks()))

and drops one it does not want by name:

    keep <- setdiff(names(narwc_checks()), "time_format")
    validate_narwc(dat, checks = narwc_checks()[keep])

Checks that depend on what the data will be *used for* belong in the
package that uses it, not here. `distsamp` keeps the ones about
declination angles and exact sighting positions for that reason: they
are only problems if you are computing a right-angle distance.

## Examples

``` r
names(narwc_checks())
#>  [1] "required_columns"        "code_books"             
#>  [3] "legstage_placement"      "legstage_sequence"      
#>  [5] "event_order"             "time_format"            
#>  [7] "coordinates"             "sighting_counts"        
#>  [9] "altitude_units"          "sightno_without_species"
#> [11] "sightno_duplicated"      "sightno_non_target"     
#> [13] "exact_position"          "extra_columns"          
```
