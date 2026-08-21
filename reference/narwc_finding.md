# Record one validation finding

Builds a single row of the report that
[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
returns. Packages that add their own checks use this so that their
findings have the same shape as the handbook-general ones and can be
bound together with them.

## Usage

``` r
narwc_finding(
  check,
  severity,
  column = NA_character_,
  rows = integer(0),
  message,
  n = length(rows)
)
```

## Arguments

- check:

  Name of the check, used as the `check` column.

- severity:

  `"error"`, `"warning"`, or `"note"`.

- column:

  The column involved, or `NA`.

- rows:

  Integer vector of affected row indices. Stored capped at 100.

- message:

  Human-readable description of the problem.

- n:

  Number of records affected. Defaults to `length(rows)`; give it
  explicitly for a finding that is about the table rather than
  particular rows, such as a column being absent altogether.

## Value

A one-row tibble.

## Examples

``` r
narwc_finding(
  "my_check", "warning", "ALT", rows = c(3L, 7L),
  message = "ALT looks implausible."
)
#> # A tibble: 1 × 6
#>   check    severity column     n rows      message               
#>   <chr>    <chr>    <chr>  <int> <list>    <chr>                 
#> 1 my_check warning  ALT        2 <int [2]> ALT looks implausible.
```
