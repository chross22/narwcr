# What the column names were changed to

The record of how
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
or
[`standardize_narwc_columns()`](https://camilleross.org/narwcr/reference/standardize_narwc_columns.md)
renamed a data frame's columns, so a rename can be checked rather than
trusted.

## Usage

``` r
narwc_column_mapping(dat)
```

## Arguments

- dat:

  A data frame from
  [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
  or
  [`standardize_narwc_columns()`](https://camilleross.org/narwcr/reference/standardize_narwc_columns.md).

## Value

A tibble with `original`, `standardized`, `match`, and `factor`. `match`
is either `"alias"` for an exact entry in the alias table or
`"inferred"` for one found after normalising case and separators.
`factor` is the multiplier applied to reach the canonical unit —
`0.3048` for an altitude that arrived in feet — and `NA` where the
values were taken as they were. Zero rows when nothing was renamed.

## Why keep it

Matching ignores case and separators, which is what makes a real export
readable without hand-editing — and also what makes it possible for a
column to be renamed onto something you did not intend. The mapping
travels with the data as an attribute so the question "where did this
column come from" has an answer after the fact, not only in a message
that has scrolled away.

## See also

[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md),
[`standardize_narwc_columns()`](https://camilleross.org/narwcr/reference/standardize_narwc_columns.md)

## Examples

``` r
raw <- data.frame(Event = 1, Lat_DD = 43, Long_DD = -69, Sea_State = 3)
dat <- standardize_narwc_columns(raw, quiet = TRUE)
narwc_column_mapping(dat)
#> # A tibble: 4 × 4
#>   original  standardized match    factor
#>   <chr>     <chr>        <chr>     <dbl>
#> 1 Event     EVENTNO      inferred     NA
#> 2 Lat_DD    LATITUDE     inferred     NA
#> 3 Long_DD   LONGITUDE    inferred     NA
#> 4 Sea_State BEAUFORT     inferred     NA
```
