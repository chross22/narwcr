# NARWC STRIP right-angle distance code books

`STRIP` encodes the right-angle distance of a sighting from the
track-line as an *interval*, not a point. Two different code books are
in use, and which applies depends on the survey programme, the date, and
the aircraft.

## Usage

``` r
narwc_strip_bins(
  scheme = c("cetap", "nlpsc"),
  platform = c("skymaster", "at-11"),
  units = c("m", "km", "nmi")
)
```

## Arguments

- scheme:

  `"cetap"` or `"nlpsc"`.

- platform:

  `"skymaster"` or `"at-11"`. Only affects the `"cetap"` scheme, where
  the bins above 1 nmi differ by aircraft.

- units:

  `"m"` (default), `"km"`, or `"nmi"`.

## Value

A tibble with `code`, `side`, `distbegin`, `distend`.

## The two schemes

- `"cetap"`:

  The original scheme (handbook 8.A.31). Codes `1,2` cover 0-1/4 nmi;
  that closest interval was subsequently split at 1/8 nmi, giving `3,4`
  and `5,6`. Both forms occur in the archive, so `1,2` is kept as the
  wider unsplit bin rather than being silently merged. Intervals beyond
  1 nmi differ by aircraft: the AT-11 has a single open bin above 1 nmi,
  the Skymaster splits at 2 nmi.

- `"nlpsc"`:

  Defined for the NLPSC / Massachusetts CEC surveys that began in
  October 2011, flown with a Skymaster. Different breakpoints entirely,
  running out to 4 nmi.

Odd codes are the left (port) side of the track, even codes the right
(starboard). Code `0` means directly on the track-line and applies only
to the AT-11, because of the Skymaster's restricted downward visibility.

## Open-ended bins

The top bin of every scheme is open (`>1`, `>2`, `>4` nmi), so `distend`
is `Inf`. This function reports the code book as the handbook defines it
and does not truncate. A detection function cannot be fitted to an
unbounded bin, so an analysis that fits one has to choose a truncation
distance — and that choice belongs to the analysis rather than to the
code book.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, section 8.A.31. NARWC
Reference Document 2023-01.

Kenney, R.D. and Scott, G.P. (1981) Calibration of the Beechcraft AT-11
forward observation bubble for population estimation purposes. In CETAP,
*A Characterization of Marine Mammals and Turtles in the Mid- and
North-Atlantic Areas of the U.S. Outer Continental Shelf, Annual Report
for 1979.* Bureau of Land Management, Washington, DC.

## Examples

``` r
narwc_strip_bins("nlpsc")
#> # A tibble: 14 × 4
#>     code side  distbegin distend
#>    <dbl> <chr>     <dbl>   <dbl>
#>  1     1 left         0     232.
#>  2     2 right        0     232.
#>  3     3 left       232.    463 
#>  4     4 right      232.    463 
#>  5     5 left       463     926 
#>  6     6 right      463     926 
#>  7     7 left       926    1852 
#>  8     8 right      926    1852 
#>  9     9 left      1852    3704 
#> 10    10 right     1852    3704 
#> 11    11 left      3704    7408 
#> 12    12 right     3704    7408 
#> 13    13 left      7408     Inf 
#> 14    14 right     7408     Inf 
narwc_strip_bins("cetap", platform = "at-11", units = "nmi")
#> # A tibble: 15 × 4
#>     code side     distbegin distend
#>    <dbl> <chr>        <dbl>   <dbl>
#>  1     0 on-track     0       0    
#>  2     1 left         0       0.25 
#>  3     2 right        0       0.25 
#>  4     3 left         0       0.125
#>  5     4 right        0       0.125
#>  6     5 left         0.125   0.25 
#>  7     6 right        0.125   0.25 
#>  8     7 left         0.25    0.5  
#>  9     8 right        0.25    0.5  
#> 10     9 left         0.5     0.75 
#> 11    10 right        0.5     0.75 
#> 12    11 left         0.75    1    
#> 13    12 right        0.75    1    
#> 14    13 left         1     Inf    
#> 15    14 right        1     Inf    
```
