# Was visibility acceptable?

Interprets the NARWC `VISIBLTY` variable, which carries two different
encodings in the same column, and reports whether each record met a
visibility threshold.

## Usage

``` r
visibility_ok(visibility, min_nmi = 2)
```

## Arguments

- visibility:

  Numeric vector of `VISIBLTY` values.

- min_nmi:

  Minimum acceptable clear visibility, in nautical miles. Defaults to
  `2`, the CETAP standard.

## Value

A logical vector, `NA` where the record cannot answer the question.

## Why this is not a simple comparison

Handbook 8.A.38 explains that `VISIBLTY` was originally a one-digit code
recording only whether visibility reached the 2-nautical-mile CETAP
standard and, if not, the weather responsible. In 2004 the field was
redefined to hold the actual estimated clear visibility in nautical
miles. During the 2021 archive update the old codes were folded back
into `VISIBLTY` **as negative numbers**:

|      |                                                |
|------|------------------------------------------------|
| `-1` | clear visibility for at least 2 nautical miles |
| `-2` | less than 2 miles, fog                         |
| `-3` | less than 2 miles, haze                        |
| `-4` | less than 2 miles, rain                        |
| `-5` | less than 2 miles, snow                        |

So a plain `VISIBLTY >= 2` test — as used by the original processing
code — marks every legacy record as unacceptable, including `-1`, which
actually records *good* visibility. On a multi-year dataset that
silently discards all pre-2004 effort.

A `-1` record asserts only that visibility reached 2 nmi, so it cannot
satisfy a threshold stricter than 2 and returns `NA` in that case rather
than a false `TRUE`.

## References

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*, section 8.A.38. NARWC
Reference Document 2023-01.

## Examples

``` r
visibility_ok(c(5, 1.5, -1, -2, NA))
#> [1]  TRUE FALSE  TRUE FALSE    NA

# A legacy "clear" code cannot support a stricter threshold
visibility_ok(-1, min_nmi = 3)
#> [1] NA
```
