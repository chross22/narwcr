# Standardise column names onto the NARWC handbook's

Renames a data frame's columns to their canonical NARWC names, matching
case-insensitively and ignoring separators. This is the step
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
does first, exported on its own so that other packages working with
NARWC survey data can use the same vocabulary rather than maintain a
second one.

## Usage

``` r
standardize_narwc_columns(dat, quiet = FALSE, prefer_source = TRUE)
```

## Arguments

- dat:

  A data frame.

- quiet:

  Suppress the report of inferred matches. Default `FALSE`.

- prefer_source:

  Let a `Trk*` GPS track column take precedence over a canonical column
  of the same name. Default `TRUE`. See
  [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md).

## Value

`dat` with columns renamed where a match was found.

## Why this is shared

Every pipeline that reads real survey exports hits the same wall: the
files say `Event`, `event_no`, or `Event No.` and the code wants
`EVENTNO`. Solving it once per package means the vocabularies drift, and
a column recognised in one place is silently dropped in another. The
alias table here is the merge of two such attempts — `distsamp`'s and
`msomgom`'s — and this package exists so that it is the only one that
grows.

## What it will not do

Guess by edit distance. `EVENTN0` with a zero stays `EVENTN0`, because a
column that is nearly a name is not that name. It also never renames
onto a canonical column that already exists, so a correctly named column
always wins, and it warns when two columns could both be one canonical
name.

Policy stays with the caller. This renames columns and nothing else: it
does not fill defaults, drop records, or coerce types.
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
does those, and packages with different needs keep their own on top. The
standing example is `ALT`: `msomgom` defaults a missing altitude to a
nominal survey height, which is reasonable for occupancy but would be
wrong in `distsamp`, where `ALT` feeds a right-angle distance and a
fabricated altitude gives a fabricated distance. Neither default belongs
here.

## See also

[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md),
[`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md)

## Examples

``` r
raw <- data.frame(Event = 1, Lat = 43, Long = -69, Sea_State = 3,
                  check.names = FALSE)
names(standardize_narwc_columns(raw, quiet = TRUE))
#> [1] "EVENTNO"   "LATITUDE"  "LONGITUDE" "BEAUFORT" 
```
