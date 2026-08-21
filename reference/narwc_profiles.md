# Survey-programme column profiles

Columns that particular survey programmes add beyond the NARWC handbook,
what each is understood to mean, and how confident that understanding
is.

## Usage

``` r
narwc_profiles(profile = NULL)
```

## Arguments

- profile:

  Optional profile name to filter to. `NULL` (default) returns every
  entry.

## Value

A tibble with columns:

- `profile`:

  Short profile name, for `read_narwc(profile = )`.

- `programme`:

  The survey programme.

- `column`:

  Column name as it appears in that programme's files.

- `meaning`:

  What the column holds.

- `role`:

  What `narwcr` does with it: `"passthrough"` if nothing.

- `confidence`:

  `"confirmed"` or `"unconfirmed"`.

## Why this exists

A NARWC extract is not the only shape this data arrives in. Individual
survey programmes carry their own derived columns, and a processed
"ready for model" file may have a dozen of them. They are not in
handbook Table 1, so
[`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md)
does not know about them and
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
would otherwise drop them without comment.

This registry records what is known about them, per programme. It is
expected to grow: CCS is the first entry because it is the one whose
files this package was originally written against, and it is an
exception rather than a representative case.

## Detection suggests, declaration acts

[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
will tell you when a file's extra columns match a known profile, but it
will never apply one because a name matched. That restraint is
deliberate, and `Tr_SIGHTING` is the reason: it means "sighting made
from the track-line" in CCS files, and there is nothing to stop another
programme using the same name for something else. A column name is not a
contract. Acting on a profile requires naming it —
`read_narwc(profile = "ccs")`.

The same caution applies in the other direction. A profile's presence in
this registry means the columns have been identified, not that this
package interprets them. `role` records which are actually used for
anything.

## Adding a programme

A new profile needs the programme's name, the columns it adds, and — the
part that is usually missing — what each column actually means, from a
data dictionary rather than from the column name. Entries whose meaning
has been guessed are marked `confidence = "unconfirmed"` and should stay
that way until someone who ran the survey confirms them.

## See also

[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md),
[`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md),
[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)

## Examples

``` r
narwc_profiles()
#> # A tibble: 8 × 6
#>   profile programme                              column meaning role  confidence
#>   <chr>   <chr>                                  <chr>  <chr>   <chr> <chr>     
#> 1 ccs     Center for Coastal Studies, Cape Cod … IS_LAT Aircra… pass… confirmed 
#> 2 ccs     Center for Coastal Studies, Cape Cod … IS_LO… Aircra… pass… confirmed 
#> 3 ccs     Center for Coastal Studies, Cape Cod … IS_SP… Specie… pass… unconfirm…
#> 4 ccs     Center for Coastal Studies, Cape Cod … Tr_SI… Whethe… pass… confirmed 
#> 5 ccs     Center for Coastal Studies, Cape Cod … OBSSI… Unknow… pass… unconfirm…
#> 6 ccs     Center for Coastal Studies, Cape Cod … Effor… An eff… pass… unconfirm…
#> 7 ccs     Center for Coastal Studies, Cape Cod … Date_… Date, … pass… unconfirm…
#> 8 ccs     Center for Coastal Studies, Cape Cod … Time_… Time, … pass… unconfirm…
narwc_profiles("ccs")
#> # A tibble: 8 × 6
#>   profile programme                              column meaning role  confidence
#>   <chr>   <chr>                                  <chr>  <chr>   <chr> <chr>     
#> 1 ccs     Center for Coastal Studies, Cape Cod … IS_LAT Aircra… pass… confirmed 
#> 2 ccs     Center for Coastal Studies, Cape Cod … IS_LO… Aircra… pass… confirmed 
#> 3 ccs     Center for Coastal Studies, Cape Cod … IS_SP… Specie… pass… unconfirm…
#> 4 ccs     Center for Coastal Studies, Cape Cod … Tr_SI… Whethe… pass… confirmed 
#> 5 ccs     Center for Coastal Studies, Cape Cod … OBSSI… Unknow… pass… unconfirm…
#> 6 ccs     Center for Coastal Studies, Cape Cod … Effor… An eff… pass… unconfirm…
#> 7 ccs     Center for Coastal Studies, Cape Cod … Date_… Date, … pass… unconfirm…
#> 8 ccs     Center for Coastal Studies, Cape Cod … Time_… Time, … pass… unconfirm…

# The names a profile would keep
narwc_profiles("ccs")$column
#> [1] "IS_LAT"      "IS_LONG"     "IS_SPECCODE" "Tr_SIGHTING" "OBSSIGHT"   
#> [6] "Effort_Type" "Date_UTC"    "Time_UTC"   
```
