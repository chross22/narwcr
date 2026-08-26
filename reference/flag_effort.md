# Flag on-effort records

Determines, for each record, whether the platform was on effort: on a
designated census track, in survey conditions good enough for the
sightings to be usable in a density estimate.

## Usage

``` r
flag_effort(
  dat,
  max_beaufort = 3,
  max_alt_m = 366,
  min_visibility_nmi = 2,
  legtype_on_effort = 2L,
  na_action = c("fail", "pass")
)
```

## Arguments

- dat:

  A data frame of NARWC survey data, ideally from
  [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md).

- max_beaufort:

  Highest acceptable Beaufort sea state. Default `3`. `NULL` drops the
  criterion.

- max_alt_m:

  Highest acceptable aircraft altitude in metres. Default `366`, which
  is 1,200 feet. `NULL` drops the criterion, which is what a shipboard
  survey needs — see above.

- min_visibility_nmi:

  Minimum acceptable visibility in nautical miles. Default `2`, the
  CETAP standard. `NULL` drops the criterion.

- legtype_on_effort:

  Integer vector of `LEGTYPE` values that count as effort. Default `2`.

- na_action:

  What to do when a criterion's value is missing: `"fail"` (default,
  treat the record as off effort) or `"pass"` (ignore the missing
  criterion).

## Value

`dat` with an integer `OnOff.Effort` column added: `1` on effort, `0`
off effort.

## Criteria

A record is on effort when all of the following hold:

- `LEGTYPE` is in `legtype_on_effort` — by default `2`, a line-transect
  survey line (handbook 8.A.21). Transits, cross-legs, and circling do
  not contribute effort to a density estimate.

- `BEAUFORT` is at most `max_beaufort`.

- `ALT` is below `max_alt_m`, in metres (handbook 8.A.1).

- Visibility clears `min_visibility_nmi`, via
  [`visibility_ok()`](https://camilleross.org/narwcr/reference/visibility_ok.md).

A criterion whose column is absent from the data is skipped, with a
message. A criterion whose value is `NA` fails, unless
`na_action = "pass"`.

## A criterion that does not apply

Passing `NULL` for a threshold drops that criterion entirely, which is
not the same as setting it wide. A missing value fails a criterion, so
on a file holding both an aerial and a shipboard survey the vessel
records fail the altitude ceiling at any height: they carry no `ALT`,
because there is no altitude for them to carry. `max_alt_m = NULL` is
how you say the criterion does not apply to this platform, rather than
that every vessel record failed it.
`distsamp::prepare_survey(platform = "vessel")` passes it for you.

`na_action = "pass"` would also let those records through, but it lets
*every* missing criterion through with them - a record with no sea state
and no visibility becomes on-effort too. Dropping the one criterion that
cannot apply keeps the rest strict.

The defaults are the CETAP standard. Kenney and Winn (1986, p. 347)
state the criteria applied to that programme's data as "observer(s)
formally on watch, clear visibility of at least 2 miles, and sea states
of Beaufort 3 or lower"; surveys were flown at 750 ft (229 m) (p. 346).
The handbook (8.A.38) confirms 2 nautical miles as the standard for
acceptable survey conditions defined during CETAP. Every threshold here
is an argument, because a different programme may reasonably choose
differently.

Records are *not* removed. Off-effort records are retained because the
distance between consecutive on-effort positions still needs them for
correct effort accounting, and because sightings made while circling are
attached back to the segment they came from.

## References

Kenney, R.D. and Winn, H.E. (1986) Cetacean high-use habitats of the
northeast United States continental shelf. *Fishery Bulletin*
84(2):345-357.

CETAP (1982) *A Characterization of Marine Mammals and Turtles in the
Mid- and North-Atlantic Areas of the U.S. Outer Continental Shelf, Final
Report.* Cetacean and Turtle Assessment Program, University of Rhode
Island. Bureau of Land Management, Washington, DC.

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*. NARWC Reference Document
2023-01.

## See also

[`visibility_ok()`](https://camilleross.org/narwcr/reference/visibility_ok.md),
[`on_effort_census_rows()`](https://camilleross.org/narwcr/reference/on_effort_census_rows.md)

## Examples

``` r
path <- system.file("extdata", "narwc-example.csv", package = "narwcr")
dat <- flag_effort(read_narwc(path))
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
table(dat$OnOff.Effort)
#> 
#>   0   1 
#>  12 101 

# A stricter sea-state cutoff
table(flag_effort(read_narwc(path), max_beaufort = 2)$OnOff.Effort)
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
#> 
#>   0   1 
#>  12 101 

# A vessel has no altitude to judge, so the criterion is dropped rather
# than raised - a missing ALT fails any ceiling.
table(flag_effort(read_narwc(path), max_alt_m = NULL)$OnOff.Effort)
#> `read_narwc()` renamed 2 columns:
#>   LAT_DD  -> LATITUDE
#>   LONG_DD -> LONGITUDE
#> All matched an exact entry in the alias table; `narwc_column_mapping()` returns this, and `quiet = TRUE` silences it.
#> 
#>   0   1 
#>  12 101 
```
