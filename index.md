# narwcr

Read and standardise North Atlantic Right Whale Consortium (NARWC)
survey data.

`narwcr` is the common data preparation layer beneath analysis packages
that read the same archive but diverge in their modelling methods. It
does the part that is the same for everyone — getting a NARWC extract
into a single, predictable shape — and nothing that is specific to any
one analysis.

|  |  |
|----|----|
| [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md) | read an extract, matching column names loosely |
| [`standardize_narwc_columns()`](https://camilleross.org/narwcr/reference/standardize_narwc_columns.md) | apply the shared vocabulary to a table you already have |
| [`narwc_column_mapping()`](https://camilleross.org/narwcr/reference/narwc_column_mapping.md) | the full dictionary of what was renamed to what |
| [`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md) | fill the fields the archive records once per leg, not once per row |
| [`narwc_fill_columns()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md), [`narwc_never_fill()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md) | which columns may be filled, and which must never be |
| [`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md) | check a table against the handbook’s own rules |
| [`narwc_checks()`](https://camilleross.org/narwcr/reference/narwc_checks.md) | the handbook-general check set, extensible by the caller |
| [`narwc_codes()`](https://camilleross.org/narwcr/reference/narwc_codes.md), [`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md), [`narwc_strip_bins()`](https://camilleross.org/narwcr/reference/narwc_strip_bins.md) | the handbook code books |
| [`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md) | how named survey programmes differ from each other |
| [`flag_effort()`](https://camilleross.org/narwcr/reference/flag_effort.md), [`visibility_ok()`](https://camilleross.org/narwcr/reference/visibility_ok.md), [`make_leg_id()`](https://camilleross.org/narwcr/reference/make_leg_id.md) | on-effort records and leg identity |
| [`fill_legstage()`](https://camilleross.org/narwcr/reference/fill_legstage.md) | reconstruct the line state on records that record no `LEGSTAGE` |
| [`angles_from_declination()`](https://camilleross.org/narwcr/reference/angles_from_declination.md) | split one declination column and a side into `ANGLEL`/`ANGLER` |
| [`track_speed()`](https://camilleross.org/narwcr/reference/track_speed.md), [`classify_platform()`](https://camilleross.org/narwcr/reference/classify_platform.md) | how fast the platform was moving, and what it therefore was |
| [`narwc_fetch()`](https://camilleross.org/narwcr/reference/narwc_fetch.md), [`narwc_cloud_roots()`](https://camilleross.org/narwcr/reference/narwc_cloud_roots.md) | fetch extracts from cloud storage |

## A column name is not a contract

Different survey programmes record the same quantity under different
names, and occasionally the same name for different quantities.
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
matches loosely and then tells you exactly what it did:

``` r

dat <- read_narwc("extract.csv")
narwc_column_mapping(dat)
```

Nothing is renamed silently. If a match is a guess, it is reported as
one.

## An archive is not one archive

A NARWC extract can be several surveys concatenated, each era recording
the same variable in a different column. One real archive holds position
in `TrkLatitude` for the years a GPS was fitted and in `LATITUDE` for
the decades before, and does the same for the date, the clock and the
event number.

[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
fills each canonical column from whichever source covers the record in
front of it, rather than picking one column for the whole file, and says
what it took from where. The difference is not academic: choosing per
column instead of per record emptied `LATITUDE` on 3.75 million records
of that archive, and the survivors looked exactly like a clean two-year
dataset.

Where a file records something the handbook does not, it must be named
rather than guessed:

``` r

dat <- read_narwc("extract.csv",
                  extra_columns = c("Decl_Angle", "Left_or_Right"))
dat <- angles_from_declination(dat, "Decl_Angle", "Left_or_Right")
```

## What is recorded once is not missing everywhere

Survey state is written when it changes. A record taken mid-line often
carries no sea state, no altitude and no `LEGSTAGE` — and a check that
treats a blank as a failure will quietly discard most of a survey.

``` r

legs <- make_leg_id(dat)
legs <- fill_legstage(legs)      # the state, not the value
```

[`fill_legstage()`](https://camilleross.org/narwcr/reference/fill_legstage.md)
walks the handbook’s own state machine: after a begin, a continue or a
resume the line is continuing. After a break-off the aircraft is
circling and after an end-line the line is over, and both keep their
`NA`. On one archive this made 1,928 of 2,280 on-effort census sightings
eligible for a distance measurement that they had been excluded from for
want of a code nobody wrote down.

[`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)
is the other direction — for columns whose *value* carries forward, like
sea state. It refuses to fill a sighting column outright, because
filling one replicates a detection.

## Nothing declares the platform

`PLATFORM` is optional, has no code book, and is empty on real extracts.
An archive can hold an aerial and a shipboard survey with nothing to
tell them apart — and the distance machinery downstream is not
platform-agnostic.

Speed cannot be mistaken for anything else:

``` r

legs$kind <- classify_platform(legs)   # aerial, vessel, stationary
```

A survey aircraft flies at 90–120 knots and a survey vessel makes about
10.

## Validation is extensible

[`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
runs the handbook-general checks by default. Packages built on top add
their own without narwcr needing to know what they are:

``` r

validate_narwc(dat)                                       # handbook rules only
validate_narwc(dat, checks = c(narwc_checks(), my_checks()))
```

## Installation

``` r

# install.packages("pak")
pak::pak("chross22/narwcr")
```

## Licence

MIT © Camille Ross
