# narwcr

Read and standardize North Atlantic Right Whale Consortium (NARWC) survey data.

`narwcr` is the common data preparation layer beneath analysis packages that
read the same archive but diverge in their modelling methods. It does the part
that is the same for everyone — getting a NARWC extract into a single,
predictable shape — and nothing that is specific to any one analysis.

| | |
|---|---|
| `read_narwc()` | read an extract, matching column names loosely |
| `standardize_narwc_columns()` | apply the shared vocabulary to a table you already have |
| `narwc_column_mapping()` | the full dictionary of what was renamed to what |
| `fill_narwc()` | fill the fields the archive records once per leg, not once per row |
| `narwc_fill_columns()`, `narwc_never_fill()` | which columns may be filled, and which must never be |
| `validate_narwc()` | check a table against the handbook's own rules |
| `narwc_checks()` | the handbook-general check set, extensible by the caller |
| `narwc_codes()`, `narwc_schema()`, `narwc_strip_bins()` | the handbook code books |
| `narwc_profiles()` | how named survey programmes differ from each other |
| `flag_effort()`, `visibility_ok()`, `make_leg_id()` | on-effort records and leg identity |
| `fill_legstage()` | reconstruct the line state on records that record no `LEGSTAGE` |
| `angles_from_declination()` | split one declination column and a side into `ANGLEL`/`ANGLER` |
| `track_speed()`, `classify_platform()` | how fast the platform was moving, and what it therefore was |
| `narwc_fetch()`, `narwc_cloud_roots()` | fetch extracts from cloud storage |
| `run_narwc_app()` | look at an extract on an interactive map |

## Column standardization

Different survey programs sometimes use different names to record the same data, and
occasionally the same name for different quantities. `read_narwc()` matches
loosely and prints in the console exactly how the names were updated. Matches that are guesses are reported as such.

```r
dat <- read_narwc("extract.csv")
narwc_column_mapping(dat)
```

Where a file records data with no corresponding column in the Kenney Handbook, it must be named explicitly in the function callrather
than guessed:

```r
dat <- read_narwc("extract.csv",
                  extra_columns = c("Decl_Angle", "Left_or_Right"))
dat <- angles_from_declination(dat, "Decl_Angle", "Left_or_Right")
```


In the handbook, `PLATFORM` is optional. Since a
data extract can hold both aerial and vessel surveys with no differentiator and the distance machinery downstream is not platform-agnostic, platform needs to be classified.

```r
legs$kind <- classify_platform(legs)   # aerial, vessel, stationary
```

A survey aircraft flies at 90–120 knots and a survey vessel makes about 10.

## Map app to check the data platform before performing analysis

```r
run_narwc_app(dat)                            # or a path, or nothing at all
run_narwc_app(dat, pam = "detections.csv")    # with the acoustic record beside it
```

An interactive map of the extract: where the platform went, what it saw, and
when. Aerial, vessel and opportunistic records are told apart by `LEGTYPE`
(handbook 8.A.21) and, where `LEGTYPE` says nothing, by how fast the platform
was moving — and the app reports which records were read and which were
inferred, because those are not the same claim. Passive acoustic monitoring is
not in the sightings archive and comes from its own file, drawn as stations
with recording effort as the denominator.

`flag_effort()`'s criteria are controls rather than constants. Moving the
Beaufort cutoff and watching the on-effort track shrink is the fastest way to
see what a threshold costs.

## Validation

`validate_narwc()` runs the handbook-general checks by default. Packages built
on top add their own without narwcr needing to know what they are:

```r
validate_narwc(dat)                                       # handbook rules only
validate_narwc(dat, checks = c(narwc_checks(), my_checks()))
```

## Installation

```r
# install.packages("pak")
pak::pak("chross22/narwcr")
```

## Licence

MIT © Camille H. Ross
