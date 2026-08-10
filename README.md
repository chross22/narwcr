# narwcr

Read and standardise North Atlantic Right Whale Consortium (NARWC) survey data.

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
| `narwc_fetch()`, `narwc_cloud_roots()` | fetch extracts from cloud storage |

## A column name is not a contract

Different survey programmes record the same quantity under different names, and
occasionally the same name for different quantities. `read_narwc()` matches
loosely and then tells you exactly what it did:

```r
dat <- read_narwc("extract.csv")
narwc_column_mapping(dat)
```

Nothing is renamed silently. If a match is a guess, it is reported as one.

## Validation is extensible

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

MIT © Camille Ross
