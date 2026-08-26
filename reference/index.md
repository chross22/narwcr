# Package index

## Reading the database

Reading a NARWC extract and putting it into one predictable shape,
reconciling the column names different survey programmes use for the
same thing.

- [`narwc_cloud_roots()`](https://camilleross.org/narwcr/reference/narwc_cloud_roots.md)
  : Where OneDrive and Google Drive sync to
- [`narwc_codes()`](https://camilleross.org/narwcr/reference/narwc_codes.md)
  : NARWC database code books
- [`narwc_column_mapping()`](https://camilleross.org/narwcr/reference/narwc_column_mapping.md)
  : What the column names were changed to
- [`narwc_fetch()`](https://camilleross.org/narwcr/reference/narwc_fetch.md)
  : Fetch a survey file from cloud storage
- [`narwc_profiles()`](https://camilleross.org/narwcr/reference/narwc_profiles.md)
  : Survey-programme column profiles
- [`narwc_schema()`](https://camilleross.org/narwcr/reference/narwc_schema.md)
  : NARWC columns recognised by narwcr
- [`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)
  : Read NARWC-format survey data
- [`standardize_narwc_columns()`](https://camilleross.org/narwcr/reference/standardize_narwc_columns.md)
  : Standardise column names onto the NARWC handbook's

## Filling and validating

Checks that report rather than stop, and the fills that are safe to
apply. Some columns must never be carried forward; narwc_never_fill()
says which.

- [`fill_legstage()`](https://camilleross.org/narwcr/reference/fill_legstage.md)
  : Reconstruct the line state on records that record no LEGSTAGE
- [`fill_narwc()`](https://camilleross.org/narwcr/reference/fill_narwc.md)
  : Carry survey state forward into blank rows
- [`narwc_checks()`](https://camilleross.org/narwcr/reference/narwc_checks.md)
  : The handbook-general check set
- [`narwc_fill_columns()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md)
  [`narwc_never_fill()`](https://camilleross.org/narwcr/reference/narwc_fill_columns.md)
  : Columns that may be carried forward, and columns that must not be
- [`narwc_finding()`](https://camilleross.org/narwcr/reference/narwc_finding.md)
  : Record one validation finding
- [`narwc_strip_bins()`](https://camilleross.org/narwcr/reference/narwc_strip_bins.md)
  : NARWC STRIP right-angle distance code books
- [`validate_narwc()`](https://camilleross.org/narwcr/reference/validate_narwc.md)
  : Check survey data against the NARWC handbook

## Effort and platform

Deriving what the survey was doing from what it recorded: on or off
effort, from which platform, at what speed, and whether conditions
allowed a sighting.

- [`angles_from_declination()`](https://camilleross.org/narwcr/reference/angles_from_declination.md)
  : Build ANGLEL and ANGLER from one declination column and a side
- [`classify_platform()`](https://camilleross.org/narwcr/reference/classify_platform.md)
  : Label each record's platform from how fast it was moving
- [`flag_effort()`](https://camilleross.org/narwcr/reference/flag_effort.md)
  : Flag on-effort records
- [`make_leg_id()`](https://camilleross.org/narwcr/reference/make_leg_id.md)
  : Identify separate occupations of a survey line
- [`on_effort_census_rows()`](https://camilleross.org/narwcr/reference/on_effort_census_rows.md)
  : Which records are eligible for a right-angle distance?
- [`track_speed()`](https://camilleross.org/narwcr/reference/track_speed.md)
  : Speed implied by consecutive position fixes
- [`visibility_ok()`](https://camilleross.org/narwcr/reference/visibility_ok.md)
  : Was visibility acceptable?

## Looking at it

An interactive map of an extract: sightings, effort and acoustic
stations over a chosen time period, with the effort criteria as controls
rather than as constants.

- [`run_narwc_app()`](https://camilleross.org/narwcr/reference/run_narwc_app.md)
  : Look at a NARWC extract on a map
