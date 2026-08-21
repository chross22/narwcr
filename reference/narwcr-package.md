# narwcr: Read and Standardise North Atlantic Right Whale Consortium Survey Data

Reads marine mammal survey data recorded in the North Atlantic Right
Whale Consortium (NARWC) sightings database format and puts it into a
single, predictable shape. Matches the column names that different
survey programmes actually use against one shared vocabulary and reports
every rename it made, fills the fields the archive records once per leg
rather than once per row, resolves on-effort records and leg identity
from the handbook code books, validates a table against the handbook's
own rules with an extensible set of checks, and fetches extracts from
cloud storage. Intended as the common data preparation layer beneath
analysis packages that diverge only in their modelling methods.

## References

The data format and survey protocol:

Kenney, R.D. (2023) *The North Atlantic Right Whale Consortium Database:
A Guide for Users and Contributors, Version 8*. NARWC Reference Document
2023-01. University of Rhode Island, Graduate School of Oceanography,
Narragansett, RI.

CETAP (1982) *A Characterization of Marine Mammals and Turtles in the
Mid- and North-Atlantic Areas of the U.S. Outer Continental Shelf, Final
Report.* Cetacean and Turtle Assessment Program, University of Rhode
Island. Bureau of Land Management, Washington, DC.

Kenney, R.D. and Winn, H.E. (1986) Cetacean high-use habitats of the
northeast United States continental shelf. *Fishery Bulletin*
84(2):345-357.

## See also

Useful links:

- <https://github.com/chross22/narwcr>

- Report bugs at <https://github.com/chross22/narwcr/issues>

## Author

**Maintainer**: Camille Ross <camille.ross@maine.edu>
([ORCID](https://orcid.org/0000-0002-1428-2294))

Authors:

- Camille Ross <camille.ross@maine.edu>
  ([ORCID](https://orcid.org/0000-0002-1428-2294))
