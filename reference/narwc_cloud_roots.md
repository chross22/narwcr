# Where OneDrive and Google Drive sync to

The conventional sync-root locations for this platform, and whether each
is present. Most cloud-stored survey data is reachable as an ordinary
file path, and this is how you find it.

## Usage

``` r
narwc_cloud_roots()
```

## Value

A tibble with `service`, `path`, and `exists`.

## Nothing is searched for

Only the documented locations are checked — this does not scan your home
directory. If your sync root is somewhere else, pass its path directly;
every function here takes an ordinary path.

On macOS both clients now sync under `~/Library/CloudStorage`, with a
folder per account (`OneDrive-UniversityofMaine`,
`GoogleDrive-you@example.com`). The older `~/OneDrive` and
`~/Google Drive` locations are still checked because existing
installations keep them.

## See also

[`narwc_fetch()`](https://camilleross.org/narwcr/reference/narwc_fetch.md),
[`read_narwc()`](https://camilleross.org/narwcr/reference/read_narwc.md)

## Examples

``` r
narwc_cloud_roots()
#> # A tibble: 3 × 3
#>   service      path                               exists
#>   <chr>        <chr>                              <lgl> 
#> 1 OneDrive     /home/runner/OneDrive              FALSE 
#> 2 Google Drive /home/runner/Google Drive          FALSE 
#> 3 Google Drive /home/runner/Google Drive/My Drive FALSE 

# Only the ones actually present
subset(narwc_cloud_roots(), exists)
#> # A tibble: 0 × 3
#> # ℹ 3 variables: service <chr>, path <chr>, exists <lgl>
```
