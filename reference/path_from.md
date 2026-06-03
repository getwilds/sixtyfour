# Get file path starting at a certain path component

Get file path starting at a certain path component

## Usage

``` r
path_from(path, from)
```

## Value

a single file path of class `fs_path`/`character`

## Examples

``` r
path_from(path = "Rtmpxsqth0/apples/mcintosh/orange.csv", from = "apples")
#> apples/mcintosh/orange.csv
```
