# s3fs connection

s3fs connection

## Usage

``` r
con_s3fs()
```

## Value

An S3 list with class 'sixtyfour_client'

## Details

we set `refresh=TRUE` on
[`s3fs::s3_file_system()`](https://rdrr.io/pkg/s3fs/man/s3_file_system.html)
so that you can change the s3 interface within an R session

You can toggle the interface set for one of minio, localstack, aws. See
[connections](https://rdrr.io/r/base/connections.html) for more
information.

## See also

[paws_clients](https://getwilds.org/sixtyfour/reference/con_iam.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
con <- con_s3fs()
con
con_s3fs()$file_copy
}
```
