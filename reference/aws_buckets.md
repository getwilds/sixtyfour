# List S3 buckets

List S3 buckets

## Usage

``` r
aws_buckets(...)
```

## Arguments

- ...:

  named parameters passed on to
  [list_objects](https://www.paws-r-sdk.com/docs/s3_list_objects/)

## Value

if no objects found, an empty tibble. if tibble has rows each is an S3
bucket, with 8 columns:

- bucket_name (character)

- key (character)

- uri (character)

- size (fs::bytes)

- type (character)

- owner (character)

- etag (character)

- last_modified (dttm)

## Details

internally uses
[`s3fs::s3_dir_info()`](https://rdrr.io/pkg/s3fs/man/info.html)

## Note

we set `refresh=TRUE` internally to make sure we return up to date
information about your buckets rather than what's cached locally

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
aws_buckets()
}
```
