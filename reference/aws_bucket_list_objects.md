# List objects in an S3 bucket

List objects in an S3 bucket

## Usage

``` r
aws_bucket_list_objects(bucket, ...)
```

## Arguments

- bucket:

  (character) bucket name. required

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

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md),
[`aws_buckets()`](https://getwilds.org/sixtyfour/reference/aws_buckets.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket_name <- random_bucket()
if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
links_file <- file.path(system.file(), "Meta/links.rds")
aws_file_upload(
  links_file,
  s3_path(bucket_name, basename(links_file))
)
aws_bucket_list_objects(bucket = bucket_name)
# cleanup
six_bucket_delete(bucket_name, force = TRUE)
}
```
