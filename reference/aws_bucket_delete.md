# Delete an S3 bucket

Delete an S3 bucket

## Usage

``` r
aws_bucket_delete(bucket, force = FALSE, ...)
```

## Arguments

- bucket:

  (character) bucket name. required

- force:

  (logical) force deletion without going through the prompt. default:
  `FALSE`. Should only be set to `TRUE` when required for
  non-interactive use.

- ...:

  named parameters passed on to
  [delete_bucket](https://www.paws-r-sdk.com/docs/s3_delete_bucket/)

## Value

`NULL`, invisibly

## Note

Requires the env var `AWS_REGION`. This function prompts you to make
sure that you want to delete the bucket.

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md),
[`aws_buckets()`](https://getwilds.org/sixtyfour/reference/aws_buckets.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket_name <- random_bucket()
if (!aws_bucket_exists(bucket_name)) {
  aws_bucket_create(bucket = bucket_name)
  aws_buckets()
  aws_bucket_delete(bucket = bucket_name, force = TRUE)
  aws_buckets()
}
}
```
