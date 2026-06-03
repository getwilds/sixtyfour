# Create an S3 bucket

Create an S3 bucket

## Usage

``` r
aws_bucket_create(bucket, ...)
```

## Arguments

- bucket:

  (character) bucket name. required

- ...:

  named parameters passed on to
  [create_bucket](https://www.paws-r-sdk.com/docs/s3_create_bucket/)

## Value

the bucket path (character)

## Note

Requires the env var `AWS_REGION`

## See also

Other buckets:
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
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
bucket2 <- random_bucket()
aws_bucket_create(bucket2)

# cleanup
six_bucket_delete(bucket2, force = TRUE)
}
```
