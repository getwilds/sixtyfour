# Check if an S3 bucket exists

Check if an S3 bucket exists

## Usage

``` r
aws_bucket_exists(bucket)
```

## Arguments

- bucket:

  (character) bucket name; must be length 1. required

## Value

a single boolean (logical)

## Note

internally uses
[head_bucket](https://www.paws-r-sdk.com/docs/s3_head_bucket/)

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md),
[`aws_buckets()`](https://getwilds.org/sixtyfour/reference/aws_buckets.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket1 <- random_bucket()
aws_bucket_create(bucket1)

# exists
aws_bucket_exists(bucket = bucket1)
# does not exist
aws_bucket_exists(bucket = "no-bucket")

# cleanup
six_bucket_delete(bucket1, force = TRUE)
}
```
