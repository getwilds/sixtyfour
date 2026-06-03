# Delete an S3 bucket

Takes care of deleting bucket objects, so that the bucket itself can be
deleted cleanly

## Usage

``` r
six_bucket_delete(bucket, force = FALSE, ...)
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

## What is magical

- Exits early if bucket does not exist

- Checks for any objects in the bucket and deletes any present

- Deletes bucket after deleting objects

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md),
[`aws_buckets()`](https://getwilds.org/sixtyfour/reference/aws_buckets.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

Other magicians:
[`six_admin_setup()`](https://getwilds.org/sixtyfour/reference/six_admin_setup.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
# bucket does not exist
six_bucket_delete("notabucket")

# bucket exists w/o objects
bucket <- random_bucket()
aws_bucket_create(bucket)
six_bucket_delete(bucket, force = TRUE)

# bucket exists w/ objects (files and directories with files)
bucket <- random_bucket()
aws_bucket_create(bucket)
demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
links_file <- file.path(system.file(), "Meta/links.rds")
aws_file_upload(
  c(demo_rds_file, links_file),
  s3_path(bucket, c(basename(demo_rds_file), basename(links_file)))
)
aws_file_upload(
  c(demo_rds_file, links_file),
  s3_path(
    bucket, "newfolder",
    c(basename(demo_rds_file), basename(links_file))
  )
)
aws_bucket_list_objects(bucket)
six_bucket_delete(bucket, force = TRUE)
}
```
