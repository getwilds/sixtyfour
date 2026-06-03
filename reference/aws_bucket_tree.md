# Print a tree of the objects in a bucket

Print a tree of the objects in a bucket

## Usage

``` r
aws_bucket_tree(bucket, recurse = TRUE, ...)
```

## Arguments

- bucket:

  (character) bucket name; must be length 1. required

- recurse:

  (logical) returns all AWS S3 objects in lower sub directories,
  default: `TRUE`

- ...:

  Additional arguments passed to
  [`s3fs::s3_dir_tree()`](https://rdrr.io/pkg/s3fs/man/s3_dir_tree.html)

## Value

character vector of objects/files within the bucket, printed as a tree

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
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
pkgs_file <- file.path(system.file(), "Meta/package.rds")
demo_file <- file.path(system.file(), "Meta/demo.rds")
aws_file_upload(
  c(links_file, pkgs_file, demo_file),
  s3_path(
    bucket_name,
    c(
      basename(links_file),
      basename(pkgs_file),
      basename(demo_file)
    )
  )
)
aws_bucket_tree(bucket_name)

# cleanup
objs <- aws_bucket_list_objects(bucket_name)
aws_file_delete(objs$uri)
aws_bucket_delete(bucket_name, force = TRUE)
aws_bucket_exists(bucket_name)
}
```
