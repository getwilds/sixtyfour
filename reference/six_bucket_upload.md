# Magically upload a mix of files and directories into a bucket

Magically upload a mix of files and directories into a bucket

## Usage

``` r
six_bucket_upload(path, remote, force = FALSE, ...)
```

## Arguments

- path:

  (character) one or more file paths to add to the `bucket`. required.
  can include directories or files

- remote:

  (character/scalar) a character string to use to upload files in
  `path`. the first component of the path will be used as the bucket
  name. any subsequent path components will be used as a key prefix for
  all objects created in the bucket

- force:

  (logical) force bucket creation without going through the prompt.
  default: `FALSE`. Should only be set to `TRUE` when required for
  non-interactive use.

- ...:

  named params passed on to
  [put_object](https://www.paws-r-sdk.com/docs/s3_put_object/)

## Value

(character) a vector of remote s3 paths where your files are located

## What is magical

- Exits early if folder or files do not exist

- Creates the bucket if it does not exist

- Adds files to the bucket at the top level with key as the file name

- Adds directories to the bucket, reconstructing the exact directory
  structure in the S3 bucket

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
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md)

Other magicians:
[`six_admin_setup()`](https://getwilds.org/sixtyfour/reference/six_admin_setup.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
# single file, single remote path
bucket1 <- random_bucket()
demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
six_bucket_upload(path = demo_rds_file, remote = bucket1, force = TRUE)

## a file and a directory - with a single remote path
bucket2 <- random_bucket()
library(fs)
tdir <- path(path_temp(), "mytmp")
dir_create(tdir)
invisible(purrr::map(letters, \(l) file_create(path(tdir, l))))
dir_tree(tdir)
six_bucket_upload(path = c(demo_rds_file, tdir), remote = bucket2,
force = TRUE)

## a directory with nested dirs - with a single remote path
bucket3 <- random_bucket()
library(fs)
tdir <- path(path_temp(), "apples")
dir_create(tdir)
dir_create(path(tdir, "mcintosh"))
dir_create(path(tdir, "pink-lady"))
cat("Some text in a readme", file = path(tdir, "README.md"))
write.csv(Orange, file = path(tdir, "mcintosh", "orange.csv"))
write.csv(iris, file = path(tdir, "pink-lady", "iris.csv"))
dir_tree(tdir)
six_bucket_upload(path = tdir, remote = path(bucket3, "fruit/basket"),
force = TRUE)

# cleanup
six_bucket_delete(bucket1, force = TRUE)
six_bucket_delete(bucket2, force = TRUE)
six_bucket_delete(bucket3, force = TRUE)
}
```
