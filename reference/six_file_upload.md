# Magically upload a file

Magically upload a file

## Usage

``` r
six_file_upload(path, bucket, force = FALSE, ...)
```

## Arguments

- path:

  (character) one or more file paths to add to the `bucket`. required.
  cannot include directories

- bucket:

  (character) bucket to copy files to. required. if the bucket does not
  exist we prompt you asking if you'd like the bucket to be created

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

- Exits early if files do not exist

- Exits early if any `path` values are directories

- Creates the bucket if it does not exist

- Adds files to the bucket, figuring out the key to use from the
  supplied path

- Function is vectoried for the `path` argument; you can pass in many
  file paths

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md)

Other magicians:
[`six_admin_setup()`](https://getwilds.org/sixtyfour/reference/six_admin_setup.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket1 <- random_bucket()
demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
six_file_upload(demo_rds_file, bucket1, force = TRUE)

# path doesn't exist, error
try(
  six_file_upload("file_doesnt_exist.txt", random_bucket())
)

# directories not supported, error
mydir <- tempdir()
try(
  six_file_upload(mydir, random_bucket())
)

# Cleanup
six_bucket_delete(bucket1, force = TRUE)
}
if (FALSE) { # interactive() && aws_has_creds()
# requires user interaction with prompts ...
bucket2 <- random_bucket()
demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
six_file_upload(demo_rds_file, bucket2)

## many files at once
links_file <- file.path(system.file(), "Meta/links.rds")
six_file_upload(c(demo_rds_file, links_file), bucket2)

# set expiration, expire 1 minute from now
six_file_upload(demo_rds_file, bucket2, Expires = Sys.time() + 60)

# bucket doesn't exist, ask if you want to create it
not_a_bucket <- random_string("not-a-bucket-")
six_file_upload(demo_rds_file, not_a_bucket)

# Cleanup
six_bucket_delete(bucket2, force = TRUE)
six_bucket_delete(not_a_bucket, force = TRUE)
}
```
