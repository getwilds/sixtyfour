# Upload a file

Upload a file

## Usage

``` r
aws_file_upload(path, remote_path, ...)
```

## Arguments

- path:

  (character) a file path to read from. required

- remote_path:

  (character) a remote path where the file should go. required

- ...:

  named parameters passed on to
  [`s3fs::s3_file_copy()`](https://rdrr.io/pkg/s3fs/man/copy.html)

## Value

(character) a vector of remote s3 paths

## Details

to upload a folder of files see
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md)

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket1 <- random_bucket()
aws_bucket_create(bucket1)
cat(bucket1)
demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
aws_file_upload(
  demo_rds_file,
  s3_path(bucket1, basename(demo_rds_file))
)

## many files at once
bucket2 <- random_bucket()
if (!aws_bucket_exists(bucket2)) {
  aws_bucket_create(bucket2)
}
cat(bucket2)
links_file <- file.path(system.file(), "Meta/links.rds")
aws_file_upload(
  c(demo_rds_file, links_file),
  s3_path(bucket2, c(basename(demo_rds_file), basename(links_file))),
  overwrite = TRUE
)

# set expiration, expire 1 minute from now
aws_file_upload(demo_rds_file, s3_path(bucket2, "ddd.rds"),
  Expires = Sys.time() + 60,
  overwrite = TRUE
)

# bucket doesn't exist
try(aws_file_upload(demo_rds_file, "s3://not-a-bucket/eee.rds"))

# path doesn't exist
try(
  aws_file_upload(
    "file_doesnt_exist.txt",
    s3_path(bucket2, "file_doesnt_exist.txt")
  )
)

# Path's without file extensions behave a little weird
## With extension
bucket3 <- random_bucket()
if (!aws_bucket_exists(bucket3)) {
  aws_bucket_create(bucket3)
}
## Both the next two lines do the same exact thing: make a file in the
## same path in a bucket
pkg_rds_file <- file.path(system.file(), "Meta/package.rds")
aws_file_upload(pkg_rds_file, s3_path(bucket3, "package2.rds"),
 overwrite = TRUE)
aws_file_upload(pkg_rds_file, s3_path(bucket3),
 overwrite = TRUE)

## Without extension
## However, it's different for a file without an extension
## This makes a file in the bucket at path DESCRIPTION
rd_file <- file.path(system.file(), "Meta/Rd.rds")
desc_file <- system.file("DESCRIPTION", package = "sixtyfour")
aws_file_upload(desc_file, s3_path(bucket3), overwrite = TRUE)

## Whereas this creates a directory called DESCRIPTION with
## a file DESCRIPTION within it
aws_file_upload(desc_file, s3_path(bucket3, "DESCRIPTION"),
  overwrite = TRUE)

# Cleanup
six_bucket_delete(bucket1, force = TRUE)
six_bucket_delete(bucket2, force = TRUE)
six_bucket_delete(bucket3, force = TRUE)
}
```
