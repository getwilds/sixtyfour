# Copy files between buckets

Copy files between buckets

## Usage

``` r
aws_file_copy(remote_path, bucket, force = FALSE, ...)
```

## Arguments

- remote_path:

  (character) one or more remote S3 paths. required

- bucket:

  (character) bucket to copy files to. required. if the bucket does not
  exist we prompt you asking if you'd like the bucket to be created

- force:

  (logical) force bucket creation without going through the prompt.
  default: `FALSE`. Should only be set to `TRUE` when required for
  non-interactive use.

- ...:

  named parameters passed on to
  [`s3fs::s3_file_copy()`](https://rdrr.io/pkg/s3fs/man/copy.html)

## Value

vector of paths, length matches `length(remote_path)`

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket1 <- random_bucket()
aws_bucket_create(bucket1)

# create files in an existing bucket
tfiles <- replicate(n = 3, tempfile())
for (i in tfiles) cat("Hello\nWorld\n", file = i)
paths <- s3_path(bucket1, c("aaa", "bbb", "ccc"), ext = "txt")
aws_file_upload(tfiles, paths)

# create a new bucket
bucket2 <- random_bucket()
new_bucket <- aws_bucket_create(bucket = bucket2)

# add existing files to the new bucket
aws_file_copy(paths, bucket2)

# or, create a bucket that doesn't exist yet
bucket3 <- random_bucket()
aws_file_copy(paths, bucket3, force = TRUE)

# Cleanup
six_bucket_delete(bucket1, force = TRUE)
six_bucket_delete(bucket2, force = TRUE)
six_bucket_delete(bucket3, force = TRUE)
}
```
