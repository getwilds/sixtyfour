# Delete a file

Delete a file

## Usage

``` r
aws_file_delete(remote_path, ...)
```

## Arguments

- remote_path:

  (character) one or more remote S3 paths. required

- ...:

  named parameters passed on to
  [delete_object](https://www.paws-r-sdk.com/docs/s3_delete_object/)

## Value

`NULL` invisibly

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
# create a file
bucket <- random_bucket()
aws_bucket_create(bucket)
tfile <- tempfile()
cat("Hello World!\n", file = tfile)
aws_file_upload(path = tfile, remote_path = s3_path(bucket))

# delete the file
aws_file_delete(s3_path(bucket, basename(tfile)))

# file does not exist - no error is raised
aws_file_delete(s3_path(bucket, "TESTING123"))

# Cleanup
six_bucket_delete(bucket, force = TRUE)
}
```
