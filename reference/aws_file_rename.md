# Rename remote files

Rename remote files

## Usage

``` r
aws_file_rename(remote_path, new_remote_path, ...)
```

## Arguments

- remote_path:

  (character) one or more remote S3 paths. required

- new_remote_path:

  (character) one or more remote S3 paths. required. length must match
  `remote_path`

- ...:

  named parameters passed on to
  [`s3fs::s3_file_move()`](https://rdrr.io/pkg/s3fs/man/s3_file_move.html)

## Value

vector of paths, length matches `length(remote_path)`

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket <- random_bucket()
aws_bucket_create(bucket)

# rename files
tfiles <- replicate(n = 3, tempfile())
for (i in tfiles) cat("Hello\nWorld\n", file = i)
paths <- s3_path(bucket, c("aaa", "bbb", "ccc"), ext = "txt")
aws_file_upload(tfiles, paths)
new_paths <- s3_path(bucket, c("new_aaa", "new_bbb", "new_ccc"),
  ext = "txt"
)
aws_file_rename(paths, new_paths)

# Cleanup
six_bucket_delete(bucket, force = TRUE)
}
```
