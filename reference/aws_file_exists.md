# Check if a file exists

Check if a file exists

## Usage

``` r
aws_file_exists(remote_path)
```

## Arguments

- remote_path:

  (character) one or more remote S3 paths. required

## Value

vector of booleans (`TRUE` or `FALSE`), length matches
`length(remote_path)`

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
library(glue)
bucket <- random_bucket()
aws_bucket_create(bucket)

# upload some files
tfiles <- replicate(n = 3, tempfile())
paths <- s3_path(bucket, glue("{basename(tfiles)}.txt"))
for (file in tfiles) cat("Hello saturn!!!!!!\n", file = file)
for (file in tfiles) print(readLines(file))
aws_file_upload(path = tfiles, remote_path = paths)

# check that files exist
aws_file_exists(paths[1])
aws_file_exists(paths[2])
aws_file_exists(s3_path(bucket, "doesnotexist.txt"))

# Cleanup
six_bucket_delete(bucket, force = TRUE)
}
```
