# File attributes

File attributes

## Usage

``` r
aws_file_attr(remote_path)
```

## Arguments

- remote_path:

  (character) one or more remote S3 paths. required

## Value

a tibble with many columns, with number of rows matching length of
`remote_path`

## Note

uses [`s3fs::s3_file_info()`](https://rdrr.io/pkg/s3fs/man/info.html)
internally

## See also

Other files:
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_download()`](https://getwilds.org/sixtyfour/reference/aws_file_download.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
library(glue)
bucket <- random_bucket()
if (!aws_bucket_exists(bucket)) {
  aws_bucket_create(bucket)
}

# upload some files
tfiles <- replicate(n = 3, tempfile())
paths <- s3_path(bucket, glue("{basename(tfiles)}.txt"))
for (file in tfiles) cat("Hello saturn!!!!!!\n", file = file)
for (file in tfiles) print(readLines(file))
aws_file_upload(path = tfiles, remote_path = paths)

# files one by one
aws_file_attr(paths[1])
aws_file_attr(paths[2])
aws_file_attr(paths[3])
# or all together
aws_file_attr(paths)

# Cleanup
six_bucket_delete(bucket, force = TRUE)
}
```
