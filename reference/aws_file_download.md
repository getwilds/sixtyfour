# Download a file

Download a file

## Usage

``` r
aws_file_download(remote_path, path, ...)
```

## Arguments

- remote_path:

  (character) one or more remote S3 paths. required

- path:

  (character) one or more file paths to write to. required

- ...:

  named parameters passed on to
  [`s3fs::s3_file_download()`](https://rdrr.io/pkg/s3fs/man/download.html)

## Value

(character) a vector of local file paths

## See also

Other files:
[`aws_file_attr()`](https://getwilds.org/sixtyfour/reference/aws_file_attr.md),
[`aws_file_copy()`](https://getwilds.org/sixtyfour/reference/aws_file_copy.md),
[`aws_file_delete()`](https://getwilds.org/sixtyfour/reference/aws_file_delete.md),
[`aws_file_exists()`](https://getwilds.org/sixtyfour/reference/aws_file_exists.md),
[`aws_file_rename()`](https://getwilds.org/sixtyfour/reference/aws_file_rename.md),
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
library(glue)

# single file
bucket1 <- random_bucket()
aws_bucket_create(bucket1)
tfile1 <- tempfile()
remote1 <- s3_path(bucket1, glue("{basename(tfile1)}.txt"))
cat("Hello World!\n", file = tfile1)
aws_file_upload(path = tfile1, remote_path = remote1)
dfile <- tempfile()
aws_file_download(remote_path = remote1, path = dfile)
readLines(dfile)

# many files
bucket2 <- random_bucket()
aws_bucket_create(bucket2)
tfiles <- replicate(n = 3, tempfile())
for (file in tfiles) cat("Hello mars!!!!!!\n", file = file)
for (file in tfiles) print(readLines(file))
for (file in tfiles) {
  aws_file_upload(file, s3_path(bucket2, glue("{basename(file)}.txt")))
}
downloadedfiles <- replicate(n = 3, tempfile())
for (file in downloadedfiles) print(file.exists(file))
remotes2 <- s3_path(bucket2, glue("{basename(tfiles)}.txt"))
aws_file_download(remote_path = remotes2, path = downloadedfiles)
for (file in downloadedfiles) print(readLines(file))

# Cleanup
six_bucket_delete(bucket1, force = TRUE)
six_bucket_delete(bucket2, force = TRUE)
}
```
