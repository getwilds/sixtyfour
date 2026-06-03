# Upload a folder of files to create an S3 bucket

Upload a folder of files to create an S3 bucket

## Usage

``` r
aws_bucket_upload(
  path,
  bucket,
  max_batch = fs::fs_bytes("100MB"),
  force = FALSE,
  ...
)
```

## Arguments

- path:

  (character) local path to a directory. required

- bucket:

  (character) bucket name. required

- max_batch:

  (fs_bytes) maximum batch size being uploaded with each multipart

- force:

  (logical) force deletion without going through the prompt. default:
  `FALSE`. Should only be set to `TRUE` when required for
  non-interactive use.

- ...:

  named parameters passed on to
  [`s3fs::s3_dir_upload()`](https://rdrr.io/pkg/s3fs/man/upload.html)

## Value

the s3 format path of the bucket uploaded to

## Details

To upload individual files see
[`aws_file_upload()`](https://getwilds.org/sixtyfour/reference/aws_file_upload.md)

## Note

Requires the env var `AWS_REGION`. This function prompts you to make
sure that you want to delete the bucket.

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_download()`](https://getwilds.org/sixtyfour/reference/aws_bucket_download.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_buckets()`](https://getwilds.org/sixtyfour/reference/aws_buckets.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
library(fs)
tdir <- path(tempdir(), "apples")
dir.create(tdir)
tfiles <- replicate(n = 10, file_temp(tmp_dir = tdir, ext = ".txt"))
invisible(lapply(tfiles, function(x) write.csv(mtcars, x)))

bucket_name <- random_bucket()
if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
aws_bucket_upload(path = tdir, bucket = bucket_name)
aws_bucket_list_objects(bucket_name)

# cleanup
objs <- aws_bucket_list_objects(bucket_name)
aws_file_delete(objs$uri)
aws_bucket_list_objects(bucket_name)
aws_bucket_delete(bucket_name, force = TRUE)
aws_bucket_exists(bucket_name)
}
```
