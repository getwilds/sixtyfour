# Download an S3 bucket

Download an S3 bucket

## Usage

``` r
aws_bucket_download(bucket, dest_path, ...)
```

## Arguments

- bucket:

  (character) bucket name. required

- dest_path:

  (character) destination directory to store files. required

- ...:

  named parameters passed on to
  [`s3fs::s3_dir_download()`](https://rdrr.io/pkg/s3fs/man/download.html)

## Value

path (character) to downloaded file(s)/directory

## Note

Requires the env var `AWS_REGION`. This function prompts you to make
sure that you want to delete the bucket.

## See also

Other buckets:
[`aws_bucket_create()`](https://getwilds.org/sixtyfour/reference/aws_bucket_create.md),
[`aws_bucket_delete()`](https://getwilds.org/sixtyfour/reference/aws_bucket_delete.md),
[`aws_bucket_exists()`](https://getwilds.org/sixtyfour/reference/aws_bucket_exists.md),
[`aws_bucket_list_objects()`](https://getwilds.org/sixtyfour/reference/aws_bucket_list_objects.md),
[`aws_bucket_tree()`](https://getwilds.org/sixtyfour/reference/aws_bucket_tree.md),
[`aws_bucket_upload()`](https://getwilds.org/sixtyfour/reference/aws_bucket_upload.md),
[`aws_buckets()`](https://getwilds.org/sixtyfour/reference/aws_buckets.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
bucket <- random_bucket()
aws_bucket_create(bucket = bucket)
desc_file <- file.path(system.file(), "DESCRIPTION")
aws_file_upload(desc_file, s3_path(bucket, "DESCRIPTION.txt"))
aws_file_upload(desc_file, s3_path(bucket, "d_file.txt"))
temp_dir <- file.path(tempdir(), bucket)
aws_bucket_download(bucket = bucket, dest_path = temp_dir)
fs::dir_ls(temp_dir)

# cleanup
six_bucket_delete(bucket, force = TRUE)
}
```
