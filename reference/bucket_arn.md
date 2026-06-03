# Get bucket ARN

Get bucket ARN

## Usage

``` r
bucket_arn(bucket, objects = "")
```

## Arguments

- bucket:

  (character) a bucket name. required.

- objects:

  (character) path for object(s). default: `""`

## Value

character string of bucket arn

## Examples

``` r
bucket_arn("somebucket")
#> arn:aws:s3:::somebucket
bucket_arn("somebucket", objects = "*")
#> arn:aws:s3:::somebucket/*
bucket_arn("somebucket", objects = "data.csv")
#> arn:aws:s3:::somebucket/data.csv
bucket_arn("somebucket", objects = "myfolder/subset/data.csv")
#> arn:aws:s3:::somebucket/myfolder/subset/data.csv
bucket_arn("somebucket", objects = "myfolder/subset/*")
#> arn:aws:s3:::somebucket/myfolder/subset/*
```
