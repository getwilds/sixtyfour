# S3 actions for full access (read and write), from the AWS managed policy `AmazonS3FullAccess`

S3 actions for full access (read and write), from the AWS managed policy
`AmazonS3FullAccess`

## Usage

``` r
s3_actions_full()
```

## Value

character vector of actions

## Examples

``` r
s3_actions_full()
#> [1] "s3:*"               "s3-object-lambda:*"
```
