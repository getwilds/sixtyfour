# S3 actions for reading, from the AWS managed policy `AmazonS3ReadOnlyAccess`

S3 actions for reading, from the AWS managed policy
`AmazonS3ReadOnlyAccess`

## Usage

``` r
s3_actions_read()
```

## Value

character vector of actions

## Examples

``` r
s3_actions_read()
#> [1] "s3:Get*"                "s3:List*"               "s3:Describe*"          
#> [4] "s3-object-lambda:Get*"  "s3-object-lambda:List*"
```
