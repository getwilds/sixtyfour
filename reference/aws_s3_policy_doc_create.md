# Create a policy document for an S3 bucket

Create a policy document for an S3 bucket

## Usage

``` r
aws_s3_policy_doc_create(
  bucket,
  action,
  resource,
  effect = "Allow",
  sid = NULL,
  ...
)
```

## Arguments

- bucket:

  (character) bucket name. required

- action:

  (character) an action. required. see Actions below.

- resource:

  (character) the object or objects the statement covers; see link below
  for more information

- effect:

  (character) valid values: "Allow" (default), "Deny". length==1

- sid:

  (character) a statement id. optional

- ...:

  Additional named arguments. See link in Details for options, and
  examples below

## Value

a policy document as JSON (of class `json`)

## Details

There's this separate function for creating policy docs for S3 because
buckets are globally unique, so AWS figures out the region and account
ID for you.

## Examples

``` r
bucket <- random_bucket()
aws_s3_policy_doc_create(
  bucket = bucket,
  action = s3_actions_read(),
  resource = c(bucket_arn(bucket), bucket_arn(bucket, objects = "*"))
)
#> {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["s3:Get*","s3:List*","s3:Describe*","s3-object-lambda:Get*","s3-object-lambda:List*"],"Resource":["arn:aws:s3:::bucket-mwlefoudviyphzbg","arn:aws:s3:::bucket-mwlefoudviyphzbg/*"]}]} 
```
