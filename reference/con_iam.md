# Get a `paws` client for a service

Get a `paws` client for a service

## Usage

``` r
con_iam()

con_s3()

con_sm()

con_ec2()

con_rds()

con_redshift()

con_ce()
```

## Value

- `con_s3`: a list with methods for interfacing with S3;
  <https://www.paws-r-sdk.com/docs/s3/>

- `con_iam`: a list with methods for interfacing with IAM;
  <https://www.paws-r-sdk.com/docs/iam/>

- `con_sm`: a list with methods for interfacing with Secrets Manager;
  <https://www.paws-r-sdk.com/docs/secretsmanager/>

- `con_ec2`: a list with methods for interfacing with EC2;
  <https://www.paws-r-sdk.com/docs/ec2/>

- `con_rds`: a list with methods for interfacing with RDS;
  <https://www.paws-r-sdk.com/docs/rds/>

- `con_redshift`: a list with methods for interfacing with Redshift;
  <https://www.paws-r-sdk.com/docs/redshift/>

- `con_ce`: a list with methods for interfacing with Cost Explorer;
  <https://www.paws-r-sdk.com/docs/costexplorer/>

## Details

Toggles the credentials used based on the environment variable
`AWS_PROFILE` for one of: minio, localstack, aws.

If `AWS_PROFILE` is "minio" then we set the following in the credentials
for the connection:

- `access_key_id` uses env var `MINIO_USER`, with default "minioadmin"

- `secret_access_key` uses env var `MINIO_PWD`, with default
  "minioadmin"

- `endpoint` uses env var `MINIO_ENDPOINT`, with default
  "http://127.0.0.1:9000"

If `AWS_PROFILE` is "localstack" then we set the following in the
credentials for the connection:

- `access_key_id` uses env var `LOCALSTACK_KEY`, with a default string
  which is essentially ignored. you do not need to set the
  `LOCALSTACK_KEY` env var. However, if you want to set an account ID
  for your Localstack you can set the env var and it will be used. see
  <https://docs.localstack.cloud/references/credentials/>

- `secret_access_key` uses env var `LOCALSTACK_SECRET`, with a default
  string which is ignored; and any value you set for `LOCALSTACK_SECRET`
  will be ignored by Localstack as well. see
  <https://docs.localstack.cloud/references/credentials/>

- `endpoint` uses env var `LOCALSTACK_ENDPOINT`. You can set this to the
  URL for where your Localstack is running at. Default is
  `http://localhost.localstack.cloud:4566`

If `AWS_PROFILE` is not set, set to "aws", or anything else (other than
"localstack") then we don't set any credentials internally, but `paws`
will gather any credentials you've set via env vars, config files, etc.-

## See also

[`con_s3fs()`](https://getwilds.org/sixtyfour/reference/con_s3fs.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
z <- con_iam()
z

withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  con_iam()
)
withr::with_envvar(
  c("AWS_PROFILE" = "minio"),
  con_s3()
)
}
```
