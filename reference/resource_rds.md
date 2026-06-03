# Create a resource string for a policy statement for RDS

Create a resource string for a policy statement for RDS

## Usage

``` r
resource_rds(
  user,
  resource_id,
  region = Sys.getenv("AWS_REGION"),
  account = account_id()
)
```

## Arguments

- user:

  (character) a user name that has an IAM account. length\>=1. required

- resource_id:

  (character) the identifier for the DB instance. length==1. required

- region:

  (character) the AWS Region for the DB instance. length==1

- account:

  (character) the AWS account number for the DB instance. length==1. The
  user must be in the same account as the account for the DB instance.
  by default calls
  [`account_id()`](https://getwilds.org/sixtyfour/reference/account_id.md)

## Value

a resource ARN (scalar, character)
