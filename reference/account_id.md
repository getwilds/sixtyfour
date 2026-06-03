# Get account ID of current user

Get account ID of current user

## Usage

``` r
account_id()
```

## Value

list with 3 elements:

- UserId: the ID for the user

- Account: account ID the user is in

- Arn: arn for the user

## Details

If env var `AWS_PROFILE` == "localstack", return `"000000000000"`
