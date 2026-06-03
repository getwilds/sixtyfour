# Get a random password

Get a random password

## Usage

``` r
aws_secrets_pwd(...)
```

## Arguments

- ...:

  named parameters passed on to `get_random_password`
  <https://www.paws-r-sdk.com/docs/secretsmanager_get_random_password/>

## Value

a single string, of length 40

## Details

The parameter `PasswordLength` is hard coded to `40L`

## Examples

``` r
if (FALSE) { # aws_has_creds() && interactive()
aws_secrets_pwd()
aws_secrets_pwd(ExcludeNumbers = TRUE)
}
```
