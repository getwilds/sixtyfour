# Delete a secret

Delete a secret

## Usage

``` r
aws_secrets_delete(id, ...)
```

## Arguments

- id:

  (character) The name or ARN of the secret. required

- ...:

  further named parameters passed on to `delete_secret`
  <https://www.paws-r-sdk.com/docs/secretsmanager_delete_secret/>

## Value

(list) with fields:

- ARN

- Name

- DeletionDate

## Examples

``` r
if (FALSE) { # aws_has_creds() && interactive()
try({
# Create a secret
secret <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret,
  secret = '{"username":"jill","password":"cow"}',
  description = "The fox jumped over the cow"
)

# Delete a secret
aws_secrets_delete(id = secret, ForceDeleteWithoutRecovery = TRUE)
})
}
```
