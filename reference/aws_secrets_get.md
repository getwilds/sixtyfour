# Get a secret

Get a secret

## Usage

``` r
aws_secrets_get(id, ...)
```

## Arguments

- id:

  (character) The name or ARN of the secret. required

- ...:

  further named parameters passed on to `get_secret_value`
  <https://www.paws-r-sdk.com/docs/secretsmanager_get_secret_value/>

## Value

(list) with fields:

- ARN

- Name

- VersionId

- SecretBinary

- SecretString

- VersionStages

- CreatedDate

## Examples

``` r
if (FALSE) { # aws_has_creds() && interactive()
try({
# Create a secret
secret <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret,
  secret = '{"username":"jane","password":"cat"}',
  description = "A string"
)

aws_secrets_get(secret)

# Does exist
aws_secrets_get(id = "MyTestDatabaseSecret")

# Does not exist
try(aws_secrets_get(id = "DoesntExist"))

# Cleanup
aws_secrets_delete(secret, ForceDeleteWithoutRecovery = TRUE)
})
}
```
