# Update a secret

Update a secret

## Usage

``` r
aws_secrets_update(id, secret, ...)
```

## Arguments

- id:

  (character) The name or ARN of the secret. required

- secret:

  (character/raw) The text or raw data to encrypt and store in this new
  version of the secret. AWS recommends for text to use a JSON structure
  of key/value pairs for your secret value (see examples below).
  required

- ...:

  further named parameters passed on to `put_secret_value`
  <https://www.paws-r-sdk.com/docs/secretsmanager_put_secret_value/>

## Value

(list) with fields:

- ARN

- Name

- VersionId

- VersionStages

## Details

Note that we autogenerate a random UUID to pass to the
`ClientRequestToken` parameter of the `paws` function used internally

## Examples

``` r
if (FALSE) { # aws_has_creds() && interactive()
try({
# Create a secret
secret <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret,
  secret = '{"username":"debby","password":"kitty"}',
  description = "A string"
)

aws_secrets_get(secret)

# Update the secret
aws_secrets_update(
  id = secret,
  secret = '{"username":"debby","password":"kitten"}'
)

aws_secrets_get(secret)

# Cleanup
aws_secrets_delete(secret, ForceDeleteWithoutRecovery = TRUE)
})
}
```
