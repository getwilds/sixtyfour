# Create a secret

This function does not create your database username and/or password.
Instead, it creates a "secret", which is typically a combination of
credentials (username + password + other metadata)

## Usage

``` r
aws_secrets_create(name, secret, description = NULL, ...)
```

## Arguments

- name:

  (character) The name of the new secret. required

- secret:

  (character/raw) The text or raw data to encrypt and store in this new
  version of the secret. AWS recommends for text to use a JSON structure
  of key/value pairs for your secret value (see examples below).
  required

- description:

  (character) The description of the secret. optional

- ...:

  further named parameters passed on to `create_secret`
  <https://www.paws-r-sdk.com/docs/secretsmanager_create_secret/>

## Value

(list) with fields:

- ARN

- Name

- VersionId

- ReplicationStatus

## Details

Note that we autogenerate a random UUID to pass to the
`ClientRequestToken` parameter of the `paws` function `create_secret`
used internally in this function.

This function creates a new secret. See
[`aws_secrets_update()`](https://getwilds.org/sixtyfour/reference/aws_secrets_update.md)
to update an existing secret. This function fails if you call it with an
existing secret with the same name or ARN

## Examples

``` r
if (FALSE) { # aws_has_creds() && interactive()
try({
# Text secret
secret1 <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret1,
  secret = '{"username":"david","password":"EXAMPLE-PASSWORD"}',
  description = "My test database secret as a string"
)
aws_secrets_get(secret1)$SecretString

# Raw secret
secret2 <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret2,
  secret = charToRaw('{"username":"david","password":"EXAMPLE-PASSWORD"}'),
  description = "My test database secret as raw"
)
aws_secrets_get(secret2)$SecretBinary

# Cleanup
aws_secrets_delete(secret1, ForceDeleteWithoutRecovery = TRUE)
aws_secrets_delete(secret2, ForceDeleteWithoutRecovery = TRUE)
})
}
```
