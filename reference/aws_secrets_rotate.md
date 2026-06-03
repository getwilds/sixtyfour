# Rotate a secret

Rotate a secret

## Usage

``` r
aws_secrets_rotate(id, lambda_arn = NULL, rules = NULL, immediately = TRUE)
```

## Arguments

- id:

  (character) The name or ARN of the secret. required

- lambda_arn:

  (character) The ARN of the Lambda rotation function. Only supply for
  secrets that use a Lambda rotation function to rotate

- rules:

  (list) asdfadf

- immediately:

  (logical) whether to rotate the secret immediately or not. default:
  `TRUE`

## Value

(list) with fields:

- ARN

- Name

- VersionId

## Details

Note that we autogenerate a random UUID to pass to the
`ClientRequestToken` parameter of the `paws` function used internally

## References

<https://www.paws-r-sdk.com/docs/secretsmanager_rotate_secret/>

## Examples

``` r
if (FALSE) { # aws_has_creds() && interactive()
try({
# Create a secret
secret <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret,
  secret = '{"username":"billy","password":"willy"}',
  description = "A string"
)

# Rotate
try(aws_secrets_rotate(id = secret))

# Cleanup
aws_secrets_delete(secret, ForceDeleteWithoutRecovery = TRUE)
})
}
```
