# Create access keys for a user

Creates a new Amazon Web Services secret access key and corresponding
Amazon Web Services access key ID

## Usage

``` r
six_user_creds(username, copy_to_cb = FALSE)
```

## Arguments

- username:

  (character) A user name. required

- copy_to_cb:

  (logical) Copy to clipboard. Default: `FALSE`. See section "Clipboard"
  below for more details.

## Value

invisibly returns named list with slots:

- UserName (character)

- AccessKeyId (character)

- Status (character)

- SecretAccessKey (character)

- CreateDate (POSIXct)

## Details

A user can have more than one pair of access keys. By default a user can
have up to 2 pairs of access keys. Using this function will not replace
an existing set of keys; but instead adds an additional set of keys.

See <https://rstats.wtf/r-startup.html> for help on bringing in secrets
to an R session.

Note that although we return the AWS Region in the output of this
function IAM does not have regional resources. You can however use IAM
to manage regions an account has access to, etc. See
<https://docs.aws.amazon.com/accounts/latest/reference/manage-acct-regions.html>
\#nolint

## Important

Save the secret key after running this function as it can not be viewed
again.

## Clipboard

If you set `copy_to_cb=TRUE` we'll copy to your clipboard an email
template with the credentials and a small amount of instructions. Please
do edit that email with information tailored to your group and how you'd
like to store secrets

## Known error behaviors

- `LimitExceeded (HTTP 409). Cannot exceed quota for AccessKeysPerUser: 2`

- `NoSuchEntity (HTTP 404). The user with name xxx cannot be found.`

## See also

[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
user <- random_user()
if (!aws_user_exists(user)) aws_user_create(user)
six_user_creds(user)
aws_user_access_key(user)
six_user_creds(user, copy_to_cb = TRUE)
aws_user_access_key(user)
# cleanup
six_user_delete(user)
}
```
