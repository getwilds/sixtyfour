# Delete current user's AWS Access Key

Delete current user's AWS Access Key

## Usage

``` r
aws_user_access_key_delete(access_key_id, username = NULL)
```

## Arguments

- access_key_id:

  (character) The access key ID for the access key ID and secret access
  key you want to delete. required.

- username:

  (character) A user name. optional. however, if you do not supply a
  username, `paws` will likely use the current user, and so may not be
  the user the access key id is associated - and then you'll get an
  error like
  `NoSuchEntity (HTTP 404). The Access Key with id xx cannot be found`

## Value

NULL, invisibly

## Details

See <https://www.paws-r-sdk.com/docs/iam_delete_access_key/> docs for
more details

## See also

Other users:
[`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_delete.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)
