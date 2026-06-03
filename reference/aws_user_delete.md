# Delete a user

Delete a user

## Usage

``` r
aws_user_delete(username)
```

## Arguments

- username:

  (character) A user name. required

## Value

NULL invisibly

## Details

See <https://www.paws-r-sdk.com/docs/iam_delete_user/> docs for more
details

## See also

Other users:
[`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md),
[`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
user_name <- random_user()
aws_user_create(user_name)
aws_user_delete(user_name)
aws_user_exists(user_name)
}
```
