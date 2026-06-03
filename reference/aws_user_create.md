# Create a user

Create a user

## Usage

``` r
aws_user_create(username, path = NULL, permission_boundary = NULL, tags = NULL)
```

## Arguments

- username:

  (character) A user name. required

- path:

  (character) The path for the user name. optional. If it is not
  included, it defaults to a slash (/).

- permission_boundary:

  (character) The ARN of the managed policy that is used to set the
  permissions boundary for the user. optional

- tags:

  (list) A list of tags that you want to attach to the new user.
  optional

## Value

A tibble with information about the user created

## Details

See <https://www.paws-r-sdk.com/docs/iam_create_user/> docs for details
on the parameters

## See also

Other users:
[`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md),
[`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_delete.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
user1 <- random_user()
if (aws_user_exists(user1)) {
  aws_user_delete(user1)
}
aws_user_create(user1)

# cleanup
aws_user_delete(user1)
}
```
