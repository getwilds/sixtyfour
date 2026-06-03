# Get AWS Access Key for a user

IMPORTANT: the secret access key is only accessible during key and user
creation

## Usage

``` r
aws_user_access_key(username = NULL, ...)
```

## Arguments

- username:

  (character) A user name. required

- ...:

  further named args passed on to
  [list_access_keys](https://www.paws-r-sdk.com/docs/iam_list_access_keys/)

## Value

a tibble with key details

## Details

See <https://www.paws-r-sdk.com/docs/iam_list_access_keys/> docs for
more details

## See also

Other users:
[`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md),
[`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_delete.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)
