# Delete a user

Delete a user

## Usage

``` r
six_user_delete(username)
```

## Arguments

- username:

  (character) A user name. required

## Value

an empty list

## Details

See <https://www.paws-r-sdk.com/docs/iam_delete_user/> docs for more
details

## What is magical

- Detaches any attached policies

- Deletes any access keys

- Then deletes the user

## See also

Other users:
[`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md),
[`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_delete.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md)

Other magicians:
[`six_admin_setup()`](https://getwilds.org/sixtyfour/reference/six_admin_setup.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
name <- random_user()
six_user_create(name)
six_user_delete(name)
}
```
