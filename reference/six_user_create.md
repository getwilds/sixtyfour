# Create a user, magically

Create a user, magically

## Usage

``` r
six_user_create(
  username,
  path = NULL,
  permission_boundary = NULL,
  tags = NULL,
  copy_to_cb = TRUE
)
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

- copy_to_cb:

  (logical) Copy to clipboard. Default: `FALSE`. See section "Clipboard"
  below for more details.

## Value

NULL invisibly. A draft email is copied to your clipboard

## Details

See
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md)
for more details. This function creates a user, adds policies so the
user can access their own account, and grants them an access key. Add
more policies using `aws_polic*` functions

## What is magical

- Adds a `UserInfo` policy to your account if doesn't exist yet

- Attaches `UserInfo` policy to the user created

- Grants an access key, copying an email template to your clipboard

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
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

Other magicians:
[`six_admin_setup()`](https://getwilds.org/sixtyfour/reference/six_admin_setup.md),
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
name <- random_user()
six_user_create(name)

# cleanup
six_user_delete(name)
}
```
