# AWS account setup for administrators

AWS account setup for administrators

## Usage

``` r
six_admin_setup(users_group = "users", admin_group = "admin")
```

## Arguments

- users_group:

  (character) name for the users group. default: "users"

- admin_group:

  (character) name for the admin group. default: "admin"

## Value

NULL invisibly

## What is magical

- Setup a users IAM group: users that do not require admin persmissions

- Add policies to the users group

- Setup an admin IAM group: users that require admin permissions

- Add policies to the admin group

## See also

Other magicians:
[`six_bucket_delete()`](https://getwilds.org/sixtyfour/reference/six_bucket_delete.md),
[`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md),
[`six_file_upload()`](https://getwilds.org/sixtyfour/reference/six_file_upload.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)
