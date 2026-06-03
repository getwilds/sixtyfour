# Get permissions for a bucket

Get permissions for a bucket

## Usage

``` r
six_bucket_permissions(bucket)
```

## Arguments

- bucket:

  (character) bucket name. required

## Value

tibble with a row for each user, with columns:

- user (always present)

- permissions (always present)

- policy_read (optionally present) the policy name behind the "read"
  permission (if present)

- policy_admin (optionally present) the policy name behind the "admin"
  permission (if present)

Note that users with no persmissions are not shown; see
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
# create a bucket
bucket <- random_bucket()
if (!aws_bucket_exists(bucket)) aws_bucket_create(bucket)

# create user
user <- random_user()
if (!aws_user_exists(user)) aws_user_create(user)

six_bucket_permissions(bucket)
six_bucket_add_user(bucket, user, permissions = "read")
six_bucket_permissions(bucket)
six_bucket_remove_user(bucket, user)
six_bucket_permissions(bucket)

# cleanup
six_user_delete(user)
aws_bucket_delete(bucket, force = TRUE)
}
```
