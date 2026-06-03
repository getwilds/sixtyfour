# Change user permissions for a bucket

Change user permissions for a bucket

## Usage

``` r
six_bucket_change_user(bucket, username, permissions)
```

## Arguments

- bucket:

  (character) bucket name. required

- username:

  (character) A user name. required

- permissions:

  (character) user permissions, one of read or write. write includes
  read

## Value

invisibly returns nothing

## Important

This function is built around policies named by this package. If you use
your own policies that you name this function may not work.

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
# create a bucket
bucket <- random_bucket()
if (!aws_bucket_exists(bucket)) {
  aws_bucket_create(bucket)
}

# create user
user <- random_user()
if (!aws_user_exists(user)) {
  aws_user_create(user)
}

# user doesn't have any permissions for the bucket
# - use six_bucket_add_user to add permissions
six_bucket_change_user(
  bucket = bucket,
  username = user, permissions = "read"
)
six_bucket_add_user(
  bucket = bucket, username = user,
  permissions = "read"
)

# want to change to read to write, makes the change
six_bucket_change_user(
  bucket = bucket, username = user,
  permissions = "write"
)

# want to change to write - but already has write
six_bucket_change_user(
  bucket = bucket, username = user,
  permissions = "write"
)

# cleanup
six_user_delete(user)
aws_bucket_delete(bucket, force = TRUE)
}
```
