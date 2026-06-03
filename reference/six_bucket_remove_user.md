# Remove a user from a bucket

Remove a user from a bucket

## Usage

``` r
six_bucket_remove_user(bucket, username)
```

## Arguments

- bucket:

  (character) bucket name. required

- username:

  (character) A user name. required

## Value

invisibly returns nothing

## Details

This function detaches a policy from a user for accessing the bucket;
the policy itself is untouched

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
# create a bucket
bucket <- random_bucket()
if (!aws_bucket_exists(bucket)) aws_bucket_create(bucket)

# create user
user <- random_user()
if (!aws_user_exists(user)) aws_user_create(user)

six_bucket_add_user(bucket, user, permissions = "read")
six_bucket_remove_user(bucket, user)

# cleanup
six_user_delete(user)
aws_bucket_delete(bucket, force = TRUE)
}
```
