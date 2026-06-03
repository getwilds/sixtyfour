# Add a user to a bucket

Add a user to a bucket

## Usage

``` r
six_bucket_add_user(bucket, username, permissions)
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

## Permissions

- read: read only; not allowed to write or do admin tasks

- write: write (in addition to read); includes deleting files; does not
  include deleting buckets

- admin: change user permissions (in addition to read and write);
  includes deleting buckets (THIS OPTION NOT ACCEPTED YET!)

## What is magical

- Exits early if permissions is not length 1

- Exits early if permissions is not in allowed set

- Exits early if bucket does not exist

- Creates bucket policy if not created yet

- If user not in bucket already, attach policy to user (which adds them
  to the bucket)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
# create a bucket
bucket <- random_bucket()
if (!aws_bucket_exists(bucket)) {
  aws_bucket_create(bucket)
}

# create a user
user <- random_user()
if (!aws_user_exists(user)) {
  aws_user_create(user)
}

six_bucket_add_user(
  bucket = bucket,
  username = user,
  permissions = "read"
)

# cleanup
six_user_delete(user)
aws_bucket_delete(bucket, force = TRUE)

if (FALSE) { # \dontrun{
# not a valid permissions string
six_bucket_add_user(
  bucket = "mybucket",
  username = "userdmgziqpt",
  permissions = "notavalidpermission"
)
} # }
}
```
