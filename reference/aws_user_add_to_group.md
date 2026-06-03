# Add or remove a user to/from a group

Add or remove a user to/from a group

## Usage

``` r
aws_user_add_to_group(username, groupname)

aws_user_remove_from_group(username, groupname)
```

## Arguments

- username:

  (character) A user name. required

- groupname:

  (character) a group name. required

## Value

a named list with slots for:

- user (tibble)

- policies (list)

- attached_policies (list)

- groups (list)

## Details

See <https://www.paws-r-sdk.com/docs/iam_add_user_to_group/>
<https://www.paws-r-sdk.com/docs/iam_remove_user_from_group/> docs for
more details

## See also

Other users:
[`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md),
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_delete.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
group1 <- random_string("group")
if (!aws_group_exists(group1)) {
  aws_group_create(group1)
}
name1 <- random_user()
if (!aws_user_exists(name1)) {
  aws_user_create(name1)
}
aws_user_add_to_group(name1, group1)
aws_group(group1) # has user name1
aws_user_remove_from_group(name1, group1)
aws_group(group1) # does not have user name1
}
```
