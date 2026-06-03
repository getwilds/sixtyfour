# Get a user

Gets user information, including policies, groups, and attached policies

## Usage

``` r
aws_user(username = NULL)
```

## Arguments

- username:

  (character) A user name. required

## Value

a named list with slots for:

- user (tibble)

- policies (list)

- attached_policies (list)

- groups (list)

## Details

See the following docs links for details

- <https://www.paws-r-sdk.com/docs/iam_get_user/>

- <https://www.paws-r-sdk.com/docs/iam_list_user_policies/>

- <https://www.paws-r-sdk.com/docs/iam_list_groups_for_user/>

- <https://www.paws-r-sdk.com/docs/iam_list_attached_user_policies/>

## Note

if username not supplied, gets logged in user

## See also

Other users:
[`aws_user_access_key()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key.md),
[`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md),
[`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
[`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
[`aws_user_current()`](https://getwilds.org/sixtyfour/reference/aws_user_current.md),
[`aws_user_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_delete.md),
[`aws_user_exists()`](https://getwilds.org/sixtyfour/reference/aws_user_exists.md),
[`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
[`six_user_create()`](https://getwilds.org/sixtyfour/reference/six_user_create.md),
[`six_user_delete()`](https://getwilds.org/sixtyfour/reference/six_user_delete.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# if username not supplied, gets the logged in user
aws_user()
} # }

if (FALSE) { # aws_has_creds()
if (aws_user_exists("testBlueBird")) {
  aws_user_delete("testBlueBird")
}
aws_user_create("testBlueBird")
aws_user("testBlueBird")

# cleanup
aws_user_delete("testBlueBird")
}
```
