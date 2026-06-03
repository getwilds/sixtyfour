# Get a group

Get a group

## Usage

``` r
aws_group(name)
```

## Arguments

- name:

  (character) the group name

## Value

a named list with slots for:

- group: information about the group (tibble)

- users: users in the group (tibble)

- policies (character)

- attached_policies (tibble)

## Details

see docs <https://www.paws-r-sdk.com/docs/iam_get_group/>

## See also

Other groups:
[`aws_group_create()`](https://getwilds.org/sixtyfour/reference/aws_group_create.md),
[`aws_group_delete()`](https://getwilds.org/sixtyfour/reference/aws_group_delete.md),
[`aws_group_exists()`](https://getwilds.org/sixtyfour/reference/aws_group_exists.md),
[`aws_groups()`](https://getwilds.org/sixtyfour/reference/aws_groups.md),
[`six_group_delete()`](https://getwilds.org/sixtyfour/reference/six_group_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
# create a group
aws_group_create("testing")
# get the group
aws_group(name = "testing")
# cleanup
aws_group_delete(name = "testing")
}
```
