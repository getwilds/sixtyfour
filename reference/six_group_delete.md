# Delete a group, magically

Delete a group, magically

## Usage

``` r
six_group_delete(name)
```

## Arguments

- name:

  (character) A group name. required

## Value

`NULL` invisibly

## Details

See <https://www.paws-r-sdk.com/docs/iam_delete_group/> docs for more
details

## See also

Other groups:
[`aws_group()`](https://getwilds.org/sixtyfour/reference/aws_group.md),
[`aws_group_create()`](https://getwilds.org/sixtyfour/reference/aws_group_create.md),
[`aws_group_delete()`](https://getwilds.org/sixtyfour/reference/aws_group_delete.md),
[`aws_group_exists()`](https://getwilds.org/sixtyfour/reference/aws_group_exists.md),
[`aws_groups()`](https://getwilds.org/sixtyfour/reference/aws_groups.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
group <- random_string("group")
aws_group_create(group)
six_group_delete(group)
}
```
