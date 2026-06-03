# Create a group

Create a group

## Usage

``` r
aws_group_create(name, path = NULL)
```

## Arguments

- name:

  (character) A group name. required

- path:

  (character) The path for the group name. optional. If it is not
  included, it defaults to a slash (/).

## Value

A tibble with information about the group created

## Details

See <https://www.paws-r-sdk.com/docs/iam_create_group/> docs for details
on the parameters

## See also

Other groups:
[`aws_group()`](https://getwilds.org/sixtyfour/reference/aws_group.md),
[`aws_group_delete()`](https://getwilds.org/sixtyfour/reference/aws_group_delete.md),
[`aws_group_exists()`](https://getwilds.org/sixtyfour/reference/aws_group_exists.md),
[`aws_groups()`](https://getwilds.org/sixtyfour/reference/aws_groups.md),
[`six_group_delete()`](https://getwilds.org/sixtyfour/reference/six_group_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
aws_group_create("testingagroup")
aws_group("testingagroup")
# cleanup
aws_group_delete("testingagroup")
}
```
