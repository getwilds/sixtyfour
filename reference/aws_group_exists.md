# Check if a group exists

Check if a group exists

## Usage

``` r
aws_group_exists(name)
```

## Arguments

- name:

  (character) the group name

## Value

a single boolean

## Details

uses `aws_group` internally. see docs
<https://www.paws-r-sdk.com/docs/iam_get_group/>

## See also

Other groups:
[`aws_group()`](https://getwilds.org/sixtyfour/reference/aws_group.md),
[`aws_group_create()`](https://getwilds.org/sixtyfour/reference/aws_group_create.md),
[`aws_group_delete()`](https://getwilds.org/sixtyfour/reference/aws_group_delete.md),
[`aws_groups()`](https://getwilds.org/sixtyfour/reference/aws_groups.md),
[`six_group_delete()`](https://getwilds.org/sixtyfour/reference/six_group_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
aws_group_create("apples")
aws_group_exists("apples")
aws_group_exists("doesnotexist")
# cleanup
aws_group_delete("apples")
}
```
