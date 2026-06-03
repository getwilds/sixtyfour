# List roles

List roles

## Usage

``` r
aws_roles(...)
```

## Arguments

- ...:

  parameters passed on to the `paws`
  [list_users](https://www.paws-r-sdk.com/docs/iam_list_roles/) method

## Value

A tibble with information about roles

## See also

Other roles:
[`aws_role()`](https://getwilds.org/sixtyfour/reference/aws_role.md),
[`aws_role_create()`](https://getwilds.org/sixtyfour/reference/aws_role_create.md),
[`aws_role_delete()`](https://getwilds.org/sixtyfour/reference/aws_role_delete.md),
[`aws_role_exists()`](https://getwilds.org/sixtyfour/reference/aws_role_exists.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
aws_roles()
}
```
