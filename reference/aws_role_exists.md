# Check if a role exists

Check if a role exists

## Usage

``` r
aws_role_exists(name)
```

## Arguments

- name:

  (character) the role name

## Value

a single boolean

## See also

Other roles:
[`aws_role()`](https://getwilds.org/sixtyfour/reference/aws_role.md),
[`aws_role_create()`](https://getwilds.org/sixtyfour/reference/aws_role_create.md),
[`aws_role_delete()`](https://getwilds.org/sixtyfour/reference/aws_role_delete.md),
[`aws_roles()`](https://getwilds.org/sixtyfour/reference/aws_roles.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
aws_role_exists("AWSServiceRoleForRedshift")
aws_role_exists("NotARole")
}
```
