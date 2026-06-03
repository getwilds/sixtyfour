# Delete a role

Delete a role

## Usage

``` r
aws_role_delete(name)
```

## Arguments

- name:

  (character) A role name. required

## Value

`NULL` invisibly

## Details

See <https://www.paws-r-sdk.com/docs/iam_delete_role/> docs for more
details

## See also

Other roles:
[`aws_role()`](https://getwilds.org/sixtyfour/reference/aws_role.md),
[`aws_role_create()`](https://getwilds.org/sixtyfour/reference/aws_role_create.md),
[`aws_role_exists()`](https://getwilds.org/sixtyfour/reference/aws_role_exists.md),
[`aws_roles()`](https://getwilds.org/sixtyfour/reference/aws_roles.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
if (aws_role_exists(name = "MyRole")) {
  aws_role_delete(name = "MyRole")
}
}
```
