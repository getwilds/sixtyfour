# List all groups or groups for a single user

List all groups or groups for a single user

## Usage

``` r
aws_groups(username = NULL, ...)
```

## Arguments

- username:

  (character) a username. optional

- ...:

  parameters passed on to `paws` `list_groups_for_user` if username is
  non-NULL, otherwise passed on to `list_users`

## Value

A tibble with information about groups

## See also

Other groups:
[`aws_group()`](https://getwilds.org/sixtyfour/reference/aws_group.md),
[`aws_group_create()`](https://getwilds.org/sixtyfour/reference/aws_group_create.md),
[`aws_group_delete()`](https://getwilds.org/sixtyfour/reference/aws_group_delete.md),
[`aws_group_exists()`](https://getwilds.org/sixtyfour/reference/aws_group_exists.md),
[`six_group_delete()`](https://getwilds.org/sixtyfour/reference/six_group_delete.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
aws_groups()
aws_groups(username = aws_user_current())
}
```
