# List policy entities

List policy entities

## Usage

``` r
aws_policy_list_entities(name, ...)
```

## Arguments

- name:

  (character) a policy name. required. within the function we lookup the
  policy arn which is what's passed to the AWS API

- ...:

  additional named arguments passed on to internal `paws` method (see
  link below to its docs)

## Value

tibble with columns:

- type: one of Users, Roles, Groups

- name: the user, role or group name

- id: the id for the user, role or group name

Zero row tibble if there are no entities

## References

<https://www.paws-r-sdk.com/docs/iam_list_entities_for_policy/>

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
[`aws_policy()`](https://getwilds.org/sixtyfour/reference/aws_policy.md),
[`aws_policy_attach()`](https://getwilds.org/sixtyfour/reference/aws_policy_attach.md),
[`aws_policy_create()`](https://getwilds.org/sixtyfour/reference/aws_policy_create.md),
[`aws_policy_delete()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete.md),
[`aws_policy_delete_version()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete_version.md),
[`aws_policy_detach()`](https://getwilds.org/sixtyfour/reference/aws_policy_detach.md),
[`aws_policy_exists()`](https://getwilds.org/sixtyfour/reference/aws_policy_exists.md),
[`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md),
[`aws_policy_update()`](https://getwilds.org/sixtyfour/reference/aws_policy_update.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
aws_policy_list_entities("AdministratorAccess")
aws_policy_list_entities("AmazonRedshiftReadOnlyAccess")
}
```
