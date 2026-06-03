# Attach a policy to a user, group, or role

Attach a policy to a user, group, or role

## Usage

``` r
aws_policy_attach(.x, policy)
```

## Arguments

- .x:

  result of a call to create or get method for user, group, or role

- policy:

  (character) a policy name or ARN

## Value

A tibble with information about policies

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
[`aws_policy()`](https://getwilds.org/sixtyfour/reference/aws_policy.md),
[`aws_policy_create()`](https://getwilds.org/sixtyfour/reference/aws_policy_create.md),
[`aws_policy_delete()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete.md),
[`aws_policy_delete_version()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete_version.md),
[`aws_policy_detach()`](https://getwilds.org/sixtyfour/reference/aws_policy_detach.md),
[`aws_policy_exists()`](https://getwilds.org/sixtyfour/reference/aws_policy_exists.md),
[`aws_policy_list_entities()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_entities.md),
[`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md),
[`aws_policy_update()`](https://getwilds.org/sixtyfour/reference/aws_policy_update.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
if (aws_user_exists("user123")) {
  aws_user_delete("user123")
}

aws_user_create("user123")
aws_policy("AmazonRDSDataFullAccess")
aws_user("user123") %>% aws_policy_attach("AmazonRDSDataFullAccess")
aws_user("user123")$attached_policies
# cleanup
six_user_delete("user123")
}
```
