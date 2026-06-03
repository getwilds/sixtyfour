# Check if a policy exists

Checks for both customer managed and AWS managed policies

## Usage

``` r
aws_policy_exists(name)
```

## Arguments

- name:

  (character) a policy name or arn

## Value

single logical, `TRUE` or `FALSE`

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
[`aws_policy_list_entities()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_entities.md),
[`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md),
[`aws_policy_update()`](https://getwilds.org/sixtyfour/reference/aws_policy_update.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
# just the policy name
aws_policy_exists("ReadOnlyAccess")
# as an ARN
aws_policy_exists("arn:aws:iam::aws:policy/ReadOnlyAccess")
# includes job-function in path
aws_policy_exists("Billing")
# includes service-role in path
aws_policy_exists("AWSCostAndUsageReportAutomationPolicy")
}
```
