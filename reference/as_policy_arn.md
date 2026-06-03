# Convert a policy name to a policy ARN

This function simply constructs a string. It only makes an HTTP request
if `local=TRUE` and environment variable `AWS_PROFILE` != "localstack"

## Usage

``` r
as_policy_arn(name, local = FALSE, path = NULL)
```

## Arguments

- name:

  (character) a policy name or arn

- local:

  (logical) if `TRUE` use your AWS account for your own managed
  policies. If `FALSE`, AWS managed policies

- path:

  (character) if not `NULL`, we add the path into the ARN before the
  `name` value

## Value

a policy ARN (character)

## See also

Other policies:
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
[`aws_policy()`](https://getwilds.org/sixtyfour/reference/aws_policy.md),
[`aws_policy_attach()`](https://getwilds.org/sixtyfour/reference/aws_policy_attach.md),
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
as_policy_arn("ReadOnlyAccess")
#> arn:aws:iam::aws:policy/ReadOnlyAccess
as_policy_arn("arn:aws:iam::aws:policy/ReadOnlyAccess")
#> [1] "arn:aws:iam::aws:policy/ReadOnlyAccess"
as_policy_arn("AmazonRDSDataFullAccess")
#> arn:aws:iam::aws:policy/AmazonRDSDataFullAccess

# path = Job function
as_policy_arn("Billing", path = "job-function")
#> arn:aws:iam::aws:policy/job-function/Billing

# path = Service role
as_policy_arn("AWSCostAndUsageReportAutomationPolicy",
  path = "service-role"
)
#> arn:aws:iam::aws:policy/service-role/AWSCostAndUsageReportAutomationPolicy

if (FALSE) { # interactive() && aws_has_creds()
as_policy_arn("MyTestPolicy", local = TRUE)
# returns an arn - and if given an arn returns self
as_policy_arn("MyTestPolicy", local = TRUE) %>%
  as_policy_arn()
}
```
