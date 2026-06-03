# Get a policy

Get a policy

## Usage

``` r
aws_policy(name, local = FALSE, path = NULL)
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

a tibble with policy details

## Details

see docs <https://www.paws-r-sdk.com/docs/iam_get_policy/>

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
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
if (FALSE) { # aws_has_creds()
# get an AWS managed policy (local = FALSE - the default)
aws_policy("AmazonS3FullAccess")

# get a policy by arn
aws_policy("arn:aws:iam::aws:policy/AmazonS3FullAccess")
}
```
