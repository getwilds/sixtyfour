# List policies

List policies

## Usage

``` r
aws_policies(refresh = FALSE, ...)
```

## Arguments

- refresh:

  (logical) refresh results? default: `FALSE`. to invalidate cache and
  refresh policy data, set `refresh=TRUE`

- ...:

  named arguments passed on to
  [list_policies](https://www.paws-r-sdk.com/docs/iam_list_policies/)

## Value

A tibble with information about policies. Each row is a policy. Columns:

- PolicyName

- PolicyId

- Path

- Arn

- CreateDate

- UpdateDate

- AttachmentCount

- PermissionsBoundaryUsageCount

- IsAttachable

- Description

- Tags

## Details

uses `memoise` internally to cache results to speed up all subsequent
calls to the function

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
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
if (FALSE) { # aws_has_creds()
# takes a while on the first execution in an R session
aws_policies()
}
if (FALSE) { # interactive() && aws_has_creds()
# faster because first call memoised the result
aws_policies()
# refresh=TRUE will pull from AWS
aws_policies(refresh = TRUE)
}
```
