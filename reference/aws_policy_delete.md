# Delete a user managed policy

Delete a user managed policy

## Usage

``` r
aws_policy_delete(name)
```

## Arguments

- name:

  (character) a policy name. required. within the function we lookup the
  policy arn which is what's passed to the AWS API

## Value

invisibly returns `NULL`

## AWS managed policies

You can not delete AWS managed policies.

## Deleting process (adapted from `paws` docs)

Before you can delete a managed policy, you must first detach the policy
from all users, groups, and roles that it is attached to. In addition,
you must delete all the policy's versions. The following steps describe
the process for deleting a managed policy:

- Detach the policy from all users, groups, and roles that the policy is
  attached to using
  [`aws_policy_attach()`](https://getwilds.org/sixtyfour/reference/aws_policy_attach.md).
  To list all the users, groups, and roles that a policy is attached to
  use
  [`aws_policy_list_entities()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_entities.md)

- Delete all versions of the policy using
  [`aws_policy_delete_version()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete_version.md).
  To list the policy's versions, use
  [`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md).
  You cannot use
  [`aws_policy_delete_version()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete_version.md)
  to delete the version that is marked as the default version. You
  delete the policy's default version in the next step of the process.

- Delete the policy using this function (this automatically deletes the
  policy's default version)

## References

[delete_policy](https://www.paws-r-sdk.com/docs/iam_delete_policy/)

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
[`aws_policy()`](https://getwilds.org/sixtyfour/reference/aws_policy.md),
[`aws_policy_attach()`](https://getwilds.org/sixtyfour/reference/aws_policy_attach.md),
[`aws_policy_create()`](https://getwilds.org/sixtyfour/reference/aws_policy_create.md),
[`aws_policy_delete_version()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete_version.md),
[`aws_policy_detach()`](https://getwilds.org/sixtyfour/reference/aws_policy_detach.md),
[`aws_policy_exists()`](https://getwilds.org/sixtyfour/reference/aws_policy_exists.md),
[`aws_policy_list_entities()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_entities.md),
[`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md),
[`aws_policy_update()`](https://getwilds.org/sixtyfour/reference/aws_policy_update.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
if (aws_policy_exists("RdsAllow456")) {
  aws_policy_delete("RdsAllow456")
}

# Create policy document
doc <- aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = "*"
  )
)

# Create policy
invisible(aws_policy_create("RdsAllow456", document = doc))

# Delete policy
aws_policy_delete("RdsAllow456")
}
```
