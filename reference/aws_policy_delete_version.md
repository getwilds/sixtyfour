# Delete a policy version

Delete a policy version

## Usage

``` r
aws_policy_delete_version(name, version_id)
```

## Arguments

- name:

  (character) a policy name. required. within the function we lookup the
  policy arn which is what's passed to the AWS API

- version_id:

  (character) The policy version to delete. required. Allows (via regex)
  a string of characters that consists of the lowercase letter 'v'
  followed by one or two digits, and optionally followed by a period '.'
  and a string of letters and digits.

## Value

invisibly returns `NULL`

## References

<https://www.paws-r-sdk.com/docs/iam_delete_policy_version/>

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
[`aws_policy()`](https://getwilds.org/sixtyfour/reference/aws_policy.md),
[`aws_policy_attach()`](https://getwilds.org/sixtyfour/reference/aws_policy_attach.md),
[`aws_policy_create()`](https://getwilds.org/sixtyfour/reference/aws_policy_create.md),
[`aws_policy_delete()`](https://getwilds.org/sixtyfour/reference/aws_policy_delete.md),
[`aws_policy_detach()`](https://getwilds.org/sixtyfour/reference/aws_policy_detach.md),
[`aws_policy_exists()`](https://getwilds.org/sixtyfour/reference/aws_policy_exists.md),
[`aws_policy_list_entities()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_entities.md),
[`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md),
[`aws_policy_update()`](https://getwilds.org/sixtyfour/reference/aws_policy_update.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
if (aws_policy_exists("RdsAllow888")) {
  aws_policy_delete("RdsAllow888")
}

# Create policy document
doc <- aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = "*"
  )
)

# Create policy
invisible(aws_policy_create("RdsAllow888", document = doc))

# Add a new version of the policy
st8ment1 <- aws_policy_statement("iam:GetUser", "*")
new_doc <- aws_policy_document_create(st8ment1)
arn <- as_policy_arn("RdsAllow888", local = TRUE)
aws_policy_update(arn, document = new_doc, defaul = TRUE)

# List versions of the policy
aws_policy_list_versions("RdsAllow888")

# Delete a policy version
aws_policy_delete_version("RdsAllow888", "v1")

# Cleanup - delete  policy
aws_policy_delete("RdsAllow888")
}
```
