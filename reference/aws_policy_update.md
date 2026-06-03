# Update a policy

Update a policy

## Usage

``` r
aws_policy_update(arn, document, default = FALSE)
```

## Arguments

- arn:

  (character) policy arn. required

- document:

  (character) the policy document you want to use as the content for the
  new policy. required

- default:

  (character) set this version as the policy's default version?
  optional. When this parameter is `TRUE`, the new policy version
  becomes the operative version. That is, it becomes the version that is
  in effect for the IAM users, groups, and roles that the policy is
  attached to. default: `FALSE`

## Value

a tibble with policy version details:

- VersionId

- IsDefaultVersion

- CreateDate

## Details

see docs <https://www.paws-r-sdk.com/docs/iam_create_policy_version/>

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
[`aws_policy_list_entities()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_entities.md),
[`aws_policy_list_versions()`](https://getwilds.org/sixtyfour/reference/aws_policy_list_versions.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
if (aws_policy_exists("polisee")) {
  aws_policy_delete("polisee")
}

# Create policy document
st8ment1 <- aws_policy_statement("iam:GetUser", "*")
st8ment2 <- aws_policy_statement("s3:ListAllMyBuckets", "*")
doc <- aws_policy_document_create(st8ment1, st8ment2)

# Create policy
invisible(aws_policy_create("polisee", document = doc))

# Update the same policy
new_doc <- aws_policy_document_create(st8ment1)
arn <- as_policy_arn("polisee", local = TRUE)
aws_policy_update(arn, document = new_doc, default = TRUE)
aws_policy_list_versions("polisee")

# cleanup - delete the policy
aws_policy_delete_version("polisee", "v1")
aws_policy_delete("polisee")
}
```
