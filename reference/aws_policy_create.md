# Create a policy

Create a policy

## Usage

``` r
aws_policy_create(name, document, path = NULL, description = NULL, tags = NULL)
```

## Arguments

- name:

  (character) a policy name. required

- document:

  (character) the policy document you want to use as the content for the
  new policy. required.

- path:

  (character) the path for the policy. if not given default is "/".
  optional

- description:

  (character) a friendly description of the policy. optional. cannot be
  changed after assigning it

- tags:

  (character) a vector of tags that you want to attach to the new IAM
  policy. Each tag consists of a key name and an associated value.
  optional

## Value

a tibble with policy details

## Details

see docs <https://www.paws-r-sdk.com/docs/iam_create_policy/>

## See also

Other policies:
[`as_policy_arn()`](https://getwilds.org/sixtyfour/reference/as_policy_arn.md),
[`aws_policies()`](https://getwilds.org/sixtyfour/reference/aws_policies.md),
[`aws_policy()`](https://getwilds.org/sixtyfour/reference/aws_policy.md),
[`aws_policy_attach()`](https://getwilds.org/sixtyfour/reference/aws_policy_attach.md),
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
if (aws_policy_exists("MyPolicy123")) {
  aws_policy_delete("MyPolicy123")
}

# Create policy document
st8ment1 <- aws_policy_statement("iam:GetUser", "*")
st8ment2 <- aws_policy_statement("s3:ListAllMyBuckets", "*")
doc <- aws_policy_document_create(st8ment1, st8ment2)

# Create policy
aws_policy_create("MyPolicy123", document = doc)

# cleanup - delete policy
aws_policy_delete("MyPolicy123")
}
```
