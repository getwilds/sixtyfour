# Create a role

Create a role

## Usage

``` r
aws_role_create(
  name,
  assume_role_policy_document,
  path = NULL,
  description = NULL,
  max_session_duration = NULL,
  permission_boundary = NULL,
  tags = NULL
)
```

## Arguments

- name:

  (character) A role name. required

- assume_role_policy_document:

  (character) The trust relationship policy document that grants an
  entity permission to assume the role. json as string. required

- path:

  (character) The path for the role name. optional. If it is not
  included, it defaults to a slash (/).

- description:

  (character) a description fo the role. optional

- max_session_duration:

  (character) The maximum session duration (in seconds) that you want to
  set for the specified role. optional

- permission_boundary:

  (character) The ARN of the managed policy that is used to set the
  permissions boundary for the role. optional

- tags:

  (list) A list of tags that you want to attach to the new user.
  optional

## Value

A tibble with information about the role created

## Details

See <https://www.paws-r-sdk.com/docs/iam_create_role/> docs for details
on the parameters

## See also

Other roles:
[`aws_role()`](https://getwilds.org/sixtyfour/reference/aws_role.md),
[`aws_role_delete()`](https://getwilds.org/sixtyfour/reference/aws_role_delete.md),
[`aws_role_exists()`](https://getwilds.org/sixtyfour/reference/aws_role_exists.md),
[`aws_roles()`](https://getwilds.org/sixtyfour/reference/aws_roles.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
role_name <- "AMinorRole"
trust_policy <- list(
  Version = "2012-10-17",
  Statement = list(
    list(
      Effect = "Allow",
      Principal = list(
        Service = "lambda.amazonaws.com"
      ),
      Action = "sts:AssumeRole"
    )
  )
)
doc <- jsonlite::toJSON(trust_policy, auto_unbox = TRUE)
desc <- "My test role"
z <- aws_role_create(role_name,
  assume_role_policy_document = doc,
  description = desc
)
# attach a policy
invisible(z %>% aws_policy_attach("AWSLambdaBasicExecutionRole"))

# cleanup
invisible(z %>% aws_policy_detach("AWSLambdaBasicExecutionRole"))
aws_role_delete(role_name)
}
```
