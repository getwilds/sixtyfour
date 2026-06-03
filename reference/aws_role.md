# Get a role

Get a role

## Usage

``` r
aws_role(name)
```

## Arguments

- name:

  (character) the role name

## Value

a named list with slots for:

- role (tibble)

- policies (character)

- attached_policies (tibble)

## Details

see docs <https://www.paws-r-sdk.com/docs/iam_get_role/>; also includes
policies and attached policies by calling `list_role_policies` and
`list_attached_role_policies`

## See also

Other roles:
[`aws_role_create()`](https://getwilds.org/sixtyfour/reference/aws_role_create.md),
[`aws_role_delete()`](https://getwilds.org/sixtyfour/reference/aws_role_delete.md),
[`aws_role_exists()`](https://getwilds.org/sixtyfour/reference/aws_role_exists.md),
[`aws_roles()`](https://getwilds.org/sixtyfour/reference/aws_roles.md)

## Examples

``` r
if (FALSE) { # aws_has_creds()
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
desc <- "Another test role"
z <- aws_role_create("ALittleRole",
  assume_role_policy_document = doc,
  description = desc
)
aws_policy_attach(z, "ReadOnlyAccess")
res <- aws_role(name = "ALittleRole")
res
res$role
res$policies
res$attached_policies

# cleanup
aws_role("ALittleRole") %>%
  aws_policy_detach("ReadOnlyAccess")
aws_role_delete("ALittleRole")
}
```
