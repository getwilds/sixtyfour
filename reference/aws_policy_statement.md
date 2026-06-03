# Create a policy statement

Create a policy statement

## Usage

``` r
aws_policy_statement(action, resource, effect = "Allow", ...)
```

## Arguments

- action:

  (character) an action. required. see Actions below.

- resource:

  (character) the object or objects the statement covers; see link below
  for more information

- effect:

  (character) valid values: "Allow" (default), "Deny". length==1

- ...:

  Additional named arguments. See link in Details for options, and
  examples below

## Value

a named list

## Details

<https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html>
\#nolint

## Examples

``` r
aws_policy_statement("iam:GetUser", "*")
#> $Effect
#> [1] "Allow"
#> 
#> $Action
#> [1] "iam:GetUser"
#> 
#> $Resource
#> [1] "*"
#> 
aws_policy_statement("iam:GetUser", "*", Sid = "MyStatementId")
#> $Effect
#> [1] "Allow"
#> 
#> $Action
#> [1] "iam:GetUser"
#> 
#> $Resource
#> [1] "*"
#> 
#> $Sid
#> [1] "MyStatementId"
#> 
aws_policy_statement("iam:GetUser", "*",
  Condition = list(
    StringEqualsIgnoreCase = list("aws:username" = "johndoe")
  )
)
#> $Effect
#> [1] "Allow"
#> 
#> $Action
#> [1] "iam:GetUser"
#> 
#> $Resource
#> [1] "*"
#> 
#> $Condition
#> $Condition$StringEqualsIgnoreCase
#> $Condition$StringEqualsIgnoreCase$`aws:username`
#> [1] "johndoe"
#> 
#> 
#> 
aws_policy_statement("iam:GetUser", "*",
  Principal = list(Service = "s3.amazonaws.com")
)
#> $Effect
#> [1] "Allow"
#> 
#> $Action
#> [1] "iam:GetUser"
#> 
#> $Resource
#> [1] "*"
#> 
#> $Principal
#> $Principal$Service
#> [1] "s3.amazonaws.com"
#> 
#> 
```
