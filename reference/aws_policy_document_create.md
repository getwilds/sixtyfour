# Create a policy document

Create a policy document

## Usage

``` r
aws_policy_document_create(..., .list = NULL)
```

## Arguments

- ..., .list:

  policy statements as created by
  [`aws_policy_statement()`](https://getwilds.org/sixtyfour/reference/aws_policy_statement.md)
  or created manually. Pass in 1 or more statements via `...` like
  `statement1, statement2` or pass in as a list like
  `.list = list(statement1, statement2)`. Each element must be a named
  list.

## Value

a json class string. use
[`as.character()`](https://rdrr.io/r/base/character.html) to coerce to a
regular string

## Note

a document item is hard-coded:

- `Version` is set to 2012-10-17"

## Actions

Actions documentation appears to be all over the web. Here's a start:

- S3:
  <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazons3.html>
  \# nolint

- EC2:
  <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html>
  \# nolint

- IAM:
  <https://docs.aws.amazon.com/IAM/latest/APIReference/API_Operations.html>
  \# nolint

## References

<https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html>
\# nolint

## Examples

``` r
library(jsonlite)

st8ment1 <- aws_policy_statement("iam:GetUser", "*")
st8ment2 <- aws_policy_statement("s3:ListAllMyBuckets", "*")
st8ment3 <- aws_policy_statement("s3-object-lambda:List*", "*")
aws_policy_document_create(st8ment1, st8ment2) %>% prettify()
#> {
#>     "Version": "2012-10-17",
#>     "Statement": [
#>         {
#>             "Effect": "Allow",
#>             "Action": "iam:GetUser",
#>             "Resource": "*"
#>         },
#>         {
#>             "Effect": "Allow",
#>             "Action": "s3:ListAllMyBuckets",
#>             "Resource": "*"
#>         }
#>     ]
#> }
#>  
aws_policy_document_create(.list = list(st8ment1, st8ment2)) %>% prettify()
#> {
#>     "Version": "2012-10-17",
#>     "Statement": [
#>         {
#>             "Effect": "Allow",
#>             "Action": "iam:GetUser",
#>             "Resource": "*"
#>         },
#>         {
#>             "Effect": "Allow",
#>             "Action": "s3:ListAllMyBuckets",
#>             "Resource": "*"
#>         }
#>     ]
#> }
#>  
aws_policy_document_create(st8ment3, .list = list(st8ment1, st8ment2)) %>%
  prettify()
#> {
#>     "Version": "2012-10-17",
#>     "Statement": [
#>         {
#>             "Effect": "Allow",
#>             "Action": "s3-object-lambda:List*",
#>             "Resource": "*"
#>         },
#>         {
#>             "Effect": "Allow",
#>             "Action": "iam:GetUser",
#>             "Resource": "*"
#>         },
#>         {
#>             "Effect": "Allow",
#>             "Action": "s3:ListAllMyBuckets",
#>             "Resource": "*"
#>         }
#>     ]
#> }
#>  

# Policy document to give a user access to RDS
resource <- "arn:aws:rds-db:us-east-2:1234567890:dbuser:db-ABCDE1212/jane"
st8ment_rds <- aws_policy_statement(
  action = "rds-db:connect",
  resource = resource
)
aws_policy_document_create(st8ment_rds) %>% prettify()
#> {
#>     "Version": "2012-10-17",
#>     "Statement": [
#>         {
#>             "Effect": "Allow",
#>             "Action": "rds-db:connect",
#>             "Resource": "arn:aws:rds-db:us-east-2:1234567890:dbuser:db-ABCDE1212/jane"
#>         }
#>     ]
#> }
#>  

if (FALSE) { # aws_has_creds()
### DB account = user in a database that has access to it
# all DB instances & DB accounts for a AWS account and AWS Region
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds("*", "*")
  )
) %>% prettify()
# all DB instances for a AWS account and AWS Region, single DB account
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds("jane_doe", "*")
  )
) %>% prettify()
# single DB instasnce, single DB account
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds("jane_doe", "db-ABCDEFGHIJKL01234")
  )
) %>% prettify()
# single DB instance, many users
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds(c("jane_doe", "mary_roe"), "db-ABCDEFGHIJKL01")
  )
) %>% prettify()
}
```
