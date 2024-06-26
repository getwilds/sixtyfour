% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policy_document_create}
\alias{aws_policy_document_create}
\title{Create a policy document}
\usage{
aws_policy_document_create(..., .list = NULL)
}
\arguments{
\item{..., .list}{policy statements as created by \code{\link[=aws_policy_statement]{aws_policy_statement()}}
or created manually. Pass in 1 or more statements via \code{...} like
\verb{statement1, statement2} or pass in as a list like
\code{.list = list(statement1, statement2)}. Each element must be a named list.}
}
\value{
a json class string. use \code{\link[=as.character]{as.character()}} to coerce to a regular
string
}
\description{
Create a policy document
}
\note{
a document item is hard-coded:
\itemize{
\item \code{Version} is set to 2012-10-17"
}
}
\section{Actions}{

Actions documentation appears to be all over the web. Here's a start:
\itemize{
\item S3: \url{https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazons3.html} # nolint
\item EC2: \url{https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html} # nolint
\item IAM: \url{https://docs.aws.amazon.com/IAM/latest/APIReference/API_Operations.html} # nolint
}
}

\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
st8ment1 <- aws_policy_statement("iam:GetUser", "*")
st8ment2 <- aws_policy_statement("s3:ListAllMyBuckets", "*")
st8ment3 <- aws_policy_statement("s3-object-lambda:List*", "*")
aws_policy_document_create(st8ment1, st8ment2)
aws_policy_document_create(.list = list(st8ment1, st8ment2))
aws_policy_document_create(st8ment3, .list = list(st8ment1, st8ment2))

# Policy document to give a user access to RDS
resource <- "arn:aws:rds-db:us-east-2:1234567890:dbuser:db-ABCDE1212/jane"
st8ment_rds <- aws_policy_statement(
  action = "rds-db:connect",
  resource = resource
)
aws_policy_document_create(st8ment_rds)

### DB account = user in a database that has access to it
# all DB instances & DB accounts for a AWS account and AWS Region
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds("*", "*")
  )
)
# all DB instances for a AWS account and AWS Region, single DB account
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds("jane_doe", "*")
  )
)
# single DB instasnce, single DB account
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds("jane_doe", "db-ABCDEFGHIJKL01234")
  )
)
# single DB instance, many users
aws_policy_document_create(
  aws_policy_statement(
    action = "rds-db:connect",
    resource = resource_rds(c("jane_doe", "mary_roe"), "db-ABCDEFGHIJKL01")
  )
)
\dontshow{\}) # examplesIf}
}
\references{
\url{https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html} # nolint
}
