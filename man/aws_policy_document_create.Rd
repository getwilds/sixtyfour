% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policy_document_create}
\alias{aws_policy_document_create}
\title{Create a policy document}
\usage{
aws_policy_document_create(
  region,
  account_id,
  resource_id,
  user,
  action,
  effect = "Allow",
  ...
)
}
\arguments{
\item{region}{(character) the AWS Region for the DB instance. length==1}

\item{account_id}{(character) the AWS account number for the DB instance.
length==1. The user must be in the same account as the account for the
DB instance}

\item{resource_id}{(character) the identifier for the DB instance. length==1}

\item{user}{(character) a user name that has an IAM account. length>=1}

\item{action}{(character) an action. required. see Actions below.}

\item{effect}{(character) valid values: "Allow" (default), "Deny". length==1}

\item{...}{named args passed to \code{\link[jsonlite:fromJSON]{jsonlite::toJSON()}}}
}
\value{
a json class string. use \code{\link[=as.character]{as.character()}} to coerce to a regular
string
}
\description{
Create a policy document
}
\note{
a few document items are hard-coded:
\itemize{
\item \code{Version} is set to 2012-10-17"
\item \code{Action} is set to "rds-db:connect"
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
### DB account = user in a database that has access to it
# all DB instances & DB accounts for a AWS account and AWS Region
aws_policy_document_create("us-east-2", "1234567890", "*", "*")
# all DB instances for a AWS account and AWS Region, single DB account
aws_policy_document_create("us-east-2", "1234567890", "*", "jane_doe")
# single DB instasnce, single DB account
aws_policy_document_create(
  "us-east-2",
  "1234567890", "db-ABCDEFGHIJKL01234", "jane_doe"
)
# single DB instance, many users
aws_policy_document_create(
  region = "us-east-2",
  account_id = "1234567890",
  resource_id = "db-ABCDEFGHIJKL01234",
  user = c("jane_doe", "mary_roe"),
  action = "rds-db:connect"
)
\dontshow{\}) # examplesIf}
}
\references{
\url{https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html} #nolint
}
