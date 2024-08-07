% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policy_create}
\alias{aws_policy_create}
\title{Create a policy}
\usage{
aws_policy_create(name, document, path = NULL, description = NULL, tags = NULL)
}
\arguments{
\item{name}{(character) a policy name. required}

\item{document}{(character) the policy document you want to use
as the content for the new policy. required.}

\item{path}{(character) the path for the policy. if not given
default is "/". optional}

\item{description}{(character) a friendly description of the policy.
optional. cannot be changed after assigning it}

\item{tags}{(character) a vector of tags that you want to attach to
the new IAM policy. Each tag consists of a key name and an associated
value. optional}
}
\value{
a tibble with policy details
}
\description{
Create a policy
}
\details{
see docs \url{https://www.paws-r-sdk.com/docs/iam_create_policy/}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
doc <- aws_policy_document_create(
  region = "us-east-2",
  account_id = "1234567890",
  resource_id = "*",
  user = "jane_doe",
  action = "rds-db:connect"
)
aws_policy_create("RdsAllow123", document = doc)
# cleanup
aws_policy_delete("RdsAllow123")
\dontshow{\}) # examplesIf}
}
\seealso{
Other policies: 
\code{\link{as_policy_arn}()},
\code{\link{aws_policies}()},
\code{\link{aws_policy}()},
\code{\link{aws_policy_attach}()},
\code{\link{aws_policy_delete}()},
\code{\link{aws_policy_delete_version}()},
\code{\link{aws_policy_detach}()},
\code{\link{aws_policy_exists}()},
\code{\link{aws_policy_list_entities}()},
\code{\link{aws_policy_list_versions}()}
}
\concept{policies}
