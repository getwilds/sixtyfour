% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policy_list_versions}
\alias{aws_policy_list_versions}
\title{List policy versions}
\usage{
aws_policy_list_versions(name, ...)
}
\arguments{
\item{name}{(character) a policy name. required. within the function
we lookup the policy arn which is what's passed to the AWS API}

\item{...}{additional named arguments passed on to internal \code{paws} method
(see link below to its docs)}
}
\value{
tibble with columns:
\itemize{
\item Document
\item VersionId
\item IsDefaultVersion
\item CreateDate
}
}
\description{
List policy versions
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
pols <- aws_policies()
aws_policy_list_versions(pols$PolicyName[1])
\dontshow{\}) # examplesIf}
}
\references{
\url{https://www.paws-r-sdk.com/docs/iam_list_policy_versions/}
}
\seealso{
Other policies: 
\code{\link{as_policy_arn}()},
\code{\link{aws_policies}()},
\code{\link{aws_policy}()},
\code{\link{aws_policy_attach}()},
\code{\link{aws_policy_create}()},
\code{\link{aws_policy_delete}()},
\code{\link{aws_policy_delete_version}()},
\code{\link{aws_policy_detach}()},
\code{\link{aws_policy_exists}()},
\code{\link{aws_policy_list_entities}()}
}
\concept{policies}
