% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policy}
\alias{aws_policy}
\title{Get a policy}
\usage{
aws_policy(name)
}
\arguments{
\item{name}{(character) a policy name or ARN}
}
\value{
a tibble with policy details
}
\description{
Get a policy
}
\details{
see docs \url{https://www.paws-r-sdk.com/docs/iam_get_policy/}
}
\examples{
\dontrun{
aws_policy("ReadOnlyAccess")
aws_policy("arn:aws:iam::aws:policy/ReadOnlyAccess")
}
}
\seealso{
Other policies: 
\code{\link{as_policy_arn}()},
\code{\link{aws_policies}()},
\code{\link{aws_policy_attach}()},
\code{\link{aws_policy_create}()},
\code{\link{aws_policy_detach}()},
\code{\link{aws_policy_exists}()}
}
\concept{policies}
