% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policies}
\alias{aws_policies}
\title{List policies}
\usage{
aws_policies(refresh = FALSE, ...)
}
\arguments{
\item{refresh}{(logical) refresh results? default: \code{FALSE}. to invalidate
cache and refresh policy data, set \code{refresh=TRUE}}

\item{...}{parameters passed on to
\href{https://www.paws-r-sdk.com/docs/iam_list_policies/}{list_policies}}
}
\value{
A tibble with information about policies
}
\description{
List policies
}
\details{
uses \code{memoise} internally to cache results to speed up all
subsequent calls to the function
}
\examples{
\dontrun{
aws_policies()
aws_policies()
aws_policies(refresh = TRUE)
}
}
\seealso{
Other policies: 
\code{\link{as_policy_arn}()},
\code{\link{aws_policy_attach}()},
\code{\link{aws_policy_create}()},
\code{\link{aws_policy_detach}()},
\code{\link{aws_policy_exists}()},
\code{\link{aws_policy}()}
}
\concept{policies}
