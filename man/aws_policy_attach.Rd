% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policies.R
\name{aws_policy_attach}
\alias{aws_policy_attach}
\title{Attach a policy to a user, group, or role}
\usage{
aws_policy_attach(.x, policy)
}
\arguments{
\item{.x}{result of a call to create or get method for user,
group, or role}

\item{policy}{(character) a policy name or ARN}
}
\value{
A tibble with information about policies
}
\description{
Attach a policy to a user, group, or role
}
\examples{
\dontshow{if (aws_has_creds()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
if (aws_user_exists("user123")) {
  aws_user_delete("user123")
}

aws_user_create("user123")
aws_policy("AmazonRDSDataFullAccess")
aws_user("user123") \%>\% aws_policy_attach("AmazonRDSDataFullAccess")
aws_user("user123")$attached_policies
# cleanup
six_user_delete("user123")
\dontshow{\}) # examplesIf}
}
\seealso{
Other policies: 
\code{\link{as_policy_arn}()},
\code{\link{aws_policies}()},
\code{\link{aws_policy}()},
\code{\link{aws_policy_create}()},
\code{\link{aws_policy_delete}()},
\code{\link{aws_policy_delete_version}()},
\code{\link{aws_policy_detach}()},
\code{\link{aws_policy_exists}()},
\code{\link{aws_policy_list_entities}()},
\code{\link{aws_policy_list_versions}()},
\code{\link{aws_policy_update}()}
}
\concept{policies}
