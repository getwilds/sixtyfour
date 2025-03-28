% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secrets_manager.R
\name{aws_secrets_rotate}
\alias{aws_secrets_rotate}
\title{Rotate a secret}
\usage{
aws_secrets_rotate(id, lambda_arn = NULL, rules = NULL, immediately = TRUE)
}
\arguments{
\item{id}{(character) The name or ARN of the secret. required}

\item{lambda_arn}{(character) The ARN of the Lambda rotation function.
Only supply for secrets that use a Lambda rotation function to rotate}

\item{rules}{(list) asdfadf}

\item{immediately}{(logical) whether to rotate the secret immediately or not.
default: \code{TRUE}}
}
\value{
(list) with fields:
\itemize{
\item ARN
\item Name
\item VersionId
}
}
\description{
Rotate a secret
}
\details{
Note that we autogenerate a random UUID to pass to the
\code{ClientRequestToken} parameter of the \code{paws} function used internally
}
\examples{
\dontshow{if (aws_has_creds() && interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
try({
# Create a secret
secret <- random_string("secret-", size = 16)
aws_secrets_create(
  name = secret,
  secret = '{"username":"billy","password":"willy"}',
  description = "A string"
)

# Rotate
try(aws_secrets_rotate(id = secret))

# Cleanup
aws_secrets_delete(secret, ForceDeleteWithoutRecovery = TRUE)
})
\dontshow{\}) # examplesIf}
}
\references{
\url{https://www.paws-r-sdk.com/docs/secretsmanager_rotate_secret/}
}
