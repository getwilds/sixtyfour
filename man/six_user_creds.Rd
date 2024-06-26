% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3.R
\name{six_user_creds}
\alias{six_user_creds}
\title{Create access keys for a user}
\usage{
six_user_creds(username, copy_to_cp = FALSE)
}
\arguments{
\item{username}{(character) A user name. required}

\item{copy_to_cp}{(logical) Copy to clipboard. Default: \code{FALSE}. See
section "Clipboard" below for more details.}
}
\value{
invisibly returns named list with slots:
\itemize{
\item UserName (character)
\item AccessKeyId (character)
\item Status (character)
\item SecretAccessKey (character)
\item CreateDate (POSIXct)
}
}
\description{
Creates a new Amazon Web Services secret access key and
corresponding Amazon Web Services access key ID
}
\details{
A user can have more than one pair of access keys.
By default a user can have up to 2 pairs of access keys.
Using this function will not replace an existing set of keys;
but instead adds an additional set of keys.

See \url{https://rstats.wtf/r-startup.html} for help on bringing in secrets
to an R session.

Note that although we return the AWS Region in the output of this
function IAM does not have regional resources. You can however use IAM
to manage regions an account has access to, etc. See
\url{https://docs.aws.amazon.com/accounts/latest/reference/manage-acct-regions.html} #nolint
}
\section{Important}{

Save the secret key after running this function as it can not be
viewed again.
}

\section{Clipboard}{

If you set \code{copy_to_cp=TRUE} we'll copy to your clipboard an
email template with the credentials and a small amount of instructions.
Please do edit that email with information tailored to your
group and how you'd like to store secrets.
}

\section{Known error behaviors}{

\itemize{
\item \verb{LimitExceeded (HTTP 409). Cannot exceed quota for AccessKeysPerUser: 2}
\item \verb{NoSuchEntity (HTTP 404). The user with name xxx cannot be found.}
}
}

\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
user <- random_user()
if (!aws_user_exists(user)) aws_user_create(user)
six_user_creds(user)
aws_user_access_key(user)
six_user_creds(user, copy_to_cp = TRUE)
aws_user_access_key(user)
# cleanup
six_user_delete(user)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=aws_user_access_key]{aws_user_access_key()}}, \code{\link[=aws_user_access_key_delete]{aws_user_access_key_delete()}}
}
