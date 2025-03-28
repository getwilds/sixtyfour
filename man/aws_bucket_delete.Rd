% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bucket.R
\name{aws_bucket_delete}
\alias{aws_bucket_delete}
\title{Delete an S3 bucket}
\usage{
aws_bucket_delete(bucket, force = FALSE, ...)
}
\arguments{
\item{bucket}{(character) bucket name. required}

\item{force}{(logical) force deletion without going through the prompt.
default: \code{FALSE}. Should only be set to \code{TRUE} when required for
non-interactive use.}

\item{...}{named parameters passed on to
\href{https://www.paws-r-sdk.com/docs/s3_delete_bucket/}{delete_bucket}}
}
\value{
\code{NULL}, invisibly
}
\description{
Delete an S3 bucket
}
\note{
Requires the env var \code{AWS_REGION}. This function prompts you to make
sure that you want to delete the bucket.
}
\examples{
\dontshow{if (aws_has_creds()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
bucket_name <- random_bucket()
if (!aws_bucket_exists(bucket_name)) {
  aws_bucket_create(bucket = bucket_name)
  aws_buckets()
  aws_bucket_delete(bucket = bucket_name, force = TRUE)
  aws_buckets()
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other buckets: 
\code{\link{aws_bucket_create}()},
\code{\link{aws_bucket_download}()},
\code{\link{aws_bucket_exists}()},
\code{\link{aws_bucket_list_objects}()},
\code{\link{aws_bucket_tree}()},
\code{\link{aws_bucket_upload}()},
\code{\link{aws_buckets}()},
\code{\link{six_bucket_delete}()},
\code{\link{six_bucket_upload}()}
}
\concept{buckets}
