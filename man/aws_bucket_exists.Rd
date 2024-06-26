% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bucket.R
\name{aws_bucket_exists}
\alias{aws_bucket_exists}
\title{Check if an S3 bucket exists}
\usage{
aws_bucket_exists(bucket)
}
\arguments{
\item{bucket}{(character) bucket name; must be length 1. required}
}
\value{
a single boolean (logical)
}
\description{
Check if an S3 bucket exists
}
\note{
internally uses
\href{https://www.paws-r-sdk.com/docs/s3_head_bucket/}{head_bucket}
}
\examples{
\dontrun{
# exists
aws_bucket_exists(bucket = "s64-test-2")
# does not exist
aws_bucket_exists(bucket = "no-bucket")
}
}
\seealso{
Other buckets: 
\code{\link{aws_bucket_create}()},
\code{\link{aws_bucket_delete}()},
\code{\link{aws_bucket_download}()},
\code{\link{aws_bucket_list_objects}()},
\code{\link{aws_bucket_tree}()},
\code{\link{aws_bucket_upload}()},
\code{\link{aws_buckets}()},
\code{\link{six_bucket_delete}()}
}
\concept{buckets}
