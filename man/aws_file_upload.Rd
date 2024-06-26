% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{aws_file_upload}
\alias{aws_file_upload}
\title{Upload a file}
\usage{
aws_file_upload(path, remote_path, ...)
}
\arguments{
\item{path}{(character) a file path to read from. required}

\item{remote_path}{(character) a remote path where the file
should go. required}

\item{...}{named parameters passed on to \code{s3fs::s3_file_copy()}}
}
\value{
(character) a vector of remote s3 paths
}
\description{
Upload a file
}
\details{
to upload a folder of files see \code{\link[=aws_bucket_upload]{aws_bucket_upload()}}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
bucket <- random_string("bucket")
aws_bucket_create(bucket)
demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
aws_file_upload(
  demo_rds_file,
  s3_path(bucket, basename(demo_rds_file))
)

## many files at once
links_file <- file.path(system.file(), "Meta/links.rds")
aws_file_upload(
  c(demo_rds_file, links_file),
  s3_path("s64-test-2", c(basename(demo_rds_file), basename(links_file)))
)

# set expiration, expire 1 minute from now
aws_file_upload(demo_rds_file, s3_path("s64-test-2", "ddd.rds"),
  Expires = Sys.time() + 60
)

# bucket doesn't exist
aws_file_upload(demo_rds_file, "s3://not-a-bucket/eee.rds")

# path doesn't exist
aws_file_upload(
  "file_doesnt_exist.txt",
  s3_path("s64-test-2", "file_doesnt_exist.txt")
)

# Path's without file extensions behave a little weird
## With extension
## Both of these lines do the same exact thing: make a file in the
## same path in a bucket
aws_file_upload("LICENSE.md", s3_path(bucket, "LICENSE.md"))
aws_file_upload("LICENSE.md", s3_path(bucket))

## Without extension
## However, it's different for a file without an extension
## This makes a file in the bucket at path DESCRIPTION
aws_file_upload("DESCRIPTION", s3_path(bucket))

## Whereas this creates a directory called DESCRIPTION with
## a file DESCRIPTION within it
aws_file_upload("DESCRIPTION", s3_path(bucket, "DESCRIPTION"))
\dontshow{\}) # examplesIf}
}
\seealso{
Other files: 
\code{\link{aws_file_attr}()},
\code{\link{aws_file_copy}()},
\code{\link{aws_file_delete}()},
\code{\link{aws_file_download}()},
\code{\link{aws_file_exists}()},
\code{\link{aws_file_rename}()},
\code{\link{six_file_upload}()}
}
\concept{files}
