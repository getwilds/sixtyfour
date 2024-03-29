% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{path_as_s3}
\alias{path_as_s3}
\title{Convert a s3 like path to a single format}
\usage{
path_as_s3(paths)
}
\arguments{
\item{paths}{(character) one or more s3 paths}
}
\value{
vector of s3 paths (character), Of the form:
\verb{s3://<bucket>/<path>/<file>}
}
\description{
Convert a s3 like path to a single format
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
path_as_s3("http://s64-test-3.s3.amazonaws.com/")
path_as_s3("https://s64-test-3.s3.amazonaws.com/")
path_as_s3(c(
  "https://s64-test-3.s3.amazonaws.com/",
  "https://mybucket.s3.amazonaws.com/"
))
path_as_s3(c("apple", "banana", "pear", "pineapple"))
\dontshow{\}) # examplesIf}
}
\keyword{internal}
