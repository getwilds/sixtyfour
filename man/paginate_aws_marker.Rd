% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{paginate_aws_marker}
\alias{paginate_aws_marker}
\title{Paginate over list_* methods with Marker/IsTruncated}
\usage{
paginate_aws_marker(fun, target, ...)
}
\arguments{
\item{fun}{(character) the name of a function to call - not the function
itself}

\item{target}{(character) a list element to get}

\item{...}{named args passed on to \code{fun}}
}
\description{
Currently works for IAM only - i.e., IAM is hard-coded internally
}
\keyword{internal}
