% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vpcs.R
\name{aws_vpcs}
\alias{aws_vpcs}
\title{List VPCs}
\usage{
aws_vpcs(...)
}
\arguments{
\item{...}{parameters passed on to \href{https://www.paws-r-sdk.com/docs/ec2_describe_vpcs/}{describe_vpcs}}
}
\value{
(list) list with VPCs, see \code{\link[=aws_vpc]{aws_vpc()}} for details
}
\description{
List VPCs
}
\examples{
\dontshow{if (interactive() && aws_has_creds()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
aws_vpcs()
aws_vpcs(MaxResults = 6)
\dontshow{\}) # examplesIf}
}
