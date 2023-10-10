# sixtyfour

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/fhdsl/sixtyfour/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fhdsl/sixtyfour/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A science-focused, more humane R interface to AWS.

## Installation

Development version

``` r
# install.packages("pak")
pak::pak("seankross/sixtyfour")
```

This package is not on CRAN (yet)

## sixtyfour high level organization

- `aws_billing`: get AWS billing details
- `aws_bucket*`: manage S3 buckets
- `aws_file_*`: manage files in S3 buckets on AWS
- `aws_user*`: manage users on AWS
- `aws_db*`: interact with AWS databases


## Getting Started

You'll need two AWS secrets and the an AWS region:

```
Sys.setenv(
  AWS_ACCESS_KEY_ID = "",
  AWS_SECRET_ACCESS_KEY = "",
  AWS_REGION = "us-west-2"
)
```

## Code of Conduct

  Please note that the sixtyfour project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
