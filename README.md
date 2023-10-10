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

- billing: get AWS billing details
- files: manage files on AWS
- users: manage users on AWS
- database: interact with AWS databases


## Getting Started

You'll need two AWS secrets and the an AWS region:

```
Sys.setenv(
  AWS_ACCESS_KEY_ID = "",
  AWS_SECRET_ACCESS_KEY = "",
  AWS_REGION = "us-west-2"
)
```
