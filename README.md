# sixtyfour

<!-- badges: start -->
[![Project Status: Concept – Not useable, no support, not open to feedback, unstable API.](https://getwilds.github.io/badges/badges/concept.svg)](https://getwilds.github.io/badges/#concept)
[![R-CMD-check](https://github.com/getwilds/sixtyfour/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/getwilds/sixtyfour/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A science-focused, more humane R interface to AWS.

## Installation

Development version


```r
# install.packages("pak")
pak::pkg_install("getwilds/sixtyfour")
```

## Setting the interface

The function `sixtyfour::set_s3_interface` makes it easier to toggle between S3 compatible backends; right now only supporting AWS S3 itself and [Minio](https://min.io/).

## Code of Conduct

  Please note that the sixtyfour project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
