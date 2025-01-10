<!-- Don't edit README.md! Edit README.Rmd, then run `make readme` -->

# sixtyfour

<!-- badges: start -->
[![Project Status: Prototype – Useable, some support, open to feedback, unstable API.](https://getwilds.org/badges/badges/prototype.svg)](https://getwilds.org/badges/#prototype)
[![R-CMD-check](https://github.com/getwilds/sixtyfour/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/getwilds/sixtyfour/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/getwilds/sixtyfour/graph/badge.svg?token=BMER9MWIDN)](https://codecov.io/gh/getwilds/sixtyfour)
<!-- badges: end -->

A science-focused, more humane R interface to AWS.

## Installation

Development version


``` r
# install.packages("pak")
pak::pak("getwilds/sixtyfour")
```

## Documentation

Go to <http://getwilds.org/sixtyfour/> for `sixtyfour` package documentation. Go to the **Get Started** vignette to get started.

This package leans primarily on the packages [paws][] and [s3fs][].

## Bugs? Features?

Open an issue on our [issue tracker](https://github.com/getwilds/sixtyfour/issues/).

## Contributors

See the [Contributing article][vigncontrib]

This package follows [Git Flow](https://nvie.com/posts/a-successful-git-branching-model/). See the [Contributing guide][vigncontrib] for details.

## Code of Conduct

Please note that the sixtyfour project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Roadmap

For the next release:

- Support for row level security in PostgreSQL and Redshift (via `tablemanners` package) ([#75](https://github.com/getwilds/sixtyfour/issues/75)) - for this package, the change may only be documentation on how to use `tablemanners` with `sixtyfour`
- Cookbook docs based around common usage patterns ([#27](https://github.com/getwilds/sixtyfour/issues/27))
- Finish magic function `six_bucket_upload` ([#67](https://github.com/getwilds/sixtyfour/issues/67))
- Send to CRAN ([#68](https://github.com/getwilds/sixtyfour/issues/68))


[paws]: https://www.paws-r-sdk.com/
[s3fs]: https://dyfanjones.github.io/s3fs/
[minio]: https://min.io/
[vigncontrib]: http://getwilds.org/sixtyfour/articles/contributing.html
