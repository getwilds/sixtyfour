<!-- Don't edit README.md! Edit README.Rmd, then run `make readme` -->

# sixtyfour

<!-- badges: start -->
[![Project Status: Prototype – Useable, some support, open to feedback, unstable API.](https://getwilds.org/badges/badges/prototype.svg)](https://getwilds.org/badges/#prototype)
[![R-CMD-check](https://github.com/getwilds/sixtyfour/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/getwilds/sixtyfour/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/getwilds/sixtyfour/branch/main/graph/badge.svg)](https://app.codecov.io/gh/getwilds/sixtyfour)
<!-- badges: end -->

A science-focused, more humane R interface to AWS.

## Installation

CRAN version


``` r
# install.packages("pak")
pak::pak("sixtyfour")
```

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

See the next release ([v0.3](https://github.com/getwilds/sixtyfour/milestone/3)).


[paws]: https://www.paws-r-sdk.com/
[s3fs]: https://dyfanjones.github.io/s3fs/
[vigncontrib]: http://getwilds.org/sixtyfour/articles/contributing.html
