# Paginate over list\_\* methods with Marker/IsTruncated

Currently works for IAM only - i.e., IAM is hard-coded internally

## Usage

``` r
paginate_aws_marker(fun, target, ...)
```

## Arguments

- fun:

  (character) the name of a function to call - not the function itself

- target:

  (character) a list element to get

- ...:

  named args passed on to `fun`
