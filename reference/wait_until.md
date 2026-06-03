# wait fxn generator

wait fxn generator

## Usage

``` r
wait_until(fun, message)
```

## Arguments

- fun:

  (function) a function to check status of something; must return a
  single boolean, e.g., `aws_db_cluster_status` or
  `aws_db_instance_status`

- message:

  (character) the message to print at the beginning of
  [`cli::cli_progress_bar`](https://cli.r-lib.org/reference/cli_progress_bar.html)
