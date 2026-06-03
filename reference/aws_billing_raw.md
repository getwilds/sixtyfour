# Fetch billing data - rawest form

Fetch billing data - rawest form

## Usage

``` r
aws_billing_raw(
  date_start,
  metrics,
  granularity = "daily",
  filter = NULL,
  group_by = NULL,
  date_end = as.character(Sys.Date())
)
```

## Arguments

- date_start, date_end:

  Start and end date to get billing data for. Date format expected:
  `yyyy-MM-dd`. required

- metrics:

  (character) which metrics to return. required. One of: AmortizedCost,
  BlendedCost, NetAmortizedCost, NetUnblendedCost,
  NormalizedUsageAmount, UnblendedCost, and UsageQuantity

- granularity:

  (character) monthly, daily, hourly. required.

- filter:

  (list) filters costs by different dimensions. optional.

- group_by:

  (list) group costs using up to two different groups, either
  dimensions, tag keys, cost categories, or any two group by types.
  optional.

## Value

list with slots for:

- NextPageToken

- GroupDefinitions

- ResultsByTime

- DimensionValueAttributes

## See also

Other billing:
[`aws_billing()`](https://getwilds.org/sixtyfour/reference/aws_billing.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
library(lubridate)
aws_billing_raw(date_start = today() - days(3), metrics = "BlendedCost")
}
```
