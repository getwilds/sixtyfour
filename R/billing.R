#' Fetch billing data - with some internal munging for ease of use
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr list_rbind
#' @importFrom rlang :=
#' @importFrom dplyr rename rename_with left_join coalesce
#' @param date_start,date_end Start and end date to get billing data for.
#' Date format expected: `yyyy-MM-dd`. required
#' @param filter (list) filters costs by different dimensions. optional.
#' @autoglobal
#' @references <https://www.paws-r-sdk.com/docs/costexplorer/>
#' @family billing
#' @section Blended vs. Unblended:
#' - Unblended: Unblended costs represent your usage costs on the day
#' they are charged to you
#' - Blended: Blended costs are calculated by multiplying each account’s
#' service usage against something called a blended rate. A blended rate
#' is the average rate of on-demand usage, as well as Savings Plans- and
#' reservation-related usage, that is consumed by member accounts in an
#' organization for a particular service.
#' @section Historical data:
#' If you supply a `date_start` older than 14 months prior to today's date
#' you will likely see an error like "You haven't enabled historical data
#' beyond 14 months". See
#' <https://docs.aws.amazon.com/cost-management/latest/userguide/ce-advanced-cost-analysis.html> #nolint
#' for help
#'
#' @section Filtering:
#' You can optionally pass a list to the `filter` argument to filter AWS costs
#' by different dimensions, tags, or cost categories. This filter expression is
#' passed on to
#' [paws](https://www.paws-r-sdk.com/docs/costexplorer_get_cost_and_usage/). See
#' possible dimensions:
#' <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html>) #nolint
#'
#' This is supplied as a list, with key-value pairs for each criteria.
#' Different filter criteria can be combined in different ways using `AND`,
#' `OR`, and `NOT`. See Examples below and more on Filter expressions at
#' <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html>. #nolint
#'
#' @return tibble with columns:
#' - id: "blended", "unblended"
#' - date: date, in format `yyyy-MM-dd`
#' - service: AWS service name, spelled out in full
#' - linked_account: account number
#' - cost: cost in USD
#' - acronym: short code for the service; if none known, this row
#' will have the value in `service`
#' @examplesIf interactive()
#' library(lubridate)
#' library(dplyr)
#'
#' start_date <- today() - months(13)
#' z <- aws_billing(date_start = start_date)
#' z %>%
#'   filter(id == "blended") %>%
#'   group_by(service) %>%
#'   summarise(sum_cost = sum(cost)) %>%
#'   filter(sum_cost > 0) %>%
#'   arrange(desc(sum_cost))
#'
#' z %>%
#'   filter(id == "blended") %>%
#'   filter(cost > 0) %>%
#'   arrange(service)
#'
#' z %>%
#'   filter(id == "blended") %>%
#'   group_by(service) %>%
#'   summarise(sum_cost = sum(cost)) %>%
#'   filter(service == "Amazon Relational Database Service")
#'
#' # Simple filter to return only "Usage" costs:
#' aws_billing(
#'   date_start = start_date,
#'   filter = list(
#'     Dimensions = list(
#'       Key = "RECORD_TYPE",
#'       Values = "Usage"
#'     )
#'   )
#' )
#'
#' # Filter to return "Usage" costs for only m4.xlarge instances:
#' aws_billing(
#'   date_start = start_date,
#'   filter = list(
#'     And = list(
#'       list(
#'         Dimensions = list(
#'           Key = "RECORD_TYPE",
#'           Values = list("Usage")
#'         )
#'       ),
#'       list(
#'         Dimensions = list(
#'           Key = "INSTANCE_TYPE",
#'           Values = list("m4.xlarge")
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' # Complex filter example, translated from the AWS Cost Explorer docs:
#' # <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html> #nolint
#' # Filter for operations within us-west-1 or us-west-2 regions OR have a
#' # specific Tag value, AND are NOT DataTransfer usage types:
#' aws_billing(
#'   date_start = start_date,
#'   filter = list(
#'     And = list(
#'       list(
#'         Or = list(
#'           list(
#'             Dimensions = list(
#'               Key = "REGION",
#'               Values = list("us-east-1", "us-west-1")
#'             )
#'           ),
#'           list(
#'             Tags = list(
#'               Key = "TagName",
#'               Values = list("Value1")
#'             )
#'           )
#'         )
#'       ),
#'       list(
#'         Not = list(
#'           Dimensions = list(
#'             Key = "USAGE_TYPE",
#'             Values = list("DataTransfer")
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
aws_billing <- function(
    date_start,
    date_end = as.character(Sys.Date()),
    filter = NULL) {
  bind_rows(
    unblended = rename(
      billing_unblended(date_start, date_end, filter = filter),
      cost = UnblendedCost
    ),
    blended = rename(
      billing_blended(date_start, date_end, filter = filter),
      cost = BlendedCost
    ),
    .id = "id"
  ) %>% rename_with(tolower)
}

#' function factory to create functions for both blended and unblended data
#' @autoglobal
#' @keywords internal
#' @noRd
billing_factory <- function(type) {
  function(date_start, date_end, filter = NULL) {
    groupby <- list(
      list(Type = "DIMENSION", Key = "SERVICE"),
      list(Type = "DIMENSION", Key = "LINKED_ACCOUNT")
    )
    raw_billing_data <- aws_billing_raw(
      date_start,
      metrics = type,
      granularity = "daily",
      group_by = groupby,
      date_end = date_end,
      filter = filter
    )
    raw_billing_data$ResultsByTime %>%
      map(function(x) {
        tibble(
          Date = x$TimePeriod$Start,
          Service = x$Groups %>%
            map(function(y) {
              y$Keys
            }) %>%
            map_chr(~ .x[1]),
          Linked_Account = x$Groups %>%
            map(function(y) {
              y$Keys
            }) %>%
            map_chr(~ .x[2]),
          "{type}" := x$Groups %>% # nolint
            map(function(y) {
              y$Metrics[[type]]$Amount
            }) %>%
            unlist() %>%
            as.double()
        )
      }) %>%
      list_rbind() %>%
      left_join(sixtyfour::service_map, by = c("Service" = "service")) %>%
      mutate(acronym = coalesce(acronym, Service))
  }
}

billing_unblended <- billing_factory("UnblendedCost")
billing_blended <- billing_factory("BlendedCost")

#' Fetch billing data - rawest form
#' @export
#' @inheritParams aws_billing
#' @param metrics (character) which metrics to return. required. One of:
#' AmortizedCost, BlendedCost, NetAmortizedCost, NetUnblendedCost,
#' NormalizedUsageAmount, UnblendedCost, and UsageQuantity
#' @param granularity (character) monthly, daily, hourly. required.
#' @param filter (list) filters costs by different dimensions. optional.
#' @param group_by (list) group costs using up to two different groups,
#' either dimensions, tag keys, cost categories, or any two group by types.
#' optional.
#' @family billing
#' @return list with slots for:
#' - NextPageToken
#' - GroupDefinitions
#' - ResultsByTime
#' - DimensionValueAttributes
#' @examplesIf interactive()
#' aws_billing_x(date_start = "2023-02-01", metrics = "BlendedCost")
aws_billing_raw <- function(
    date_start, metrics, granularity = "daily",
    filter = NULL, group_by = NULL, date_end = as.character(Sys.Date())) {
  grans <- c("hourly", "daily", "monthly")
  stopifnot(
    "`granularity` must be one of hourly/daily/monthly" =
      granularity %in% grans
  )
  con_ce()$get_cost_and_usage(
    TimePeriod = list(Start = date_start, End = date_end),
    Granularity = toupper(granularity),
    Filter = filter,
    Metrics = metrics,
    GroupBy = group_by
  )
}
