#' Fetch billing data
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr list_rbind
#' @importFrom rlang :=
#' @param date_start,date_end Start and end date to get billing data for.
#' Date format expected: `YYYY-MM-DD`
#' @examples \dontrun{
#' billing(date_start="2023-01-01")
#' }
billing <- function(date_start, date_end = as.character(Sys.Date())) {
  # TODO: assertions on date formats, and possibly max date - check to
  # see if paws does any date validation first
  list(
    unblended = billing_unblended(date_start, date_end),
    blended = billing_blended(date_start, date_end)
  )
}

# function factory to create functions for both blended and unblended data
billing_factory <- function(type) {
  function(date_start, date_end) {
    raw_billing_data <- env64$costexplorer$get_cost_and_usage(
      TimePeriod = list(Start = date_start, End = date_end),
      Granularity = "DAILY",
      Metrics = type,
      GroupBy = list(
        list(Type = "DIMENSION", Key = "SERVICE"),
        list(Type = "DIMENSION", Key = "LINKED_ACCOUNT")
      )
    )

    raw_billing_data$ResultsByTime %>%
      map(function(x){
        tibble(Date = x$TimePeriod$Start,
               Service = x$Groups %>% map(function(y){y$Keys}) %>% map_chr(~.x[1]),
               Linked_Account = x$Groups %>% map(function(y){y$Keys}) %>% map_chr(~.x[2]),
               "{type}" := x$Groups %>%
                map(function(y){y$Metrics[[type]]$Amount}) %>%
                unlist() %>%
                as.double())
      }) %>%
      list_rbind()
  }
}

billing_unblended <- billing_factory("UnblendedCost")
billing_blended <- billing_factory("BlendedCost")
