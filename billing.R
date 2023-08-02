library(tidyverse)
library(paws)

Sys.setenv(
  AWS_ACCESS_KEY_ID = "",
  AWS_SECRET_ACCESS_KEY = "",
  AWS_REGION = "us-east-1"
)

ce <- paws::costexplorer()

raw_billing_data <- ce$get_cost_and_usage(
  TimePeriod = list(Start = "2023-01-01", End = as.character(Sys.Date())),
  Granularity = "DAILY",
  Metrics = "UnblendedCost",
  GroupBy = list(
    list(Type = "DIMENSION", Key = "SERVICE"),
    list(Type = "DIMENSION", Key = "LINKED_ACCOUNT")
  )
)

billing_data <- raw_billing_data$ResultsByTime %>%
  map(function(x){
    tibble(Date = x$TimePeriod$Start,
           Service = x$Groups %>% map(function(y){y$Keys}) %>% map_chr(~.x[1]),
           Linked_Account = x$Groups %>% map(function(y){y$Keys}) %>% map_chr(~.x[2]),
           Unblended_Cost = x$Groups %>% map(function(y){y$Metrics$UnblendedCost$Amount}) %>% unlist() %>% as.double())
  }) %>%
  list_rbind()

raw_billing_data <- ce$get_cost_and_usage(
  TimePeriod = list(Start = "2023-01-01", End = as.character(Sys.Date())),
  Granularity = "DAILY",
  Metrics = "BlendedCost",
  GroupBy = list(
    list(Type = "DIMENSION", Key = "SERVICE"),
    list(Type = "DIMENSION", Key = "LINKED_ACCOUNT")
  )
)

billing_data <- raw_billing_data$ResultsByTime %>%
  map(function(x){
    tibble(Date = x$TimePeriod$Start,
           Service = x$Groups %>% map(function(y){y$Keys}) %>% map_chr(~.x[1]),
           Linked_Account = x$Groups %>% map(function(y){y$Keys}) %>% map_chr(~.x[2]),
           Blended_Cost = x$Groups %>% map(function(y){y$Metrics$BlendedCost$Amount}) %>% unlist() %>% as.double())
  }) %>%
  list_rbind()

ubilling_data$Unblended_Cost |> sum()
billing_data$Blended_Cost |> sum()
