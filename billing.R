library(tidyverse)
library(paws)

Sys.setenv(
  AWS_ACCESS_KEY_ID = "",
  AWS_SECRET_ACCESS_KEY = "",
  AWS_REGION = "us-east-1"
)

ce <- paws::costexplorer()

raw_billing_data <- ce$get_cost_and_usage(
  TimePeriod = list(Start = "2023-01-01", End = "2023-05-10"),
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
