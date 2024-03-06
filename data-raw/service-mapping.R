## code to prepare `service_mapping` dataset goes here

# originally from https://tommymaynard.com/aws-service-acronyms/

service_map <- readr::read_csv("data-raw/service-mapping.csv")
usethis::use_data(service_map, overwrite = TRUE)
