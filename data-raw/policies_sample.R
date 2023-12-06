## code to prepare `policies_sample` dataset goes here

library(sixtyfour)
policies_sample <- aws_policies()[1:50, ]
usethis::use_data(policies_sample, internal = TRUE, overwrite = TRUE)
