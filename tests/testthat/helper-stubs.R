library(webmockr)

# vcr::vcr_configure(dir = "tests/fixtures")
# cas <- vcr::use_cassette("billing_last_two_days", {
#   library(lubridate)
#   start_date <- today() - days(2)
#   aws_billing(date_start = start_date)
# })

# cas_yaml <- yaml::yaml.load_file(cas$file())
# responses <- vapply(cas_yaml$http_interactions, \(w) w$response$body$string, "")

# random_int <- function() {
# 	as.character(round(runif(1, 10^10, 10^12)))
# }

# library(glue)
# library(jqr)

# randomize_json <- function(json_str, which) {
# 	jq(json_str, ".DimensionValueAttributes[].Attributes.description |= \"some-org\"") %>%
# 		jq(glue(".DimensionValueAttributes[].Attributes.value |= \"{random_int()}\"")) %>%
# 		jq(glue(".DimensionValueAttributes[].Value |= \"{random_int()}\"")) %>%
# 		jq(glue(".ResultsByTime[].Groups[].Keys[1] |= \"{random_int()}\"")) %>%
# 		jq(glue(".ResultsByTime[].Groups[].Metrics.{which}.Amount |= \"{round(runif(1, 0, 10), digits = 8)}\""))
# }
# randomize_json(responses[1], "UnblendedCost") %>% jsonlite::prettify(indent = 2)
# randomize_json(responses[2], "BlendedCost") %>% jsonlite::prettify(indent = 2)

response_billing_unblended_1 <- '{
  "DimensionValueAttributes": [
    {
      "Attributes": {
        "description": "some-org",
        "value": "946728054637"
      },
      "Value": "336503573358"
    }
  ],
  "GroupDefinitions": [
    {
      "Key": "SERVICE",
      "Type": "DIMENSION"
    },
    {
      "Key": "LINKED_ACCOUNT",
      "Type": "DIMENSION"
    }
  ],
  "ResultsByTime": [
    {
      "Estimated": true,
      "Groups": [
        {
          "Keys": [
            "AWS Cost Explorer",
            "224587672609"
          ],
          "Metrics": {
            "UnblendedCost": {
              "Amount": "4.41095285",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "AWS Secrets Manager",
            "224587672609"
          ],
          "Metrics": {
            "UnblendedCost": {
              "Amount": "4.41095285",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "Amazon Simple Storage Service",
            "224587672609"
          ],
          "Metrics": {
            "UnblendedCost": {
              "Amount": "4.41095285",
              "Unit": "USD"
            }
          }
        }
      ],
      "TimePeriod": {
        "End": "2024-10-04",
        "Start": "2024-10-03"
      },
      "Total": {

      }
    },
    {
      "Estimated": true,
      "Groups": [
        {
          "Keys": [
            "AWS Cost Explorer",
            "224587672609"
          ],
          "Metrics": {
            "UnblendedCost": {
              "Amount": "4.41095285",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "AWS Secrets Manager",
            "224587672609"
          ],
          "Metrics": {
            "UnblendedCost": {
              "Amount": "4.41095285",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "Amazon Simple Storage Service",
            "224587672609"
          ],
          "Metrics": {
            "UnblendedCost": {
              "Amount": "4.41095285",
              "Unit": "USD"
            }
          }
        }
      ],
      "TimePeriod": {
        "End": "2024-10-05",
        "Start": "2024-10-04"
      },
      "Total": {

      }
    }
  ]
}'

response_billing_blended_1 <- '{
  "DimensionValueAttributes": [
    {
      "Attributes": {
        "description": "some-org",
        "value": "787333819538"
      },
      "Value": "988949372892"
    }
  ],
  "GroupDefinitions": [
    {
      "Key": "SERVICE",
      "Type": "DIMENSION"
    },
    {
      "Key": "LINKED_ACCOUNT",
      "Type": "DIMENSION"
    }
  ],
  "ResultsByTime": [
    {
      "Estimated": true,
      "Groups": [
        {
          "Keys": [
            "AWS Cost Explorer",
            "62859488630"
          ],
          "Metrics": {
            "BlendedCost": {
              "Amount": "7.11790631",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "AWS Secrets Manager",
            "62859488630"
          ],
          "Metrics": {
            "BlendedCost": {
              "Amount": "7.11790631",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "Amazon Simple Storage Service",
            "62859488630"
          ],
          "Metrics": {
            "BlendedCost": {
              "Amount": "7.11790631",
              "Unit": "USD"
            }
          }
        }
      ],
      "TimePeriod": {
        "End": "2024-10-04",
        "Start": "2024-10-03"
      },
      "Total": {

      }
    },
    {
      "Estimated": true,
      "Groups": [
        {
          "Keys": [
            "AWS Cost Explorer",
            "62859488630"
          ],
          "Metrics": {
            "BlendedCost": {
              "Amount": "7.11790631",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "AWS Secrets Manager",
            "62859488630"
          ],
          "Metrics": {
            "BlendedCost": {
              "Amount": "7.11790631",
              "Unit": "USD"
            }
          }
        },
        {
          "Keys": [
            "Amazon Simple Storage Service",
            "62859488630"
          ],
          "Metrics": {
            "BlendedCost": {
              "Amount": "7.11790631",
              "Unit": "USD"
            }
          }
        }
      ],
      "TimePeriod": {
        "End": "2024-10-05",
        "Start": "2024-10-04"
      },
      "Total": {

      }
    }
  ]
}'
