#' sixtyfour: Humane Interface to AWS
#'
#' Write some more things here to give folks a
#' sense of what the goal of this package is,
#' high level entrypoints, etc.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>% %<>%
#' @importFrom paws s3 iam costexplorer
#' @importFrom s3fs s3_file_system
#' @importFrom glue glue
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom curl curl_fetch_memory
## usethis namespace: end
NULL

#' Mapping of full names of AWS services to acronyms
#'
#' @format ## `service_map`
#' A data frame with 178 rows and 2 columns:
#' \describe{
#'   \item{service}{Service name in full}
#'   \item{acronym}{The acronym, from 2 to 5 characters in length}
#'   ...
#' }
#' @source <https://tommymaynard.com/aws-service-acronyms/>
"service_map"
